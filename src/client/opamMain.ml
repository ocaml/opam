(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open OpamTypes
open OpamStateTypes
open OpamTypesBase
open OpamStd.Op

(* Handle git-like plugins *)
let check_and_run_external_commands () =
  let plugin_prefix = "opam-" in
  match Array.to_list Sys.argv with
  | [] | [_] -> ()
  | _ :: ("-y" | "--yes") :: name :: args
  | _ :: name :: args ->
    if
      not (OpamStd.String.starts_with ~prefix:"-" name)
      && List.for_all (fun (_,info) ->
          not (OpamStd.String.starts_with ~prefix:name (Term.name info)))
        OpamCommands.commands
    then
      (* No such command, check if there is a matching plugin *)
      let command = plugin_prefix ^ name in
      let answer = match Sys.argv.(1) with
        | "-y" | "--yes" -> Some true
        | _ -> OpamStd.Config.env_bool "YES"
      in
      OpamStd.Config.init ~answer ();
      OpamFormatConfig.init ();
      let root_dir = OpamStateConfig.opamroot () in
      let has_init = OpamStateConfig.load_defaults root_dir <> None in
      let plugins_bin = OpamPath.plugins_bin root_dir in
      let env =
        if has_init then
          let updates =
            ["PATH", PlusEq, OpamFilename.Dir.to_string plugins_bin, None]
          in
          OpamStateConfig.init ~root_dir ();
          match OpamStateConfig.get_switch_opt () with
          | None -> env_array (OpamEnv.get_pure ~updates ())
          | Some sw ->
            env_array
              (OpamEnv.full_with_path ~force_path:false ~updates root_dir sw)
        else
          Unix.environment ()
      in
      match OpamSystem.resolve_command ~env command with
      | Some command ->
        let argv = Array.of_list (command :: args) in
        raise (OpamStd.Sys.Exec (command, argv, env))
      | None when not has_init -> ()
      | None ->
        (* Look for a corresponding package *)
        match OpamStateConfig.get_switch_opt () with
        | None -> ()
        | Some sw ->
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          OpamSwitchState.with_ `Lock_none gt ~switch:sw @@ fun st ->
          let prefixed_name = plugin_prefix ^ name in
          let candidates =
            OpamPackage.packages_of_names
              (Lazy.force st.available_packages)
              (OpamPackage.Name.Set.of_list @@
               (OpamStd.List.filter_map
                  (fun s ->
                     try Some (OpamPackage.Name.of_string s)
                     with Failure _ -> None)
                  [ prefixed_name; name ]))
          in
          let plugins =
            OpamPackage.Set.filter (fun nv ->
                OpamFile.OPAM.has_flag Pkgflag_Plugin (OpamSwitchState.opam st nv))
              candidates
          in
          let installed = OpamPackage.Set.inter plugins st.installed in
          if OpamPackage.Set.is_empty candidates then ()
          else if not OpamPackage.Set.(is_empty installed) then
            (OpamConsole.error
               "Plugin %s is already installed, but no %s command was found.\n\
                Try upgrading, and report to the package maintainer if \
                the problem persists."
               (OpamPackage.to_string (OpamPackage.Set.choose installed))
               command;
             exit (OpamStd.Sys.get_exit_code `Package_operation_error))
          else if OpamPackage.Set.is_empty plugins then
            (OpamConsole.error
               "%s is not a known command or plugin (package %s does \
                not have the 'plugin' flag set)."
               name
               (OpamPackage.to_string (OpamPackage.Set.max_elt candidates));
             exit (OpamStd.Sys.get_exit_code `Bad_arguments))
          else if
            OpamConsole.confirm "Opam plugin \"%s\" is not installed. \
                                 Install it on the current switch?"
              name
          then
            let nv =
              try
                OpamPackage.max_version plugins
                  (OpamPackage.Name.of_string prefixed_name)
              with Not_found ->
                OpamPackage.max_version plugins
                  (OpamPackage.Name.of_string name)
            in
            OpamRepositoryConfig.init ();
            OpamSolverConfig.init ();
            OpamClientConfig.init ();
            OpamSwitchState.with_ `Lock_write gt (fun st ->
                ignore @@
                OpamClient.install st [OpamSolution.eq_atom_of_package nv]
              );
            match OpamSystem.resolve_command ~env command with
            | None ->
              OpamConsole.error_and_exit `Package_operation_error
                "Plugin %s was installed, but no %s command was found.\n\
                 This is probably an error in the plugin package."
                (OpamPackage.to_string nv)
                command
            | Some command ->
              OpamConsole.header_msg "Carrying on to \"%s\""
                (String.concat " " (Array.to_list Sys.argv));
              OpamConsole.msg "\n";
              let argv = Array.of_list (command :: args) in
              raise (OpamStd.Sys.Exec (command, argv, env))

let run default commands =
  OpamStd.Option.iter OpamVersion.set_git OpamGitVersion.version;
  OpamSystem.init ();
  try
    check_and_run_external_commands ();
    let admin, argv1 =
      if Array.length Sys.argv > 1 && Sys.argv.(1) = "admin" then
        true,
        Array.init (Array.length Sys.argv - 1) (function
            | 0 -> Sys.argv.(0)
            | i -> Sys.argv.(i+1))
      else false, Sys.argv
    in
    let eval () =
      if admin then
        Term.eval_choice ~catch:false ~argv:argv1
          OpamAdminCommand.default_subcommand OpamAdminCommand.admin_subcommands
      else
        Term.eval_choice ~catch:false ~argv:argv1 default commands
    in
    match eval () with
    | `Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
    | _        -> exit (OpamStd.Sys.get_exit_code `Success)
  with
  | OpamStd.Sys.Exit 0 -> ()
  | OpamStd.Sys.Exec (cmd,args,env) ->
    OpamStd.Sys.exec_at_exit ();
    Unix.execvpe cmd args env
  | e                  ->
    flush stdout;
    flush stderr;
    if (OpamConsole.verbose ()) then
      Printf.eprintf "'%s' failed.\n" (String.concat " " (Array.to_list Sys.argv));
    let exit_code = match e with
      | OpamStd.Sys.Exit i ->
        if (OpamConsole.debug ()) && i <> 0 then
          Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
        i
      | OpamSystem.Internal_error _ ->
        Printf.eprintf "%s\n" (Printexc.to_string e);
        OpamStd.Sys.get_exit_code `Internal_error
      | OpamSystem.Process_error result ->
        Printf.eprintf "%s Command %S failed:\n%s\n"
          (OpamConsole.colorise `red "[ERROR]")
          (try List.assoc "command" result.OpamProcess.r_info with
           | Not_found -> "")
          (Printexc.to_string e);
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
      | Sys.Break
      | OpamParallel.Errors (_, (_, Sys.Break)::_, _) ->
        OpamStd.Sys.get_exit_code `User_interrupt
      | Sys_error e when e = "Broken pipe" ->
        (* workaround warning 52, this is a fallback (we already handle the
           signal) and there is no way around at the moment *)
        141
      | Failure msg ->
        Printf.eprintf "Fatal error: %s\n" msg;
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
      | _ ->
        Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
    in
    exit exit_code

let json_out () =
  match OpamClientConfig.(!r.json_out) with
  | None   -> ()
  | Some s ->
    let file_name () =
      match OpamStd.String.cut_at s '%' with
      | None -> OpamFilename.of_string s
      | Some (pfx, sfx) ->
        let rec getname i =
          let f = OpamFilename.of_string (Printf.sprintf "%s%d%s" pfx i sfx) in
          if OpamFilename.exists f then getname (i+1) else f
        in
        getname 1
    in
    try
      let f = OpamFilename.open_out (file_name ()) in
      OpamJson.flush f;
      close_out f
    with e ->
      OpamConsole.warning "Couldn't write json log: %s"
        (Printexc.to_string e)

let () =
  OpamStd.Sys.at_exit (fun () ->
      flush stderr;
      flush stdout;
      if OpamClientConfig.(!r.print_stats) then (
        OpamFile.Stats.print ();
        OpamSystem.print_stats ();
      );
      json_out ()
    );
  run OpamCommands.default OpamCommands.commands
