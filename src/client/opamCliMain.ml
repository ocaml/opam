(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
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

exception InvalidCLI of (OpamCLIVersion.t * provenance, string option) OpamCompat.Result.t

(* Filter and parse "--cli=v" or "--cli v" options *)
let rec filter_cli_arg cli acc args =
  match args with
  | []
  | "--" :: _ -> (cli, List.rev_append acc args)
  | "--cl" :: args -> filter_cli_arg cli acc ("--cli"::args)
  | ["--cli"] | "--cli" :: "--" :: _ ->
    raise (InvalidCLI(Error None))
  | "--cli" :: arg :: args ->
    let version =
      match OpamCLIVersion.of_string_opt arg with
      | Some cli ->
        if OpamCLIVersion.is_supported cli then
          cli
        else
          raise (InvalidCLI(Ok(cli, `Command_line)))
      | None -> raise (InvalidCLI(Error(Some arg)))
    in
    filter_cli_arg (Some version) acc args
  | arg :: args ->
    match OpamStd.String.cut_at arg '=' with
    | Some ("--cl", value)
    | Some ("--cli", value) ->
      filter_cli_arg cli acc ("--cli"::value::args)
    | _ ->
      filter_cli_arg cli (arg::acc) args

(* Pre-process argv processing the --yes and --cli. Returns Some cli, if --cli
   was encountered, a boolean indicating if a valid --yes/-y was encountered
   (it returns false if multiple flags were encountered) and the list of
   arguments to continue with processing. *)
let rec preprocess_argv cli yes args =
  let is_valid_yes = function [_] -> true | _ -> false in
  match args with
  | [] ->
    (cli, is_valid_yes yes, yes)
  | "--" :: _ ->
    (cli, is_valid_yes yes, yes @ args)
  (* Note that because this is evaluated before a sub-command, all the
     prefixes of --yes are assumed to valid at all times. *)
  | ("-y" | "--y" | "--ye" | "--yes") as yes_opt :: args ->
    if yes = [] then
      preprocess_argv cli [yes_opt] args
    else
      (cli, false, yes @ [yes_opt])
  | "--cl" :: args -> preprocess_argv cli yes ("--cli"::args)
  | ["--cli"] | "--cli" :: "--" :: _ ->
    raise (InvalidCLI(Error None))
  | "--cli" :: arg :: args ->
    let version =
      match OpamCLIVersion.of_string_opt arg with
      | Some cli ->
        if OpamCLIVersion.is_supported cli then
          cli
        else
          raise (InvalidCLI(Ok(cli, `Command_line)))
      | _ -> raise (InvalidCLI(Error(Some arg)))
    in
    preprocess_argv (Some version) yes args
  | arg :: rest ->
    match OpamStd.String.cut_at arg '=' with
    | Some ("--cl", value)
    | Some ("--cli", value) ->
      preprocess_argv cli yes ("--cli"::value::rest)
    | _ ->
      if OpamCommands.is_builtin_command arg then
        let (cli, rest) = filter_cli_arg cli [] rest in
        (cli, is_valid_yes yes, arg :: (yes @ rest))
      else
        (cli, is_valid_yes yes, args)

(* Handle git-like plugins *)
let check_and_run_external_commands () =
  let plugin_prefix = "opam-" in
  (* Pre-process the --yes and --cli options *)
  let (cli, yes, argv) =
    match Array.to_list Sys.argv with
    | prog::args ->
      let (cli, yes, args) = preprocess_argv None [] args in
      let cli =
        match cli with
        | Some cli ->
          if OpamCLIVersion.(cli < (2, 1)) then begin
            let cli = OpamCLIVersion.to_string cli in
            OpamConsole.warning
              "--cli is not supported by opam %s; setting OPAMCLI=%s \
               is more portable"
              cli cli
          end;
          (cli, `Command_line)
        | None ->
          match OpamCLIVersion.env "CLI" with
          | Some cli ->
            if OpamCLIVersion.is_supported cli then
              let () =
                if OpamCLIVersion.(cli >= (2, 1)) then
                  OpamConsole.warning
                    "Setting OPAMCLI is brittle - consider using the \
                     '--cli <major>.<minor>' flag."
              in
              cli, `Env
            else
              raise (InvalidCLI(Ok(cli, `Env)))
          | None ->
            OpamCLIVersion.current, `Default
      in
      (cli, yes, prog::args)
    | args -> ((OpamCLIVersion.current, `Default), false, args)
  in
  match argv with
  | [] | [_] -> (cli, argv)
  | _ :: name :: args ->
    if String.length name > 0 && name.[0] = '-'
      || OpamCommands.is_builtin_command name
    then (cli, argv)
    else
    (* No such command, check if there is a matching plugin *)
    let command = plugin_prefix ^ name in
    let answer = if yes then Some true else OpamStd.Config.env_bool "YES" in
    OpamStd.Config.init ~answer ();
    OpamFormatConfig.init ();
    let root_dir = OpamStateConfig.opamroot () in
    let has_init = OpamStateConfig.load_defaults root_dir <> None in
    let plugins_bin = OpamPath.plugins_bin root_dir in
    let env =
      if has_init then
        let updates =
          ["PATH", OpamParserTypes.PlusEq,
           OpamFilename.Dir.to_string plugins_bin, None]
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
    | None when not has_init -> (cli, argv)
    | None ->
      (* Look for a corresponding package *)
      match OpamStateConfig.get_switch_opt () with
      | None -> (cli, argv)
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
        if OpamPackage.Set.is_empty candidates then (cli, argv)
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
              OpamSwitchState.drop @@
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
        else (cli, argv)

let display_cli_error msg =
  Format.eprintf
    "@[<v>opam: @[%a@]@,@[Usage: @[opam COMMAND ...@]@]@,\
     Try `opam --help' for more information.@]@."
    Format.pp_print_text msg

let display_cli_error fmt =
  Format.ksprintf display_cli_error fmt

let rec main_catch_all f =
  try f () with
  | OpamStd.Sys.Exit 0 -> ()
  | OpamStd.Sys.Exec (cmd,args,env) ->
    OpamStd.Sys.exec_at_exit ();
    Unix.execvpe cmd args env
  | OpamFormatUpgrade.Upgrade_done conf ->
    main_catch_all @@ fun () ->
    OpamConsole.header_msg "Rerunning init and update";
    OpamClient.reinit ~interactive:true ~update_config:false conf
      (OpamStd.Sys.guess_shell_compat ());
    OpamConsole.msg
      "Update done, please now retry your command.\n";
    exit (OpamStd.Sys.get_exit_code `Aborted)
  | e ->
    flush stdout;
    flush stderr;
    if (OpamConsole.verbose ()) then
      OpamConsole.errmsg "'%s' failed.\n"
        (String.concat " " (Array.to_list Sys.argv));
    let exit_code = match e with
      | OpamStd.Sys.Exit i ->
        if (OpamConsole.debug ()) && i <> 0 then
          OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        i
      | OpamSystem.Internal_error _ ->
        OpamConsole.errmsg "%s\n" (Printexc.to_string e);
        OpamStd.Sys.get_exit_code `Internal_error
      | OpamSystem.Process_error result ->
        OpamConsole.errmsg "%s Command %S failed:\n%s\n"
          (OpamConsole.colorise `red "[ERROR]")
          (try List.assoc "command" result.OpamProcess.r_info with
           | Not_found -> "")
          (Printexc.to_string e);
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
      | Sys.Break
      | OpamParallel.Errors (_, (_, Sys.Break)::_, _) ->
        OpamStd.Sys.get_exit_code `User_interrupt
      | Sys_error e when e = "Broken pipe" ->
        (* workaround warning 52, this is a fallback (we already handle the
           signal) and there is no way around at the moment *)
        141
      | InvalidCLI (Ok(cli, source)) ->
        (* Unsupported CLI version *)
        let suffix =
          if source = `Env then
            " Please fix the value of the OPAMCLI environment variable, \
             or use the '--cli <major>.<minor>' flag"
          else
            ""
        in
        OpamConsole.error "opam command-line version %s is not supported.%s"
          (OpamCLIVersion.to_string cli) suffix;
        OpamStd.Sys.get_exit_code `Bad_arguments
      | InvalidCLI (Error None) ->
        (* No CLI version given *)
        display_cli_error "option `--cli' needs an argument";
        OpamStd.Sys.get_exit_code `Bad_arguments
      | InvalidCLI (Error (Some invalid)) ->
        (* Corrupt CLI version *)
        display_cli_error
          "option `--cli': invalid value `%s', expected major.minor"
          invalid;
        OpamStd.Sys.get_exit_code `Bad_arguments
      | Failure msg ->
        OpamConsole.errmsg "Fatal error: %s\n" msg;
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
      | _ ->
        OpamConsole.errmsg "Fatal error:\n%s\n" (Printexc.to_string e);
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e);
        OpamStd.Sys.get_exit_code `Internal_error
    in
    exit exit_code

let run () =
  OpamStd.Option.iter OpamVersion.set_git OpamGitVersion.version;
  OpamSystem.init ();
  main_catch_all @@ fun () ->
  let (cli, _), argv = check_and_run_external_commands () in
  let (default, commands), argv1 =
    match argv with
    | prog :: command :: argv when OpamCommands.is_admin_subcommand command ->
      OpamAdminCommand.get_cmdliner_parser cli, prog::argv
    | _ ->
      OpamCommands.get_cmdliner_parser cli, argv
  in
  let argv = Array.of_list argv1 in
  match Term.eval_choice ~catch:false ~argv default commands with
  | `Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
  | _        -> exit (OpamStd.Sys.get_exit_code `Success)

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

let main () =
  OpamStd.Sys.at_exit (fun () ->
      flush stderr;
      flush stdout;
      if OpamClientConfig.(!r.print_stats) then (
        OpamFile.Stats.print ();
        OpamSystem.print_stats ();
      );
      json_out ()
    );
  run ()
