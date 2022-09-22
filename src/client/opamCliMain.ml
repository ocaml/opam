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
open OpamStateTypes
open OpamTypesBase
open OpamStd.Op

exception InvalidCLI of OpamCLIVersion.Sourced.t

(* [InvalidFlagContent (flag_name, Some (invalid_value, expected_value))] *)
exception InvalidFlagContent of string * (string * string) option

(* [InvalidNewFlag (requested_cli, flag_name, flag_valid_since)] *)
exception InvalidNewFlag of OpamCLIVersion.Sourced.t * string * OpamCLIVersion.t

let raise_invalid_cli :
  (OpamCLIVersion.Sourced.t, string option) result -> 'a
  = function
    | Ok ocli -> raise (InvalidCLI ocli)
    | Error None -> raise (InvalidFlagContent ("cli", None))
    | Error (Some invalid) ->
      raise (InvalidFlagContent ("cli", Some (invalid, "major.minor")))

let raise_invalid_confirm_level invalid =
  let invalid =
    OpamStd.Option.map (fun i ->
        i, "one of " ^
           (OpamArg.confirm_enum
            |> List.map (fun (_,s,_) -> Printf.sprintf "`%s'" s)
            |> OpamStd.Format.pretty_list ~last:"or"))
      invalid
  in
  raise (InvalidFlagContent ("confirm-level", invalid))

(* Filter and parse "--cli=v" or "--cli v" options *)
let rec filter_cli_arg cli acc args =
  match args with
  | []
  | "--" :: _ -> (cli, List.rev_append acc args)
  | "--cl" :: args -> filter_cli_arg cli acc ("--cli"::args)
  | ["--cli"] | "--cli" :: "--" :: _ -> raise_invalid_cli (Error None)
  | "--cli" :: arg :: args ->
    let version =
      match OpamCLIVersion.of_string_opt arg with
      | Some cli ->
        let ocli = cli, `Command_line in
        if OpamCLIVersion.is_supported cli then ocli else
          raise_invalid_cli (Ok ocli)
      | None -> raise_invalid_cli (Error (Some arg))
    in
    filter_cli_arg (Some version) acc args
  | arg :: args ->
    match OpamStd.String.cut_at arg '=' with
    | Some ("--cl", value)
    | Some ("--cli", value) ->
      filter_cli_arg cli acc ("--cli"::value::args)
    | _ ->
      filter_cli_arg cli (arg::acc) args

let is_confirm_level =
  OpamStd.String.is_prefix_of ~from:4 ~full:"--confirm-level"

(* Pre-process argv processing the --yes, --confirm-level, and --cli. Returns
   Some cli, if --cli was encountered, a boolean indicating if --yes/-y and
   the list of arguments to continue with processing. *)
let rec preprocess_argv cli yes_args confirm args =
  let yes = yes_args <> [] in
  match args with
  | [] ->
    (cli, yes, confirm, yes_args)
  | "--" :: _ ->
    (cli, yes, confirm, yes_args @ args)
  (* Note that because this is evaluated before a sub-command, all the
     prefixes of --yes are assumed to valid at all times. *)
  | ("-y" | "--y" | "--ye" | "--yes") as yes_opt :: args ->
    preprocess_argv cli [yes_opt] confirm args
  | ([c] | c :: "--" :: _) when is_confirm_level c ->
    raise_invalid_confirm_level None
  | confirm_level :: cl_arg :: args when is_confirm_level confirm_level ->
    let answer =
      match OpamStd.List.find_opt (fun (_,n,_) -> n = cl_arg)
              OpamArg.confirm_enum with
      | Some (_, _, a) -> a
      | None -> raise_invalid_confirm_level (Some cl_arg)
    in
    preprocess_argv cli yes_args (Some answer) args
  | "--cl" :: args -> preprocess_argv cli yes_args confirm ("--cli"::args)
  | ["--cli"] | "--cli" :: "--" :: _ -> raise_invalid_cli (Error None)
  | "--cli" :: arg :: args ->
    let version =
      match OpamCLIVersion.of_string_opt arg with
      | Some cli ->
        let ocli = cli, `Command_line in
        if OpamCLIVersion.is_supported cli then ocli else
          raise_invalid_cli (Ok ocli)
      | _ -> raise_invalid_cli (Error (Some arg))
    in
    preprocess_argv (Some version) yes_args confirm args
  | arg :: rest ->
    match OpamStd.String.cut_at arg '=' with
    | Some ("--cl", value)
    | Some ("--cli", value) ->
      preprocess_argv cli yes_args confirm ("--cli"::value::rest)
    | Some (pre, value) when is_confirm_level pre ->
      preprocess_argv cli yes_args confirm ("--confirm-level"::value::rest)
    | _ ->
      if OpamCommands.is_builtin_command arg then
        let (cli, rest) = filter_cli_arg cli [] rest in
        (cli, yes, confirm, arg :: (yes_args @ rest))
      else
        (cli, yes, confirm, args)

(* Handle git-like plugins *)
let check_and_run_external_commands () =
  (* Pre-process the --yes and --cli options *)
  let (cli, yes, confirm_level, argv) =
    match Array.to_list Sys.argv with
    | prog::args ->
      let (ocli, yes, confirm, args) = preprocess_argv None [] None args in
      let ocli =
        match ocli with
        | Some ((cli, _) as ocli) ->
          if OpamCLIVersion.(cli < (2, 1)) then begin
            let cli = OpamCLIVersion.to_string cli in
            OpamConsole.warning
              "%s cannot be understood by opam %s; set %s to %s instead."
              (OpamConsole.colorise `bold ("--cli=" ^ cli)) cli
              (OpamConsole.colorise `bold "OPAMCLI") (OpamConsole.colorise `bold cli)
          end;
          ocli
        | None ->
          match OpamCLIVersion.Sourced.env (OpamClientConfig.E.cli ()) with
          | Some ((cli, _) as ocli) ->
            if OpamCLIVersion.is_supported cli then
              let () =
                if OpamCLIVersion.(cli >= (2, 1)) then
                  let flag = "--cli=" ^ OpamCLIVersion.(to_string cli) in
                  OpamConsole.warning
                    "OPAMCLI should only ever be set to %s - use '%s' instead."
                      (OpamConsole.colorise `bold "2.0")
                      (OpamConsole.colorise `bold flag)
              in
              ocli
            else
              raise_invalid_cli (Ok ocli)
          | None ->
            OpamCLIVersion.Sourced.current
      in
      let confirm =
        (* hardcoded cli validation *)
        match confirm with
        | Some _ when OpamCLIVersion.(fst ocli < (2,1)) ->
          raise (InvalidNewFlag (ocli, "confirm-level",
                                 OpamCLIVersion.of_string "2.1"))
        | _ -> confirm
      in
      (ocli, yes, confirm, prog::args)
    | args -> (OpamCLIVersion.Sourced.current, false, None, args)
  in
  match argv with
  | [] | [_] -> (cli, argv)
  | _ :: name :: args ->
    if String.length name > 0 && name.[0] = '-'
      || OpamCommands.is_builtin_command name
    then (cli, argv)
    else
    (* No such command, check if there is a matching plugin *)
    let command = OpamPath.plugin_prefix ^ name in
    OpamArg.init_opam_env_variabes cli;
    (* `--no` is not taken into account, only `--yes/--confirm-lzvel` are
       preprocessed *)
    let yes = if yes then Some (Some true) else None in
    OpamCoreConfig.init ?yes ?confirm_level ();
    OpamFormatConfig.init ();
    let root_dir = OpamStateConfig.opamroot () in
    let has_init, root_upgraded =
      match OpamStateConfig.load_defaults ~lock_kind:`Lock_read root_dir with
      | None -> (false, false)
      | Some config ->
        let root_upgraded =
          let cmp =
            OpamVersion.compare OpamFile.Config.root_version
              (OpamFile.Config.opam_root_version config)
          in
          if cmp < 0 then
            OpamConsole.error_and_exit `Configuration_error
              "%s reports a newer opam version, aborting."
              (OpamFilename.Dir.to_string root_dir)
          else
            cmp = 0
        in
        (true, root_upgraded)
    in
    let plugins_bin = OpamPath.plugins_bin root_dir in
    let plugin_symlink_present =
      OpamFilename.is_symlink (OpamPath.plugin_bin root_dir (OpamPackage.Name.of_string name))
    in
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
    | Some command when plugin_symlink_present && root_upgraded ->
      let argv = Array.of_list (command :: args) in
      raise (OpamStd.Sys.Exec (command, argv, env))
    | None when not has_init -> (cli, argv)
    | cmd ->
      (* Look for a corresponding package *)
      match OpamStateConfig.get_switch_opt () with
      | None -> (cli, argv)
      | Some sw ->
        OpamGlobalState.with_ `Lock_none @@ fun gt ->
        OpamSwitchState.with_ `Lock_none gt ~switch:sw @@ fun st ->
        let prefixed_name = OpamPath.plugin_prefix ^ name in
        let candidates =
          OpamPackage.packages_of_names
            st.packages
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
        let plugins =
          (* NOTE: Delay the availablity check as late as possible for performance reasons *)
          (* See https://github.com/ocaml/opam/issues/5296 *)
          if OpamPackage.Set.is_empty plugins then
            plugins
          else
            OpamPackage.Set.inter plugins (Lazy.force st.available_packages)
        in
        let installed = OpamPackage.Set.inter plugins st.installed in
        if OpamPackage.Set.is_empty candidates then (cli, argv)
        else if not OpamPackage.Set.(is_empty installed) && cmd = None then
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
          (if cmd = None then
             OpamConsole.confirm "Opam plugin \"%s\" is not installed. \
                                  Install it on the current switch?"
           else
             OpamConsole.confirm "Opam plugin \"%s\" may require upgrading/reinstalling. \
                                  Reinstall the plugin on the current switch?") name
        then
          let nv =
            try
              (* If the command was resolved, attempt to find the package to reinstall. *)
              if cmd = None then
                raise Not_found
              else
                OpamPackage.package_of_name installed (OpamPackage.Name.of_string prefixed_name)
            with Not_found ->
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
              OpamSwitchState.drop @@ (
              if cmd = None then
                OpamClient.install st [OpamSolution.eq_atom_of_package nv]
              else if root_upgraded then
                OpamClient.reinstall st [OpamSolution.eq_atom_of_package nv]
              else
                OpamClient.upgrade st ~all:false [OpamSolution.eq_atom_of_package nv])
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

let flush_all_noerror () =
  (try flush stderr with _ -> ());
  (try flush stdout with _ -> ())

let rec main_catch_all f =
  try f () with
  | OpamStd.Sys.Exit 0 -> ()
  | OpamStd.Sys.Exec (cmd,args,env) ->
    OpamStd.Sys.exec_at_exit ();
    if Sys.win32 then
      OpamProcess.create_process_env cmd args env
        Unix.stdin Unix.stdout Unix.stderr
      |> Unix.waitpid []
      |> function
      | _, Unix.WEXITED n -> exit n
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> exit (128 - n)
      (* This is not how you should handle `WSTOPPED` ; but it doesn't happen on
         Windows anyway. *)
    else
      Unix.execvpe cmd args env
  | OpamFormatUpgrade.Upgrade_done (conf, reinit) ->
    main_catch_all @@ fun () ->
    OpamConsole.header_msg "Rerunning init and update";
    (match reinit with
     | Some reinit ->
       reinit conf;
       OpamConsole.msg "Update done.\n";
       exit (OpamStd.Sys.get_exit_code `Success)
     | None ->
       OpamClient.reinit ~interactive:true ~update_config:false
         ~bypass_checks:true conf (OpamStd.Sys.guess_shell_compat ());
       OpamConsole.msg
         "Update done, please now retry your command.\n";
       exit (OpamStd.Sys.get_exit_code `Aborted))
  | e ->
    OpamStd.Exn.register_backtrace e;
    (try Sys.set_signal Sys.sigpipe Sys.Signal_default
     with Invalid_argument _ -> ());
    flush_all_noerror ();
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
      | InvalidCLI (cli, source) ->
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
      | InvalidFlagContent (flag, None) ->
        (* No argument given to flag *)
        display_cli_error "option `--%s' needs an argument" flag;
        OpamStd.Sys.get_exit_code `Bad_arguments
      | InvalidFlagContent (flag, Some (invalid, expected)) ->
        (* Wrong argument kind given to flag *)
        display_cli_error
          "option `--%s': invalid value `%s', expected %s"
          flag invalid expected;
        OpamStd.Sys.get_exit_code `Bad_arguments
      | InvalidNewFlag ((req_cli, _), flag, flag_cli) ->
        (* Requested cli is older than flag introduction *)
        display_cli_error
          "--%s was added in version %s of the opam CLI, \
           but version %s has been requested, which is older."
          flag (OpamCLIVersion.to_string flag_cli)
          (OpamCLIVersion.to_string req_cli);
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
  OpamArg.preinit_opam_env_variables ();
  main_catch_all @@ fun () ->
  let cli, argv = check_and_run_external_commands () in
  let (default, commands), argv1 =
    match argv with
    | prog :: command :: argv when OpamCommands.is_admin_subcommand command ->
      OpamAdminCommand.get_cmdliner_parser cli, prog::argv
    | _ ->
      OpamCommands.get_cmdliner_parser cli, argv
  in
  let argv = Array.of_list argv1 in
  (* TODO: Get rid of this whenever https://github.com/dbuenzli/cmdliner/pull/161 is available *)
  let to_new_cmdliner_api (term, info) = Cmd.v info term in
  let default, default_info = default in
  let commands = List.map to_new_cmdliner_api commands in
  match Cmd.eval_value ~catch:false ~argv (Cmd.group ~default default_info commands) with
  | Error _ -> exit (OpamStd.Sys.get_exit_code `Bad_arguments)
  | Ok _    -> exit (OpamStd.Sys.get_exit_code `Success)

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
      flush_all_noerror ();
      if OpamClientConfig.(!r.print_stats) then (
        OpamFile.Stats.print ();
        OpamSystem.print_stats ();
      );
      json_out ()
    );
  run ()
