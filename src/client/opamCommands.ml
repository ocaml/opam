(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open OpamArg
open OpamTypes
open OpamStateTypes
open OpamStd.Op

let self_upgrade_exe opamroot =
  OpamFilename.Op.(opamroot // "opam", opamroot // "opam.version")

let self_upgrade_bootstrapping_value = "bootstrapping"

let switch_to_updated_self debug opamroot =
  let updated_self, updated_self_version = self_upgrade_exe opamroot in
  let updated_self_str = OpamFilename.to_string updated_self in
  let updated_self_version_str = OpamFilename.to_string updated_self_version in
  if updated_self_str <> Sys.executable_name &&
     OpamFilename.exists updated_self &&
     OpamFilename.is_exec updated_self &&
     OpamFilename.exists updated_self_version
  then
    let no_version = OpamVersion.of_string "" in
    let update_version =
      try
        let s = OpamSystem.read updated_self_version_str in
        let s =
          let len = String.length s in if len > 0 && s.[len-1] = '\n'
          then String.sub s 0 (len-1) else s in
        OpamVersion.of_string s
      with e -> OpamStd.Exn.fatal e; no_version in
    if update_version = no_version then
      OpamConsole.error "%s exists but cannot be read, disabling self-upgrade."
        updated_self_version_str
    else if OpamVersion.compare update_version OpamVersion.current <= 0 then
      OpamConsole.warning "Obsolete opam self-upgrade package v.%s found, \
                           not using it (current system version is %s)."
        (OpamVersion.to_string update_version)
        (OpamVersion.to_string OpamVersion.current)
    else (
      if OpamVersion.is_dev_version () then
        OpamConsole.warning "Using opam self-upgrade to %s while the system \
                             opam is a development version (%s)"
          (OpamVersion.to_string update_version)
          (OpamVersion.to_string (OpamVersion.full ()));
      (if debug || (OpamConsole.debug ()) then
         OpamConsole.errmsg "!! %s found, switching to it !!\n"
           updated_self_str;
       let env =
         Array.append
           [|"OPAMNOSELFUPGRADE="^ self_upgrade_bootstrapping_value|]
           (Unix.environment ()) in
       try
         OpamStd.Sys.exec_at_exit ();
         Unix.execve updated_self_str Sys.argv env
       with e ->
         OpamStd.Exn.fatal e;
         OpamConsole.error
           "Couldn't run the upgraded opam %s found at %s. \
            Continuing with %s from the system."
           (OpamVersion.to_string update_version)
           updated_self_str
           (OpamVersion.to_string OpamVersion.current)))

let global_options cli =
  let no_self_upgrade =
    mk_flag ~cli cli_original ~section:global_option_section ["no-self-upgrade"]
      (Printf.sprintf
        "Opam will replace itself with a newer binary found \
         at $(b,OPAMROOT%sopam) if present. This disables this behaviour."
        OpamArg.dir_sep) in
  let self_upgrade no_self_upgrade options =
    let self_upgrade_status =
      if OpamClientConfig.E.noselfupgrade () =
         Some self_upgrade_bootstrapping_value
      then `Running
      else if no_self_upgrade then `Disable
      else if OpamStd.Option.Op.((OpamClientConfig.E.noselfupgrade ())
                                 >>= OpamStd.Config.bool_of_string)
              = Some true
      then `Disable
      else `None
    in
    if self_upgrade_status = `None then
      switch_to_updated_self
        OpamStd.Option.Op.(options.debug_level ++
                           OpamCoreConfig.E.debug () +! 0 |> abs > 0)
        (OpamStateConfig.opamroot ?root_dir:options.opt_root ());
    let root_is_ok =
      OpamStd.Option.default false (OpamClientConfig.E.rootisok ())
    in
    if not (options.safe_mode || root_is_ok) &&
       Unix.getuid () = 0 then
      OpamConsole.warning "Running as root is not recommended";
    {options with cli = fst cli}, self_upgrade_status
  in
  Term.(const self_upgrade $ no_self_upgrade $ global_options cli)

let apply_global_options cli (options,self_upgrade) =
  apply_global_options cli options;
  OpamConsole.log "CLI" "Parsing CLI version %s"
    (OpamCLIVersion.to_string options.cli);
  try
    let argv0 = OpamFilename.of_string Sys.executable_name in
    if self_upgrade <> `Running &&
       OpamFilename.starts_with OpamStateConfig.(!r.root_dir) argv0 &&
       not !OpamCoreConfig.r.OpamCoreConfig.safe_mode
    then
      OpamConsole.warning "You should not be running opam from within %s. \
                           Copying %s to your PATH first is advised."
        (OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir))
        (OpamFilename.to_string argv0)
  with e -> OpamStd.Exn.fatal e

let self_upgrade_status global_options = snd global_options

let get_init_config ~no_sandboxing ~no_default_config_file ~add_config_file =
  let builtin_config =
    OpamInitDefaults.init_config ~sandboxing:(not no_sandboxing) ()
  in
  let config_files =
    (if no_default_config_file then []
     else List.filter OpamFile.exists (OpamPath.init_config_files ()))
    @ List.map (fun url ->
        match OpamUrl.local_file url with
        | Some f -> OpamFile.make f
        | None ->
          let f = OpamFilename.of_string (OpamSystem.temp_file "conf") in
          OpamProcess.Job.run (OpamDownload.download_as ~overwrite:false url f);
          let hash = OpamHash.compute ~kind:`SHA256 (OpamFilename.to_string f) in
          if OpamConsole.confirm
              "Using configuration file from %s. \
               Please verify the following SHA256:\n    %s\n\
               Is this correct?"
              (OpamUrl.to_string url) (OpamHash.contents hash)
          then OpamFile.make f
          else OpamStd.Sys.exit_because `Aborted
      ) add_config_file
  in
  try
    let others =
      match config_files with
      | [] -> ""
      | [file] -> OpamFile.to_string file ^ " and then from "
      | _ ->
        (OpamStd.List.concat_map ~right:", and finally from " ", then "
           OpamFile.to_string (List.rev config_files))
    in
    if config_files = [] then
      OpamConsole.msg "No configuration file found, using built-in defaults.\n"
    else
      OpamConsole.msg "Configuring from %sbuilt-in defaults.\n" others;
    List.fold_left (fun acc f ->
        OpamFile.InitConfig.add acc (OpamFile.InitConfig.read f))
      builtin_config config_files
  with e ->
    OpamConsole.error
      "Error in configuration file, fix it, use '--no-opamrc', or check \
       your '--config FILE' arguments:";
    OpamConsole.errmsg "%s\n" (Printexc.to_string e);
    OpamStd.Sys.exit_because `Configuration_error

(* INIT *)
let init_doc = "Initialize opam state, or set init options."
let init cli =
  let doc = init_doc in
  let man = [
    `S Manpage.s_description;
    `P "Initialise the opam state, or update opam init options";
    `P (Printf.sprintf
         "The $(b,init) command initialises a local \"opam root\" (by default, \
          $(i,~%s.opam%s)) that holds opam's data and packages. This is a \
          necessary step for normal operation of opam. The initial software \
          repositories are fetched, and an initial 'switch' can also be \
          installed, according to the configuration and options. These can be \
          afterwards configured using $(b,opam switch) and $(b,opam \
          repository)."
         OpamArg.dir_sep OpamArg.dir_sep);
    `P (Printf.sprintf
         "The initial repository and defaults can be set through a \
          configuration file found at $(i,~%s.opamrc) or $(i,/etc/opamrc)."
         OpamArg.dir_sep);
    `P "Additionally, this command allows one to customise some aspects of opam's \
        shell integration, when run initially (avoiding the interactive \
        dialog), but also at any later time.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    `S "CONFIGURATION FILE";
    `P (Printf.sprintf
         "Any field from the built-in initial configuration can be overridden \
          through $(i,~%s.opamrc), $(i,/etc/opamrc), or a file supplied with \
          $(i,--config). The default configuration for this version of opam \
          can be obtained using $(b,--show-default-opamrc)."
         OpamArg.dir_sep);
  ] @ OpamArg.man_build_option_section
  in
  let compiler =
    mk_opt ~cli cli_original ["c";"compiler"] "PACKAGE"
      "Set the compiler to install (when creating an initial switch)"
      Arg.(some string) None
  in
  let no_compiler =
    mk_flag ~cli cli_original ["bare"]
      "Initialise the opam state, but don't setup any compiler switch yet."
  in
  let repo_name =
    let doc =
      Arg.info [] ~docv:"NAME" ~doc:
        "Name of the initial repository, when creating a new opam root."
    in
    Arg.(value & pos ~rev:true 1 repository_name OpamRepositoryName.default
         & doc)
  in
  let repo_url =
    let doc =
      Arg.info [] ~docv:"ADDRESS" ~doc:
        "Address of the initial package repository, when creating a new opam \
         root."
    in
    Arg.(value & pos ~rev:true 0 (some string) None & doc)
  in
  let interactive =
    mk_vflag ~cli None [
        cli_original, (Some false), ["a";"auto-setup"],
          "Automatically do a full setup, including adding a line to your \
           shell init files.";
        cli_original, (Some true), ["i";"interactive"],
          "Run the setup interactively (this is the default for an initial \
           run, or when no more specific options are specified)";
      ]
  in
  let update_config =
    mk_vflag ~cli None [
        cli_original, (Some true), ["shell-setup"],
          "Automatically setup the user shell configuration for opam, e.g. \
           adding a line to the `~/.profile' file.";
        cli_original, (Some false), ["n";"no-setup"],
          "Do not update the user shell configuration to setup opam. Also \
           implies $(b,--disable-shell-hook), unless $(b,--interactive) or \
           specified otherwise";
      ]
  in
  let setup_completion =
    mk_vflag ~cli None [
        cli_original, (Some true), ["enable-completion"],
          "Setup shell completion in opam init scripts, for supported \
           shells.";
        cli_original, (Some false), ["disable-completion"],
          "Disable shell completion in opam init scripts.";
      ]
  in
  let env_hook =
    mk_vflag ~cli None [
        cli_original, (Some true), ["enable-shell-hook"],
          "Setup opam init scripts to register a shell hook that will \
           automatically keep the shell environment up-to-date at every \
           prompt.";
        cli_original, (Some false), ["disable-shell-hook"],
          "Disable registration of a shell hook in opam init scripts.";
      ]
  in
  let config_file =
    mk_opt_all ~cli cli_original ["config"] "FILE"
      "Use the given init config file. If repeated, latest has the highest \
       priority ($(b,i.e.) each field gets its value from where it was defined \
       last). Specifying a URL pointing to a config file instead is \
       allowed."
      OpamArg.url
  in
  let no_config_file =
    mk_flag ~cli cli_original ["no-opamrc"]
      (Printf.sprintf
      "Don't read `/etc/opamrc' or `~%s.opamrc': use the default settings and \
       the files specified through $(b,--config) only" OpamArg.dir_sep)
  in
  let reinit =
    mk_flag ~cli cli_original ["reinit"]
      "Re-run the initial checks and setup, according to opamrc, even if this \
       is not a new opam root"
  in
  let show_default_opamrc =
    mk_flag ~cli cli_original ["show-default-opamrc"]
      "Print the built-in default configuration to stdout and exit"
  in
  let bypass_checks =
    mk_flag ~cli cli_original ["bypass-checks"]
      "Skip checks on required or recommended tools, and assume everything is \
       fine"
  in
  let no_sandboxing =
    mk_flag ~cli cli_original ["disable-sandboxing"]
      "Use a default configuration with sandboxing disabled (note that this \
       may be overridden by `opamrc' if $(b,--no-opamrc) is not specified or \
       $(b,--config) is used). Use this at your own risk, without sandboxing \
       it is possible for a broken package script to delete all your files."
  in
  let init global_options
      build_options repo_kind repo_name repo_url
      interactive update_config completion env_hook no_sandboxing shell
      dot_profile_o compiler no_compiler config_file no_config_file reinit
      show_opamrc bypass_checks
      () =
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    (* If show option is set, dump opamrc and exit *)
    if show_opamrc then
      (OpamFile.InitConfig.write_to_channel stdout @@
         OpamInitDefaults.init_config ~sandboxing:(not no_sandboxing) ();
       OpamStd.Sys.exit_because `Success);
    (* Else continue init *)
    if compiler <> None && no_compiler then
      OpamConsole.error_and_exit `Bad_arguments
        "Options --bare and --compiler are incompatible";

    let root = OpamStateConfig.(!r.root_dir) in
    let config_f = OpamPath.config root in
    let already_init = OpamFile.exists config_f in
    (* handling of `-ni` option *)
    let inplace =
      interactive = Some true && update_config = Some false
      && env_hook = None && completion = None
    in
    let interactive, update_config, completion, env_hook =
      match interactive with
      | Some false ->
        OpamStd.Option.Op.(
          false,
          update_config ++ Some true,
          completion ++ Some true,
          env_hook ++ Some true
        )
      | None ->
        (not already_init ||
         update_config = None && completion = None && env_hook = None),
        update_config, completion, OpamStd.Option.Op.(env_hook ++ update_config)
      | Some true ->
        if update_config = None && completion = None && env_hook = None then
          true, None, None, None
        else
        let reconfirm = function
          | None | Some false -> None
          | Some true -> Some true
        in
        true,
        (if update_config = Some true then update_config else Some false),
        reconfirm completion,
        reconfirm env_hook
    in
    let shell = match shell with
      | Some s -> s
      | None -> OpamStd.Sys.guess_shell_compat ()
    in
    let dot_profile = match dot_profile_o with
      | Some n -> n
      | None ->
        OpamFilename.of_string (OpamStd.Sys.guess_dot_profile shell)
    in
    if already_init then
      if reinit then
        let init_config =
          get_init_config ~no_sandboxing
            ~no_default_config_file:no_config_file ~add_config_file:config_file
        in
        let reinit conf =
          OpamClient.reinit ~init_config ~interactive ~dot_profile
            ?update_config ?env_hook ?completion ~inplace ~bypass_checks
            ~check_sandbox:(not no_sandboxing)
            conf shell
        in
        let config =
          match OpamStateConfig.load ~lock_kind:`Lock_write root with
          | Some c -> c
          | exception (OpamPp.Bad_version _ as e) ->
            OpamFormatUpgrade.hard_upgrade_from_2_1_intermediates ~reinit root;
            raise e
          | None -> OpamFile.Config.empty
        in
        reinit config
      else
        (if not interactive
            && update_config <> Some true
            && completion <> Some true
            && env_hook <> Some true then
           OpamConsole.msg
             "Opam was already initialised. If you want to set it up again, \
              use `--interactive', `--reinit', or choose a different \
              `--root'.\n";
         OpamEnv.setup root ~interactive ~dot_profile ?update_config ?env_hook
           ?completion ~inplace shell)
    else
    let init_config =
      get_init_config ~no_sandboxing
        ~no_default_config_file:no_config_file ~add_config_file:config_file
    in
    let repo =
      OpamStd.Option.map (fun url ->
          let repo_url = OpamUrl.parse ?backend:repo_kind ~from_file:false url in
          { repo_name; repo_url; repo_trust = None })
        repo_url
    in
    let gt, rt, default_compiler =
      OpamClient.init
        ~init_config ~interactive
        ?repo ~bypass_checks ~dot_profile
        ?update_config ?env_hook ?completion
        ~check_sandbox:(not no_sandboxing)
        shell
    in
    OpamStd.Exn.finally (fun () -> OpamRepositoryState.drop rt)
    @@ fun () ->
    if no_compiler then () else
    let invariant, default_compiler, name =
      match compiler with
      | Some comp when String.length comp > 0 ->
        OpamSwitchCommand.guess_compiler_invariant rt [comp],
        [],
        comp
      | _ ->
        OpamFile.Config.default_invariant gt.config,
        default_compiler, "default"
    in
    OpamConsole.header_msg "Creating initial switch '%s' (invariant %s%s)"
      name
      (match invariant with
       | OpamFormula.Empty -> "empty"
       | c -> OpamFileTools.dep_formula_to_string c)
      (match default_compiler with
       | [] -> ""
       | comp -> " - initially with "^ (OpamFormula.string_of_atoms comp));
    let (), st =
      try
        OpamSwitchCommand.create
          gt ~rt ~invariant ~update_config:true (OpamSwitch.of_string name) @@
        (fun st ->
           (),
           OpamSwitchCommand.install_compiler st
             ~ask:false
             ~additional_installs:default_compiler)
      with e ->
        OpamStd.Exn.finalise e @@ fun () ->
        OpamConsole.note
          "Opam has been initialised, but the initial switch creation \
           failed.\n\
           Use 'opam switch create <compiler>' to get started."
    in
    OpamSwitchState.drop st
  in
  mk_command  ~cli cli_original "init" ~doc ~man
    Term.(const init
          $global_options cli $build_options cli $repo_kind_flag cli
          cli_original $repo_name $repo_url $interactive $update_config
          $setup_completion $env_hook $no_sandboxing $shell_opt cli
          cli_original $dot_profile_flag cli cli_original $compiler
          $no_compiler $config_file $no_config_file $reinit $show_default_opamrc
          $bypass_checks)

(* LIST *)
let list_doc = "Display the list of available packages."
let list ?(force_search=false) cli =
  let doc = list_doc in
  let selection_docs = OpamArg.package_selection_section in
  let display_docs = OpamArg.package_listing_section in
  let man = [
    `S Manpage.s_description;
    `P "List selections of opam packages.";
    `P "Without argument, the command displays the list of currently installed \
        packages. With pattern arguments, lists all available packages \
        matching one of the patterns.";
    `P "Unless the $(b,--short) switch is used, the output format displays one \
        package per line, and each line contains the name of the package, the \
        installed version or `--' if the package is not installed, and a short \
        description. In color mode, manually installed packages (as opposed to \
        automatically installed ones because of dependencies) are underlined.";
    `P ("See section $(b,"^selection_docs^") for all the ways to select the \
         packages to be displayed, and section $(b,"^display_docs^") to \
         customise the output format.");
    `P "For a more detailed description of packages, see $(b,opam show). For \
        extended search capabilities within the packages' metadata, see \
        $(b,opam search).";
    `S Manpage.s_arguments;
    `S selection_docs;
    `S display_docs;
  ] in
  let pattern_list =
    arg_list "PATTERNS"
      "Package patterns with globs. Unless $(b,--search) is specified, they \
       match againsta $(b,NAME) or $(b,NAME.VERSION)"
      Arg.string
  in
  let state_selector =
    mk_vflag_all ~cli ~section:selection_docs [
        cli_original, OpamListCommand.Any, ["A";"all"],
          "Include all, even uninstalled or unavailable packages";
        cli_original, OpamListCommand.Installed, ["i";"installed"],
          "List installed packages only. This is the default when no \
                further arguments are supplied";
        cli_original, OpamListCommand.Root, ["roots";"installed-roots"],
          "List only packages that were explicitly installed, excluding \
                the ones installed as dependencies";
        cli_original, OpamListCommand.Available, ["a";"available"],
          "List only packages that are available on the current system";
        cli_original, OpamListCommand.Installable, ["installable"],
          "List only packages that can be installed on the current switch \
                (this calls the solver and may be more costly; a package \
                depending on an unavailable package may be available, but is \
                never installable)";
        cli_original, OpamListCommand.Compiler, ["base"],
          "List only the immutable base of the current switch (i.e. \
                compiler packages)";
        cli_original, OpamListCommand.Pinned, ["pinned"],
          "List only the pinned packages";
      ]
  in
  let section = selection_docs in
  let search =
    if force_search then Term.const true else
      mk_flag ~cli cli_original ["search"] ~section
        "Match $(i,PATTERNS) against the full descriptions of packages, and \
         require all of them to match, instead of requiring at least one to \
         match against package names (unless $(b,--or) is also specified)."
  in
  let repos =
    mk_opt ~cli cli_original ["repos"] "REPOS" ~section
      "Include only packages that took their origin from one of the given \
       repositories (unless $(i,no-switch) is also specified, this excludes \
       pinned packages)."
      Arg.(some & list & repository_name) None
  in
  let owns_file =
    mk_opt ~cli cli_original ["owns-file"] "FILE" ~section
      "Finds installed packages responsible for installing the given file"
      Arg.(some OpamArg.filename) None
  in
  let no_switch =
    mk_flag ~cli cli_original ["no-switch"] ~section:selection_docs
      "List what is available from the repositories, without consideration for \
       the current (or any other) switch (installed or pinned packages, etc.)"
  in
  let disjunction =
    mk_flag ~cli cli_original ["or"] ~section:selection_docs
      "Instead of selecting packages that match $(i,all) the criteria, select \
       packages that match $(i,any) of them"
  in
  let depexts =
    mk_flag ~cli cli_original ["e";"external";"depexts"] ~section:display_docs
      "Instead of displaying the packages, display their external dependencies \
       that are associated with the current system. This excludes other \
       display options. Rather than using this directly, you should probably \
       head for the `depext' plugin, that will use your system package \
       management system to handle the installation of the dependencies. Run \
       `opam depext'."
  in
  let vars =
    mk_opt ~cli cli_original ["vars"] "[VAR=STR,...]" ~section:display_docs
      "Define the given variable bindings. Typically useful with \
       $(b,--external) to override the values for $(i,arch), $(i,os), \
       $(i,os-distribution), $(i,os-version), $(i,os-family)."
      OpamArg.variable_bindings []
  in
  let silent =
    mk_flag_replaced ~cli [
      cli_between ~option:`default cli2_0 cli2_1 ~replaced:"--check", ["silent"];
      cli_from cli2_1, ["check"]
    ] "Don't write anything in the output, exit with return code 0 if the list \
       is not empty, 1 otherwise."
  in
  let no_depexts =
    mk_flag ~cli cli_original ["no-depexts"]
      "Disable external dependencies handling for the query. This can be used \
       to include packages that are marked as unavailable because of an unavailable \
       system dependency."
  in
  let list
      global_options selection state_selector no_switch depexts vars repos
      owns_file disjunction search silent no_depexts format packages () =
    apply_global_options cli global_options;
    let no_switch =
      no_switch || OpamStateConfig.get_switch_opt () = None
    in
    let format =
      let force_all_versions =
        not search && state_selector = [] && match packages with
        | [single] ->
          let nameglob =
            match OpamStd.String.cut_at single '.' with
            | None -> single
            | Some (n, _v) -> n
          in
          (try ignore (OpamPackage.Name.of_string nameglob); true
           with Failure _ -> false)
        | _ -> false
      in
      format ~force_all_versions
    in
    let join =
      if disjunction then OpamFormula.ors else OpamFormula.ands
    in
    let state_selector =
      if state_selector = [] then
        if no_switch || search || owns_file <> None then Empty
        else if packages = [] && selection = []
        then Atom OpamListCommand.Installed
        else Or (Atom OpamListCommand.Installed,
                 Atom OpamListCommand.Available)
      else join (List.map (fun x -> Atom x) state_selector)
    in
    let pattern_selector =
      if search then
        join
          (List.map (fun p ->
               Atom (OpamListCommand.(Pattern (default_pattern_selector, p))))
              packages)
      else OpamListCommand.pattern_selector packages
    in
    let filter =
      OpamFormula.ands [
        join
          (pattern_selector ::
           (if no_switch then Empty else
            match repos with None -> Empty | Some repos ->
              Atom (OpamListCommand.From_repository repos)) ::
           OpamStd.Option.Op.
             ((owns_file >>| fun f -> Atom (OpamListCommand.Owns_file f)) +!
              Empty) ::
           List.map (fun x -> Atom x) selection);
        state_selector;
      ]
    in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
    if no_depexts then OpamStateConfig.update ~no_depexts:true ();
    let st =
      if no_switch then OpamSwitchState.load_virtual ?repos_list:repos gt rt
      else OpamSwitchState.load `Lock_none gt rt (OpamStateConfig.get_switch ())
    in
    let st =
      let open OpamFile.Switch_config in
      let conf = st.switch_config in
      { st with switch_config =
        { conf with variables =
          conf.variables @ List.map (fun (var, v) -> var, S v) vars } }
    in
    if not depexts &&
       not format.OpamListCommand.short &&
       filter <> OpamFormula.Empty &&
       not silent
    then
      OpamConsole.msg "# Packages matching: %s\n"
        (OpamListCommand.string_of_formula filter);
    let all = OpamPackage.Set.union st.packages st.installed in
    let results =
      OpamListCommand.filter ~base:all st filter
    in
    if not no_depexts && not silent then
      (let drop_by_depexts =
         List.fold_left (fun missing str ->
             let is_missing pkgs =
                 if OpamStd.String.contains_char str '.' then
                   let nv = OpamPackage.of_string str in
                   if OpamPackage.Set.mem nv results then None else
                     OpamPackage.Set.find_opt (OpamPackage.equal nv) pkgs
                 else
                 let n = OpamPackage.Name.of_string str in
                 if OpamPackage.has_name results n then None else
                 let exist = OpamPackage.packages_of_name pkgs n in
                 if OpamPackage.Set.is_empty exist then None else
                   Some (OpamPackage.Set.max_elt exist)
             in
             match OpamStd.Option.Op.(
                 is_missing OpamPackage.Set.Op.(st.packages ++ st.pinned)
                 >>= OpamSwitchState.depexts_unavailable st) with
             | Some nf ->  OpamStd.String.Map.add str nf missing
             | None -> missing
             | exception Failure _ -> missing (* invalid package *)
           ) OpamStd.String.Map.empty packages
       in
       if not (OpamStd.String.Map.is_empty drop_by_depexts) then
         OpamConsole.note
           "Some packages are unavailable because of their external dependencies. \
            Use `--no-depexts' to show them anyway.\n%s"
           (OpamStd.Format.itemize (fun (n, spkgs) ->
                Printf.sprintf "%s: %s" n
                  (OpamStd.Format.pretty_list
                     (List.map OpamSysPkg.to_string
                        (OpamSysPkg.Set.elements spkgs))))
               (OpamStd.String.Map.bindings drop_by_depexts)));
    if not depexts then
      (if not silent then
         OpamListCommand.display st format results
       else if OpamPackage.Set.is_empty results then
         OpamStd.Sys.exit_because `False)
    else
    let results_depexts = OpamListCommand.get_depexts st results in
    if not silent then
      OpamListCommand.print_depexts results_depexts
    else if OpamSysPkg.Set.is_empty results_depexts then
      OpamStd.Sys.exit_because `False
  in
  mk_command  ~cli cli_original "list" ~doc ~man
    Term.(const list $global_options cli $package_selection cli $state_selector
          $no_switch $depexts $vars $repos $owns_file $disjunction $search
          $silent $no_depexts $package_listing cli $pattern_list)

(* TREE *)
let tree_doc = "Draw the dependency forest of installed packages."
let tree ?(why=false) cli =
  let doc = tree_doc in
  let build_docs = "TREE BUILDING OPTIONS" in
  let filter_docs = "TREE FILTERING OPTIONS" in
  let selection_docs = OpamArg.package_selection_section in
  let display_docs = OpamArg.package_listing_section in
  let man = [
    `S Manpage.s_description;
    `P "This command displays the forest of currently installed \
        packages as a Unicode/ASCII-art.";
    `P "Without argument, it draws the dependency forest of packages with no \
        dependants. With packages arguments, draws the forest of the specified \
        packages. When non-installed packages are specified in the arguments, \
        it will try to simulate installing them before drawing the forest.";
    `P "When the $(b,--rev-deps) option is used, it draws the \
        reverse-dependency forest instead. Without argument, draws the forest \
        of packages with no dependencies. With packages arguments, draws the \
        forest of the specified packages. Note that non-installed packages are \
        ignored when this option is used.";
    `P ("When a package appears twice or more in the forest, the second or \
         later occurrences of the said package will be marked as a duplicate, \
         and be annotated with the $(i,"^OpamTreeCommand.duplicate_symbol^") \
         symbol. Any sub-trees rooted from such duplicates will be truncated \
         to avoid redundancy.");
    `P ("See section $(b,"^filter_docs^") and $(b,"^selection_docs^") for all \
         the ways to select the packages to be displayed, and section \
         $(b,"^display_docs^") to customise the output format.");
    `P "For a flat list of packages which may not be installed, \
        see $(b,opam list).";
    `S Manpage.s_arguments;
    `S build_docs;
    `S filter_docs;
    `P "These options only take effect when $(i,PACKAGES) are present.";
    `S selection_docs;
    `S display_docs;
  ] in
  let mode =
    let default = OpamTreeCommand.(if why then ReverseDeps else Deps) in
    mk_vflag default ~cli ~section:build_docs [
      cli_from cli2_2, OpamTreeCommand.Deps, ["deps"],
      "Draw a dependency forest, starting from the packages not required by \
       any other packages (this is the default).";
      cli_from cli2_2, OpamTreeCommand.ReverseDeps, ["rev-deps"],
      "Draw a reverse-dependency forest, starting from the packages which \
       have no dependencies.";
    ]
  in
  let filter =
    let default = OpamTreeCommand.Roots_from in
    mk_vflag default ~cli ~section:filter_docs [
      cli_from cli2_2, OpamTreeCommand.Roots_from, ["roots-from"],
      "Display only the trees which roots from one of the $(i,PACKAGES) \
       (this is the default).";
      cli_from cli2_2, OpamTreeCommand.Leads_to,   ["leads-to"],
      "Display only the branches which leads to one of the $(i,PACKAGES).";
    ]
  in
  let no_cstr =
    mk_flag ~cli (cli_from cli2_2) ["no-constraint"] ~section:display_docs
      "Do not display the version constraints e.g. $(i,(>= 1.0.0))."
  in
  let no_switch =
    mk_flag ~cli (cli_from cli2_2) ["no-switch"] ~section:selection_docs
      "Ignore active switch and simulate installing packages from an empty \
       switch to draw the forest"
  in
  let tree global_options mode filter post dev doc test dev_setup no_constraint
      no_switch names () =
    if names = [] && no_switch then
      `Error
        (true, "--no-switch can't be used without specifying a name")
    else
      (apply_global_options cli global_options;
       OpamGlobalState.with_ `Lock_none @@ fun gt ->
       OpamSwitchState.with_ `Lock_none gt @@ fun st ->
       let tog = OpamListCommand.{
           post; test; doc; dev; dev_setup;
           recursive = false;
           depopts = false;
           build = true;
         } in
       OpamTreeCommand.run st tog ~no_constraint ~no_switch mode filter names;
       `Ok ())
  in
  mk_command_ret ~cli (cli_from cli2_2) "tree" ~doc ~man
    Term.(const tree $global_options cli $mode $filter
          $post cli $dev cli $doc_flag cli $test cli $dev_setup cli
          $no_cstr $no_switch
          $name_list)

(* SHOW *)
let show_doc = "Display information about specific packages."
let show cli =
  let doc = show_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command displays the information block for the selected \
        package(s).";
    `P "The information block consists of the name of the package, \
        the installed version if this package is installed in the currently \
        selected compiler, the list of available (installable) versions, and a \
        complete description.";
    `P "$(b,opam list) can be used to display the list of \
        available packages as well as a short description for each.";
    `P "Paths to package definition files or to directories containing package \
        definitions can also be specified, in which case the corresponding \
        metadata will be shown."
  ] in
  let fields =
    mk_opt ~cli cli_original ["f";"field"] "FIELDS"
      (Printf.sprintf
         "Only display the values of these fields. Fields can be selected \
          among %s. Multiple fields can be separated with commas, in which case \
          field titles will be printed; the raw value of any opam-file \
          field can be queried by combinig with $(b,--raw)"
         (OpamStd.List.concat_map ", " (Printf.sprintf "$(i,%s)" @* snd)
            OpamListCommand.field_names))
      Arg.(list string) []
  in
  let show_empty =
    mk_flag ~cli cli_original ["empty-fields"]
      "Show fields that are empty. This is implied when $(b,--field) is \
       given."
  in
  let raw =
    mk_flag ~cli cli_original ["raw"] "Print the raw opam file for this package" in
  let where =
    mk_flag ~cli cli_original ["where"]
      "Print the location of the opam file used for this package" in
  let list_files =
    mk_flag ~cli cli_original ["list-files"]
      "List the files installed by the package. Equivalent to \
       $(b,--field=installed-files), and only available for installed \
       packages"
  in
  let file =
    mk_opt ~cli (cli_between cli2_0 cli2_1 ~replaced:"--just-file")
      ["file"] "FILE"
      "DEPRECATED: use an explicit path argument as package instead. \
       Get package information from the given FILE instead of from \
       known packages. This implies $(b,--raw) unless $(b,--fields) is \
       used. Only raw opam-file fields can be queried."
      existing_filename_or_dash None
  in
  let normalise =
    mk_flag ~cli cli_original ["normalise"]
      "Print the values of opam fields normalised (no newlines, no implicit \
       brackets)"
  in
  let no_lint =
    mk_flag ~cli cli_original ["no-lint"]
      "Don't output linting warnings or errors when reading from files"
  in
  let just_file =
    mk_flag ~cli (cli_from cli2_1) ["just-file"]
      "Load and display information from the given files (allowed \
       $(i,PACKAGES) are file or directory paths), without consideration for \
       the repositories or state of the package. This implies $(b,--raw) unless \
       $(b,--fields) is used. Only raw opam-file fields can be queried. If no \
       PACKAGES argument is given, read opam file from stdin."
  in
  let all_versions =
    mk_flag ~cli (cli_from cli2_1) ["all-versions"]
      "Display information of all packages matching $(i,PACKAGES), not \
       restrained to a single package matching $(i,PACKAGES) constraints."
  in
  let sort = mk_flag ~cli (cli_from cli2_1) ["sort"] "Sort opam fields" in
  let opam_files_in_dir d =
    match OpamPinned.files_in_source d with
    | [] -> []
    | l -> List.map (fun nf -> nf.pin.pin_file) l
  in
  let show global_options fields show_empty raw where
      list_files file normalise no_lint just_file all_versions sort atom_locs
      () =
    let print_just_file opamf opam =
      if not no_lint then OpamFile.OPAM.print_errors opam;
      let opam =
        if not sort then opam else
          OpamFileTools.sort_opam opam
      in
      if where then
        OpamConsole.msg "%s\n"
          (match opamf with
           | Some opamf ->
             OpamFilename.(Dir.to_string (dirname (OpamFile.filename opamf)))
           | None -> ".")
      else
      let opam_content_list = OpamFile.OPAM.to_list opam in
      let get_field f =
        try OpamListCommand.mini_field_printer ~prettify:true ~normalise
              (List.assoc f opam_content_list)
        with Not_found -> ""
      in
      match fields with
      | [] ->
        OpamFile.OPAM.write_to_channel stdout opam
      | [f] ->
        OpamConsole.msg "%s\n" (get_field f)
      | flds ->
        let tbl =
          List.map (fun fld ->
              [ OpamConsole.colorise `blue (fld ^ ":"); get_field fld ])
            flds
        in
        OpamStd.Format.align_table tbl |>
        OpamConsole.print_table stdout ~sep:" "
    in
    apply_global_options cli global_options;
    match file with
    | Some file ->
      let opamf = OpamFile.make file in
      print_just_file (Some opamf) (OpamFile.OPAM.safe_read opamf);
      `Ok ()
    | None ->
      (match atom_locs, just_file with
       | [], false ->
         `Error (true, "required argument PACKAGES is missing")
       | [], true ->
         (try
            let opam = OpamFile.OPAM.read_from_channel stdin in
            print_just_file None opam;
            `Ok ()
          with
          | Parsing.Parse_error | OpamLexer.Error _
          | OpamPp.Bad_version _ | OpamPp.Bad_format _ as exn ->
            OpamConsole.error_and_exit `File_error
              "Stdin parsing failed:\n%s" (Printexc.to_string exn))
       | atom_locs, false ->
         let fields, show_empty =
           if list_files then
             fields @ [OpamListCommand.(string_of_field Installed_files)],
             show_empty
           else fields, show_empty || fields <> []
         in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
         let st = OpamListCommand.get_switch_state gt rt in
         let st, atoms =
           OpamAuxCommands.simulate_autopin ~quiet:no_lint ~for_view:true st
             atom_locs
         in
         if atoms = [] then
           OpamConsole.error_and_exit `Not_found "No package found"
         else
           OpamListCommand.info st
             ~fields ~raw ~where ~normalise ~show_empty ~all_versions ~sort atoms;
         `Ok ()
       | atom_locs, true ->
         if List.exists (function `Atom _ -> true | _ -> false) atom_locs then
           `Error (true, "packages can't be specified with --just-file")
         else
         let opamfs =
           List.fold_left (fun acc al ->
               match al with
               | `Filename f -> (OpamFile.make f) :: acc
               | `Dirname d -> opam_files_in_dir d @ acc
               | _ -> acc)
             [] atom_locs
         in
         if opamfs = [] then
           let dirnames =
             OpamStd.List.filter_map (function
                 | `Dirname d -> Some (OpamFilename.Dir.to_string d)
                 | _ -> None)
               atom_locs
           in
           OpamConsole.error_and_exit `Not_found "No opam files found at %s"
             (OpamStd.List.concat_map ", " ~last_sep:" and "
                (fun x -> x) dirnames )
         else
         let errors, opams =
           List.fold_left (fun (errors,opams) opamf ->
               try
                 errors, (Some opamf, (OpamFile.OPAM.read opamf))::opams
               with
               | Parsing.Parse_error | OpamLexer.Error _
               | OpamPp.Bad_version _ | OpamPp.Bad_format _ as exn ->
                 (opamf, exn)::errors, opams)
             ([],[]) opamfs
         in
         List.iter (fun (f,o) -> print_just_file f o) opams;
         (if errors <> [] then
            let sgl = match errors with [_] -> true | _ -> false in
            let tostr (opamf, error) =
              (OpamFilename.to_string (OpamFile.filename opamf))
              ^ ":\n" ^ Printexc.to_string error
            in
            OpamConsole.error "Parsing error on%s:%s"
              (if sgl then "" else "some opam files")
              (match errors with
               | [f] -> tostr f
               | fs -> OpamStd.Format.itemize tostr fs));
         if opams = [] then
           OpamStd.Sys.exit_because `File_error
         else
           `Ok ()
      )
  in
  mk_command_ret  ~cli cli_original "show" ~doc ~man
    Term.(const show $global_options cli $fields $show_empty $raw $where
          $list_files $file $normalise $no_lint $just_file $all_versions
          $sort $atom_or_local_list)

(* Shared between [option] and [var] *)
module Var_Option_Common = struct

  let global cli =
    mk_flag ~cli (cli_from cli2_1) ["global"] "Act on global configuration"

  let var_option global global_options cmd var =
    let switch_set = (fst global_options).opt_switch <> None in
    if global && switch_set then
      `Error (true, "--global and --switch sw option can't be used together")
    else
    let scope =
      if global then `Global
      else if switch_set then `Switch
      else match var with
        | None -> `All
        | Some f -> match cmd with
          | `var -> `All_var
          | `option -> OpamConfigCommand.get_scope f
    in
    let apply =
      match var with
      | None -> `empty
      | Some var ->
        try `value_eq (OpamConfigCommand.parse_update var)
        with Invalid_argument _ -> `value_wo_eq var
    in
    match scope with
    | `None field ->
      (* must be an option command *)
      OpamConsole.error_and_exit `Bad_arguments
        "No option named '%s' found. Use 'opam option [--global]' to list them"
        field
    | `All ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (match cmd with
       | `var -> OpamConfigCommand.vars_list gt
       | `option -> OpamConfigCommand.options_list gt);
      `Ok ()
    | `All_var ->
      (match apply with
       | `value_wo_eq v ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamConfigCommand.var_show gt v;
         `Ok ()
       | `empty -> assert false (* can't happen *)
       | `value_eq _ ->
         `Error (true, "variable setting needs a scope, \
                        use '--global' or '--switch <switch>'"))
    | (`Global | `Switch) as scope ->
      match cmd, apply with
      | _ , `empty ->
        (match scope with
         | `Switch ->
           OpamGlobalState.with_ `Lock_none @@ fun gt ->
           (match cmd with
            | `var -> OpamConfigCommand.vars_list_switch gt
            | `option -> OpamConfigCommand.options_list_switch gt);
           `Ok ()
         | `Global ->
           OpamGlobalState.with_ `Lock_none @@ fun gt ->
           (match cmd with
            | `var -> OpamConfigCommand.vars_list_global gt
            | `option -> OpamConfigCommand.options_list_global gt);
           `Ok ())
      | _, `value_wo_eq v ->
        (match scope with
         | `Switch ->
           OpamGlobalState.with_ `Lock_none @@ fun gt ->
           (match cmd with
            | `var -> OpamConfigCommand.var_show_switch gt v
            | `option -> OpamConfigCommand.option_show_switch gt v);
           `Ok ()
         | `Global ->
           OpamGlobalState.with_ `Lock_none @@ fun gt ->
           (match cmd with
            | `var -> OpamConfigCommand.var_show_global gt v
            | `option -> OpamConfigCommand.option_show_global gt v);
           `Ok ())
      | `var, `value_eq (_,#OpamConfigCommand.append_op) ->
        `Error (true, "var: append operation are not permitted")
      | _, `value_eq (v,u) ->
        match scope with
        | `Switch ->
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          let _st =
            match cmd with
            | `var ->
              OpamConfigCommand.set_var_switch gt v
                (OpamConfigCommand.whole_of_update_op u)
            | `option -> OpamConfigCommand.set_opt_switch gt v u
          in
          `Ok ()
        | `Global ->
          OpamGlobalState.with_ `Lock_write @@ fun gt ->
          let _st =
            match cmd with
            | `var -> OpamConfigCommand.set_var_global gt v
                        (OpamConfigCommand.whole_of_update_op u)
            | `option -> OpamConfigCommand.set_opt_global gt v u
          in
          `Ok ()

end

(* VAR *)
let var_doc = "Display and update the value associated with a given variable"
let var cli =
  let doc = var_doc in
  let man = [
    `S Manpage.s_description;
    `P "Without argument, lists the opam variables currently defined. With a \
        $(i,VAR) argument, prints the value associated with $(i,VAR). \
        Otherwise, sets or updates $(i,VAR)'s value. \
        If no scope is given, it acts on switch variables by default. \
        This command does not perform any variable expansion.";
  ] in
  let open Var_Option_Common in
  let varvalue =
    let docv = "VAR[=[VALUE]]" in
    let doc =
      "If only $(i,VAR) is given, displays its associated value. \
       If $(i,VALUE) is absent, $(i,VAR)'s value is removed. Otherwise, its \
       value is overwritten."
    in
    Arg.(value & pos 0 (some string) None & info ~docv ~doc [])
  in
  let package =
    mk_opt ~cli cli_original ~section:Manpage.s_options ["package"] "PACKAGE"
      "List all variables defined for the given package"
      Arg.(some package_name) None
  in
  let print_var global_options package varvalue global () =
    apply_global_options cli global_options;
    match varvalue, package with
    | _, None ->
      var_option global global_options `var varvalue
    | None, Some pkg ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      (try `Ok (OpamConfigCommand.list st [pkg])
       with Failure msg -> `Error (false, msg))
    | _, _ ->
      `Error (true, "--package can't be specified with a var argument, use \
                     'pkg:var' instead.")
  in
  mk_command_ret  ~cli cli_original "var" ~doc ~man
    Term.(const print_var
          $global_options cli $package $varvalue $global cli)

(* OPTION *)
let option_doc = "Global and switch configuration options settings"
let option cli =
  let doc = option_doc in
  let man = [
    `S Manpage.s_description;
    `P "Without argument, list all configurable fields. If a field name \
        $(i,FIELD) is given, display its content. Otherwise, sets or updates \
        the given field in the global/switch configuration file. \
        For global configuration, $(i,FIELD) is reset to its default initial \
        value, as after a fresh init (use `opam init show-default-opamrc` to \
        display it)."
  ] in
  let open Var_Option_Common in
  let fieldvalue =
    let docv = "FIELD[(=|+=|-=)[VALUE]]" in
    let doc =
      "If only $(i,FIELD) is given, displays its associated value. If \
       $(i,VALUE) is absent, $(i,FIELD)'s value is reverted. Otherwise, its \
       value is updated: overwrite (=), append (+=), or remove of an element \
       (-=)."
    in
    Arg.(value & pos 0 (some string) None & info ~docv ~doc [])
  in
  let option global_options fieldvalue global () =
    apply_global_options cli global_options;
    var_option global global_options `option fieldvalue
  in
  mk_command_ret  ~cli (cli_from cli2_1) "option" ~doc ~man
    Term.(const option
          $global_options cli $fieldvalue $global cli)

module Common_config_flags = struct
  let sexp cli =
    mk_flag ~cli cli_original ["sexp"]
      "Print environment as an s-expression rather than in shell format"

  let inplace_path cli =
    mk_flag ~cli cli_original ["inplace-path"]
      "When updating the $(i,PATH) variable, replace any pre-existing opam \
       path in-place rather than putting the new path in front. This means \
       programs installed in opam that were shadowed will remain so after \
       $(b,opam env)"

  let set_opamroot cli =
    mk_flag ~cli cli_original ["set-root"]
      "With the $(b,env) and $(b,exec) subcommands, also sets the \
       $(i,OPAMROOT) variable, making sure further calls to opam will use the \
       same root."

  let set_opamswitch cli =
    mk_flag ~cli cli_original ["set-switch"]
      "With the $(b,env) and $(b,exec) subcommands, also sets the \
       $(i,OPAMSWITCH) variable, making sure further calls to opam will use \
       the same switch as this one."

end

(* CONFIG *)
let config_doc = "Display configuration options for packages."
let config cli =
  let shell = OpamStd.Sys.guess_shell_compat () in
  let doc = config_doc in
  let commands = [
    cli_original, "env", `env, [],
    Printf.sprintf
      "Returns the bindings for the environment variables set in the \
       current switch, e.g. PATH, in a format intended to be evaluated by \
       a shell. With $(i,-v), add comments documenting the reason or \
       package of origin for each binding. This is most usefully used as \
       $(b,%s) to have further shell commands be evaluated in the proper \
       opam context. Can also be accessed through $(b,opam env)."
      OpamEnv.(shell_eval_invocation shell "opam config env" |> Manpage.escape);
    cli_original, "revert-env", `revert_env, [],
    Printf.sprintf
      "Reverts environment changes made by opam, e.g. $(b,%s) undoes what \
       $(b,%s) did, as much as possible."
      OpamEnv.(shell_eval_invocation shell "opam config revert-env"
               |> Manpage.escape)
      OpamEnv.(shell_eval_invocation shell "opam config env" |> Manpage.escape);
    cli_original, "list", `list, ["[PACKAGE]..."],
    "Without argument, prints a documented list of all available variables. \
     With $(i,PACKAGE), lists all the variables available for these packages.";
    cli_original, "expand", `expand, ["STRING"],
    "Expand variable interpolations in the given string";
    cli_original, "subst", `subst, ["FILE..."],
    "Substitute variables in the given files. The strings $(i,%{var}%) are \
     replaced by the value of variable $(i,var) (see $(b,var)).";
    cli_original, "report", `report, [],
    "Prints a summary of your setup, useful for bug-reports.";
    cli_original, "cudf-universe", `cudf, ["[FILE]"],
    "Outputs the current available package universe in CUDF format.";
    cli_original, "pef-universe", `pef, ["[FILE]"],
    "Outputs the current package universe in PEF format.";
    (* Deprecated options *)
    cli_between ~option:`default cli2_0 cli2_1 ~replaced:"opam exec", "exec",
    `exec, ["[--] COMMAND"; "[ARG]..."],
    "Execute $(i,COMMAND) with the correct environment variables. This command \
     can be used to cross-compile between switches using $(b,opam config exec \
     --switch=SWITCH -- COMMAND ARG1 ... ARGn). Opam expansion takes place in \
     command and args. If no switch is present on the command line or in the \
     $(i,OPAMSWITCH) environment variable, $(i,OPAMSWITCH) is not set in \
     $(i,COMMAND)'s environment. Can also be accessed through $(b,opam exec).";
    cli_between ~option:`default cli2_0 cli2_1 ~replaced:"opam var", "set", `set,
    ["VAR";"VALUE"], "Set switch variable";
    cli_between ~option:`default cli2_0 cli2_1 ~replaced:"opam var", "unset",
    `unset, ["VAR"], "Unset switch variable";
    cli_between ~option:`default cli2_0 cli2_1 ~replaced:"opam var", "set-global",
    `set_global, ["VAR";"VALUE"], "Set global variable";
    cli_between ~option:`default cli2_0 cli2_1 ~replaced:"opam var",
    "unset-global", `unset_global, ["VAR"], "Unset global variable";
    cli_between ~option:`default cli2_0 cli2_1 ~replaced:"opam var", "var", `var,
    ["VAR"],
    "Return the value associated with variable $(i,VAR), looking in switch \
     first, global if not found. Package variables can be accessed with the \
     syntax $(i,pkg:var). Can also be accessed through $(b,opam var VAR)";
  ] in
  let man = [
    `S Manpage.s_description;
    `P "This command uses opam state to output information on how to use \
        installed libraries, update the $(b,PATH), and substitute \
        variables used in opam packages.";
    `P "Apart from $(b,opam config env), most of these commands are used \
        by opam internally, and are of limited interest for the casual \
        user.";
  ] @ mk_subdoc ~cli commands
    @ [`S Manpage.s_options]
  in

  let command, params = mk_subcommands ~cli commands in
  let open Common_config_flags in

  let config global_options
      command shell sexp inplace_path
      set_opamroot set_opamswitch params () =
    apply_global_options cli global_options;
    let shell = match shell with
      | Some s -> s
      | None -> OpamStd.Sys.guess_shell_compat ()
    in
    let pwsh = match shell with SH_pwsh _ -> true | _ -> false in
    match command, params with
    | Some `env, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (match OpamStateConfig.get_switch_opt () with
       | None -> `Ok ()
       | Some sw ->
         `Ok (OpamConfigCommand.env gt sw
                ~set_opamroot ~set_opamswitch
                ~csh:(shell=SH_csh) ~sexp ~fish:(shell=SH_fish)
                ~pwsh ~cmd:(shell=SH_win_cmd)
                ~inplace_path))
    | Some `revert_env, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (match OpamStateConfig.get_switch_opt () with
       | None -> `Ok ()
       | Some sw ->
         `Ok (OpamConfigCommand.ensure_env gt sw;
              OpamConfigCommand.print_eval_env
                ~csh:(shell=SH_csh) ~sexp ~fish:(shell=SH_fish)
                ~pwsh ~cmd:(shell=SH_win_cmd)
                (OpamEnv.add [] [])))
    | Some `list, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.vars_list gt)
    | Some `list, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      (try `Ok (OpamConfigCommand.list st
                  (List.map OpamPackage.Name.of_string params))
       with Failure msg -> `Error (false, msg))
    | Some `expand, [str] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.expand gt str)
    | Some `var, [var] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (try `Ok (OpamConfigCommand.var_show gt var)
       with Failure msg -> `Error (false, msg))
    | Some `subst, (_::_ as files) ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.subst gt
             (List.map OpamFilename.Base.of_string files))
    | Some `pef, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      (match params with
       | [] | ["-"] -> OpamSwitchState.dump_pef_state st stdout; `Ok ()
       | [file] ->
         let oc = open_out file in
         OpamSwitchState.dump_pef_state st oc;
         close_out oc;
         `Ok ()
       | _ -> bad_subcommand ~cli commands ("config", command, params))
    | Some `cudf, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun opam_state ->
      let opam_univ =
        OpamSwitchState.universe opam_state
          ~requested:opam_state.packages
          Query
      in
      let dump oc = OpamSolver.dump_universe opam_univ oc in
      (match params with
       | [] -> `Ok (dump stdout)
       | [file] -> let oc = open_out file in dump oc; close_out oc; `Ok ()
       | _ -> bad_subcommand ~cli commands ("config", command, params))
    | Some `report, [] -> (
        let print label fmt = OpamConsole.msg ("# %-20s "^^fmt^^"\n") label in
        OpamConsole.msg "# opam config report\n";
        print "opam-version" "%s "
          (OpamVersion.to_string (OpamVersion.full ()));
        print "self-upgrade" "%s"
          (if self_upgrade_status global_options = `Running then
             OpamFilename.prettify
               (fst (self_upgrade_exe (OpamStateConfig.(!r.root_dir))))
           else "no");
        try
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          print "system" "%s" (OpamSysPoll.to_string gt.global_variables);
          let module Solver = (val OpamSolverConfig.(Lazy.force !r.solver)) in
          print "solver" "%s"
            (OpamCudfSolver.get_name (module Solver));
          print "install-criteria" "%s"
            (OpamSolverConfig.criteria `Default);
          print "upgrade-criteria" "%s"
            (OpamSolverConfig.criteria `Upgrade);
          let nprint label n =
            if n <> 0 then [Printf.sprintf "%d (%s)" n label]
            else [] in
          print "jobs" "%d" (Lazy.force OpamStateConfig.(!r.jobs));
          match OpamStateConfig.get_switch_opt () with
          | None -> print "current-switch" "%s" "non set"; `Ok ()
          | Some switch ->
            OpamSwitchState.with_ `Lock_none ~switch gt @@ fun state ->
            print "repositories" "%s"
              (let repos = state.switch_repos.repositories in
               let default, nhttp, nlocal, nvcs =
                 OpamRepositoryName.Map.fold
                   (fun _ repo (dft, nhttp, nlocal, nvcs) ->
                      let dft =
                        if OpamUrl.root repo.repo_url =
                           OpamUrl.root OpamInitDefaults.repository_url
                        then
                          OpamRepositoryName.Map.find
                            repo.repo_name
                            state.switch_repos.repos_definitions |>
                          OpamFile.Repo.stamp
                        else dft
                      in
                      match repo.repo_url.OpamUrl.backend with
                      | `http -> dft, nhttp+1, nlocal, nvcs
                      | `rsync -> dft, nhttp, nlocal+1, nvcs
                      | _ -> dft, nhttp, nlocal, nvcs+1)
                   repos (None,0,0,0)
               in
               String.concat ", "
                 (nprint "http" nhttp @
                  nprint "local" nlocal @
                  nprint "version-controlled" nvcs) ^
               match default with
               | Some v -> Printf.sprintf " (default repo at %s)" v
               | None -> ""
              );
            print "pinned" "%s"
              (if OpamPackage.Set.is_empty state.pinned then "0" else
               let pinnings =
                 OpamPackage.Set.fold (fun nv acc ->
                     let opam = OpamSwitchState.opam state nv in
                     let kind =
                       if Some opam =
                          OpamPackage.Map.find_opt nv state.repos_package_index
                       then "version"
                       else
                         OpamStd.Option.to_string ~none:"local"
                           (fun u -> OpamUrl.string_of_backend u.OpamUrl.backend)
                           (OpamFile.OPAM.get_url opam)
                     in
                     OpamStd.String.Map.update kind succ 0 acc)
                   state.pinned OpamStd.String.Map.empty
               in
               String.concat ", "
                 (List.flatten (List.map (fun (k,v) -> nprint k v)
                                  (OpamStd.String.Map.bindings pinnings)))
              );
            print "current-switch" "%s"
              (OpamSwitch.to_string state.switch);
            let process nv =
              try
                let conf = OpamSwitchState.package_config state nv.name in
                let bindings =
                  let f (name, value) =
                    (OpamVariable.Full.create nv.name name,
                     OpamVariable.string_of_variable_contents value)
                  in
                  List.map f (OpamFile.Dot_config.bindings conf)
                in
                let print (name, value) =
                  let name = OpamVariable.Full.to_string name in
                  print name "%s" value
                in
                List.iter print bindings
              with Not_found -> ()
            in
            state.installed
            |> OpamPackage.Set.filter (fun p ->
                match OpamSwitchState.opam_opt state p with
                | Some o -> OpamFile.OPAM.has_flag Pkgflag_Compiler o
                | None -> false)
            |> OpamSolver.dependencies ~depopts:true ~post:true ~build:true
              ~installed:true
              (OpamSwitchState.universe ~test:true ~doc:true ~dev_setup:true
                 ~requested:OpamPackage.Set.empty state Query)
            |> OpamPackage.Set.iter process;
            if List.mem "." (OpamStd.Sys.split_path_variable (Sys.getenv "PATH"))
            then OpamConsole.warning
                "PATH contains '.' : this is a likely cause of trouble.";
            `Ok ()
        with e -> print "read-state" "%s" (Printexc.to_string e); `Ok ())
    (* deprecated *)
    | Some `exec, (_::_ as c) ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.exec
             gt ~set_opamroot ~set_opamswitch ~inplace_path ~no_switch:false c)
    | Some (`set | `unset as cmd), var::value ->
      let args =
        match cmd,value with
        | `unset, [] -> Some None
        | `set, v::_ -> Some (Some v)
        |  _, _ -> None
      in
      (match args with
       | None ->
         bad_subcommand ~cli commands ("config", command, params)
       | Some opt_value ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         let value =
           OpamStd.Option.map_default (fun v -> `Overwrite v)
             `Revert opt_value
         in
         let _ = OpamConfigCommand.set_var_switch gt var value in
         `Ok ())
    | Some (`set_global | `unset_global as cmd), var::value ->
      let args =
        match cmd,value with
        |`unset_global, [] -> Some None
        | `set_global, v::_ -> Some (Some v)
        |  _, _ -> None
      in
      (match args with
       | None ->
         bad_subcommand ~cli commands ("config", command, params)
       | Some opt_value ->
         OpamGlobalState.with_ `Lock_write @@ fun gt ->
         let value =
           OpamStd.Option.map_default (fun v -> `Overwrite v)
             `Revert opt_value
         in
         let _gt = OpamConfigCommand.set_var_global gt var value in
         `Ok ())
    | command, params -> bad_subcommand ~cli commands ("config", command, params)
  in

  mk_command_ret  ~cli cli_original "config" ~doc ~man
    Term.(const config
          $global_options cli $command $shell_opt cli cli_original $sexp cli
          $inplace_path cli
          $set_opamroot cli $set_opamswitch cli
          $params)

(* EXEC *)
let exec_doc = "Executes a command in the proper opam environment"
let exec cli =
  let doc = exec_doc in
  let man = [
    `S Manpage.s_description;
    `P "Execute $(i,COMMAND) with the correct environment variables. This \
        command can be used to cross-compile between switches using $(b,opam \
        config exec --switch=SWITCH -- COMMAND ARG1 ... ARGn). Opam expansion \
        takes place in command and args. If no switch is present on the \
        command line or in the $(i,OPAMSWITCH) environment variable, \
        $(i,OPAMSWITCH) is not set in $(i,COMMAND)'s environment.";
    `P "This is a shortcut, and equivalent to $(b,opam config exec).";
  ] in
  let cmd =
    Arg.(non_empty & pos_all string [] & info ~docv:"COMMAND [ARG]..." [])
  in
  let no_switch =
    mk_flag ~cli (cli_from cli2_2) ["no-switch"]
      "Execute the command with the updates to the environment done by opam \
       reverted instead."
  in
  let exec global_options inplace_path set_opamroot set_opamswitch no_switch cmd () =
    apply_global_options cli global_options;
    if set_opamswitch && no_switch then
      `Error (true, "--no-switch and --set-switch option can't be used together")
    else
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.exec gt
             ~set_opamroot ~set_opamswitch ~inplace_path ~no_switch cmd)
  in
  let open Common_config_flags in
  mk_command_ret  ~cli cli_original "exec" ~doc ~man
    Term.(const exec $global_options cli $inplace_path cli
          $set_opamroot cli $set_opamswitch cli $no_switch
          $cmd)

(* ENV *)
let env_doc = "Prints appropriate shell variable assignments to stdout"
let env cli =
  let shell = OpamStd.Sys.guess_shell_compat () in
  let doc = env_doc in
  let man = [
    `S Manpage.s_description;
    `P (Printf.sprintf
        "Returns the bindings for the environment variables set in the current \
         switch, e.g. PATH, in a format intended to be evaluated by a shell. \
         With $(i,-v), add comments documenting the reason or package of origin \
         for each binding. This is most usefully used as $(b,%s) \
         to have further shell commands be evaluated in the proper opam \
         context."
        OpamEnv.(
          shell_eval_invocation shell (opam_env_invocation shell)
            |> Manpage.escape));
    `P "This is a shortcut, and equivalent to $(b,opam config env).";
  ] in
  let revert =
    mk_flag ~cli cli_original ["revert"]
      "Output the environment with updates done by opam reverted instead."
  in
  let check =
    mk_flag ~cli (cli_from cli2_1) ["check"]
      "Exits with 0 if the environment is already up-to-date, 1 otherwise, \
       after printing the list of not up-to-date variables."
  in
  let env
      global_options shell sexp inplace_path set_opamroot set_opamswitch
      revert check () =
    apply_global_options cli global_options;
    if check then
      (OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      if not (OpamEnv.is_up_to_date ~skip:false st) then
        OpamStd.Sys.exit_because `False)
    else
    let shell = match shell with
      | Some s -> s
      | None -> OpamStd.Sys.guess_shell_compat ()
    in
    let pwsh = match shell with SH_pwsh _ -> true | _ -> false in
    match revert with
    | false ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (match OpamStateConfig.get_switch_opt () with
       | None -> ()
       | Some sw ->
         OpamConfigCommand.env gt sw
           ~set_opamroot ~set_opamswitch
           ~csh:(shell=SH_csh) ~sexp ~fish:(shell=SH_fish)
           ~pwsh ~cmd:(shell=SH_win_cmd)
           ~inplace_path);
    | true ->
      OpamConfigCommand.print_eval_env
        ~csh:(shell=SH_csh) ~sexp ~fish:(shell=SH_fish)
        ~pwsh ~cmd:(shell=SH_win_cmd)
        (OpamEnv.add [] [])
  in
  let open Common_config_flags in
  mk_command  ~cli cli_original "env" ~doc ~man
  Term.(const env
        $global_options cli $shell_opt cli cli_original $sexp cli
        $inplace_path cli $set_opamroot cli $set_opamswitch cli
        $revert $check)

(* INSTALL *)
let install_doc = "Install a list of packages."
let install cli =
  let doc = install_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command installs one or more packages inside the currently \
        selected switch (see $(b,opam switch)). Once installed, you can remove \
        packages with $(b,opam remove), upgrade them with $(b,opam upgrade), \
        and list them with $(b,opam list). See $(b,opam pin) as well to manage \
        package versions, reroute existing packages or add packages that are \
        not defined in the repositories.";
    `P "All required dependencies of the selected packages will be installed \
        first. Any already installed packages having dependencies, or optional \
        dependencies to the changed packages will be recompiled. The proposed \
        solution may also imply removing incompatible or conflicting \
        packages.";
    `P "If paths are provided as argument instead of packages, they are \
        assumed to point to either project source directories containing one \
        or more package definitions ($(i,opam) files), or directly to \
        $(i,opam) files. Then the corresponding packages will be pinned to \
        their local directory and installed (unless $(b,--deps-only) was \
        specified).";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
  ] @ OpamArg.man_build_option_section
   in
  let add_to_roots =
    mk_vflag ~cli None [
      cli_original, (Some true), ["set-root"],
      "Mark given packages as installed roots. This is the default \
       for newly manually-installed packages.";
      cli_original, (Some false), ["unset-root"],
      "Mark given packages as \"installed automatically\".";
    ]
  in
  let deps_only =
    mk_flag ~cli cli_original  ["deps-only"]
      "Install all its dependencies, but don't actually install the package."
  in
  let ignore_conflicts =
    mk_flag ~cli (cli_from cli2_1) ["ignore-conflicts"]
      "Used with $(b,--deps-only), ignores conflicts of given package"
  in
  let download_only =
    mk_flag ~cli (cli_from cli2_1) ["download-only"]
      "Fetch the sources of the packages, but don't build or install anything."
  in
  let restore =
    mk_flag ~cli cli_original ["restore"]
      "Attempt to restore packages that were marked for installation but have \
       been removed due to errors"
  in
  let destdir =
    mk_opt ~cli cli_original ["destdir"] "DIR"
      "Copy the files installed by the given package within the current opam \
       switch below the prefix $(i,DIR), respecting their hierarchy, after \
       installation. Caution, calling this can overwrite, but never remove \
       files, even if they were installed by a previous use of $(b,--destdir), \
       e.g. on a previous version of the same package. See $(b,opam remove \
       --destdir) to revert."
      Arg.(some dirname) None
  in
  let check =
    mk_flag ~cli (cli_from cli2_1) ["check"]
      "Exit with 0 if all the dependencies of $(i,PACKAGES) are already \
       installed. If not, output the names of the missing dependencies to \
       stdout, and exits with 1."
  in
  let depext_only =
    mk_flag ~cli (cli_from cli2_1) ["depext-only"]
      "Resolves the package installation normally, but only installs \
       the required system dependencies, without affecting the opam switch \
       state or installing opam packages."
  in
  let install
      global_options build_options add_to_roots deps_only ignore_conflicts
      restore destdir assume_built check recurse subpath depext_only formula
      download_only atoms_or_locals () =
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    if atoms_or_locals = [] && not restore && formula = OpamFormula.Empty then
      `Error (true, "required argument PACKAGES is missing")
    else
    if assume_built && (deps_only || formula <> OpamFormula.Empty || depext_only) then
      `Error (true, "option --assume-built is not compatible with --deps-only, \
                     --formula or --depext-only")
    else
    if depext_only
    && (OpamClientConfig.(!r.assume_depexts)
        || OpamStateConfig.(!r.no_depexts)) then
      `Error (true,
              Printf.sprintf "--depext-only and --%s can't be used together"
                (if OpamClientConfig.(!r.assume_depexts) then "assume-depexts"
                 else "no-depexts"))
    else
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    let pure_atoms =
      OpamStd.List.filter_map (function `Atom a -> Some a | _ -> None)
        atoms_or_locals
    in
    let atoms_or_locals =
      if restore then
        let to_restore = OpamPackage.Set.diff st.installed_roots st.installed in
        if OpamPackage.Set.is_empty to_restore then
          OpamConsole.msg "No packages to restore found\n"
        else
          OpamConsole.msg "Packages to be restored: %s\n"
            (OpamPackage.Name.Set.to_string
               (OpamPackage.names_of_packages to_restore));
        atoms_or_locals @
        List.map (fun p -> `Atom (OpamSolution.atom_of_package p))
          (OpamPackage.Set.elements to_restore)
      else atoms_or_locals
    in
    if formula = OpamFormula.Empty && atoms_or_locals = [] then `Ok () else
    let st, atoms =
      OpamAuxCommands.autopin
        st ~recurse ?subpath ~quiet:check ~simulate:(deps_only||check||depext_only)
        ?locked:OpamStateConfig.(!r.locked) atoms_or_locals
    in
    if formula = OpamFormula.Empty && atoms = [] then
      (OpamConsole.msg "Nothing to do\n";
       OpamStd.Sys.exit_because `Success);
    if check then
       let missing =
         OpamPackage.Map.fold (fun _ -> OpamPackage.Name.Set.union)
           (OpamClient.check_installed ~build:true ~post:true st atoms)
           (OpamPackage.Name.Set.empty)
       in
       if OpamPackage.Name.Set.is_empty missing then
         (OpamConsole.errmsg "All dependencies installed\n";
          OpamStd.Sys.exit_because `Success)
       else
         (OpamConsole.errmsg "Missing dependencies:\n";
          OpamConsole.msg "%s\n"
            (OpamStd.List.concat_map " " OpamPackage.Name.to_string
               (OpamPackage.Name.Set.elements missing));
          OpamStd.Sys.exit_because `False)
    else
    let st =
      OpamClient.install st atoms ~formula
        ~autoupdate:pure_atoms ?add_to_roots ~deps_only ~ignore_conflicts
        ~assume_built ~depext_only ~download_only
    in
    match destdir with
    | None -> `Ok ()
    | Some dest ->
      let packages = OpamFormula.packages_of_atoms st.installed atoms in
      OpamAuxCommands.copy_files_to_destdir st dest packages;
      `Ok ()
  in
  mk_command_ret  ~cli cli_original "install" ~doc ~man
    Term.(const install $global_options cli $build_options cli
          $add_to_roots $deps_only $ignore_conflicts $restore $destdir
          $assume_built cli $check $recurse cli $subpath cli $depext_only
          $formula_flag cli $download_only $atom_or_local_list)

(* REMOVE *)
let remove_doc = "Remove a list of packages."
let remove cli =
  let doc = remove_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command uninstalls one or more packages currently \
        installed in the currently selected compiler switch. To remove packages \
        installed in another compiler, you need to switch compilers using \
        $(b,opam switch) or use the $(b,--switch) flag. This command is the \
        inverse of $(b,opam-install).";
    `P "If a directory name is specified as package, packages pinned to that \
        directory are both unpinned and removed.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
  ] @ OpamArg.man_build_option_section
  in
  let autoremove =
    mk_flag ~cli cli_original ["a";"auto-remove"]
      "Remove all the packages which have not been explicitly installed and \
       which are not necessary anymore. It is possible to prevent the removal \
       of an already-installed package by running $(b,opam install <pkg> \
       --set-root). This flag can also be set using the $(b,\\$OPAMAUTOREMOVE) \
       configuration variable." in
  let force =
    mk_flag ~cli cli_original ["force"]
      "Execute the remove commands of given packages directly, even if they are \
       not considered installed by opam." in
  let destdir =
    mk_opt ~cli cli_original ["destdir"] "DIR"
      "Instead of uninstalling the packages, reverts the action of $(b,opam \
       install --destdir): remove files corresponding to what the listed \
       packages installed to the current switch from the given $(i,DIR). Note \
       that the package needs to still be installed to the same version that \
       was used for $(b,install --destdir) for this to work reliably. The \
       packages are not removed from the current opam switch when this is \
       specified."
      Arg.(some dirname) None
  in
  let remove global_options build_options autoremove force destdir recurse
      subpath formula atom_locs () =
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    match destdir with
    | Some d ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      let atoms = OpamAuxCommands.resolve_locals_pinned st atom_locs in
      let packages = OpamFormula.packages_of_atoms st.installed atoms in
      let uninst =
        List.filter (fun (name, _) -> not (OpamPackage.has_name packages name))
          atoms
      in
      if uninst <> [] then
        OpamConsole.warning
          "Can't remove the following packages from the given destdir, they \
           need to be installed in opam: %s"
          (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom uninst);
      OpamAuxCommands.remove_files_from_destdir st d packages
    | None ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let pure_atoms, pin_atoms =
        List.partition (function `Atom _ -> true | _ -> false) atom_locs
      in
      let pin_atoms =
        OpamAuxCommands.resolve_locals_pinned st ~recurse ?subpath pin_atoms
      in
      let st =
        if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show) then st
        else OpamPinCommand.unpin st (List.map fst pin_atoms)
      in
      let atoms =
        List.map (function `Atom a -> a | _ -> assert false) pure_atoms
        @ pin_atoms
      in
      let autoremove = autoremove || OpamClientConfig.(!r.autoremove) in
      OpamSwitchState.drop
        (OpamClient.remove st ~autoremove ~force ~formula atoms)
  in
  mk_command  ~cli cli_original "remove" ~doc ~man
    Term.(const remove $global_options cli $build_options cli $autoremove
          $force $destdir $recurse cli $subpath cli $formula_flag cli
          $atom_or_dir_list)

(* REINSTALL *)
let reinstall cli =
  let doc = "Reinstall a list of packages." in
  let man = [
    `S Manpage.s_description;
    `P "This command removes the given packages and the ones that depend on \
        them, and reinstalls the same versions. Without arguments, assume \
        $(b,--pending) and reinstall any package with upstream changes.";
    `P "If a directory is specified as argument, anything that is pinned to \
        that directory is selected for reinstall.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
  ] @ OpamArg.man_build_option_section
  in
  let cmd =
    mk_vflag ~cli `Default [
        cli_original, `Pending, ["pending"],
          "Perform pending reinstallations, i.e. reinstallations of \
                packages that have changed since installed";
        cli_original, `List_pending, ["list-pending"],
          "List packages that have been changed since installed and are \
                marked for reinstallation";
        cli_original, `Forget_pending, ["forget-pending"],
        "Forget about pending reinstallations of listed packages. This implies \
         making opam assume that your packages were installed with a newer \
         version of their metadata, so only use this if you know what you are \
         doing, and the actual changes you are overriding. It may be safer to \
         use $(b,opam pin --current) on the packages with pending reinstalls, \
         explicitely locking them in their current state."
      ]
  in
  let reinstall global_options build_options assume_built recurse subpath
      atoms_locs cmd () =
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    let open OpamPackage.Set.Op in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    match cmd, atoms_locs with
    | `Default, (_::_ as atom_locs) ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      OpamSwitchState.drop @@ OpamClient.reinstall st ~assume_built
        (OpamAuxCommands.resolve_locals_pinned st ~recurse ?subpath atom_locs);
      `Ok ()
    | `Pending, [] | `Default, [] ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let atoms = OpamSolution.eq_atoms_of_packages (Lazy.force st.reinstall) in
      OpamSwitchState.drop @@ OpamClient.reinstall st atoms;
      `Ok ()
    | `List_pending, [] ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      OpamListCommand.display st
        { OpamListCommand.default_package_listing_format
          with OpamListCommand.
            columns = [OpamListCommand.Package];
            short = true;
            header = false;
            order = `Dependency;
        }
        (Lazy.force st.reinstall);
      `Ok ()
    | `Forget_pending, atom_locs ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let atoms = OpamAuxCommands.resolve_locals_pinned ~recurse ?subpath st atom_locs in
      let reinstall = Lazy.force st.reinstall in
      let to_forget = match atoms with
        | [] -> reinstall
        | atoms -> OpamFormula.packages_of_atoms reinstall atoms
      in
      OpamPackage.Set.iter (fun nv ->
          try
            let installed = OpamPackage.Map.find nv st.installed_opams in
            let upstream = OpamPackage.Map.find nv st.opams in
            if not (OpamFile.OPAM.effectively_equal installed upstream) &&
               OpamConsole.confirm
                 "Metadata of %s were updated. Force-update, without performing \
                  the reinstallation?" (OpamPackage.to_string nv)
            then OpamSwitchAction.install_metadata st nv
          with Not_found -> ())
        to_forget;
      let reinstall = reinstall -- to_forget in
      OpamSwitchState.drop @@ OpamSwitchAction.update_switch_state ~reinstall st;
      `Ok ()
    | _, _::_ ->
      `Error (true, "Package arguments not allowed with this option")
  in
  mk_command_ret  ~cli cli_original "reinstall" ~doc ~man
    Term.(const reinstall $global_options cli $build_options cli
          $assume_built cli $recurse cli $subpath cli $atom_or_dir_list
          $cmd)

(* UPDATE *)
let update_doc = "Update the list of available packages."
let update cli =
  let doc = update_doc in
  let man = [
    `S Manpage.s_description;
    `P "Update the package definitions. This fetches the newest version of the \
        repositories configured through $(b, opam repository), and the sources \
        of installed development packages and packages pinned in the current \
        switch. To use the updated sources and definitions, use \
        $(b,opam upgrade).";
  ] in
  let repos_only =
    mk_flag ~cli cli_original ["R"; "repositories"]
      "Update repositories (skipping development packages unless \
       $(b,--development) is also specified)." in
  let dev_only =
    mk_flag ~cli cli_original ["development"]
      "Update development packages (skipping repositories unless \
       $(b,--repositories) is also specified)." in
  let depexts_only =
    mk_flag ~cli (cli_from cli2_1) ["depexts"]
      "Request the system package manager to update its databases (skipping \
       all opam packages, unless $(b,--development) or $(b,--repositories) is \
       also specified). This generally requires $(b,sudo) rights." in
  let upgrade =
    mk_flag ~cli cli_original ["u";"upgrade"]
      "Automatically run $(b,opam upgrade) after the update." in
  let name_list =
    arg_list "NAMES"
      "List of repository or development package names to update."
      Arg.string in
  let all =
    mk_flag ~cli cli_original ["a"; "all"]
      "Update all configured repositories, not only what is set in the current \
       switch" in
  let check =
    mk_flag ~cli cli_original ["check"]
      "Do the update, then return with code 0 if there were any upstream \
       changes, 1 if there were none. Repositories or development packages \
       that failed to update are considered without changes. With \
       $(b,--upgrade), applies to the upgrade step: that is $(b,opam update \
       --upgrade --check) behaves like $(b,opam update && opam upgrade --check), \
       returning 0 if there are available upgrades, rather than upstream updates." in
  let update global_options jobs names repos_only dev_only depexts_only all
      check upgrade () =
    apply_global_options cli global_options;
    OpamStateConfig.update
      ?jobs:OpamStd.Option.Op.(jobs >>| fun j -> lazy j)
      ();
    OpamClientConfig.update ();
    if depexts_only then OpamSysInteract.update ();
    if depexts_only && not (repos_only || dev_only) then () else
    OpamGlobalState.with_ `Lock_write @@ fun gt ->
    let success, changed, rt =
      OpamClient.update gt
        ~repos_only:(repos_only && not dev_only)
        ~dev_only:(dev_only && not repos_only)
        ~all
        names
    in
    OpamStd.Exn.finally (fun () -> OpamRepositoryState.drop rt)
    @@ fun () ->
    if upgrade then
      OpamSwitchState.with_ `Lock_write gt ~rt @@ fun st ->
      OpamConsole.msg "\n";
      OpamSwitchState.drop @@ OpamClient.upgrade st ~check ~all:true []
    else if check then
      OpamStd.Sys.exit_because (if changed then `Success else `False)
    else if changed then
      OpamConsole.msg "Now run 'opam upgrade' to apply any package updates.\n";
    if not success then OpamStd.Sys.exit_because `Sync_error
  in
  mk_command  ~cli cli_original "update" ~doc ~man
  Term.(const update $global_options cli $jobs_flag cli cli_original $name_list
        $repos_only $dev_only $depexts_only $all $check $upgrade)

(* UPGRADE *)
let upgrade_doc = "Upgrade the installed package to latest version."
let upgrade cli =
  let doc = upgrade_doc in
  let man = [
    `S Manpage.s_description;
    `P "This command upgrades the installed packages to their latest available \
        versions. More precisely, this command calls the dependency solver to \
        find a consistent state where $(i,most) of the installed packages are \
        upgraded to their latest versions.";
    `P "If a directory is specified as argument, anything that is pinned to \
        that directory is selected for upgrade.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
  ] @ OpamArg.man_build_option_section
  in
  let fixup =
    mk_flag ~cli cli_original ["fixup"]
      "Recover from a broken state (eg. missing dependencies, two conflicting \
       packages installed together...)." in
  let check =
    mk_flag ~cli cli_original ["check"]
      "Don't run the upgrade: just check if anything could be upgraded. \
       Returns 0 if that is the case, 1 if there is nothing that can be \
       upgraded." in
  let all =
    mk_flag ~cli cli_original ["a";"all"]
      "Run an upgrade of all installed packages. This is the default if \
       $(i,PACKAGES) was not specified, and can be useful with $(i,PACKAGES) \
       to upgrade while ensuring that some packages get or remain installed."
  in
  let installed =
    mk_flag ~cli (cli_from cli2_1) ["installed"]
      "When a directory is provided as argument, do not install pinned package \
       that are not yet installed." in
  let upgrade global_options build_options fixup check only_installed all
      recurse subpath formula atom_locs () =
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    let all = all || atom_locs = [] in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    if fixup then
      if atom_locs <> [] || check then
        `Error (true, Printf.sprintf "--fixup doesn't allow extra arguments")
      else
        OpamSwitchState.with_ `Lock_write gt @@ fun st ->
        OpamSwitchState.drop @@ OpamClient.fixup ~formula st;
        `Ok ()
    else
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let atoms =
        OpamAuxCommands.resolve_locals_pinned st ~recurse ?subpath atom_locs
      in
      OpamSwitchState.drop @@
      OpamClient.upgrade st ~check ~only_installed ~all ~formula atoms;
      `Ok ()
  in
  mk_command_ret  ~cli cli_original "upgrade" ~doc ~man
    Term.(const upgrade $global_options cli $build_options cli $fixup $check
          $installed $all $recurse cli $subpath cli $formula_flag cli
          $atom_or_dir_list)

(* REPOSITORY *)
let repository_doc = "Manage opam repositories."
let repository cli =
  let doc = repository_doc in
  let scope_section = "SCOPE SPECIFICATION OPTIONS" in
  let commands = [
    cli_original, "add", `add, ["NAME"; "[ADDRESS]"; "[QUORUM]"; "[FINGERPRINTS]"],
    "Adds under $(i,NAME) the repository at address $(i,ADDRESS) to the list \
     of configured repositories, if not already registered, and sets this \
     repository for use in the current switch (or the specified scope). \
     $(i,ADDRESS) is required if the repository name is not already \
     registered, and is otherwise an error if different from the registered \
     address. The quorum is a positive integer that determines the validation \
     threshold for signed repositories, with fingerprints the trust anchors \
     for said validation.";
    cli_original, " ", `add, [],
    (* using an unbreakable space here will indent the text paragraph at the level
       of the previous labelled paragraph, which is what we want for our note. *)
    "$(b,Note:) By default, the repository is only added to the current \
     switch. To add a repository to other switches, you need to use the \
     $(b,--all) or $(b,--set-default) options (see below). If you want to \
     enable a repository only to install its switches, you may be \
     looking for $(b,opam switch create --repositories=REPOS).";
    cli_original, "remove", `remove, ["NAME..."],
    "Removes the given repositories from the selected scopes (the default scope is the current switch, \
     see $(b,--this-switch)). Repositories are removed completely with $(b,--all-switches). \
     If the last repository is removed in the current switch scope, then the selection of \
     repositories is restored to the default selection ($(b,opam list --set-default)).";
    cli_original, "set-repos", `set_repos, ["NAME..."],
    "Explicitly selects the list of repositories to look up package \
     definitions from, in the specified priority order (overriding previous \
     selection and ranks), according to the specified scope.";
    cli_original, "set-url",  `set_url,
    ["NAME"; "ADDRESS"; "[QUORUM]"; "[FINGERPRINTS]"],
    "Updates the URL and trust anchors associated with a given repository \
     name. Note that if you don't specify $(i,[QUORUM]) and \
     $(i,[FINGERPRINTS]), any previous settings will be erased.";
    cli_original, "list", `list, [],
    "Lists the currently selected repositories in priority order from rank 1. \
     With $(b,--all), lists all configured repositories and the switches \
     where they are active.";
    cli_original, "priority", `priority, ["NAME"; "RANK"],
    "Synonym to $(b,add NAME --rank RANK)";
  ] in
  let man = [
    `S Manpage.s_description;
    `P "This command is used to manage package repositories. Repositories can \
        be registered through subcommands $(b,add), $(b,remove) and \
        $(b,set-url), and are updated from their URLs using $(b,opam update). \
        Their names are global for all switches, and each switch has its own \
        selection of repositories where it gets package definitions from.";
    `P ("Main commands $(b,add), $(b,remove) and $(b,set-repos) act only on \
         the current switch, unless differently specified using options \
         explained in $(b,"^scope_section^").");
    `P "Without a subcommand, or with the subcommand $(b,list), lists selected \
        repositories, or all configured repositories with $(b,--all).";
  ] @ mk_subdoc ~cli ~defaults:["","list"] commands @ [
      `S scope_section;
      `P "These flags allow one to choose which selections are changed by $(b,add), \
          $(b,remove), $(b,set-repos). If no flag in this section is specified \
          the updated selections default to the current switch. Multiple scopes \
          can be selected, e.g. $(b,--this-switch --set-default).";
      `S Manpage.s_options;
    ]
  in
  let command, params = mk_subcommands ~cli commands in
  let scope =
    let scope_info ?docv flags doc =
      Arg.info ~docs:scope_section ~doc ?docv flags
    in
    let flags =
      mk_vflag_all ~cli ~section:scope_section [
        cli_original, `No_selection, ["dont-select"],
          "Don't update any selections";
        cli_original, `This_switch, ["this-switch"],
          "Act on the selections for the current switch (this is the default)";
        cli_original, `Default, ["set-default"],
          "Act on the default repository selection that is used for newly \
           created switches";
        cli_original, `All, ["all-switches";"a"],
          "Act on the selections of all configured switches";
      ]
    in
    let switches =
      Arg.opt Arg.(list string) []
        (scope_info ["on-switches"] ~docv:"SWITCHES"
           "Act on the selections of the given list of switches")
    in
    let switches =
      Term.(const (List.map (fun s -> `Switch (OpamSwitch.of_string s)))
            $ Arg.value switches)
    in
    Term.(const (fun l1 l2 -> match l1@l2 with [] -> [`Current_switch] | l -> l)
          $ flags $ switches)
  in
  let rank =
    mk_opt ~cli cli_original ["rank"] "RANK"
      "Set the rank of the repository in the list of configured repositories. \
      Package definitions are looked in the repositories in increasing rank \
      order, therefore 1 is the highest priority.  Negative ints can be used to \
      select from the lowest priority, -1 being last. $(b,set-repos) can \
      otherwise be used to explicitly set the repository list at once."
      Arg.(int) 1
  in
  let repository global_options command kind short scope rank params () =
    apply_global_options cli global_options;
    let global = List.mem `Default scope in
    let command, params, rank = match command, params, rank with
      | Some `priority, [name; rank], 1 ->
        (try Some `add, [name], int_of_string rank
         with Failure _ ->
           OpamConsole.error_and_exit `Bad_arguments
             "Invalid rank specification %S" rank)
      | Some `priority, [], rank -> Some `add, [], rank
      | command, params, rank -> command, params, rank
    in
    let update_repos new_repo repos =
      let rank =
        if rank < 0 then List.length repos + rank + 1 else rank - 1
      in
      OpamStd.List.insert_at rank new_repo
        (List.filter (( <> ) new_repo ) repos)
    in
    let check_for_repos rt names err =
      match
        List.filter (fun n ->
            not (OpamRepositoryName.Map.mem n rt.repositories))
          names
      with [] -> () | l ->
        err (OpamStd.List.concat_map " " OpamRepositoryName.to_string l)
    in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    let all_switches = OpamFile.Config.installed_switches gt.config in
    let switches =
      let all = OpamSwitch.Set.of_list all_switches in
      List.fold_left (fun acc -> function
          | `Default | `No_selection -> acc
          | `All -> all_switches
          | `Switch sw ->
            if not (OpamSwitch.Set.mem sw all) &&
               not (OpamSwitch.is_external sw)
            then
              OpamConsole.error_and_exit `Not_found
                "No switch %s found"
                (OpamSwitch.to_string sw)
            else if List.mem sw acc then acc
            else acc @ [sw]
          | `Current_switch | `This_switch ->
            match OpamStateConfig.get_switch_opt () with
            | None ->
              OpamConsole.warning "No switch is currently set, perhaps you meant \
                                   '--set-default'?";
              acc
            | Some sw ->
              if List.mem sw acc then acc
              else acc @ [sw])
        [] scope
    in
    match command, params with
    | Some `add, name :: url :: security ->
      let name = OpamRepositoryName.of_string name in
      let backend =
        match kind with
        | Some _ -> kind
        | None -> OpamUrl.guess_version_control url
      in
      let url = OpamUrl.parse ?backend ~from_file:false url in
      let trust_anchors = match security with
        | [] -> None
        | quorum::fingerprints ->
          try
            let quorum = int_of_string quorum in
            if quorum < 0 then failwith "neg" else Some { quorum; fingerprints }
          with Failure _ -> failwith ("Invalid quorum: "^quorum)
      in
      OpamRepositoryState.with_ `Lock_write gt (fun rt ->
          let rt = OpamRepositoryCommand.add rt name url trust_anchors in
          let failed, rt =
            OpamRepositoryCommand.update_with_auto_upgrade rt [name]
          in
          if failed <> [] then
            (OpamRepositoryState.drop @@ OpamRepositoryCommand.remove rt name;
             OpamConsole.error_and_exit `Sync_error
               "Initial repository fetch failed"));
      OpamGlobalState.drop @@ OpamRepositoryCommand.update_selection gt ~global
        ~switches (update_repos name);
      if scope = [`Current_switch] then
        OpamConsole.note
          "Repository %s has been added to the selections of switch %s \
           only.\n\
           Run `opam repository add %s --all-switches|--set-default' to use it \
           in all existing switches, or in newly created switches, \
           respectively.\n"
          (OpamRepositoryName.to_string name)
          (OpamSwitch.to_string (OpamStateConfig.get_switch ()))
          (OpamRepositoryName.to_string name);
      `Ok ()
    | Some `remove, names ->
      let names = List.map OpamRepositoryName.of_string names in
      let rm = List.filter (fun n -> not (List.mem n names)) in
      let full_wipe = List.mem `All scope in
      let global = global || full_wipe in
      let gt =
        OpamRepositoryCommand.update_selection gt
          ~global ~switches:switches rm
      in
      if full_wipe then
        OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
        check_for_repos rt names
          (OpamConsole.warning
             "No configured repositories by these names found: %s");
        OpamRepositoryState.drop @@
        List.fold_left OpamRepositoryCommand.remove rt names
      else if scope = [`Current_switch] then
        OpamConsole.msg
          "Repositories removed from the selections of switch %s. \
           Use '--all' to forget about them altogether.\n"
          (OpamSwitch.to_string (OpamStateConfig.get_switch ()));
      `Ok ()
    | Some `add, [name] ->
      let name = OpamRepositoryName.of_string name in
      OpamRepositoryState.with_ `Lock_none gt (fun rt ->
          check_for_repos rt [name]
            (OpamConsole.error_and_exit `Not_found
               "No configured repository '%s' found, you must specify an URL"));
      OpamGlobalState.drop @@
      OpamRepositoryCommand.update_selection gt ~global ~switches
        (update_repos name);
      `Ok ()
    | Some `set_url, (name :: url :: security) ->
      let name = OpamRepositoryName.of_string name in
      let backend =
        match kind with
        | Some _ -> kind
        | None -> OpamUrl.guess_version_control url
      in
      let url = OpamUrl.parse ?backend ~from_file:false url in
      let trust_anchors = match security with
        | [] -> None
        | quorum::fingerprints ->
          try
            let quorum = int_of_string quorum in
            if quorum < 0 then failwith "neg" else Some { quorum; fingerprints }
          with Failure _ -> failwith ("Invalid quorum: "^quorum)
      in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
      let rt = OpamRepositoryCommand.set_url rt name url trust_anchors in
      let _failed, _rt =
        OpamRepositoryCommand.update_with_auto_upgrade rt [name]
      in
      `Ok ()
    | Some `set_repos, names ->
      let names = List.map OpamRepositoryName.of_string names in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let repos =
        OpamStateConfig.Repos.safe_read ~lock_kind:`Lock_read gt
      in
      let not_found =
        List.filter (fun r -> not (OpamRepositoryName.Map.mem r repos)) names
      in
      if not_found = [] then
        (OpamGlobalState.drop @@
         OpamRepositoryCommand.update_selection gt ~global ~switches
           (fun _ -> names);
         `Ok ())
      else
        OpamConsole.error_and_exit `Bad_arguments
          "No configured repositories by these names found: %s"
          (OpamStd.List.concat_map " " OpamRepositoryName.to_string not_found)
    | (None | Some `list), [] ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      if List.mem `All scope then
        OpamRepositoryCommand.list_all rt ~short;
      let global = List.mem `Default scope in
      let switches =
        if scope = [] ||
           List.exists (function
               | `This_switch | `Current_switch | `Switch _ -> true
               | _ -> false)
             scope
        then switches
        else []
      in
      if not short && scope = [`Current_switch] then
        OpamConsole.note
          "These are the repositories in use by the current switch. Use \
           '--all' to see all configured repositories.";
      OpamRepositoryCommand.list rt ~global ~switches ~short;
      `Ok ()
    | command, params -> bad_subcommand ~cli commands ("repository", command, params)
  in
  mk_command_ret  ~cli cli_original "repository" ~doc ~man
    Term.(const repository $global_options cli $command
          $repo_kind_flag cli cli_original $print_short_flag cli cli_original
          $scope $rank $params)


(* SWITCH *)

(* From a list of strings (either "repo_name" or "repo_name=URL"), configure the
   repos with URLs if possible, and return the updated repos_state and selection of
   repositories *)
let with_repos_rt gt repos f =
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let repos, rt =
  match repos with
  | None -> None, rt
  | Some repos ->
    let repos =
      List.map (fun s ->
          match OpamStd.String.cut_at s '=' with
          | None -> OpamRepositoryName.of_string s, None
          | Some (name, url) -> OpamRepositoryName.of_string name, Some url)
        repos
    in
    let new_defs =
      OpamStd.List.filter_map (function
          | (_, None) -> None
          | (n, Some url) ->
            let repo =
              OpamStd.Option.Op.(
                OpamUrl.parse_opt ~handle_suffix:false ~from_file:false url
                >>| fun u -> n, u)
            in
            if repo = None then
              OpamConsole.warning "Skipping %s, malformed url" url;
            repo)
        repos
    in
    if List.for_all
        (fun (name,url) ->
           match OpamRepositoryName.Map.find_opt name rt.repositories with
           | Some r -> r.repo_url = url
           | None -> false)
        new_defs
    then
      Some (List.map fst repos), rt
    else
      OpamRepositoryState.with_write_lock rt @@ fun rt ->
      let rt =
        List.fold_left (fun rt (name, url) ->
            OpamConsole.msg "Creating repository %s...\n"
              (OpamRepositoryName.to_string name);
            OpamRepositoryCommand.add rt name url None)
          rt new_defs
      in
      let failed, rt =
        OpamRepositoryCommand.update_with_auto_upgrade rt
          (List.map fst new_defs)
      in
      if failed <> [] then
        (OpamRepositoryState.drop @@ List.fold_left
           OpamRepositoryCommand.remove rt failed;
         OpamConsole.error_and_exit `Sync_error
           "Initial fetch of these repositories failed: %s"
           (OpamStd.List.concat_map ", " OpamRepositoryName.to_string failed))
      else
        Some (List.map fst repos), rt
  in
  f (repos, rt)

let switch_doc = "Manage multiple installation prefixes."
let switch cli =
  let shell = OpamStd.Sys.guess_shell_compat () in
  let doc = switch_doc in
  let commands = [
    cli_original, "create", `install, ["SWITCH"; "[COMPILER]"],
    "Create a new switch, and install the given compiler there. $(i,SWITCH) \
     can be a plain name, or a directory, absolute or relative, in which case \
     a local switch is created below the given directory. $(i,COMPILER), if \
     omitted, defaults to $(i,SWITCH) if it is a plain name, unless \
     $(b,--packages), $(b,--formula) or $(b,--empty) is specified. When \
     creating a local switch, and none of these options are present, the \
     compiler is chosen according to the configuration default (see \
     opam-init(1)). If the chosen directory contains package definitions, a \
     compatible compiler is searched within the default selection, and the \
     packages will automatically get installed.";
    cli_original, "set", `set, ["SWITCH"],
    "Set the currently active switch, among the installed switches.";
    cli_original, "remove", `remove, ["SWITCH"],
    "Remove the given switch from disk.";
    cli_original, "export", `export, ["FILE"],
    "Save the current switch state to a file. If $(b,--full) is specified, it \
     includes the metadata of all installed packages, and if $(b,--freeze) is \
     specified, it freezes all vcs to their current commit.";
    cli_original, "import", `import, ["FILE"],
    "Import a saved switch state. If $(b,--switch) is specified and doesn't \
     point to an existing switch, the switch will be created for the import.";
    cli_original, "reinstall", `reinstall, ["[SWITCH]"],
    "Reinstall the given compiler switch and all its packages.";
    cli_original, "list", `list, [],
    "Lists installed switches.";
    cli_original, "list-available", `list_available, ["[PATTERN]"],
    "Lists all the possible packages that are advised for installation when \
     creating a new switch, i.e. packages with the $(i,compiler) flag set. If \
     no pattern is supplied, all versions are shown.";
    cli_original, "show", `current, [],
    "Prints the name of the current switch.";
    cli_from cli2_1, "invariant", `show_invariant, [],
    "Prints the active switch invariant.";
    cli_from cli2_1, "set-invariant", `set_invariant, ["PACKAGES"],
    "Updates the switch invariant, that is, the formula that the switch must \
     keep verifying throughout all operations. The previous setting is \
     overriden. See also options $(b,--force) and $(b,--no-action). Without \
     arguments, an invariant is chosen automatically.";
    cli_original, "set-description", `set_description, ["STRING"],
    "Sets the description for the selected switch.";
    cli_original, "link", `link, ["SWITCH";"[DIR]"],
    "Sets a local alias for a given switch, so that the switch gets \
     automatically selected whenever in that directory or a descendant.";
    cli_between cli2_0 cli2_1 ~replaced:"create", "install", `install, ["SWITCH"],
    "Install a new switch";
    cli_between cli2_0 cli2_1 ~replaced:"set-invariant", "set-base", `set_invariant, ["PACKAGES"],
    "Define a set of switch base packages.";
  ] in
  let man = [
    `S Manpage.s_description;
    `P "This command is used to manage \"switches\", which are independent \
        installation prefixes with their own compiler and sets of installed \
        and pinned packages. This is typically useful to have different \
        versions of the compiler available at once.";
    `P "Use $(b,opam switch create) to create a new switch, and $(b,opam \
        switch set) to set the currently active switch. Without argument, \
        lists installed switches, with one switch argument, defaults to \
        $(b,set).";
    `P (Printf.sprintf
         "Switch handles $(i,SWITCH) can be either a plain name, for switches \
         that will be held inside $(i,~%s.opam), or a directory name, which in \
         that case is the directory where the switch prefix will be installed, as \
         %s. Opam will automatically select a switch by that name found in the \
         current directory or its parents, unless $(i,OPAMSWITCH) is set or \
         $(b,--switch) is specified. When creating a directory switch, if \
         package definitions are found locally, the user is automatically \
         prompted to install them after the switch is created unless \
         $(b,--no-install) is specified."
        OpamArg.dir_sep OpamSwitch.external_dirname);
    `P (Printf.sprintf
         "$(b,opam switch set) sets the default switch globally, but it is also \
         possible to select a switch in a given shell session, using the \
         environment. For that, use $(i,%s)."
        OpamEnv.(
          shell_eval_invocation shell
            (opam_env_invocation ~switch:"SWITCH" ~set_opamswitch:true shell)
            |> Manpage.escape));
  ] @
    mk_subdoc ~cli ~defaults:["","list";"SWITCH","set"]
      ~extra_defaults:[
        cli_from cli2_2, "-",
        "Switches back to the previous non-local switch (similar to $(b,git switch -)). \
         To set the current switch to a switch named $(i,-) use $(b,--cli=2.1) \
         or lower, or use $(b,opam switch set -)"
      ] commands
    @ [
      `S Manpage.s_examples;
      `Pre "    opam switch create 4.08.0";
      `P "Create a new switch called \"4.08.0\" and select it, with a compiler \
          automatically selected at version 4.08.0 (note that this can fail in \
          case there is more than one compiler matching that version).";
      `Pre "    opam switch create ./ --deps-only";
      `P "Prepare a local switch for building the packages defined in $(i,./). \
          This scans the current directory for package definitions, chooses a \
          compatible compiler, creates a local switch and installs the local \
          package dependencies.";
      `Pre "    opam switch create trunk --repos \
            default,beta=git+https://github.com/ocaml/ocaml-beta-repository.git \
            ocaml-variants.4.10.0+trunk";
      `P "Create a new switch called \"trunk\", with \
          $(b,ocaml-variants.4.10.0+trunk) as compiler, with a new $(i,beta) \
          repository bound to the given URL selected besides the default one."
    ]
    @ [`S Manpage.s_options]
    @ OpamArg.man_build_option_section
  in

  let command, params = mk_subcommands_with_default ~cli commands in
  let no_switch =
    mk_flag ~cli cli_original ["no-switch"]
      "Don't automatically select newly installed switches." in
  let packages =
    mk_opt ~cli cli_original ["packages"] "PACKAGES"
      "When installing a switch, explicitly define the set of packages to \
       enforce as the switch invariant."
      Arg.(some (list atom)) None in
  let formula =
    mk_opt ~cli (cli_from cli2_1) ["formula"] "FORMULA"
      "Allows specifying a complete \"dependency formula\", possibly including \
       disjunction cases, as the switch invariant. The format is the same as \
       for expressing dependencies in package definition files, e.g. '\"foo\" \
       {>= \"1.1\"}'"
      Arg.(some OpamArg.dep_formula) None in
  let empty =
    mk_flag ~cli cli_original ["empty"]
      "Allow creating an empty switch, with no invariant." in
  let repos =
    mk_opt ~cli cli_original ["repositories"] "REPOS"
      "When creating a new switch, use the given selection of repositories \
       instead of the default. $(i,REPOS) should be a comma-separated list of \
       either already registered repository names (configured through e.g. \
       $(i,opam repository add --dont-select)), or $(b,NAME)=$(b,URL) \
       bindings, in which case $(b,NAME) should not be registered already to a \
       different URL, and the new repository will be registered. See $(i,opam \
       repository) for more details. This option also affects \
       $(i,list-available)."
      Arg.(some (list string)) None
  in
  let descr =
    mk_opt ~cli cli_original ["description"] "STRING"
      "Attach the given description to a switch when creating it. Use the \
       $(i,set-description) subcommand to modify the description of an \
       existing switch."
      Arg.(some string) None
  in
  let full =
    mk_flag ~cli cli_original ["full"]
      "When exporting, include the metadata of all installed packages, \
       allowing to re-import even if they don't exist in the repositories (the \
       default is to include only the metadata of pinned packages)."
  in
  let freeze =
    mk_flag ~cli (cli_from cli2_1) ["freeze"]
      "When exporting, locks all VCS urls to their current commit, failing if \
       it can not be retrieved. This ensures that an import will restore the \
       exact state. Implies $(b,--full)."
  in
  let no_install =
    mk_flag ~cli cli_original ["no-install"]
      "When creating a local switch, don't look for any local package \
       definitions to install."
  in
  let deps_only =
    mk_flag ~cli cli_original ["deps-only"]
      "When creating a local switch in a project directory (i.e. a directory \
       containing opam package definitions), install the dependencies of the \
       project but not the project itself."
  in
  let force =
    mk_flag ~cli (cli_from cli2_1) ["force"]
      "Only for $(i,set-invariant): force setting the invariant, bypassing \
       consistency checks."
  in
  let no_action =
    mk_flag ~cli (cli_from cli2_1) ["n"; "no-action"]
      "Only for $(i,set-invariant): set the invariant, but don't enforce it \
       right away: wait for the next $(i,install), $(i,upgrade) or similar \
       command."
  in
  (* Deprecated options *)
  let d_alias_of =
    mk_opt ~cli (cli_between cli2_0 cli2_1)
      ["A";"alias-of"] "COMP" "Deprecated" Arg.(some string) None
  in
  let d_no_autoinstall =
      mk_flag ~cli (cli_between cli2_0 cli2_1) ["no-autoinstall"] "Deprecated"
  in
  let switch
      global_options build_options command print_short
      no_switch packages formula empty descr full freeze no_install deps_only repos
      force no_action
      d_alias_of d_no_autoinstall params () =
    if d_alias_of <> None then
      OpamConsole.warning
        "Option %s is deprecated, ignoring it. \
         Use instead 'opam switch <switch-name> <compiler>'"
        (OpamConsole.colorise `bold "--alias-of");
    if d_no_autoinstall then
      OpamConsole.warning "Option %s is deprecated, ignoring it."
        (OpamConsole.colorise `bold "--no-autoinstall");
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    let invariant_arg ?repos rt args =
      match args, packages, formula, empty with
      | [], None, None, false -> None
      | _::_ as packages, None, None, false ->
        Some (OpamSwitchCommand.guess_compiler_invariant ?repos rt packages)
      | [], Some atoms, None, false ->
        let atoms = List.map (fun p -> Atom p) atoms in
        Some (OpamFormula.of_atom_formula (OpamFormula.ands atoms))
      | [], None, (Some f), false -> Some f
      | [], None, None, true -> Some OpamFormula.Empty
      | _::_ as packages, Some atoms, None, false ->
        if fst cli = cli2_0 then
          let atoms = List.map (fun p -> Atom p) atoms in
          let pkgs_formula =
            OpamFormula.of_atom_formula (OpamFormula.ands atoms)
          in
          let args_formula =
            OpamSwitchCommand.guess_compiler_invariant ?repos rt packages
          in
          Some (OpamFormula.And (args_formula, pkgs_formula))
        else
          OpamConsole.error_and_exit `Bad_arguments
            "Individual package and option '--packages' can not be specified at \
             the same time. Use just '--packages' instead, e.g.\n\
             opam switch create flambda \
             --packages=ocaml.4.12.0,ocaml-option-flambda\n\
             or '--formula'\n\
             opam switch create flambda \
             --formula='[\"ocaml\" {=\"4.12.0\"} \"ocaml-option-flambda\"]'"
      | _ ->
        OpamConsole.error_and_exit `Bad_arguments
          "Individual packages, options --packages, --formula and --empty may \
           not be specified at the same time"
    in
    match command, params with
    | None      , []
    | Some `list, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchCommand.list gt ~print_short;
      `Ok ()
    | Some `list_available, pattlist ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      with_repos_rt gt repos @@ fun (repos, rt) ->
      let compilers = OpamSwitchCommand.get_compiler_packages ?repos rt in
      let st = OpamSwitchState.load_virtual ?repos_list:repos gt rt in
      OpamConsole.msg "# Listing available compilers from repositories: %s\n"
        (OpamStd.List.concat_map ", " OpamRepositoryName.to_string
           (OpamStd.Option.default (OpamGlobalState.repos_list gt) repos));
      let filters =
        List.map (fun patt ->
            OpamListCommand.Pattern
              ({ OpamListCommand.default_pattern_selector with
                 OpamListCommand.fields = ["name"; "version"] },
               patt))
          pattlist
      in
      let compilers =
        OpamListCommand.filter ~base:compilers st
          (OpamFormula.ands (List.map (fun f -> OpamFormula.Atom f) filters))
      in
      let format =
        if print_short then OpamListCommand.([ Package ])
        else OpamListCommand.([ Name; Version; Synopsis; ])
      in
      let order nv1 nv2 =
        if nv1.version = nv2.version
        then OpamPackage.Name.compare nv1.name nv2.name
        else OpamPackage.Version.compare nv1.version nv2.version
      in
      OpamListCommand.display st
        {OpamListCommand.default_package_listing_format
         with OpamListCommand.
           short = print_short;
           header = not print_short;
           columns = format;
           all_versions = true;
           order = `Custom order;
        }
        compilers;
      `Ok ()
    | Some `install, switch_arg::params ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      with_repos_rt gt repos @@ fun (repos, rt) ->
      let switch = OpamSwitch.of_string switch_arg in
      let use_local =
        not no_install && not empty && OpamSwitch.is_external switch
      in
      let is_implicit =
        params = [] && packages = None && formula = None && not empty
      in
      let pkg_params =
        if is_implicit && not (OpamSwitch.is_external switch) then [switch_arg]
        else params
      in
      (match invariant_arg ?repos rt pkg_params with
       | exception Failure e -> `Error (false, e)
       | invariant_opt ->
         let invariant =
           OpamStd.Option.default
             (OpamFile.Config.default_invariant rt.repos_global.config)
             invariant_opt
         in
         let (), st =
           OpamSwitchCommand.create gt ~rt
             ?synopsis:descr ?repos
             ~update_config:(not no_switch)
             ~invariant
             switch
           @@ fun st ->
           let st, additional_installs =
             if use_local then
               let st, atoms =
                 OpamAuxCommands.autopin st ~simulate:deps_only ~quiet:true
                   ?locked:OpamStateConfig.(!r.locked)
                   [`Dirname (OpamFilename.Dir.of_string switch_arg)]
               in
               let st =
                 if is_implicit then
                   let local_compilers =
                     OpamStd.List.filter_map
                       (fun (name, _) ->
                          (* The opam file for the local package might not be
                             the current pinning (e.g. with deps-only), but it's
                             guaranteed to be the only available version by
                             autopin. *)
                          match
                            OpamSwitchState.opam st
                              (OpamPackage.package_of_name
                                 (Lazy.force st.available_packages)
                                 name)
                          with
                          | opam ->
                            if OpamFile.OPAM.has_flag Pkgflag_Compiler opam then
                              Some (Atom (name, None))
                            else None
                          | exception Not_found -> None)
                       atoms
                   in
                   if local_compilers <> [] then
                     OpamSwitchCommand.set_invariant_raw st
                       OpamFormula.(of_atom_formula (ands local_compilers))
                   else st
                 else st
               in
               st, atoms
             else st, []
           in
           (),
           OpamSwitchCommand.install_compiler st
             ~additional_installs
             ~deps_only
             ~ask:(additional_installs <> [])
         in
         OpamSwitchState.drop st;
         `Ok ())
    | Some `export, [filename] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      OpamSwitchCommand.export rt
        ~full:(full || freeze) ~freeze
        (if filename = "-" then None
         else Some (OpamFile.make (OpamFilename.of_string filename)));
      `Ok ()
    | Some `import, [filename] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let switch = OpamStateConfig.get_switch () in
      let is_new_switch = not (OpamGlobalState.switch_exists gt switch) in
      let import_source =
        if filename = "-" then None
        else Some (OpamFile.make (OpamFilename.of_string filename))
      in
      if is_new_switch then
        with_repos_rt gt repos @@ fun (repos, rt) ->
        let synopsis = "Import from " ^ Filename.basename filename in
        let (), gt =
          OpamGlobalState.with_write_lock gt @@ fun gt ->
          let gt, st =
            OpamSwitchCommand.create gt ~rt
              ~synopsis ?repos ~invariant:OpamFormula.Empty
              ~update_config:(not no_switch)
              switch
            @@ fun st ->
            let st = OpamSwitchCommand.import st import_source in
            let invariant = OpamSwitchState.infer_switch_invariant st in
            let st = OpamSwitchCommand.set_invariant_raw st invariant in
            st.switch_global, st
          in
          OpamSwitchState.drop st;
          (), gt
        in
        OpamGlobalState.drop gt
      else begin
        if repos <> None then
          OpamConsole.warning
            "Switch exists, '--repositories' argument ignored";
        OpamSwitchState.with_ `Lock_write gt ~switch @@ fun st ->
        OpamSwitchState.drop @@ OpamSwitchCommand.import st import_source
      end;
      `Ok ()
    | Some `remove, switches ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let _gt =
        List.fold_left
          (fun gt switch ->
             let opam_dir = OpamFilename.Op.(
                 OpamFilename.Dir.of_string switch / OpamSwitch.external_dirname
               ) in
             if OpamFilename.is_symlink_dir opam_dir then
               (OpamFilename.rmdir opam_dir;
                gt)
             else OpamSwitchCommand.remove gt (OpamSwitch.of_string switch))
          gt
          switches
      in
      `Ok ()
    | Some `reinstall, switch ->
      let switch = match switch with
        | [sw] -> OpamSwitch.of_string sw
        | [] -> OpamStateConfig.get_switch ()
        | _ ->
          OpamConsole.error_and_exit `Bad_arguments
            "Only one switch argument is supported"
      in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt ~switch @@ fun st ->
      OpamSwitchState.drop @@ OpamSwitchCommand.reinstall st;
      `Ok ()
    | Some `current, [] ->
      OpamSwitchCommand.show ();
      `Ok ()
    | Some `default "-", [] when OpamCLIVersion.Op.(cli @>= cli2_2) ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamSwitchCommand.switch_previous `Lock_none gt;
      `Ok ()
    | Some `set, [switch]
    | Some `default switch, [] ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let switch_name = OpamSwitch.of_string switch in
      OpamSwitchCommand.switch `Lock_none gt switch_name;
      `Ok ()
    | Some `show_invariant, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      OpamConsole.msg "%s\n"
        (OpamFileTools.dep_formula_to_string st.switch_invariant);
      `Ok ()
    | Some `set_invariant, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let repos = OpamSwitchState.repos_list st in
      (match invariant_arg ~repos rt params with
       | exception Failure e -> `Error (false, e)
       | invariant_opt ->
         let invariant = match invariant_opt with
           | Some i -> i
           | None -> OpamSwitchState.infer_switch_invariant st
         in
         let st = OpamSwitchCommand.set_invariant ~force st invariant in
         OpamConsole.msg "The switch invariant was set to %s\n"
           (OpamFormula.to_string invariant);
         let st =
           if no_action || OpamFormula.satisfies_depends st.installed invariant
           then st
           else OpamClient.install_t
               st ~ask:true [] None ~formula:invariant
               ~deps_only:false ~assume_built:false
         in
         OpamSwitchState.drop st;
         `Ok ())
    | Some `link, args ->
      (try
         let switch, dir = match args with
           | switch::dir::[] ->
             OpamSwitch.of_string switch,
             OpamFilename.Dir.of_string dir
           | switch::[] ->
             OpamSwitch.of_string switch,
             OpamFilename.cwd ()
           | [] -> failwith "Missing SWITCH argument"
           | _::_::_::_ -> failwith "Extra argument"
         in
         let open OpamFilename.Op in
         let linkname = dir / OpamSwitch.external_dirname in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         if not (OpamGlobalState.switch_exists gt switch) then
           OpamConsole.error_and_exit `Not_found
             "The switch %s was not found"
             (OpamSwitch.to_string switch);
         if OpamFilename.is_symlink_dir linkname then
           OpamFilename.rmdir linkname;
         if OpamFilename.exists_dir linkname then
           OpamConsole.error_and_exit `Bad_arguments
             "There already is a local switch in %s. Remove it and try again."
             (OpamFilename.Dir.to_string dir);
         if OpamFilename.exists (dir // OpamSwitch.external_dirname) then
           OpamConsole.error_and_exit `Bad_arguments
             "There is a '%s' file in the way. Remove it and try again."
             (OpamFilename.Dir.to_string linkname);
         OpamFilename.link_dir ~link:linkname
           ~target:(OpamPath.Switch.root gt.root switch);
         OpamConsole.msg "Directory %s set to use switch %s.\n\
                          Just remove %s to unlink.\n"
           (OpamConsole.colorise `cyan (OpamFilename.Dir.to_string dir))
           (OpamConsole.colorise `bold (OpamSwitch.to_string switch))
           (OpamConsole.colorise `cyan (OpamFilename.Dir.to_string linkname));
         `Ok ()
       with Failure e -> `Error (true, e))
    | Some `set_description, text ->
      let synopsis = String.concat " " text in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let config =
        { st.switch_config with OpamFile.Switch_config.synopsis }
      in
      OpamSwitchAction.install_switch_config gt.root st.switch config;
      `Ok ()
    | command, params -> bad_subcommand ~cli commands ("switch", command, params)
  in
  mk_command_ret  ~cli cli_original "switch" ~doc ~man
    Term.(const switch
          $global_options cli $build_options cli $command
          $print_short_flag cli cli_original
          $no_switch
          $packages $formula $empty $descr $full $freeze $no_install
          $deps_only $repos $force $no_action $d_alias_of $d_no_autoinstall
          $params)

(* PIN *)
let pin_doc = "Pin a given package to a specific version or source."
let pin ?(unpin_only=false) cli =
  let doc = pin_doc in
  let commands = [
    cli_original, "list", `list, [], "Lists pinned packages.";
    cli_from cli2_1, "scan", `scan, ["DIR"],
    "Lists available packages to pin in directory.";
    cli_original, "add", `add, ["PACKAGE"; "TARGET"],
    "Pins package $(i,PACKAGE) to $(i,TARGET), which may be a version, a path, \
     or a URL.\n\
     $(i,PACKAGE) can be omitted if $(i,TARGET) contains one or more package \
     descriptions. $(i,TARGET) can be replaced by $(b,--dev-repo) if a package \
     by that name is already known, or $(b,--current) if the package is \
     already installed. If $(i,TARGET) is $(b,-), the package is pinned as a \
     virtual package, without any source. opam will infer the kind of pinning \
     from the format (and contents, if local) of $(i,TARGET), Use $(b,--kind) \
     or an explicit URL to disable that behaviour.\n\
     Pins to version control systems may target a specific branch or commit \
     using $(b,#branch) e.g. $(b,git://host/me/pkg#testing).\n\
     If $(i,PACKAGE) is not a known package name, a new package by that name \
     will be locally created.\n\
     For source pinnings, the package version may be specified by using the \
     format $(i,NAME).$(i,VERSION) for $(i,PACKAGE), in the source opam file, \
     or with $(b,edit).";
    cli_original, "remove", `remove, ["NAMES...|TARGET"],
    "Unpins packages $(i,NAMES), restoring their definition from the \
     repository, if any. With a $(i,TARGET), unpins everything that is \
     currently pinned to that target.";
    cli_original, "edit", `edit, ["NAME"],
    "Opens an editor giving you the opportunity to change the package \
     definition that opam will locally use for package $(i,NAME), including \
     its version and source URL. Using the format $(i,NAME.VERSION) will \
     update the version in the opam file in advance of editing, without \
     changing the actual target. The chosen editor is determined from \
     environment variables $(b,OPAM_EDITOR), $(b,VISUAL) or $(b,EDITOR), in \
     order.";
  ] in
  let man = [
    `S Manpage.s_description;
    `P "This command allows local customisation of the packages in a given \
        switch. A pinning can either just enforce a given version, or provide \
        a local, editable version of the definition of the package. It is also \
        possible to create a new package just by pinning a non-existing \
        package name.";
    `P "Any customisation is available through the $(i,edit) subcommand, but \
        the command-line gives facility for altering the source URL of the \
        package, since it is the most common use: $(i,opam pin add PKG URL) \
        modifies package $(i,PKG) to fetch its source from $(i,URL). If a \
        package definition is found in the package's source tree, it will be \
        used locally.";
    `P "If (or $(i,-)) is specified, the package is pinned without a source \
        archive. The package name can be omitted if the target is a directory \
        containing one or more valid package definitions (this allows one to do \
        e.g. $(i,opam pin add .) from a source directory.";
    `P "If $(i,PACKAGE) has the form $(i,name.version), the pinned package \
        will be considered as version $(i,version) by opam. Beware that this \
        doesn't relate with the version of the source actually used for the \
        package. See also the $(b,--with-version) option.";
    `P "The default subcommand is $(i,list) if there are no further arguments, \
        and $(i,add) otherwise if unambiguous.";
  ] @ mk_subdoc ~cli ~defaults:["","list"] commands @ [
      `S Manpage.s_options;
    ] @ OpamArg.man_build_option_section
  in
  let command, params =
    if unpin_only then
      Term.const (Some `remove),
      Arg.(value & pos_all string [] & Arg.info [])
    else
      mk_subcommands_with_default ~cli commands in
  let edit =
    mk_flag ~cli cli_original ["e";"edit"]
      "With $(i,opam pin add), edit the opam file as with `opam pin edit' \
       after pinning." in
  let kind =
    let main_kinds = [
      "version", `version;
      "path"   , `rsync;
      "http"   , `http;
      "git"    , `git;
      "darcs"  , `darcs;
      "hg"     , `hg;
      "none"   , `none;
      "auto"   , `auto;
    ] in
    let help =
      Printf.sprintf
        "Sets the kind of pinning. Must be one of %s. \
         If unset or $(i,auto), is inferred from the format of the target, \
         defaulting to the appropriate version control if one is detected in \
         the given directory, or to $(i,path) otherwise. $(i,OPAMPINKINDAUTO) \
         can be set to \"0\" to disable automatic detection of version control.\
         Use $(i,none) to pin without a target (for virtual packages)."
        (Arg.doc_alts_enum main_kinds)
    in
    let doc = Arg.info ~docv:"KIND" ~doc:help ["k";"kind"] in
    let kinds = main_kinds @ [
        "local"  , `rsync;
        "rsync"  , `rsync;
      ] in
    Arg.(value & opt (some & enum kinds) None & doc) in
  let no_act =
    mk_flag ~cli cli_original ["n";"no-action"]
      "Just record the new pinning status, and don't prompt for \
       (re)installation or removal of affected packages."
  in
  let dev_repo =
    mk_flag ~cli cli_original ["dev-repo"]
      "Pin to the upstream package source for the latest development version"
  in
  let normalise =
    mk_flag ~cli (cli_from cli2_1) ["normalise"]
      (Printf.sprintf
         "Print list of available package to pin in format \
          `name.version%curl`, that is comprehensible by `opam pin \
          add`. Available only with the scan subcommand. An example of use is \
          `opam pin scan . --normalise | grep foo | xargs opam pin add`"
         OpamPinCommand.scan_sep)
  in
  let with_version =
    mk_opt ~cli (cli_from cli2_1) ["with-version"] "VERSION"
      "Set the pinning version to $(i,VERSION) for named $(i,PACKAGES) or \
       packages retrieved from $(i,TARGET). It has priority over any other \
       version specification (opam file version field, $(b,name.vers) \
       argument)). When pinning to a version, the package source from that \
       version is used, but declared as being $(i,VERSION) to opam.\n\
       Using $(b,--with-version) is equivalent to using $(b,--edit) and \
       adjusting the version in the package definition file."
      Arg.(some package_version) None
  in
  let current =
    mk_flag ~cli (cli_from cli2_2) ["current"]
      "When pinning, use the currently installed version and metadata of the \
       package: this will avoid reinstallations due to upstream metadata \
       changes, and may also be used to keep a package that was removed \
       upstream."
  in
  let guess_names kind ?locked ~recurse ?subpath url k =
    let found, cleanup =
      match OpamUrl.local_dir url with
      | Some d ->
        let same_kind url =
          match kind, url.OpamUrl.backend with
          | (None | Some `auto), _
          | Some `rsync, `rsync
          | Some `http, `http -> true
          | Some (#OpamUrl.version_control as vc1), (#OpamUrl.version_control as vc2) ->
            vc1 = vc2
          | Some (`none | `version), _ -> assert false
          | _ -> false
        in
        let pkgs =
          OpamAuxCommands.opams_of_dir_w_target
          ?locked ~recurse ?subpath ~same_kind url d
          |> List.map (fun nf ->
              { pinned_name = nf.pin_name;
                pinned_version = None;
                pinned_opam =
                  OpamFile.OPAM.read_opt nf.pin.pin_file
                  |> OpamStd.Option.map
                    (OpamFile.OPAM.with_locked_opt nf.pin.pin_locked);
                pinned_url = nf.pin.pin_url;
                pinned_subpath = nf.pin.pin_subpath;
              })
        in
        pkgs, None
      | None ->
        let pin_cache_dir = OpamRepositoryPath.pin_cache url in
        let cleanup = fun () ->
          OpamFilename.rmdir @@ OpamRepositoryPath.pin_cache_dir ()
        in
        let basename =
          match OpamStd.String.split (OpamUrl.basename url) '.' with
          | [] ->
            OpamConsole.error_and_exit `Bad_arguments
              "Can not retrieve a path from '%s'"
              (OpamUrl.to_string url)
          | b::_ -> b
        in
        try
          let open OpamProcess.Job.Op in
          OpamProcess.Job.run @@
          OpamRepository.pull_tree
            ~cache_dir:(OpamRepositoryPath.download_cache
                          OpamStateConfig.(!r.root_dir))
            basename pin_cache_dir [] [url] @@| function
          | Not_available (_,u) ->
            OpamConsole.error_and_exit `Sync_error
              "Could not retrieve %s" u
          | Result _ | Up_to_date _ ->
            let pkgs =
              OpamAuxCommands.opams_of_dir ?locked ~recurse ?subpath
                pin_cache_dir
              |> List.map (fun nf ->
                  { pinned_name = nf.pin_name;
                    pinned_version = None;
                    pinned_opam =
                      OpamFile.OPAM.read_opt nf.pin.pin_file
                      |> OpamStd.Option.map
                        (OpamFile.OPAM.with_locked_opt nf.pin.pin_locked);
                    pinned_url = url;
                    pinned_subpath = nf.pin.pin_subpath;
                  })
            in
            pkgs, Some cleanup
        with e -> OpamStd.Exn.finalise e cleanup
    in
    let finalise = OpamStd.Option.default (fun () -> ()) cleanup in
    OpamStd.Exn.finally finalise @@ fun () -> k found
  in
  let pin_target kind target =
    let looks_like_version_re =
      Re.(compile @@
          seq [
            bos;
            opt @@ char 'v';
            digit;
            rep @@ diff any (set "/\\");
            eos])
    in
    let parse ?backend ?handle_suffix target =
      match OpamUrl.parse_opt ?backend ?handle_suffix ~from_file:false
              target with
      | Some url -> `Source url
      | None ->
        OpamConsole.error_and_exit `Bad_arguments
          "No package pinned, invalid url"
    in
    let auto () =
      if target = "-" then
        `None
      else if Re.execp looks_like_version_re target then
        `Version (OpamPackage.Version.of_string target)
      else
      let backend = OpamUrl.guess_version_control target in
      parse ?backend ~handle_suffix:true target
    in
    let target =
      match kind with
      | Some `version -> `Version (OpamPackage.Version.of_string target)
      | Some (#OpamUrl.backend as k) -> parse ~backend:k target
      | Some `none -> `None
      | Some `auto -> auto ()
      | None when OpamClientConfig.(!r.pin_kind_auto) -> auto ()
      | None -> parse ~handle_suffix:false target
    in
    match target with
    | `Source url -> `Source (OpamAuxCommands.url_with_local_branch url)
    | _ -> target
  in
  let pin
      global_options build_options
      kind edit no_act dev_repo print_short recurse subpath normalise
      with_version current
      command params () =
    apply_global_options cli global_options;
    apply_build_options cli build_options;
    let locked = OpamStateConfig.(!r.locked) in
    let action = not no_act in
    let main_command =
      let is_add =
        match command with Some (`add | `default _) -> true | _ -> false
      in
      if dev_repo && current || not is_add && (dev_repo || current)
      then `incorrect else
      match command, params with
      | Some `list, [] | None, [] ->
        `list
      | Some `scan, [url] ->
        `scan url
      | Some `remove, (_::_ as arg) ->
        `remove arg
      | Some `edit, [nv]  ->
        `edit nv
      | Some `add, pins when OpamPinCommand.looks_like_normalised pins ->
        `add_normalised pins
      | Some `default p, pins when
          OpamPinCommand.looks_like_normalised (p::pins) ->
        `add_normalised (p::pins)
      | Some `add, [nv] | Some `default nv, [] when dev_repo ->
        `add_dev nv
      | Some `add, [nv] | Some `default nv, [] when current ->
        `add_current nv
      | Some `add, [arg] | Some `default arg, [] ->
        `add_url arg
      | Some `add, [n; target] | Some `default n, [target]
        when not (current || dev_repo) ->
        `add_wtarget (n,target)
      | _ -> `incorrect
    in
    match main_command with
    | `list ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      OpamClient.PIN.list st ~short:print_short;
      `Ok ()
    | `scan url ->
      let backend, handle_suffix =
        match kind with
        | Some (#OpamUrl.backend as k) -> Some k, None
        | Some `auto -> OpamUrl.guess_version_control url, Some true
        | None when OpamClientConfig.(!r.pin_kind_auto) ->
          OpamUrl.guess_version_control url, Some true
        | _ -> None, None
      in
      OpamUrl.parse ?backend ?handle_suffix url
      |> OpamAuxCommands.url_with_local_branch
      |> OpamPinCommand.scan ~normalise ~recurse ?subpath;
      `Ok ()
    | `remove arg ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let err, to_unpin =
        let open OpamStd.Option.Op in
        List.fold_left (fun (err, acc) arg ->
            let as_url =
              OpamUrl.parse_opt ~handle_suffix:false ~from_file:false arg
              >>| fun url ->
              OpamPackage.Set.filter
                (fun nv ->
                   match OpamSwitchState.url st nv with
                   | Some u ->
                     let spu = OpamFile.URL.subpath u in
                     let u = OpamFile.URL.url u in
                     let path_equality () =
                       let open OpamUrl in
                       match subpath, recurse with
                       | Some sp, false ->
                         u.path = url.path && spu = Some sp
                       | Some sp, true ->
                         (match spu with
                          | Some spp ->
                            let open OpamUrl.Op in
                            OpamStd.String.starts_with
                              ~prefix:(url / OpamFilename.SubPath.to_string sp).path
                              (u / OpamFilename.SubPath.to_string spp).path
                          | None -> false)
                       | None, true ->
                         u.path = url.path
                       | None, false ->
                         spu = None && u.path = url.path
                     in
                     OpamUrl.(u.transport = url.transport) && path_equality ()
                   | None -> false)
                st.pinned |>
              OpamPackage.names_of_packages |>
              OpamPackage.Name.Set.elements
            in
            match as_url with
            | Some ((_::_) as url) -> err, url @ acc
            | _->
              match (fst package_name) arg with
              | `Ok name -> err, name::acc
              | `Error _ ->
                OpamConsole.error
                  "No package pinned to this target found, or invalid package \
                   name/url: %s" arg;
                true, acc)
          (false,[]) arg
      in
      if err then OpamStd.Sys.exit_because `Bad_arguments
      else
        (OpamSwitchState.drop @@ OpamClient.PIN.unpin st ~action to_unpin;
         `Ok ())
    | `edit nv  ->
      (match (fst package) nv with
       | `Ok (name, version) ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         let version = OpamStd.Option.Op.(with_version ++ version) in
         OpamSwitchState.drop @@
         OpamClient.PIN.edit st ?locked ~action ?version name;
         `Ok ()
       | `Error e -> `Error (false, e))
    | `add_normalised pins ->
      let pins = OpamPinCommand.parse_pins pins in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      OpamSwitchState.drop @@
      OpamClient.PIN.url_pins st ?locked ~edit ~action
        (List.map (fun pin ->
             { pin with
               pinned_version =
                 OpamStd.Option.Op.(with_version ++ pin.pinned_version)})
            pins);
      `Ok ()
    | `add_dev nv ->
      (match (fst package) nv with
       | `Ok (name,version) ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         let name = OpamSolution.fuzzy_name st name in
         let version = OpamStd.Option.Op.(with_version ++ version) in
         OpamSwitchState.drop @@
         OpamClient.PIN.pin st name ?locked ~edit ?version ~action
           `Dev_upstream;
         `Ok ()
       | `Error e ->
         if command = Some `add then `Error (false, e)
         else bad_subcommand ~cli commands ("pin", command, params))
    | `add_url arg ->
      (match pin_target kind arg with
       | `None | `Version _ ->
         let msg =
           Printf.sprintf "Ambiguous argument %S, if it is the pinning target, \
                           you must specify a package name first" arg
         in
         `Error (true, msg)
       | `Source url ->
         guess_names kind ?locked ~recurse ?subpath url @@ fun names ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         OpamSwitchState.drop @@
         OpamClient.PIN.url_pins st ?locked ~edit ~action
           (List.map (fun pin ->
                { pin with pinned_version = with_version })
               names);
         `Ok ())
    | `add_current n ->
      (match (fst package) n with
       | `Error e -> `Error (false, e)
       | `Ok (name,version) ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         match OpamPackage.package_of_name_opt st.installed name, version with
         | Some nv, Some v when nv.version <> v ->
           OpamConsole.error_and_exit `Bad_arguments
             "%s.%s is not installed (version %s is), invalid flag `--current'"
             (OpamPackage.Name.to_string name)
             (OpamPackage.Version.to_string v)
             (OpamPackage.Version.to_string nv.version)
         | None, _ ->
           OpamConsole.error_and_exit `Bad_arguments
             "%s is not installed, invalid flag `--current'"
             (OpamPackage.Name.to_string name)
         | Some nv, _ ->
           OpamSwitchState.drop @@
           OpamPinCommand.pin_current st nv;
           `Ok ())
    | `add_wtarget (n, target) ->
      (match (fst package) n with
       | `Ok (name,version) ->
         let pin =
           match pin_target kind target, with_version with
           | `Version v, Some v' -> `Source_version (v, v')
           | p, _ -> p
         in
         let version = OpamStd.Option.Op.(with_version ++ version) in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         OpamSwitchState.drop @@
         OpamClient.PIN.pin st name ?locked ?version ~edit ~action ?subpath pin;
         `Ok ()
       | `Error e -> `Error (false, e))
    | `incorrect -> bad_subcommand ~cli commands ("pin", command, params)
  in
  mk_command_ret  ~cli cli_original "pin" ~doc ~man
    Term.(const pin
          $global_options cli $build_options cli
          $kind $edit $no_act $dev_repo $print_short_flag cli cli_original
          $recurse cli $subpath cli
          $normalise $with_version $current
          $command $params)

(* SOURCE *)
let source_doc = "Get the source of an opam package."
let source cli =
  let doc = source_doc in
  let man = [
    `S Manpage.s_description;
    `P "Downloads the source for a given package to a local directory \
        for development, bug fixing or documentation purposes."
  ] in
  let atom =
    Arg.(required & pos 0 (some atom) None & info ~docv:"PACKAGE" []
           ~doc:"A package name with an optional version constraint")
  in
  let dev_repo =
    mk_flag ~cli cli_original ["dev-repo"]
      "Get the latest version-controlled source rather than the \
       release archive" in
  let pin =
    mk_flag ~cli cli_original ["pin"]
      "Pin the package to the downloaded source (see `opam pin')." in
  let dir =
    mk_opt ~cli cli_original ["dir"] "DIR" "The directory where to put the source."
      Arg.(some dirname) None in
  let no_switch =
    mk_flag ~cli (cli_from cli2_2) ["no-switch"]
      "Choose package without consideration for \
       the current (or any other) switch (installed or pinned packages, etc.)"
  in
  let source global_options atom dev_repo pin no_switch dir () =
    apply_global_options cli global_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    let get_package_dir t =
      let nv =
        try
          OpamPackage.Set.max_elt
            (OpamPackage.Set.filter (OpamFormula.check atom) t.packages)
        with Not_found ->
          OpamConsole.error_and_exit `Not_found
            "No package matching %s found."
            (OpamFormula.short_string_of_atom atom)
      in
      let dir = match dir with
        | Some d -> d
        | None ->
          let dirname =
            if dev_repo then OpamPackage.name_to_string nv
            else OpamPackage.to_string nv in
          OpamFilename.Op.(OpamFilename.cwd () / dirname)
      in
      nv, dir
    in
    let get_source t nv dir =
      let open OpamFilename in
      if exists_dir dir then
        OpamConsole.error_and_exit `Bad_arguments
          "Directory %s already exists. Please remove it or use a different one \
           (see option `--dir')"
          (Dir.to_string dir);
      let opam = OpamSwitchState.opam t nv in
      let subpath =
        OpamStd.Option.map_default OpamFile.URL.subpath
          None (OpamFile.OPAM.url opam)
      in
      if dev_repo then
        (match OpamFile.OPAM.dev_repo opam with
         | None ->
           OpamConsole.error_and_exit `Not_found
             "Version-controlled repo for %s unknown \
              (\"dev-repo\" field missing from metadata)"
             (OpamPackage.to_string nv)
         | Some url ->
           mkdir dir;
           match
             OpamProcess.Job.run
               (OpamRepository.pull_tree
                  ~cache_dir:(OpamRepositoryPath.download_cache
                                OpamStateConfig.(!r.root_dir))
                  ?subpath
                  (OpamPackage.to_string nv) dir []
                  [url])
           with
           | Not_available (_,u) ->
             OpamConsole.error_and_exit `Sync_error "%s is not available" u
           | Result _ | Up_to_date _ ->
             OpamConsole.formatted_msg
               "Successfully fetched %s development repo to %s\n"
               (OpamPackage.name_to_string nv)
               (OpamFilename.Dir.to_string dir))
      else
        (let job =
           let open OpamProcess.Job.Op in
           OpamUpdate.download_package_source t nv dir @@+ function
           | Some (Not_available (_,s)), _ | _, (_, Not_available (_, s)) :: _ ->
             OpamConsole.error_and_exit `Sync_error "Download failed: %s" s
           | None, _ | Some (Result _ | Up_to_date _), _ ->
             OpamAction.prepare_package_source t nv dir @@| function
             | None ->
               OpamConsole.formatted_msg "Successfully extracted to %s\n"
                 (Dir.to_string dir)
             | Some e ->
               OpamConsole.warning "Some errors extracting to %s: %s\n"
                 (Dir.to_string dir) (Printexc.to_string e)
         in
         OpamProcess.Job.run job;
         if OpamPinned.find_opam_file_in_source nv.name
             OpamFilename.SubPath.(dir /? subpath)
            = None
         then
           let f =
             if OpamFilename.exists_dir Op.(dir / "opam")
             then OpamFile.make Op.(dir / "opam" // "opam")
             else OpamFile.make Op.(dir // "opam")
           in
           OpamFile.OPAM.write f
             (OpamFile.OPAM.with_substs [] @@
              OpamFile.OPAM.with_patches [] @@
              opam))
    in
    if no_switch || OpamGlobalState.switches gt = [] then
      (if pin then
         OpamConsole.error_and_exit `Bad_arguments
           (if no_switch then
              "Options '--pin' and '--no-switch' may not be specified at the \
               same time"
            else
              "No switch is defined in current opam root, \
               pinning is impossible");
       OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
       let t = OpamSwitchState.load_virtual ?repos_list:None gt rt in
       let nv, dir = get_package_dir t in
       get_source t nv dir)
    else if not pin then
      OpamSwitchState.with_ `Lock_none gt @@ fun t ->
      let nv, dir = get_package_dir t in
      get_source t nv dir
    else
      OpamSwitchState.with_ `Lock_write gt @@ fun t ->
      let nv, dir = get_package_dir t in
      get_source t nv dir;
      let opam = OpamSwitchState.opam t nv in
      let backend =
        if dev_repo then match OpamFile.OPAM.dev_repo opam with
          | Some {OpamUrl.backend = #OpamUrl.version_control as kind; _} -> kind
          | _ -> `rsync
        else `rsync
      in
      let target =
        `Source (OpamUrl.parse ~backend ~from_file:false
                   ("file://"^OpamFilename.Dir.to_string dir))
      in
      OpamSwitchState.drop
        (OpamClient.PIN.pin t nv.name ~version:nv.version target)
  in
  mk_command  ~cli cli_original "source" ~doc ~man
    Term.(const source
          $global_options cli $atom $dev_repo $pin $no_switch $dir)

(* LINT *)
let lint_doc = "Checks and validate package description ('opam') files."
let lint cli =
  let doc = lint_doc in
  let man = [
    `S Manpage.s_description;
    `P "Given an $(i,opam) file, performs several quality checks on it and \
        outputs recommendations, warnings or errors on stderr.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    `S "LINT CODES"
  ] @
    List.map (fun (c,t,s) ->
        `P (Printf.sprintf "%s$(b,%d): %s"
              (match t with | `Warning -> "W"  | `Error -> "$(i,E)")
              c s))
      (OpamFileTools.all_lint_warnings ())
  in
  let files =
    Arg.(value & pos_all (existing_filename_dirname_or_dash) [] &
         info ~docv:Manpage.s_files []
           ~doc:"Name of the opam files to check, or directory containing \
                 them. Current directory if unspecified")
  in
  let normalise =
    mk_flag ~cli cli_original ["normalise"]
      "Output a normalised version of the opam file to stdout"
  in
  let short =
    mk_flag ~cli cli_original ["short";"s"]
      "Only print the warning/error numbers, space-separated, if any"
  in
  let warnings =
    mk_opt ~cli cli_original ["warnings";"W"] "WARNS"
      "Select the warnings to show or hide. $(i,WARNS) should be a \
       concatenation of $(b,+N), $(b,-N), $(b,+N..M), $(b,-N..M) to \
       respectively enable or disable warning or error number $(b,N) or \
       all warnings with numbers between $(b,N) and $(b,M) inclusive.\n\
       All warnings are enabled by default, unless $(i,WARNS) starts with \
       $(b,+), which disables all but the selected ones."
      warn_selector []
  in
  let package =
    mk_opt ~cli cli_original ["package"] "PKG"
      "Lint the current definition of the given package instead of specifying \
       an opam file directly."
      Arg.(some package) None
  in
  let check_upstream =
    mk_flag ~cli cli_original ["check-upstream"]
      "Check upstream, archive availability and checksum(s)"
  in
  let lint global_options files package normalise short warnings_sel
      check_upstream recurse subpath () =
    apply_global_options cli global_options;
    let opam_files_in_dir d =
      match OpamPinned.files_in_source ~recurse ?subpath d with
      | [] ->
        OpamConsole.warning "No opam files found in %s"
          (OpamFilename.Dir.to_string d);
        []
      | l ->
        List.map (fun nf -> `file nf.pin.pin_file) l
    in
    let files = match files, package with
      | [], None -> (* Lookup in cwd if nothing was specified *)
        opam_files_in_dir (OpamFilename.cwd ())
      | files, None ->
        List.map (function
            | None -> [`stdin] (* this means '-' was specified for stdin *)
            | Some (OpamFilename.D d) ->
              opam_files_in_dir d
            | Some (OpamFilename.F f) ->
              [`file (OpamFile.make f)])
          files
        |> List.flatten
      | [], Some pkg ->
        (OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_none gt @@ fun st ->
         try
           let nv = match pkg with
             | name, Some v -> OpamPackage.create name v
             | name, None -> OpamSwitchState.get_package st name
           in
           let opam = OpamSwitchState.opam st nv in
           match OpamPinned.orig_opam_file st (OpamPackage.name nv) opam with
           | None -> raise Not_found
           | Some f ->
             let filename =
               match OpamFile.OPAM.metadata_dir opam with
               | None -> None
               | Some (None, abs) ->
                 let filename =
                   if OpamFilename.starts_with
                       (OpamPath.Switch.Overlay.dir gt.root st.switch)
                       (OpamFilename.of_string abs) then
                     Printf.sprintf "<pinned>/%s" (OpamPackage.to_string nv)
                   else abs
                 in
                 Some filename
               | Some (Some repo, _rel) ->
                 Some (Printf.sprintf "<%s>/%s"
                         (OpamRepositoryName.to_string repo)
                         (OpamPackage.to_string nv))
             in
             [`pkg (OpamFilename.read (OpamFile.filename f), filename)]
         with Not_found ->
           OpamConsole.error_and_exit `Not_found "No opam file found for %s%s"
             (OpamPackage.Name.to_string (fst pkg))
             (match snd pkg with None -> ""
                               | Some v -> "."^OpamPackage.Version.to_string v))
      | _::_, Some _ ->
        OpamConsole.error_and_exit `Bad_arguments
          "--package and a file argument are incompatible"
    in
    let msg = if normalise then OpamConsole.errmsg else OpamConsole.msg in
    let json =
      match OpamClientConfig.(!r.json_out) with
      | None -> None
      | Some _ -> Some []
    in
    let err,json =
      List.fold_left (fun (err,json) opam_f ->
          try
            let (warnings, opam), opam_f =
              let to_file f = OpamFile.make (OpamFilename.of_string f) in
              let stdin_f = to_file "-" in
              match opam_f with
              | `file f ->
                OpamFileTools.lint_file ~check_upstream ~handle_dirname:true f,
                Some (OpamFile.to_string f)
              | `pkg (content, filename) ->
                OpamFileTools.lint_string
                  ~check_upstream ~handle_dirname:false
                  OpamStd.Option.(default stdin_f (map to_file filename))
                  content,
                filename
              | `stdin ->
                OpamFileTools.lint_channel ~check_upstream ~handle_dirname:false
                  stdin_f stdin,
                None
            in
            let enabled =
              let default = match warnings_sel with
                | (_,true) :: _ -> false
                | _ -> true
              in
              let map =
                List.fold_left
                  (fun acc (wn, enable) -> OpamStd.IntMap.add wn enable acc)
                  OpamStd.IntMap.empty warnings_sel
              in
              fun w -> try OpamStd.IntMap.find w map with Not_found -> default
            in
            let warnings = List.filter (fun (n, _, _) -> enabled n) warnings in
            let failed =
              List.exists (function _,`Error,_ -> true | _ -> false) warnings
            in
            if short then
              (if warnings <> [] then
                 msg "%s\n"
                   (OpamStd.List.concat_map " " (fun (n,_,_) -> string_of_int n)
                      warnings))
            else if warnings = [] then
              (if not normalise then
                 msg "%s%s\n"
                   (OpamStd.Option.to_string (Printf.sprintf "%s: ") opam_f)
                   (OpamConsole.colorise `green "Passed."))
            else
              msg "%s%s\n%s\n"
                (OpamStd.Option.to_string (Printf.sprintf "%s: ") opam_f)
                (if failed then OpamConsole.colorise `red "Errors."
                 else OpamConsole.colorise `yellow "Warnings.")
                (OpamFileTools.warns_to_string warnings);
            if normalise then
              OpamStd.Option.iter (OpamFile.OPAM.write_to_channel stdout) opam;
            let json =
              OpamStd.Option.map
                (OpamStd.List.cons
                   (OpamFileTools.warns_to_json ?filename:opam_f warnings))
                json
            in
            (err || failed), json
          with
          | Parsing.Parse_error
          | OpamLexer.Error _
          | OpamPp.Bad_version _
          | OpamPp.Bad_format _ ->
            msg "File format error\n";
            (true, json))
        (false, json) files
    in
    OpamStd.Option.iter (fun json -> OpamJson.append "lint" (`A json)) json;
    if err then OpamStd.Sys.exit_because `False
  in
  mk_command  ~cli cli_original "lint" ~doc ~man
    Term.(const lint $global_options cli $files $package $normalise $short
          $warnings $check_upstream $recurse cli $subpath cli)

(* CLEAN *)
let clean_doc = "Cleans up opam caches"
let clean cli =
  let doc = clean_doc in
  let man = [
    `S Manpage.s_description;
    `P "Cleans up opam caches, reclaiming some disk space. If no options are \
        specified, the default is $(b,--logs --download-cache \
        --switch-cleanup)."
  ] in
  let dry_run =
    mk_flag ~cli cli_original ["dry-run"]
      "Print the removal commands, but don't execute them"
  in
  let download_cache =
    mk_flag ~cli cli_original ["c"; "download-cache"]
      (Printf.sprintf
        "Clear the cache of downloaded files (\\$OPAMROOT%sdownload-cache), as \
         well as the obsolete \\$OPAMROOT%sarchives, if that exists."
        OpamArg.dir_sep OpamArg.dir_sep)
  in
  let repos =
    mk_flag ~cli cli_original ["unused-repositories"]
      "Clear any configured repository that is not used by any switch nor the \
       default."
  in
  let repo_cache =
    mk_flag ~cli cli_original ["r"; "repo-cache"]
      "Clear the repository cache. It will be rebuilt by the next opam command \
       that needs it."
  in
  let logs =
    mk_flag ~cli cli_original ["logs"] "Clear the logs directory."
  in
  let switch =
    mk_flag ~cli cli_original ["s";"switch-cleanup"]
      "Run the switch-specific cleanup: clears backups, build dirs, \
       uncompressed package sources of non-dev packages, local metadata of \
       previously pinned packages, etc."
  in
  let all_switches =
    mk_flag ~cli cli_original ["a"; "all-switches"]
      "Run the switch cleanup commands in all switches. Implies $(b,--switch-cleanup)"
  in
  let untracked =
    mk_flag ~cli (cli_from cli2_2) ["untracked"]
      "Clean untracked file and directories"
  in
  let remove_untracked ~dry_run st  =
    let root = st.switch_global.root in
    let sw = st.switch in
    let open OpamFilename in
    (* installed files *)
    let files, dirs =
      OpamPackage.Set.fold (fun nv (files, dirs) ->
          let changes =
            OpamFile.Changes.safe_read
              (OpamPath.Switch.changes root sw (OpamPackage.name nv))
          in
          List.fold_left (fun (files, dirs) relpath ->
              let path =
                Op.(OpamPath.Switch.root root sw // relpath)
              in
              if exists path then Set.add path files, dirs else
              let pathd =
                Op.(OpamPath.Switch.root root sw / relpath)
              in
              if exists_dir pathd then files, Dir.Set.add pathd dirs
              else files, dirs)
            (files, dirs) (OpamStd.String.Map.keys changes))
        st.installed (Set.empty, Dir.Set.empty)
    in
    let dirs_of_files =
      Set.fold (fun f dof -> Dir.Set.add (dirname f) dof)
        files Dir.Set.empty
    in
    let dirs = Dir.Set.Op.(dirs ++ dirs_of_files) in
    (* current state *)
    let top_internal_structure =
      (* man dirs are in man, and stublibs & toplevel are in lib *)
      OpamPath.Switch.[ lib_dir; doc_dir; etc_dir ; man_dir;
                        bin; sbin; ]
    in
    let internal_structure =
      (List.map (fun p -> p root sw st.switch_config)
         (OpamPath.Switch.[ stublibs; toplevel; ]
          @ top_internal_structure))
      @ OpamPath.Switch.man_dirs root sw st.switch_config
      |> Dir.Set.of_list
    in
    let all_files, all_dirs =
      List.fold_left (fun (files, dirs) sysdir ->
          Set.union files
            (Set.of_list (rec_files
                            (sysdir root sw st.switch_config))),
          Dir.Set.union dirs
            (Dir.Set.of_list (rec_dirs
                                (sysdir root sw st.switch_config))))
        (Set.empty, Dir.Set.empty) top_internal_structure
    in
    (* remaining ones *)
    let remaining_files = Set.Op.(all_files -- files) in
    let remaining_dirs =
      Dir.Set.Op.(all_dirs -- dirs ++ internal_structure)
    in
    let remaining_dirs, remaining_files =
      Dir.Set.fold (fun dir (dirmap, files) ->
          if Dir.Set.mem dir internal_structure then dirmap, files else
          let of_dir, files =
            Set.partition (fun f -> Dir.equal (dirname f) dir) files
          in
          Dir.Map.add dir of_dir dirmap, files)
        remaining_dirs (Dir.Map.empty, remaining_files)
    in
    let remaining_dirs =
      Dir.Map.filter (fun _ files -> not (Set.is_empty files)) remaining_dirs
    in
    if Dir.Map.is_empty remaining_dirs && Set.is_empty remaining_files then ()
    else if dry_run then
      (let rms str e = OpamConsole.msg "rm -rf %s\n" (str e) in
       Dir.Map.iter (fun dir files ->
           Set.iter (rms to_string) files;
           rms Dir.to_string dir)
         remaining_dirs;
       Set.iter (rms to_string) remaining_files)
    else
      (let remainings =
         List.map to_string (Set.elements remaining_files)
         @ ((List.map (fun (dir, files) ->
             (List.map (fun file -> to_string file) (Set.elements files))
             @ [ Dir.to_string dir ])
             (Dir.Map.bindings remaining_dirs))
            |> List.flatten)
       in
       OpamConsole.msg "Remaining %s:\n%s\n"
         (if Dir.Map.is_empty remaining_dirs then "files"
          else if Set.is_empty remaining_files then "directories"
          else "directories and files")
         (OpamStd.Format.itemize (fun x -> x) remainings);
       if OpamConsole.confirm "Remove them?" then
         Dir.Map.iter (fun dir _ -> rmdir dir) remaining_dirs;
       Set.iter remove remaining_files)
  in
  let clean global_options dry_run
      download_cache repos repo_cache logs switch all_switches untracked () =
    apply_global_options cli global_options;
    let logs, download_cache, switch =
      if logs || download_cache || repos || repo_cache || switch || all_switches || untracked
      then logs, download_cache, switch
      else true, true, true
    in
    OpamGlobalState.with_ `Lock_write @@ fun gt ->
    let root = gt.root in
    let cleandir d =
      if dry_run then
        OpamConsole.msg "rm -rf \"%s\"/*\n"
          (OpamFilename.Dir.to_string d)
      else
      try OpamFilename.cleandir d
      with OpamSystem.Internal_error msg -> OpamConsole.warning "Error ignored: %s" msg
    in
    let rmdir d =
      if dry_run then
        OpamConsole.msg "rm -rf \"%s\"\n"
          (OpamFilename.Dir.to_string d)
      else
      try OpamFilename.rmdir d
      with OpamSystem.Internal_error msg -> OpamConsole.warning "Error ignored: %s" msg
    in
    let rm f =
      if dry_run then
        OpamConsole.msg "rm -f \"%s\"\n"
          (OpamFilename.to_string f)
      else
        OpamFilename.remove f
    in
    let switches =
      if all_switches then OpamGlobalState.switches gt
      else if switch || untracked then match OpamStateConfig.get_switch_opt () with
        | Some s -> [s]
        | None -> []
      else []
    in
    if switches <> [] then
      (OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
       List.iter (fun sw ->
           OpamSwitchState.with_ `Lock_write gt ~rt ~switch:sw @@ fun st ->
           OpamConsole.msg "Cleaning up switch %s\n"
             (OpamSwitch.to_string sw);
           if untracked then remove_untracked ~dry_run st;
           cleandir (OpamPath.Switch.backup_dir root sw);
           cleandir (OpamPath.Switch.build_dir root sw);
           cleandir (OpamPath.Switch.remove_dir root sw);
           cleandir (OpamPath.Switch.extra_files_dir root sw);
           let pinning_overlay_dirs =
             List.map
               (fun nv -> OpamPath.Switch.Overlay.package root sw nv.name)
               (OpamPackage.Set.elements st.pinned)
           in
           List.iter (fun d ->
               if not (List.mem d pinning_overlay_dirs) then rmdir d)
             (OpamFilename.dirs (OpamPath.Switch.Overlay.dir root sw));
           let keep_sources_dir =
             OpamPackage.Set.elements
               (OpamPackage.Set.union st.pinned
                  (OpamPackage.Set.filter (OpamSwitchState.is_dev_package st)
                     st.installed))
             |> List.map (OpamSwitchState.source_dir st)
           in
           OpamFilename.dirs (OpamPath.Switch.sources_dir root sw) |>
           List.iter (fun d ->
               if not (List.mem d keep_sources_dir) then rmdir d))
         switches);
    if repos then
      (OpamFilename.with_flock `Lock_write (OpamPath.repos_lock gt.root)
       @@ fun _lock ->
       let repos_config =
         OpamStateConfig.Repos.safe_read ~lock_kind:`Lock_write gt
       in
       let all_repos =
         OpamRepositoryName.Map.keys repos_config |>
         OpamRepositoryName.Set.of_list
       in
       let default_repos =
         OpamGlobalState.repos_list gt |>
         OpamRepositoryName.Set.of_list
       in
       let unused_repos =
         List.fold_left (fun repos sw ->
             let switch_config =
               OpamStateConfig.Switch.safe_load
                 ~lock_kind:`Lock_read gt sw
             in
             let used_repos =
               OpamStd.Option.default []
                 switch_config.OpamFile.Switch_config.repos |>
               OpamRepositoryName.Set.of_list
             in
             OpamRepositoryName.Set.diff repos used_repos)
           (OpamRepositoryName.Set.diff all_repos default_repos)
           (OpamGlobalState.switches gt)
       in
       OpamRepositoryName.Set.iter (fun r ->
           OpamConsole.msg "Removing repository %s\n"
             (OpamRepositoryName.to_string r);
           rmdir (OpamRepositoryPath.root root r);
           rm (OpamRepositoryPath.tar root r))
         unused_repos;
       let repos_config =
         OpamRepositoryName.Map.filter
           (fun r _ -> not (OpamRepositoryName.Set.mem r unused_repos))
           repos_config
       in
       OpamConsole.msg "Updating %s\n"
         (OpamFile.to_string (OpamPath.repos_config root));
       if not dry_run then
         OpamFile.Repos_config.write (OpamPath.repos_config root) repos_config);
    if repo_cache then
      (OpamConsole.msg "Clearing repository cache\n";
       if not dry_run then OpamRepositoryState.Cache.remove ());
    if download_cache then
      (OpamConsole.msg "Clearing cache of downloaded files\n";
       List.iter (fun dir ->
           match OpamFilename.(Base.to_string (basename_dir dir)) with
           | "git" ->
             (try OpamFilename.exec dir ~name:"git gc" [["git"; "gc"]]
              with e -> OpamStd.Exn.fatal e)
           | _ -> cleandir dir
         )
         (OpamFilename.dirs (OpamRepositoryPath.download_cache root)));
    if logs then
      (OpamConsole.msg "Clearing logs\n";
       cleandir (OpamPath.log root))
  in
  mk_command  ~cli cli_original "clean" ~doc ~man
    Term.(const clean $global_options cli $dry_run $download_cache $repos
          $repo_cache $logs $switch $all_switches $untracked)

(* LOCK *)
let lock_doc = "Create locked opam files to share build environments across hosts."
let lock cli =
  let doc = lock_doc in
  let man = [
    `S Manpage.s_description;
    `P "Generates a lock file of a package: checks the current \
        state of their installed dependencies, and outputs modified versions of \
        the opam file with a $(i,.locked) suffix, where all the (transitive) \
        dependencies and pinnings have been bound strictly to the currently \
        installed version.";
    `P "By using these locked opam files, it is then possible to recover the \
        precise build environment that was setup when they were generated.";
    `P "If paths (filename or directory) are given, those opam files are locked. \
        If package is given, installed one is locked, otherwise its latest \
        version. If a locally pinned package is given, its current local opam \
        file is locked, even if not versioned or uncommitted changes";
    `P "Fails if all mandatory dependencies are not installed in the switch.";
    `S "LOCK FILE CHANGED FIELDS";
    `P "- $(i,depends) are fixed to their specific versions, with all filters \
        removed (except for the exceptions below";
    `P "- $(i,depopts) that are installed in the current switch are turned into \
        depends, with their version set. Others are set in the $(i,conflict) field";
    `P "- `{dev}`, `{with-test}, and `{with-doc}` filters are kept if all \
        packages of a specific filters are installed in the switch. Versions are \
        fixed and the same filter is on all dependencies that are added from \
        them";
    `P "- $(i,pin-depends) are kept and new ones are added if in the \
        dependencies some packages are pinned ";
    `P "- pins are resolved: if a package is locally pinned, opam tries to get \
        its remote url and branch, and sets this as the target URL";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
  ]
  in
  let only_direct_flag =
    mk_flag ~cli cli_original ["d"; "direct-only"]
      "Only lock direct dependencies, rather than the whole dependency tree."
  in
  let lock_suffix = OpamArg.lock_suffix cli in
  let lock global_options only_direct lock_suffix atom_locs () =
    apply_global_options cli global_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_none gt @@ fun st ->
    let st, packages = OpamLockCommand.select_packages atom_locs st in
    if OpamPackage.Set.is_empty packages then
      OpamConsole.msg "No lock file generated\n"
    else
    let st =
      (* Suppose the packages are installed to avoid errors on mutual
         dependencies *)
      { st with installed = OpamPackage.Set.union st.installed packages }
    in
    let pkg_done =
      OpamPackage.Set.fold (fun nv msgs ->
          let opam = OpamSwitchState.opam st nv in
          let locked = OpamLockCommand.lock_opam ~only_direct st opam in
          let locked_fname =
            OpamFilename.add_extension
              (OpamFilename.of_string (OpamPackage.name_to_string nv))
              ("opam." ^ lock_suffix)
          in
          if not (OpamCoreConfig.(!r).OpamCoreConfig.safe_mode
                  || OpamStateConfig.(!r.dryrun)) then
            OpamFile.OPAM.write_with_preserved_format
              (OpamFile.make locked_fname) locked;
          (nv, locked_fname)::msgs)
        packages []
    in
    OpamConsole.msg "Generated %slock files for:\n%s"
      (if OpamCoreConfig.(!r).safe_mode || OpamStateConfig.(!r.dryrun) then
         "(not saved) " else "")
      (OpamStd.Format.itemize (fun (nv, file) ->
           Printf.sprintf "%s: %s"
             (OpamPackage.to_string nv)
             (OpamFilename.to_string file)) pkg_done)
  in
  mk_command  ~cli (cli_from cli2_1) "lock" ~doc ~man
    Term.(const lock $global_options cli $only_direct_flag $lock_suffix
          $atom_or_local_list)

(* HELP *)
let help =
  let doc = "Display help about opam and opam commands." in
  let man = [
    `S Manpage.s_description;
    `P "Prints help about opam commands.";
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  let help man_format cmds topic = match topic with
    | None       -> `Help (`Pager, None)
    | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" ->
          List.iter (OpamConsole.msg "%s\n") cmds; `Ok ()
      | `Ok t -> `Help (man_format, Some t) in

  Term.(ret (const help $Arg.man_format $Term.choice_names $topic)),
  Cmd.info "help" ~doc ~man

let default cli =
  let doc = "source-based package management" in
  let man = [
    `S Manpage.s_description;
    `P "Opam is a package manager. It uses the powerful mancoosi tools to \
        handle dependencies, including support for version constraints, \
        optional dependencies, and conflict management. The default \
        configuration binds it to the official package repository for OCaml.";
    `P "It has support for different remote repositories such as HTTP, rsync, \
        git, darcs and mercurial. Everything is installed within a local opam \
        directory, that can include multiple installation prefixes with \
        different sets of intalled packages.";
    `P "Use either $(b,opam <command> --help) or $(b,opam help <command>) \
        for more information on a specific command.";
    `S Manpage.s_commands;
    `S "COMMAND ALIASES";
  ] @ help_sections cli
  in
  let usage global_options =
    apply_global_options cli global_options;
    OpamConsole.formatted_msg
      "usage: opam [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
       \n\
       The most commonly used opam commands are:\n\
      \    init         %s\n\
      \    list         %s\n\
      \    show         %s\n\
      \    install      %s\n\
      \    remove       %s\n\
      \    update       %s\n\
      \    upgrade      %s\n\
      \    config       %s\n\
      \    repository   %s\n\
      \    switch       %s\n\
      \    pin          %s\n\
      \    admin        %s\n\
       \n\
       See 'opam help <command>' for more information on a specific command.\n"
      init_doc list_doc show_doc install_doc remove_doc update_doc
      upgrade_doc config_doc repository_doc switch_doc pin_doc
      OpamAdminCommand.admin_command_doc
  in
  Term.(const usage $global_options cli),
  Cmd.info "opam"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:global_option_section
    ~doc
    ~man

let admin =
  (* cmdliner never sees the admin subcommand, so this "can't happen" *)
  let doc = "Internal opam error - main admin command invoked" in
  Term.(ret (const (`Error (true, doc)))),
  Cmd.info "admin" ~doc:OpamAdminCommand.admin_command_doc

(* Note: for cli versionning check, all commands must be constructed with
   [OpamArg.mk_command] or [OpamArg.mk_command_ret]. *)
let commands cli =
  let show = show cli in
  let remove = remove cli in
  let repository = repository cli in
  (* This list must always include *all* commands, regardless of cli *)
  [
    init cli;
    list cli;
    make_command_alias ~cli (list ~force_search:true cli) ~options:" --search" "search";
    tree cli;
    make_command_alias ~cli (tree ~why:true cli) ~options:" --rev-deps" "why";
    show; make_command_alias ~cli show "info";
    install cli;
    remove; make_command_alias ~cli remove "uninstall";
    reinstall cli;
    update cli; upgrade cli;
    var cli; option cli;
    config cli;
    exec cli; env cli;
    repository; make_command_alias ~cli repository "remote";
    switch cli;
    pin cli; make_command_alias ~cli (pin ~unpin_only:true cli) ~options:" remove" "unpin";
    source cli;
    lint cli;
    clean cli;
    lock cli;
    admin;
    help;
  ]

let current_commands = commands OpamCLIVersion.Sourced.current

(* TODO: Remove that horror whenever https://github.com/dbuenzli/cmdliner/pull/161 is merged *)
let name_of_new_api_info info = Cmd.name (Cmd.v info (Term.const ()))

let is_builtin_command prefix =
  List.exists (fun (_,info) ->
                 OpamStd.String.starts_with ~prefix (name_of_new_api_info info))
              current_commands

let is_admin_subcommand prefix =
  prefix = "admin" ||
  let matches =
    List.filter (fun (_,info) ->
                   OpamStd.String.starts_with ~prefix (name_of_new_api_info info))
                current_commands in
  match matches with
  | [(_,info)] when name_of_new_api_info info = "admin" -> true
  | _ -> false

let get_cmdliner_parser cli =
  (default cli, commands cli)
