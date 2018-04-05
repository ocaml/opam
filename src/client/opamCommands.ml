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
open OpamArg
open OpamTypes
open OpamStateTypes
open OpamTypesBase
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
      if OpamVersion.git () <> None then
        OpamConsole.warning "Using opam self-upgrade to %s while the system \
                             opam is a development version (%s)"
          (OpamVersion.to_string update_version)
          (OpamVersion.to_string (OpamVersion.full ()));
      (if debug || (OpamConsole.debug ()) then
         Printf.eprintf "!! %s found, switching to it !!\n%!" updated_self_str;
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

let global_options =
  let no_self_upgrade =
    mk_flag ~section:global_option_section ["no-self-upgrade"]
      "Opam will replace itself with a newer binary found \
       at $(b,OPAMROOT/opam) if present. This disables this behaviour." in
  let self_upgrade no_self_upgrade options =
    let self_upgrade_status =
      if OpamStd.Config.env_string "NOSELFUPGRADE" =
         Some self_upgrade_bootstrapping_value
      then `Running
      else if no_self_upgrade then `Disable
      else if OpamStd.Config.env_bool "NOSELFUPGRADE" = Some true then `Disable
      else `None
    in
    if self_upgrade_status = `None then
      switch_to_updated_self
        OpamStd.Option.Op.(options.debug_level ++
                           OpamStd.Config.env_level "DEBUG" +! 0 > 0)
        (OpamStateConfig.opamroot ?root_dir:options.opt_root ());
    if not (options.safe_mode || OpamClientConfig.(!r.root_is_ok)) &&
       Unix.getuid () = 0 then
      OpamConsole.warning "Running as root is not recommended";
    options, self_upgrade_status
  in
  Term.(const self_upgrade $ no_self_upgrade $ global_options)

let apply_global_options (options,self_upgrade) =
  apply_global_options options;
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

let init_dot_profile shell dot_profile =
  match dot_profile with
  | Some n -> n
  | None   -> OpamFilename.of_string (OpamStd.Sys.guess_dot_profile shell)

type command = unit Term.t * Term.info

(* INIT *)
let init_doc = "Initialize opam state."
let init =
  let doc = init_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,init) command initialises a local \"opam root\" (by default, \
        $(i,~/.opam/)) that holds opam's data and packages. This is a \
        necessary step for normal operation of opam.";
    `P "Additionally, it prompts the user with the option to add a \
        configuration hook in their shell init files. The initial software \
        repositories are fetched, and an initial 'switch' can also be \
        installed, according to the configuration and options.";
    `P "The initial repository and defaults can be set through a \
        configuration file found at $(i,~/.opamrc) or $(i,/etc/opamrc).";
    `P "For further customisation once opam has been initialised, see \
        $(b,opam switch) and $(b,opam repository).";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S "CONFIGURATION FILE";
    `P "Any field from the built-in initial configuration can be overriden \
        through $(i,~/.opamrc), $(i,/etc/opamrc), or a file supplied with \
        $(i,--config). The default configuration for this version of opam is:";
    `Pre (OpamFile.InitConfig.write_to_string (OpamInitDefaults.init_config));
    `P "Additional fields in the same format as for the $(i,~/.opam/config) \
        file are also supported: $(b,jobs:), $(b,download-command:), \
        $(b,download-jobs:), $(b,solver-criteria:), \
        $(b,solver-upgrade-criteria:), \
        $(b,solver-fixup-criteria:), $(b,solver:), $(b,wrap-build-commands:), \
        $(b,wrap-install-commands:), $(b,wrap-remove-commands:), \
        $(b,global-variables:).";
    `S OpamArg.build_option_section;
  ] in
  let compiler =
    mk_opt ["c";"compiler"] "PACKAGE" "Set the compiler to install"
      Arg.(some string) None
  in
  let no_compiler =
    mk_flag ["bare"]
      "Initialise the opam state, but don't setup any compiler switch yet."
  in
  let repo_name =
    let doc = Arg.info ~docv:"NAME" ~doc:"Name of the repository." [] in
    Arg.(value & pos ~rev:true 1 repository_name OpamRepositoryName.default
         & doc)
  in
  let repo_url =
    let doc = Arg.info ~docv:"ADDRESS" ~doc:"Address of the repository." [] in
    Arg.(value & pos ~rev:true 0 (some string) None & doc) in
  let no_setup   =
    mk_flag ["n";"no-setup"]
      "Do not update the global and user configuration options to setup opam."
  in
  let auto_setup =
    mk_flag ["a";"auto-setup"]
      "Automatically setup all the global and user configuration options for \
       opam."
  in
  let config_file =
    mk_opt_all ["config"] "FILE"
      "Use the given init config file. If repeated, latest has the highest \
       priority ($(b,i.e.) each field gets its value from where it was defined \
       latest). Specifying a URL pointing to a config file instead is \
       allowed."
      OpamArg.url
  in
  let no_config_file =
    mk_flag ["no-opamrc"]
      "Don't read `/etc/opamrc' or `~/.opamrc': use the default settings and \
       the files specified through $(b,--config) only"
  in
  let bypass_checks =
    mk_flag ["bypass-checks"]
      "Skip checks on required or recommended tools, and assume everything is \
       fine"
  in
  let init global_options
      build_options repo_kind repo_name repo_url
      no_setup auto_setup shell dot_profile_o
      compiler no_compiler config_file no_config_file bypass_checks =
    apply_global_options global_options;
    apply_build_options build_options;
    if compiler <> None && no_compiler then
      OpamConsole.error_and_exit `Bad_arguments
        "Options --bare and --compiler are incompatible";
    let config_files =
      (if no_config_file then []
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
        ) config_file
    in
    let init_config =
      try
        OpamConsole.note "Will configure from built-in defaults%s."
          (OpamStd.List.concat_map ~nil:"" ~left:", " ", "
             OpamFile.to_string config_files);
        List.fold_left (fun acc f ->
            OpamFile.InitConfig.add acc (OpamFile.InitConfig.read f))
          OpamInitDefaults.init_config
          config_files
      with e ->
        OpamConsole.error
          "Error in configuration file, fix it, use '--no-opamrc', or check \
           your '--config FILE' arguments:";
        OpamConsole.errmsg "%s\n" (Printexc.to_string e);
        OpamStd.Sys.exit_because `Configuration_error
    in
    let repo =
      OpamStd.Option.map (fun url ->
        let repo_url = OpamUrl.parse ?backend:repo_kind url in
        let repo_root =
          OpamRepositoryPath.create (OpamStateConfig.(!r.root_dir))
            repo_name
        in
        { repo_root; repo_name; repo_url; repo_trust = None })
        repo_url
    in
    let update_config =
      if no_setup then `no
      else if auto_setup then `yes
      else `ask in
    let dot_profile = init_dot_profile shell dot_profile_o in
    let gt, rt, default_compiler =
      OpamClient.init
        ~init_config ?repo ~bypass_checks
        shell dot_profile update_config
    in
    if not no_compiler &&
       OpamFile.Config.installed_switches gt.config = [] then
      match compiler with
      | Some comp ->
        let packages =
          OpamSwitchCommand.guess_compiler_package rt comp
        in
        OpamConsole.header_msg "Creating initial switch (%s)"
          (OpamFormula.string_of_atoms packages);
        OpamSwitchCommand.install
          gt ~rt ~packages ~update_config:true (OpamSwitch.of_string comp)
        |> ignore
      | None ->
        let candidates = OpamFormula.to_dnf default_compiler in
        let all_packages = OpamSwitchCommand.get_compiler_packages rt in
        let compiler_packages =
          try
            Some (List.find (fun atoms ->
                let names = List.map fst atoms in
                let pkgs = OpamFormula.packages_of_atoms all_packages atoms in
                List.for_all (OpamPackage.has_name pkgs) names)
                candidates)
          with Not_found -> None
        in
        match compiler_packages with
        | Some packages ->
          OpamConsole.header_msg "Creating initial switch (%s)"
            (OpamFormula.string_of_atoms packages);
          OpamSwitchCommand.install
            gt ~rt ~packages ~update_config:true
            (OpamSwitch.of_string "default")
          |> ignore
        | None ->
          OpamConsole.note
            "No compiler selected, and no available default switch found: \
             no switch has been created.\n\
             Use 'opam switch create <compiler>' to get started."
  in
  Term.(const init
        $global_options $build_options $repo_kind_flag $repo_name $repo_url
        $no_setup $auto_setup $shell_opt $dot_profile_flag $compiler $no_compiler
        $config_file $no_config_file $bypass_checks),
  term_info "init" ~doc ~man

(* LIST *)
let list_doc = "Display the list of available packages."
let list ?(force_search=false) () =
  let doc = list_doc in
  let selection_docs = OpamArg.package_selection_section in
  let display_docs = OpamArg.package_listing_section in
  let man = [
    `S "DESCRIPTION";
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
    `S "ARGUMENTS";
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
    let docs = selection_docs in
    Arg.(value & vflag_all [] [
        OpamListCommand.Any, info ~docs ["A";"all"]
          ~doc:"Include all, even uninstalled or unavailable packages";
        OpamListCommand.Installed, info ~docs ["i";"installed"]
          ~doc:"List installed packages only. This is the default when no \
                further arguments are supplied";
        OpamListCommand.Root, info ~docs ["roots";"installed-roots"]
          ~doc:"List only packages that were explicitely installed, excluding \
                the ones installed as dependencies";
        OpamListCommand.Available, info ~docs ["a";"available"]
          ~doc:"List only packages that are available on the current system";
        OpamListCommand.Installable, info ~docs ["installable"]
          ~doc:"List only packages that can be installed on the current switch \
                (this calls the solver and may be more costly; a package \
                depending on an unavailable package may be available, but is \
                never installable)";
        OpamListCommand.Compiler, info ~docs ["base"]
          ~doc:"List only the immutable base of the current switch (i.e. \
                compiler packages)";
        OpamListCommand.Pinned, info ~docs ["pinned"]
          ~doc:"List only the pinned packages";
      ])
  in
  let search =
    if force_search then Term.const true else
      mk_flag ["search"] ~section:selection_docs
        "Match $(i,PATTERNS) against the full descriptions of packages, and \
         require all of them to match, instead of requiring at least one to \
         match against package names (unless $(b,--or) is also specified)."
  in
  let repos =
    mk_opt ["repos"] "REPOS" ~section:selection_docs
      "Include only packages that took their origin from one of the given \
       repositories (unless $(i,no-switch) is also specified, this excludes \
       pinned packages)."
      Arg.(some & list & repository_name) None
  in
  let owns_file =
    let doc =
      "Finds installed packages responsible for installing the given file"
    in
    Arg.(value & opt (some OpamArg.filename) None &
         info ~doc ~docv:"FILE" ~docs:selection_docs ["owns-file"])
  in
  let no_switch =
    mk_flag ["no-switch"] ~section:selection_docs
      "List what is available from the repositories, without consideration for \
       the current (or any other) switch (installed or pinned packages, etc.)"
  in
  let disjunction =
    mk_flag ["or"] ~section:selection_docs
      "Instead of selecting packages that match $(i,all) the criteria, select \
       packages that match $(i,any) of them"
  in
  let depexts =
    mk_flag ["e";"external";"depexts"] ~section:display_docs
      "Instead of displaying the packages, display their external dependencies \
       that are associated with the current system. This excludes other \
       display options. Rather than using this directly, you should probably \
       head for the `depext' plugin, that will use your system package \
       management system to handle the installation of the dependencies. Run \
       `opam depext'."
  in
  let vars =
    mk_opt ["vars"] "[VAR=STR,...]" ~section:display_docs
      "Define the given variable bindings. Typically useful with \
       $(b,--external) to override the values for $(i,arch), $(i,os), \
       $(i,os-distribution), $(i,os-version), $(i,os-family)."
      OpamArg.variable_bindings []
  in
  let list
      global_options selection state_selector no_switch depexts vars repos
      owns_file disjunction search format packages =
    apply_global_options global_options;
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
        state_selector;
        join
          (pattern_selector ::
           (if no_switch then Empty else
            match repos with None -> Empty | Some repos ->
              Atom (OpamListCommand.From_repository repos)) ::
           OpamStd.Option.Op.
             ((owns_file >>| fun f -> Atom (OpamListCommand.Owns_file f)) +!
              Empty) ::
           List.map (fun x -> Atom x) selection)
      ]
    in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    let st =
      let rt = OpamRepositoryState.load `Lock_none gt in
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
       filter <> OpamFormula.Empty
    then
      OpamConsole.msg "# Packages matching: %s\n"
        (OpamListCommand.string_of_formula filter);
    let all = OpamPackage.Set.union st.packages st.installed in
    let results =
      OpamListCommand.filter ~base:all st filter
    in
    if not depexts then
      OpamListCommand.display st format results
    else
      OpamListCommand.print_depexts st results
  in
  Term.(const list $global_options $package_selection $state_selector
        $no_switch $depexts $vars $repos $owns_file $disjunction $search
        $package_listing $pattern_list),
  term_info "list" ~doc ~man


(* SHOW *)
let show_doc = "Display information about specific packages."
let show =
  let doc = show_doc in
  let man = [
    `S "DESCRIPTION";
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
    let doc =
      Arg.info
        ~docv:"FIELDS"
        ~doc:("Only display the values of these fields. Fields can be selected \
               among "^
              OpamStd.List.concat_map ", " (Printf.sprintf "$(i,%s)" @* snd)
                OpamListCommand.field_names
              ^". Multiple fields can be separated with commas, in which case \
                field titles will be printed; the raw value of any opam-file \
                field can be queried by suffixing a colon character (:), e.g. \
                $(b,--field=depopts:).")
        ["f";"field"] in
    Arg.(value & opt (list string) [] & doc) in
  let show_empty =
    mk_flag ["empty-fields"]
      "Show fields that are empty. This is implied when $(b,--field) is \
       given."
  in
  let raw =
    mk_flag ["raw"] "Print the raw opam file for this package" in
  let where =
    mk_flag ["where"]
      "Print the location of the opam file used for this package" in
  let list_files =
    mk_flag ["list-files"]
      "List the files installed by the package. Equivalent to \
       $(b,--field=installed-files), and only available for installed \
       packages"
  in
  let file =
    let doc =
      Arg.info
        ~docv:"FILE"
        ~doc:"DEPRECATED: use an explicit path argument as package instead. \
              Get package information from the given FILE instead of from \
              known packages. This implies $(b,--raw) unless $(b,--fields) is \
              used. Only raw opam-file fields can be queried."
        ["file"] in
    Arg.(value & opt (some existing_filename_or_dash) None & doc) in
  let normalise = mk_flag ["normalise"]
      "Print the values of opam fields normalised (no newlines, no implicit \
       brackets)"
  in
  let no_lint = mk_flag ["no-lint"]
      "Don't output linting warnings or errors when reading from files"
  in
  let pkg_info global_options fields show_empty raw where
      list_files file normalise no_lint atom_locs =
    apply_global_options global_options;
    match file, atom_locs with
    | None, [] ->
      `Error (true, "required argument PACKAGES is missing")
    | Some _, _::_ ->
      `Error (true,
              "arguments PACKAGES and `--file' can't be specified together")
    | None, atom_locs ->
      let fields, show_empty =
        if list_files then
          fields @ [OpamListCommand.(string_of_field Installed_files)],
          show_empty
        else fields, show_empty || fields <> []
      in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let st = OpamListCommand.get_switch_state gt in
      let st, atoms =
        OpamAuxCommands.simulate_autopin ~quiet:no_lint ~keep_url:true st atom_locs
      in
      OpamListCommand.info st
        ~fields ~raw_opam:raw ~where ~normalise ~show_empty atoms;
      `Ok ()
    | Some f, [] ->
      let opam = match f with
        | Some f -> OpamFile.OPAM.read (OpamFile.make f)
        | None -> OpamFile.OPAM.read_from_channel stdin
      in
      if not no_lint then OpamFile.OPAM.print_errors opam;
      if where then
        (OpamConsole.msg "%s\n"
           (match f with Some f -> OpamFilename.(Dir.to_string (dirname f))
                       | None -> ".");
         `Ok ())
      else
      let opam_content_list = OpamFile.OPAM.to_list opam in
      let get_field f =
        let f = OpamStd.String.remove_suffix ~suffix:":" f in
        try OpamListCommand.mini_field_printer ~prettify:true ~normalise
              (List.assoc f opam_content_list)
        with Not_found -> ""
      in
      match fields with
      | [] ->
        OpamFile.OPAM.write_to_channel stdout opam; `Ok ()
      | [f] ->
        OpamConsole.msg "%s\n" (get_field f); `Ok ()
      | flds ->
        let tbl =
          List.map (fun fld ->
              [ OpamConsole.colorise `blue
                  (OpamStd.String.remove_suffix ~suffix:":" fld ^ ":");
                get_field fld ])
            flds
        in
        OpamStd.Format.align_table tbl |>
        OpamStd.Format.print_table stdout ~sep:" ";
        `Ok ()
  in
  Term.(ret
          (const pkg_info $global_options $fields $show_empty $raw $where $list_files
           $file $normalise $no_lint $atom_or_local_list)),
  term_info "show" ~doc ~man

module Common_config_flags = struct
  let sexp =
    mk_flag ["sexp"]
      "Print environment as an s-expression rather than in shell format"

  let inplace_path =
    mk_flag ["inplace-path"]
      "When updating the $(i,PATH) variable, replace any pre-existing opam \
       path in-place rather than putting the new path in front. This means \
       programs installed in opam that were shadowed will remain so after \
       $(b,opam env)"

  let set_opamroot =
    mk_flag ["set-root"]
      "With the $(b,env) and $(b,exec) subcommands, also sets the \
       $(i,OPAMROOT) variable, making sure further calls to opam will use the \
       same root."

  let set_opamswitch =
    mk_flag ["set-switch"]
      "With the $(b,env) and $(b,exec) subcommands, also sets the \
       $(i,OPAMSWITCH) variable, making sure further calls to opam will use \
       the same switch as this one."

end

(* CONFIG *)
let config_doc = "Display configuration options for packages."
let config =
  let doc = config_doc in
  let commands = [
    "env", `env, [],
    "Returns the bindings for the environment variables set in the current \
     switch, e.g. PATH, in a format intended to be evaluated by a shell. With \
     $(i,-v), add comments documenting the reason or package of origin for \
     each binding. This is most usefully used as $(b,eval \\$(opam config \
     env\\)) to have further shell commands be evaluated in the proper opam \
     context. Can also be accessed through $(b,opam env).";
    "revert-env", `revert_env, [],
    "Reverts environment changes made by opam, e.g. $(b,eval \\$(opam config \
     revert-env)) undoes what $(b,eval \\$(opam config env\\)) did, as much as \
     possible.";
    "setup", `setup, [],
    "Configure global and user parameters for opam. Use $(b, opam config \
     setup) to display more options. Use $(b,--list) to display the current \
     configuration options. You can use this command to automatically update: \
     (i) user-configuration files such as ~/.profile; and (ii) \
     global-configuration files controlling which shell scripts are loaded on \
     startup, such as auto-completion. These configuration options can be \
     updated using $(b,opam config setup --global) to setup the global \
     configuration files stored in $(b,~/.opam/opam-init/) and $(b,opam config \
     setup --user) to setup the user ones. To modify both the global and user \
     configuration, use $(b,opam config setup --all).";
    "exec", `exec, ["[--] COMMAND"; "[ARG]..."],
    "Execute $(i,COMMAND) with the correct environment variables. This command \
     can be used to cross-compile between switches using $(b,opam config exec \
     --switch=SWITCH -- COMMAND ARG1 ... ARGn). Opam expansion takes place in \
     command and args. If no switch is present on the command line or in the \
     $(i,OPAMSWITCH) environment variable, $(i,OPAMSWITCH) is not set in \
     $(i,COMMAND)'s environment. Can also be accessed through $(b,opam exec).";
    "var", `var, ["VAR"],
    "Return the value associated with variable $(i,VAR). Package variables can \
     be accessed with the syntax $(i,pkg:var). Can also be accessed through \
     $(b,opam var)";
    "list", `list, ["[PACKAGE]..."],
    "Without argument, prints a documented list of all available variables. With \
     $(i,PACKAGE), lists all the variables available for these packages. Use \
     $(i,-) to include global configuration variables for this switch.";
    "set", `set, ["VAR";"VALUE"],
    "Set the given opam variable for the current switch. Warning: changing a \
     configured path will not move any files! This command does not perform \
     any variable expansion.";
    "unset", `unset, ["VAR"],
    "Unset the given opam variable for the current switch. Warning: \
     unsetting built-in configuration variables can cause problems!";
    "set-global", `set_global, ["VAR";"VALUE"],
    "Set the given variable globally in the opam root, to be visible in all \
     switches";
    "unset-global", `unset_global, ["VAR"],
    "Unset the given global variable";
    "expand", `expand, ["STRING"],
    "Expand variable interpolations in the given string";
    "subst", `subst, ["FILE..."],
    "Substitute variables in the given files. The strings $(i,%{var}%) are \
     replaced by the value of variable $(i,var) (see $(b,var)).";
    "report", `report, [],
    "Prints a summary of your setup, useful for bug-reports.";
    "cudf-universe",`cudf, ["[FILE]"],
    "Outputs the current available package universe in CUDF format.";
    "pef-universe", `pef, ["[FILE]"],
    "Outputs the current package universe in PEF format.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command uses opam state to output information on how to use \
        installed libraries, update the $(b,PATH), and substitute \
        variables used in opam packages.";
    `P "Apart from $(b,opam config env), most of these commands are used \
        by opam internally, and are of limited interest for the casual \
        user.";
  ] @ mk_subdoc commands
    @ [`S "OPTIONS"]
  in

  let command, params = mk_subcommands commands in
  let all_doc         = "Enable all the global and user configuration options." in
  let global_doc      = "Enable all the global configuration options." in
  let user_doc        = "Enable all the user configuration options." in
  let profile_doc     = "Modify ~/.profile (or ~/.zshrc, etc., depending on your shell) to \
                         setup an opam-friendly environment when starting a new shell." in
  let no_complete_doc = "Do not load the auto-completion scripts in the environment." in
  let dot_profile_doc = "Select which configuration file to update (default is ~/.profile)." in
  let list_doc        = "List the current configuration." in
  let profile         = mk_flag ["profile"]        profile_doc in
  let no_complete     = mk_flag ["no-complete"]    no_complete_doc in
  let all             = mk_flag ["a";"all"]        all_doc in
  let user            = mk_flag ["u";"user"]       user_doc in
  let global          = mk_flag ["g";"global"]     global_doc in
  let list            = mk_flag ["l";"list"]       list_doc in
  let open Common_config_flags in

  let config global_options
      command shell sexp inplace_path
      dot_profile_o list all global user
      profile no_complete set_opamroot set_opamswitch params =
    apply_global_options global_options;
    match command, params with
    | Some `env, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (match OpamStateConfig.get_switch_opt () with
       | None -> `Ok ()
       | Some sw ->
         `Ok (OpamConfigCommand.env gt sw
                ~set_opamroot ~set_opamswitch
                ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish) ~inplace_path))
    | Some `revert_env, [] ->
       `Ok (OpamConfigCommand.print_eval_env
              ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish)
              (OpamEnv.add [] []))
    | Some `setup, [] ->
      let user        = all || user in
      let global      = all || global in
      let profile     = user  || profile in
      let completion    = global && not no_complete in
      let dot_profile = init_dot_profile shell dot_profile_o in
      if list then
        `Ok (OpamConfigCommand.setup_list shell dot_profile)
      else if profile || completion then
        let dot_profile = if profile then Some dot_profile else None in
        OpamGlobalState.with_ `Lock_write @@ fun gt ->
        `Ok (OpamConfigCommand.setup gt
               ?dot_profile ~completion ~shell
               ~user ~global)
      else
        `Ok (OpamConsole.msg
               "usage: opam config setup [options]\n\
                \n\
                Main options\n\
               \    -l, --list           %s\n\
               \    -a, --all            %s\n\
               \    --shell=<bash|sh|csh|zsh|fish>\n\
               \                         Configure assuming the given shell.\n\
                \n\
                User configuration\n\
               \    -u, --user           %s\n\
               \    --profile            %s\n\
               \    --dot-profile FILE   %s\n\
                \n\
                Global configuration\n\
               \    -g,--global          %s\n\
               \    --no-complete        %s\n\n"
               list_doc all_doc
               user_doc profile_doc dot_profile_doc
               global_doc no_complete_doc)
    | Some `exec, (_::_ as c) ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.exec
             gt ~set_opamroot ~set_opamswitch ~inplace_path c)
    | Some `list, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (try `Ok (OpamConfigCommand.list gt (List.map OpamPackage.Name.of_string params))
       with Failure msg -> `Error (false, msg))
    | Some `set, [var; value] ->
      `Ok (OpamConfigCommand.set (OpamVariable.Full.of_string var) (Some value))
    | Some `unset, [var] ->
      `Ok (OpamConfigCommand.set (OpamVariable.Full.of_string var) None)
    | Some `set_global, [var; value] ->
      `Ok (OpamConfigCommand.set_global
             (OpamVariable.Full.of_string var) (Some value))
    | Some `unset_global, [var] ->
      `Ok (OpamConfigCommand.set_global
             (OpamVariable.Full.of_string var) None)
    | Some `expand, [str] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.expand gt str)
    | Some `var, [var] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (try `Ok (OpamConfigCommand.variable gt (OpamVariable.Full.of_string var))
       with Failure msg -> `Error (false, msg))
    | Some `subst, (_::_ as files) ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      `Ok (OpamConfigCommand.subst gt (List.map OpamFilename.Base.of_string files))
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
       | _ -> bad_subcommand commands ("config", command, params))
    | Some `cudf, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun opam_state ->
      let opam_univ =
        OpamSwitchState.universe opam_state
          ~requested:(OpamPackage.names_of_packages opam_state.packages)
          Query
      in
      let dump oc = OpamSolver.dump_universe opam_univ oc in
      (match params with
       | [] -> `Ok (dump stdout)
       | [file] -> let oc = open_out file in dump oc; close_out oc; `Ok ()
       | _ -> bad_subcommand commands ("config", command, params))
    | Some `report, [] -> (
        let print label fmt = Printf.printf ("# %-17s "^^fmt^^"\n") label in
        Printf.printf "# opam config report\n";
        print "opam-version" "%s " (OpamVersion.to_string (OpamVersion.full ()));
        print "self-upgrade" "%s"
          (if self_upgrade_status global_options = `Running then
             OpamFilename.prettify (fst (self_upgrade_exe (OpamStateConfig.(!r.root_dir))))
           else "no");
        print "system" "arch=%s os=%s os-distribution=%s os-version=%s"
          OpamStd.Option.Op.(OpamSysPoll.arch () +! "unknown")
          OpamStd.Option.Op.(OpamSysPoll.os () +! "unknown")
          OpamStd.Option.Op.(OpamSysPoll.os_distribution () +! "unknown")
          OpamStd.Option.Op.(OpamSysPoll.os_version () +! "unknown");
        try
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          OpamSwitchState.with_ `Lock_none gt @@ fun state ->
          let module Solver = (val OpamSolverConfig.(Lazy.force !r.solver)) in
          print "solver" "%s" Solver.name;
          print "install-criteria" "%s"
            (OpamSolverConfig.criteria `Default);
          print "upgrade-criteria" "%s"
            (OpamSolverConfig.criteria `Upgrade);
          let nprint label n =
            if n <> 0 then [Printf.sprintf "%d (%s)" n label]
            else [] in
          print "jobs" "%d" (Lazy.force OpamStateConfig.(!r.jobs));
          print "repositories" "%s"
            (let repos = state.switch_repos.repositories in
             let default, nhttp, nlocal, nvcs =
               OpamRepositoryName.Map.fold
                 (fun _ repo (dft, nhttp, nlocal, nvcs) ->
                    let dft =
                      if OpamUrl.root repo.repo_url =
                         OpamUrl.root OpamInitDefaults.repository_url
                      then
                        OpamFile.Repo.safe_read
                          (OpamRepositoryPath.repo repo.repo_root) |>
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
          if List.mem "." (OpamStd.Sys.split_path_variable (Sys.getenv "PATH"))
          then OpamConsole.warning
              "PATH contains '.' : this is a likely cause of trouble.";
          `Ok ()
        with e -> print "read-state" "%s" (Printexc.to_string e); `Ok ())
    | command, params -> bad_subcommand commands ("config", command, params)
  in

  Term.ret (
    Term.(const config
          $global_options $command $shell_opt $sexp
          $inplace_path
          $dot_profile_flag $list $all $global $user
          $profile $no_complete
          $set_opamroot $set_opamswitch
          $params)
  ),
  term_info "config" ~doc ~man

(* VAR *)
let var_doc = "Prints the value associated with a given variable"
let var =
  let doc = var_doc in
  let man = [
    `S "DESCRIPTION";
    `P "With a $(i,VAR) argument, prints the value associated with $(i,VAR). \
        Without argument, lists the opam variables currently defined. This \
        command is a shortcut to `opam config var` and `opam config list`.";
  ] in
  let varname =
    Arg.(value & pos 0 (some string) None & info ~docv:"VAR" [])
  in
  let package =
    Arg.(value & opt (some package_name) None &
         info ~docv:"PACKAGE" ["package"]
           ~doc:"List all variables defined for the given package")
  in
  let print_var global_options package var =
    apply_global_options global_options;
    match var, package with
    | None, None ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (try `Ok (OpamConfigCommand.list gt [])
       with Failure msg -> `Error (false, msg))
    | None, Some pkg ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (try `Ok (OpamConfigCommand.list gt [pkg])
       with Failure msg -> `Error (false, msg))
    | Some v, None ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (try `Ok (OpamConfigCommand.variable gt (OpamVariable.Full.of_string v))
       with Failure msg -> `Error (false, msg))
    | Some _, Some _ ->
      `Error (true, "--package can't be specified with a var argument, use \
                     'pkg:var' instead.")
  in
  Term.ret (
    Term.(const print_var $global_options $package $varname)
  ),
  term_info "var" ~doc ~man

(* EXEC *)
let exec_doc = "Executes a command in the proper opam environment"
let exec =
  let doc = exec_doc in
  let man = [
    `S "DESCRIPTION";
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
  let exec global_options inplace_path set_opamroot set_opamswitch cmd =
    apply_global_options global_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamConfigCommand.exec gt
      ~set_opamroot ~set_opamswitch ~inplace_path cmd
  in
  let open Common_config_flags in
  Term.(const exec $global_options $inplace_path
        $set_opamroot $set_opamswitch
        $cmd),
  term_info "exec" ~doc ~man

(* ENV *)
let env_doc = "Prints appropriate shell variable assignments to stdout"
let env =
  let doc = env_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Returns the bindings for the environment variables set in the current \
        switch, e.g. PATH, in a format intended to be evaluated by a shell. \
        With $(i,-v), add comments documenting the reason or package of origin \
        for each binding. This is most usefully used as $(b,eval \\$(opam \
        env\\)) to have further shell commands be evaluated in the proper opam \
        context.";
    `P "This is a shortcut, and equivalent to $(b,opam config env).";
  ] in
  let revert =
    mk_flag ["revert"]
      "Output the environment with updates done by opam reverted instead."
  in
  let env
      global_options shell sexp inplace_path set_opamroot set_opamswitch
      revert =
    apply_global_options global_options;
    match revert with
    | false ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      (match OpamStateConfig.get_switch_opt () with
       | None -> ()
       | Some sw ->
         OpamConfigCommand.env gt sw
           ~set_opamroot ~set_opamswitch
           ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish) ~inplace_path)
    | true ->
      OpamConfigCommand.print_eval_env
        ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish)
        (OpamEnv.add [] [])
  in
  let open Common_config_flags in
  Term.(const env
        $global_options $shell_opt $sexp $inplace_path
        $set_opamroot $set_opamswitch$revert),
  term_info "env" ~doc ~man

(* INSTALL *)
let install_doc = "Install a list of packages."
let install =
  let doc = install_doc in
  let man = [
    `S "DESCRIPTION";
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
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.build_option_section;
  ] in
  let add_to_roots =
    let root =
      Some true, Arg.info ["set-root"]
        ~doc:"Mark given packages as installed roots. This is the default \
              for newly manually-installed packages." in
    let unroot =
      Some false, Arg.info ["unset-root"]
        ~doc:"Mark given packages as \"installed automatically\"." in
    Arg.(value & vflag None[root; unroot])
  in
  let deps_only =
    Arg.(value & flag & info ["deps-only"]
           ~doc:"Install all its dependencies, but don't actually install the \
                 package.") in
  let restore =
    Arg.(value & flag & info ["restore"]
           ~doc:"Attempt to restore packages that were marked for installation \
                 but have been removed due to errors") in
  let destdir =
    mk_opt ["destdir"] "DIR"
      "Copy the files installed by the given package within the current opam \
       switch below the prefix $(i,DIR), respecting their hierarchy, after \
       installation. Caution, calling this can overwrite, but never remove \
       files, even if they were installed by a previous use of $(b,--destdir), \
       e.g. on a previous version of the same package. See $(b,opam remove \
       --destdir) to revert."
      Arg.(some dirname) None
  in
  let locked =
    mk_flag ["locked"]
      "With a directory as argument, when a package definition file is found, \
       if a file by the same name with a $(i,.locked) extension is present \
       beside it, use the latter. This allows alternate local package \
       definitions, and is typically useful to provide more constrained \
       dependencies that describe a precise development environment."
  in
  let install
      global_options build_options add_to_roots deps_only restore destdir locked
      atoms_or_locals =
    apply_global_options global_options;
    apply_build_options build_options;
    if atoms_or_locals = [] && not restore then
      `Error (true, "required argument PACKAGES is missing")
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
    if atoms_or_locals = [] then `Ok () else
    let st, atoms =
      OpamAuxCommands.autopin st ~simulate:deps_only ~locked atoms_or_locals
    in
    if atoms = [] then
      (OpamConsole.msg "Nothing to do\n";
       OpamStd.Sys.exit_because `Success);
    let st =
      OpamClient.install st atoms
        ~autoupdate:pure_atoms ?add_to_roots ~deps_only
    in
    match destdir with
    | None -> `Ok ()
    | Some dest ->
      let packages = OpamFormula.packages_of_atoms st.installed atoms in
      OpamAuxCommands.copy_files_to_destdir st dest packages;
      `Ok ()
  in
  Term.ret
    Term.(const install $global_options $build_options
          $add_to_roots $deps_only $restore $destdir $locked
          $atom_or_local_list),
  term_info "install" ~doc ~man

(* REMOVE *)
let remove_doc = "Remove a list of packages."
let remove =
  let doc = remove_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command uninstalls one or more packages currently \
        installed in the currently selected compiler switch. To remove packages \
        installed in another compiler, you need to switch compilers using \
        $(b,opam switch) or use the $(b,--switch) flag. This command is the \
        inverse of $(b,opam-install).";
    `P "If a directory name is specified as package, packages pinned to that \
        directory are both unpinned and removed.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.build_option_section;
  ] in
  let autoremove =
    mk_flag ["a";"auto-remove"]
      "Remove all the packages which have not been explicitly installed and \
       which are not necessary anymore. It is possible to prevent the removal \
       of an already-installed package by running $(b,opam install <pkg> \
       --set-root). This flag can also be set using the $(b,\\$OPAMAUTOREMOVE) \
       configuration variable." in
  let force =
    mk_flag ["force"]
      "Execute the remove commands of given packages directly, even if they are \
       not considered installed by opam." in
  let destdir =
    mk_opt ["destdir"] "DIR"
      "Instead of uninstalling the packages, reverts the action of $(b,opam \
       install --destdir): remove files corresponding to what the listed \
       packages installed to the current switch from the given $(i,DIR). Note \
       that the package needs to still be installed to the same version that \
       was used for $(b,install --destdir) for this to work reliably. The \
       packages are not removed from the current opam switch when this is \
       specified."
      Arg.(some dirname) None
  in
  let remove global_options build_options autoremove force destdir atom_locs =
    apply_global_options global_options;
    apply_build_options build_options;
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
      let pin_atoms = OpamAuxCommands.resolve_locals_pinned st pin_atoms in
      let st =
        if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.show) then st
        else OpamPinCommand.unpin st (List.map fst pin_atoms)
      in
      let atoms =
        List.map (function `Atom a -> a | _ -> assert false) pure_atoms
        @ pin_atoms
      in
      ignore @@ OpamClient.remove st ~autoremove ~force atoms
  in
  Term.(const remove $global_options $build_options $autoremove $force $destdir
        $atom_or_dir_list),
  term_info "remove" ~doc ~man

(* REINSTALL *)
let reinstall =
  let doc = "Reinstall a list of packages." in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes the given packages and the ones that depend on \
        them, and reinstalls the same versions. Without arguments, assume \
        $(b,--pending) and reinstall any package with upstream changes.";
    `P "If a directory is specified as argument, anything that is pinned to \
        that directory is selected for reinstall.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.build_option_section;
  ] in
  let cmd =
    Arg.(value & vflag `Default [
        `Pending, info ["pending"]
          ~doc:"Perform pending reinstallations, i.e. reinstallations of \
                packages that have changed since installed";
        `List_pending, info ["list-pending"]
          ~doc:"List packages that have been changed since installed and are \
                marked for reinstallation";
        `Forget_pending, info ["forget-pending"]
          ~doc:"Forget about pending reinstallations of listed packages. This \
                implies making opam assume that your packages were installed \
                with a newer version of their metadata, so only use this if \
                you know what you are doing, and the actual changes you are \
                overriding."
      ])
  in
  let reinstall global_options build_options atoms_locs cmd =
    apply_global_options global_options;
    apply_build_options build_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    match cmd, atoms_locs with
    | `Default, (_::_ as atom_locs) ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      ignore @@ OpamClient.reinstall st
        (OpamAuxCommands.resolve_locals_pinned st atom_locs);
      `Ok ()
    | `Pending, [] | `Default, [] ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let atoms = OpamSolution.eq_atoms_of_packages st.reinstall in
      ignore @@ OpamClient.reinstall st atoms;
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
        st.reinstall;
      `Ok ()
    | `Forget_pending, atom_locs ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let atoms = OpamAuxCommands.resolve_locals_pinned st atom_locs in
      let to_forget = match atoms with
        | [] -> st.reinstall
        | atoms -> OpamFormula.packages_of_atoms st.reinstall atoms
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
      let reinstall = OpamPackage.Set.Op.(st.reinstall -- to_forget) in
      ignore @@ OpamSwitchAction.update_switch_state ~reinstall st;
      `Ok ()
    | _, _::_ ->
      `Error (true, "Package arguments not allowed with this option")
  in
  Term.(ret (const reinstall $global_options $build_options $atom_or_dir_list
             $cmd)),
  term_info "reinstall" ~doc ~man

(* UPDATE *)
let update_doc = "Update the list of available packages."
let update =
  let doc = update_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Update the package definitions. This fetches the newest version of the \
        repositories configured through $(b, opam repository), and the sources \
        of installed development packages and packages pinned in the current \
        switch. To use the updated sources and definitions, use \
        $(b,opam upgrade).";
  ] in
  let repos_only =
    mk_flag ["R"; "repositories"]
      "Update repositories (skipping development packages unless \
       $(b,--development) is also specified)." in
  let dev_only =
    mk_flag ["development"]
      "Update development packages (skipping repositories unless \
       $(b,--repositories) is also specified)." in
  let upgrade =
    mk_flag ["u";"upgrade"]
      "Automatically run $(b,opam upgrade) after the update." in
  let name_list =
    arg_list "NAMES"
      "List of repository or development package names to update."
      Arg.string in
  let all =
    mk_flag ["a"; "all"]
      "Update all configured repositories, not only what is set in the current \
       switch" in
  let check =
    mk_flag ["check"]
      "Do the update, then return with code 0 if there were any upstream \
       changes, 1 if there were none. Repositories or development packages \
       that failed to update are considered without changes. With \
       $(b,--upgrade), behaves like $(b,opam upgrade --check), that is, \
       returns 0 only if there are currently availbale updates." in
  let update global_options jobs names repos_only dev_only all check upgrade =
    apply_global_options global_options;
    OpamStateConfig.update
      ?jobs:OpamStd.Option.Op.(jobs >>| fun j -> lazy j)
      ();
    OpamClientConfig.update ();
    OpamGlobalState.with_ `Lock_write @@ fun gt ->
    let success, changed, rt =
      OpamClient.update gt
        ~repos_only:(repos_only && not dev_only)
        ~dev_only:(dev_only && not repos_only)
        ~all
        names
    in
    if upgrade then
      OpamSwitchState.with_ `Lock_write gt ~rt @@ fun st ->
      OpamConsole.msg "\n";
      ignore @@ OpamClient.upgrade st ~check ~all:true []
    else if check then
      OpamStd.Sys.exit_because (if changed then `Success else `False)
    else
      OpamConsole.msg "Now run 'opam upgrade' to apply any package updates.\n";
    if not success then OpamStd.Sys.exit_because `Sync_error
  in
  Term.(const update $global_options $jobs_flag $name_list
        $repos_only $dev_only $all $check $upgrade),
  term_info "update" ~doc ~man

(* UPGRADE *)
let upgrade_doc = "Upgrade the installed package to latest version."
let upgrade =
  let doc = upgrade_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command upgrades the installed packages to their latest available \
        versions. More precisely, this command calls the dependency solver to \
        find a consistent state where $(i,most) of the installed packages are \
        upgraded to their latest versions.";
    `P "If a directory is specified as argument, anything that is pinned to \
        that directory is selected for upgrade.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.build_option_section;
  ] in
  let fixup =
    mk_flag ["fixup"]
      "Recover from a broken state (eg. missing dependencies, two conflicting \
       packages installed together...)." in
  let check =
    mk_flag ["check"]
      "Don't run the upgrade: just check if anything could be upgraded. \
       Returns 0 if that is the case, 1 if there is nothing that can be \
       upgraded." in
  let all =
    mk_flag ["a";"all"]
      "Run an upgrade of all installed packages. This is the default if \
       $(i,PACKAGES) was not specified, and can be useful with $(i,PACKAGES) \
       to upgrade while ensuring that some packages get or remain installed."
  in
  let upgrade global_options build_options fixup check all atom_locs =
    apply_global_options global_options;
    apply_build_options build_options;
    let all = all || atom_locs = [] in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    if fixup then
      if atom_locs <> [] || check then
        `Error (true, Printf.sprintf "--fixup doesn't allow extra arguments")
      else
        OpamSwitchState.with_ `Lock_write gt @@ fun st ->
        ignore @@ OpamClient.fixup st;
        `Ok ()
    else
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let atoms = OpamAuxCommands.resolve_locals_pinned st atom_locs in
      ignore @@ OpamClient.upgrade st ~check ~all atoms;
      `Ok ()
  in
  Term.(ret (const upgrade $global_options $build_options $fixup $check $all
             $atom_or_dir_list)),
  term_info "upgrade" ~doc ~man

(* REPOSITORY *)
let repository_doc = "Manage opam repositories."
let repository =
  let doc = repository_doc in
  let scope_section = "SCOPE SPECIFICATION OPTIONS" in
  let commands = [
    "add", `add, ["NAME"; "[ADDRESS]"; "[QUORUM]"; "[FINGERPRINTS]"],
    "Adds under $(i,NAME) the repository at address $(i,ADDRESS) to the list \
     of configured repositories, if not already registered, and sets this \
     repository for use in the current switch (or the specified scope). \
     $(i,ADDRESS) is required if the repository name is not already \
     registered, and is otherwise an error if different from the registered \
     address. The quorum is a positive integer that determines the validation \
     threshold for signed repositories, with fingerprints the trust anchors \
     for said validation.";
    "remove", `remove, ["NAME..."],
    "Unselects the given repositories so that they will not be used to get \
     package definitions anymore. With $(b,--all), makes opam forget about \
     these repositories completely.";
    "set-repos", `set_repos, ["NAME..."],
    "Explicitely selects the list of repositories to look up package \
     definitions from, in the specified priority order (overriding previous \
     selection and ranks), according to the specified scope.";
    "set-url", `set_url, ["NAME"; "ADDRESS"; "[QUORUM]"; "[FINGERPRINTS]"],
    "Updates the URL and trust anchors associated with a given repository \
     name. Note that if you don't specify $(i,[QUORUM]) and \
     $(i,[FINGERPRINTS]), any previous settings will be erased.";
    "list", `list, [],
    "Lists the currently selected repositories in priority order from rank 1. \
     With $(b,--all), lists all configured repositories and the switches where \
     they are active.";
    "priority", `priority, ["NAME"; "RANK"],
    "Synonym to $(b,add NAME --rank RANK)";
  ] in
  let man = [
    `S "DESCRIPTION";
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
  ] @ mk_subdoc ~defaults:["","list"] commands @ [
      `S scope_section;
      `P "These flags allow to choose what selections are changed by $(b,add), \
          $(b,remove), $(b,set-repos). If no flag in this section is specified \
          the updated selections default to the current switch. Multiple scopes \
          can be selected, e.g. $(b,--this-switch --set-default).";
      `S "OPTIONS";
    ]
  in
  let command, params = mk_subcommands commands in
  let scope =
    let scope_info ?docv flags doc =
      Arg.info ~docs:scope_section ~doc ?docv flags
    in
    let flags =
      Arg.vflag_all [] [
        `No_selection, scope_info ["dont-select"]
          "Don't update any selections";
        `Current_switch, scope_info ["this-switch"]
          "Act on the selections for the current switch (this is the default)";
        `Default, scope_info ["set-default"]
          "Act on the default repository selection that is used for newly \
           created switches";
        `All, scope_info ["all-switches";"a"]
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
          $ Arg.value flags $ switches)
  in
  let rank =
    Arg.(value & opt int 1 & info ~docv:"RANK" ["rank"] ~doc:
           "Set the rank of the repository in the list of configured \
            repositories. Package definitions are looked in the repositories \
            in increasing rank order, therefore 1 is the highest priority. \
            Negative ints can be used to select from the lowest priority, -1 \
            being last. $(b,set-repos) can otherwise be used to explicitely \
            set the repository list at once.")
  in
  let repository global_options command kind short scope rank params =
    apply_global_options global_options;
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
          | `Current_switch ->
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
      let url = OpamUrl.parse ?backend:kind url in
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
            (let _rt = OpamRepositoryCommand.remove rt name in
             OpamConsole.error_and_exit `Sync_error
               "Initial repository fetch failed"));
      let _gt =
        OpamRepositoryCommand.update_selection gt ~global ~switches
          (update_repos name)
      in
      if scope = [`Current_switch] then
        OpamConsole.msg
          "Repository %s has been added to the selections of switch %s.\n"
          (OpamRepositoryName.to_string name)
          (OpamSwitch.to_string (OpamStateConfig.get_switch ()));
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
        let _rt = List.fold_left OpamRepositoryCommand.remove rt names in
        ()
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
      let _gt =
        OpamRepositoryCommand.update_selection gt ~global ~switches
          (update_repos name)
      in
      `Ok ()
    | Some `set_url, (name :: url :: security) ->
      let name = OpamRepositoryName.of_string name in
      let url = OpamUrl.parse ?backend:kind url in
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
        OpamFile.Repos_config.safe_read (OpamPath.repos_config gt.root)
      in
      let not_found =
        List.filter (fun r -> not (OpamRepositoryName.Map.mem r repos)) names
      in
      if not_found = [] then
        let _gt =
          OpamRepositoryCommand.update_selection gt ~global ~switches
            (fun _ -> names)
        in
        `Ok ()
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
           List.exists (function `Current_switch | `Switch _ -> true
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
    | command, params -> bad_subcommand commands ("repository", command, params)
  in
  Term.ret
    Term.(const repository $global_options $command $repo_kind_flag
          $print_short_flag $scope $rank $params),
  term_info "repository" ~doc ~man


(* SWITCH *)

(* From a list of strings (either "repo_name" or "repo_name=URL"), configure the
   repos with URLs if possible, and return the updated repos_state and selection of
   repositories *)
let get_repos_rt gt repos =
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
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
          | (n, Some u) -> Some (n, OpamUrl.of_string u))
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
        let _rt = List.fold_left OpamRepositoryCommand.remove rt failed in
        OpamConsole.error_and_exit `Sync_error
          "Initial fetch of these repositories failed: %s"
          (OpamStd.List.concat_map ", " OpamRepositoryName.to_string failed)
      else
        Some (List.map fst repos), rt

let switch_doc = "Manage multiple installation prefixes."
let switch =
  let doc = switch_doc in
  let commands = [
    "create", `install, ["SWITCH"; "[COMPILER]"],
    "Create a new switch, and install the given compiler there. $(i,SWITCH) \
     can be a plain name, or a directory, absolute or relative, in which case \
     a local switch is created below the given directory. $(i,COMPILER), if \
     omitted, defaults to $(i,SWITCH) if it is a plain name, unless \
     $(b,--packages) or $(b,--empty) is specified. When creating a local \
     switch, and none of these options are present, the compiler is chosen \
     according to the configuration default (see opam-init(1)). If the chosen \
     directory contains package definitions, a compatible compiler is searched \
     within the default selection, and the packages will automatically get \
     installed.";
    "set", `set, ["SWITCH"],
    "Set the currently active switch, among the installed switches.";
    "remove", `remove, ["SWITCH"], "Remove the given switch from disk.";
    "export", `export, ["FILE"],
    "Save the current switch state to a file.";
    "import", `import, ["FILE"],
    "Import a saved switch state. If $(b,--switch) is specified and doesn't \
     point to an existing switch, the switch will be created for the import.";
    "reinstall", `reinstall, ["[SWITCH]"],
    "Reinstall the given compiler switch and all its packages.";
    "list", `list, [],
    "Lists installed switches.";
    "list-available", `list_available, ["[PATTERN]"],
    "Lists base packages that can be used to create a new switch, i.e. \
     packages with the $(i,compiler) flag set. Only standard versions are \
     shown by default if no pattern is supplied, use $(b,--all) to show all.";
    "show", `current, [], "Prints the name of the current switch.";
    "set-base", `set_compiler, ["PACKAGES"],
    "Sets the packages forming the immutable base for the selected switch, \
     overriding the current setting.";
    "set-description", `set_description, ["STRING"],
    "Sets the description for the selected switch";
    "install", `install, ["SWITCH"],
    "Deprecated alias for 'create'."
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command is used to manage \"switches\", which are independent \
        installation prefixes with their own compiler and sets of installed \
        and pinned packages. This is typically useful to have different \
        versions of the compiler available at once.";
    `P "Use $(b,opam switch create) to create a new switch, and $(b,opam \
        switch set) to set the currently active switch. Without argument, \
        lists installed switches, with one switch argument, defaults to \
        $(b,set).";
    `P ("Switch handles $(i,SWITCH) can be either a plain name, for switches \
         that will be held inside $(i,~/.opam), or a directory name, which in \
         that case is the directory where the switch prefix will be installed, as "
        ^ OpamSwitch.external_dirname ^
        ". Opam will automatically select a switch by that name found in the \
         current directory or its parents, unless $(i,OPAMSWITCH) is set or \
         $(b,--switch) is specified. When creating a directory switch, if \
         package definitions are found locally, the user is automatically \
         prompted to install them after the switch is created unless \
         $(b,--no-install) is specified.");
    `P "$(b,opam switch set) sets the default switch globally, but it is also \
        possible to select a switch in a given shell session, using the \
        environment. For that, use $(i,eval \\$(opam env \
        --switch=SWITCH --set-switch\\)).";
  ] @ mk_subdoc ~defaults:["","list";"SWITCH","set"] commands
    @ [`S "OPTIONS"]
    @ [`S OpamArg.build_option_section]
  in

  let command, params = mk_subcommands_with_default commands in
  let no_switch =
    mk_flag ["no-switch"]
      "Don't automatically select newly installed switches" in
  let packages =
    mk_opt ["packages"] "PACKAGES"
      "When installing a switch, explicitely define the set of packages to set \
       as the compiler."
      Arg.(some (list atom)) None in
  let empty =
    mk_flag ["empty"]
      "Allow creating an empty (without compiler) switch." in
  let repos =
    mk_opt ["repositories"] "REPOS"
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
    mk_opt ["description"] "STRING"
      "Attach the given description to a switch when creating it. Use the \
       $(i,set-description) subcommand to modify the description of an \
       existing switch."
      Arg.(some string) None
  in
  let full =
    mk_flag ["full"]
      "When exporting, include the metadata of all installed packages, \
       allowing to re-import even if they don't exist in the repositories (the \
       default is to include only the metadata of pinned packages)"
  in
  let no_install =
    mk_flag ["no-install"]
      "When creating a local switch, don't look for any local package \
       definitions to install."
  in
  let deps_only =
    mk_flag ["deps-only"]
      "When creating a local switch in a project directory (i.e. a directory \
       containing opam package definitions), install the dependencies of the \
       project but not the project itself."
  in
  let locked =
    mk_flag ["locked"]
      "With a directory as argument, when a package definition file is found, \
       if a file by the same name with a $(i,.locked) extension is present \
       beside it, use the latter. This allows alternate local package \
       definitions, and is typically useful to provide more constrained \
       dependencies that describe a precise development environment."
  in
  let switch
      global_options build_options command print_short
      no_switch packages empty descr full no_install deps_only locked repos
      params =
    apply_global_options global_options;
    apply_build_options build_options;
    let packages =
      match packages, empty with
      | None, true -> Some []
      | Some packages, true when packages <> [] ->
        OpamConsole.error_and_exit `Bad_arguments
          "Options --packages and --empty may not be specified at the same time"
      | packages, _ -> packages
    in
    let compiler_packages rt ?repos switch compiler_opt =
      match packages, compiler_opt, OpamSwitch.is_external switch with
      | None, None, false ->
        OpamSwitchCommand.guess_compiler_package ?repos rt
          (OpamSwitch.to_string switch)
      | None, None, true ->
        OpamAuxCommands.get_compatible_compiler ?repos ~locked rt
          (OpamFilename.dirname_dir
             (OpamSwitch.get_root rt.repos_global.root switch))
      | _ ->
        OpamStd.Option.Op.(
          ((compiler_opt >>|
            OpamSwitchCommand.guess_compiler_package ?repos rt) +! []) @
          packages +! [])
    in
    let param_compiler = function
      | [] -> None
      | [comp] -> Some comp
      | args ->
        OpamConsole.error_and_exit `Bad_arguments
          "Invalid extra arguments %s"
          (String.concat " " args)
    in
    match command, params with
    | None      , []
    | Some `list, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchCommand.list gt ~print_short;
      `Ok ()
    | Some `list_available, pattlist ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let repos, rt = get_repos_rt gt repos in
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
      let repos, rt = get_repos_rt gt repos in
      let switch = OpamSwitch.of_string switch_arg in
      let packages =
        compiler_packages rt ?repos switch (param_compiler params)
      in
      let _gt, st =
        OpamSwitchCommand.install gt ~rt
          ?synopsis:descr ?repos
          ~update_config:(not no_switch)
          ~packages
          switch
      in
      let st =
        if not no_install && not empty && OpamSwitch.is_external switch then
          let st, atoms =
            OpamAuxCommands.autopin st ~simulate:deps_only ~locked ~quiet:true
              [`Dirname (OpamFilename.Dir.of_string switch_arg)]
          in
          OpamClient.install st atoms
            ~autoupdate:[] ~add_to_roots:true ~deps_only
        else st
      in
      ignore (OpamSwitchState.unlock st);
      `Ok ()
    | Some `export, [filename] ->
      OpamSwitchCommand.export
        ~full
        (if filename = "-" then None
         else Some (OpamFile.make (OpamFilename.of_string filename)));
      `Ok ()
    | Some `import, [filename] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let switch = OpamStateConfig.get_switch () in
      let installed_switches = OpamFile.Config.installed_switches gt.config in
      let is_new_switch = not (List.mem switch installed_switches) in
      let gt, rt =
        if is_new_switch then
          let repos, rt = get_repos_rt gt repos in
          let (), gt =
            OpamGlobalState.with_write_lock gt @@ fun gt ->
            (), OpamSwitchAction.create_empty_switch gt ?repos switch
          in
          gt, rt
        else
          (if repos <> None then
             OpamConsole.warning
               "Switch exists, '--repositories' argument ignored";
           gt, OpamRepositoryState.load `Lock_none gt)
      in
      OpamSwitchState.with_ `Lock_write gt ~rt ~switch @@ fun st ->
      let _st =
        try
          OpamSwitchCommand.import st
            (if filename = "-" then None
             else Some (OpamFile.make (OpamFilename.of_string filename)))
        with e ->
          if is_new_switch then
            OpamConsole.warning
              "Switch %s may have been left partially installed"
              (OpamSwitch.to_string switch);
          raise e
      in
      `Ok ()
    | Some `remove, switches ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let _gt =
        List.fold_left
          (fun gt switch ->
             OpamSwitchCommand.remove gt (OpamSwitch.of_string switch))
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
      let _st = OpamSwitchCommand.reinstall st in
      `Ok ()
    | Some `current, [] ->
      OpamSwitchCommand.show ();
      `Ok ()
    | Some `set, [switch]
    | Some `default switch, [] ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let switch_name = OpamSwitch.of_string switch in
      OpamSwitchCommand.switch `Lock_none gt switch_name |> ignore;
      `Ok ()
    | Some `set_compiler, packages ->
      (try
         let parse_namev s = match fst OpamArg.package s with
           | `Ok (name, version_opt) -> name, version_opt
           | `Error e -> failwith e
         in
         let namesv = List.map parse_namev packages in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         let _st = OpamSwitchCommand.set_compiler st namesv in
         `Ok ()
       with Failure e -> `Error (false, e))
    | Some `set_description, text ->
      let synopsis = String.concat " " text in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let config =
        { st.switch_config with OpamFile.Switch_config.synopsis }
      in
      OpamSwitchAction.install_switch_config gt.root st.switch config;
      `Ok ()
    | command, params -> bad_subcommand commands ("switch", command, params)
  in
  Term.(ret (const switch
             $global_options $build_options $command
             $print_short_flag
             $no_switch
             $packages $empty $descr $full $no_install $deps_only $locked
             $repos $params)),
  term_info "switch" ~doc ~man

(* PIN *)
let pin_doc = "Pin a given package to a specific version or source."
let pin ?(unpin_only=false) () =
  let doc = pin_doc in
  let commands = [
    "list", `list, [], "Lists pinned packages.";
    "add", `add, ["PACKAGE"; "TARGET"],
    "Pins package $(i,PACKAGE) to $(i,TARGET), which may be a version, a path, \
     or a URL.\n\
     $(i,PACKAGE) can be omitted if $(i,TARGET) is a local path containing a \
     package description with a name. $(i,TARGET) can be replaced by \
     $(b,--dev-repo) if a package by that name is already known. If \
     $(i,TARGET) is $(b,-), the package is pinned as a virtual package, \
     without any source. opam will infer the kind of pinning from the format \
     (and contents, if local) of $(i,TARGET), Use $(b,--kind) or an explicit \
     URL to disable that behaviour.\n\
     Pins to version control systems may target a specific branch or commit \
     using $(b,#branch) e.g. $(b,git://host/me/pkg#testing).\n\
     If $(i,PACKAGE) is not a known package name, a new package by that name \
     will be locally created.\n\
     The package version may be specified by using the format \
     $(i,NAME).$(i,VERSION) for $(i,PACKAGE), in the source opam file, or with \
     $(b,edit).";
    "remove", `remove, ["NAMES...|TARGET"],
    "Unpins packages $(i,NAMES), restoring their definition from the \
     repository, if any. With a $(i,TARGET), unpins everything that is \
     currently pinned to that target.";
    "edit", `edit, ["NAME"],
    "Opens an editor giving you the opportunity to change the package \
     definition that opam will locally use for package $(i,NAME), including \
     its version and source URL. Using the format $(i,NAME.VERSION) will \
     update the version in the opam file in advance of editing, without \
     changing the actual target. The chosen editor is determined from \
     environment variables $(b,OPAM_EDITOR), $(b,VISUAL) or $(b,EDITOR), in \
     order.";
  ] in
  let man = [
    `S "DESCRIPTION";
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
        containing one or more valid package definitions (this allows to do \
        e.g. $(i,opam pin add .) from a source directory.";
    `P "If $(i,PACKAGE) has the form $(i,name.version), the pinned package \
        will be considered as version $(i,version) by opam. Beware that this \
        doesn't relate with the version of the source actually used for the \
        package.";
    `P "The default subcommand is $(i,list) if there are no further arguments, \
        and $(i,add) otherwise if unambiguous.";
  ] @ mk_subdoc ~defaults:["","list"] commands @ [
      `S "OPTIONS";
      `S OpamArg.build_option_section;
    ]
  in
  let command, params =
    if unpin_only then
      Term.const (Some `remove),
      Arg.(value & pos_all string [] & Arg.info [])
    else
      mk_subcommands_with_default commands in
  let edit =
    mk_flag ["e";"edit"]
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
    mk_flag ["n";"no-action"]
      "Just record the new pinning status, and don't prompt for \
       (re)installation or removal of affected packages."
  in
  let dev_repo =
    mk_flag ["dev-repo"] "Pin to the upstream package source for the latest \
                          development version"
  in
  let guess_names url =
    let from_opam_files dir =
      OpamStd.List.filter_map
        (fun (nameopt, f) -> match nameopt with
           | None -> OpamFile.OPAM.(name_opt (safe_read f))
           | some -> some)
        (OpamPinned.files_in_source dir)
    in
    let basename =
      List.hd (OpamStd.String.split (OpamUrl.basename url) '.')
    in
    let found =
      match OpamUrl.local_dir url with
      | Some d -> from_opam_files d
      | None ->
        let open OpamProcess.Job.Op in
        OpamProcess.Job.run @@
        OpamFilename.with_tmp_dir_job @@ fun dir ->
        OpamRepository.pull_tree
          ~cache_dir:(OpamRepositoryPath.download_cache
                        OpamStateConfig.(!r.root_dir))
          basename dir [] [url] @@| function
        | Not_available u ->
          OpamConsole.error_and_exit `Sync_error
            "Could not retrieve %s" u
        | Result _ | Up_to_date _ -> from_opam_files dir
    in
    match found with
    | _::_ -> found
    | [] ->
      try [OpamPackage.Name.of_string basename] with
      | Failure _ ->
        OpamConsole.error_and_exit `Bad_arguments
          "Could not infer a package name from %s, please specify it on the \
           command-line, e.g. 'opam pin NAME TARGET'"
          (OpamUrl.to_string url)
  in
  let pin_target kind target =
    let looks_like_version_re =
      Re.(compile @@ seq [bos; opt @@ char 'v'; digit; rep @@ diff any (set "/\\"); eos])
    in
    let auto () =
      if target = "-" then
        `None
      else if Re.execp looks_like_version_re target then
        `Version (OpamPackage.Version.of_string target)
      else
      let backend = OpamUrl.guess_version_control target in
      `Source (OpamUrl.parse ?backend ~handle_suffix:true target)
    in
    let target =
      match kind with
      | Some `version -> `Version (OpamPackage.Version.of_string target)
      | Some (#OpamUrl.backend as k) ->
        `Source (OpamUrl.parse ~backend:k target)
      | Some `none -> `None
      | Some `auto -> auto ()
      | None when OpamClientConfig.(!r.pin_kind_auto) -> auto ()
      | None -> `Source (OpamUrl.parse ~handle_suffix:false target)
    in
    match target with
    | `Source url -> `Source (OpamAuxCommands.url_with_local_branch url)
    | _ -> target
  in
  let pin
      global_options build_options
      kind edit no_act dev_repo print_short command params =
    apply_global_options global_options;
    apply_build_options build_options;
    let action = not no_act in
    match command, params with
    | Some `list, [] | None, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      OpamClient.PIN.list st ~short:print_short;
      `Ok ()
    | Some `remove, (_::_ as arg) ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let err, to_unpin =
        List.fold_left (fun (err, acc) arg ->
            let as_url =
              let url = OpamUrl.of_string arg in
              OpamPackage.Set.filter
                (fun nv ->
                   match OpamSwitchState.url st nv with
                   | Some u ->
                     let u = OpamFile.URL.url u in
                     OpamUrl.(u.transport = url.transport && u.path = url.path)
                   | None -> false)
                st.pinned |>
              OpamPackage.names_of_packages |>
              OpamPackage.Name.Set.elements
            in
            match as_url with
            | _::_ -> err, as_url @ acc
            | [] ->
              match (fst package_name) arg with
              | `Ok name -> err, name::acc
              | `Error _ ->
                OpamConsole.error
                  "No package pinned to this target found, or invalid package \
                   name: %s" arg;
                true, acc)
          (false,[]) arg
      in
      if err then OpamStd.Sys.exit_because `Bad_arguments
      else
        (ignore @@ OpamClient.PIN.unpin st ~action to_unpin;
         `Ok ())
    | Some `edit, [nv]  ->
      (match (fst package) nv with
       | `Ok (name, version) ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         ignore @@ OpamClient.PIN.edit st ~action ?version name;
         `Ok ()
       | `Error e -> `Error (false, e))
    | Some `add, [nv] | Some `default nv, [] when dev_repo ->
      (match (fst package) nv with
       | `Ok (name,version) ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         let name = OpamSolution.fuzzy_name st name in
         ignore @@ OpamClient.PIN.pin st name ~edit ?version ~action
           `Dev_upstream;
         `Ok ()
       | `Error e ->
         if command = Some `add then `Error (false, e)
         else bad_subcommand commands ("pin", command, params))
    | Some `add, [arg] | Some `default arg, [] ->
      (match pin_target kind arg with
       | `None | `Version _ ->
         let msg =
           Printf.sprintf "Ambiguous argument %S, if it is the pinning target, \
                           you must specify a package name first" arg
         in
         `Error (true, msg)
       | `Source url ->
         let names = guess_names url in
         let names = match names with
           | _::_::_ ->
             if OpamConsole.confirm
                 "This will pin the following packages: %s. Continue?"
                 (OpamStd.List.concat_map ", " OpamPackage.Name.to_string names)
             then names
             else OpamStd.Sys.exit_because `Aborted
           | _ -> names
         in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         let st =
           List.fold_left (fun st name ->
               try OpamPinCommand.source_pin st name ~edit (Some url)
               with OpamPinCommand.Aborted
                  | OpamPinCommand.Nothing_to_do -> st)
             st names
         in
         if action then
           let _st =
             OpamClient.upgrade_t
               ~strict_upgrade:false ~auto_install:true ~ask:true ~all:false
               (List.map (fun n -> n, None) names) st
           in
           `Ok ()
         else `Ok ())
    | Some `add, [n; target] | Some `default n, [target] ->
      (match (fst package) n with
       | `Ok (name,version) ->
         let pin = pin_target kind target in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         ignore @@
         OpamClient.PIN.pin st name ?version ~edit ~action pin;
         `Ok ()
       | `Error e -> `Error (false, e))
    | command, params -> bad_subcommand commands ("pin", command, params)
  in
  Term.ret
    Term.(const pin
          $global_options $build_options
          $kind $edit $no_act $dev_repo $print_short_flag $command $params),
  term_info "pin" ~doc ~man

(* SOURCE *)
let source_doc = "Get the source of an opam package."
let source =
  let doc = source_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Downloads the source for a given package to a local directory \
        for development, bug fixing or documentation purposes."
  ] in
  let atom =
    Arg.(required & pos 0 (some atom) None & info ~docv:"PACKAGE" []
           ~doc:"A package name with an optional version constraint")
  in
  let dev_repo =
    mk_flag ["dev-repo"]
      "Get the latest version-controlled source rather than the \
       release archive" in
  let pin =
    mk_flag ["pin"]
      "Pin the package to the downloaded source (see `opam pin')." in
  let dir =
    mk_opt ["dir"] "DIR" "The directory where to put the source."
      Arg.(some dirname) None in
  let source global_options atom dev_repo pin dir =
    apply_global_options global_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    (* Fixme: this needs a write lock, because it uses the routines that
       download to opam's shared switch cache.
       (it's needed anyway when --pin is used) *)
    OpamSwitchState.with_ `Lock_write gt @@ fun t ->
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
    let open OpamFilename in
    if exists_dir dir then
      OpamConsole.error_and_exit `Bad_arguments
        "Directory %s already exists. Please remove it or use a different one \
         (see option `--dir')"
        (Dir.to_string dir);
    let opam = OpamSwitchState.opam t nv in
    if dev_repo then (
      match OpamFile.OPAM.dev_repo opam with
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
               (OpamPackage.to_string nv) dir []
               [url])
        with
        | Not_available u ->
          OpamConsole.error_and_exit `Sync_error "%s is not available" u
        | Result _ | Up_to_date _ ->
          OpamConsole.formatted_msg
            "Successfully fetched %s development repo to ./%s/\n"
            (OpamPackage.name_to_string nv) (OpamPackage.name_to_string nv)
    ) else (
      let job =
        let open OpamProcess.Job.Op in
        OpamUpdate.download_package_source t nv dir @@+ function
        | Some (Not_available s) ->
          OpamConsole.error_and_exit `Sync_error "Download failed: %s" s
        | None | Some (Result () | Up_to_date ()) ->
          OpamAction.prepare_package_source t nv dir @@| function
          | None ->
            OpamConsole.formatted_msg "Successfully extracted to %s\n"
              (Dir.to_string dir)
          | Some e ->
            OpamConsole.warning "Some errors extracting to %s: %s\n"
              (Dir.to_string dir) (Printexc.to_string e)
      in
      OpamProcess.Job.run job;
      if OpamPinned.find_opam_file_in_source nv.name dir = None
      then
        let f =
          if OpamFilename.exists_dir Op.(dir / "opam")
          then OpamFile.make Op.(dir / "opam" // "opam")
          else OpamFile.make Op.(dir // "opam")
        in
        OpamFile.OPAM.write f
          (OpamFile.OPAM.with_substs [] @@
           OpamFile.OPAM.with_patches [] @@
           opam)
    );
    if pin then
      let backend =
        if dev_repo then match OpamFile.OPAM.dev_repo opam with
          | Some {OpamUrl.backend = #OpamUrl.version_control as kind; _} -> kind
          | _ -> `rsync
        else `rsync
      in
      let target =
        `Source (OpamUrl.parse ~backend
                   ("file://"^OpamFilename.Dir.to_string dir))
      in
      ignore @@ OpamClient.PIN.pin t nv.name ~version:nv.version target
  in
  Term.(const source
        $global_options $atom $dev_repo $pin $dir),
  term_info "source" ~doc ~man

(* LINT *)
let lint_doc = "Checks and validate package description ('opam') files."
let lint =
  let doc = lint_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Given an $(i,opam) file, performs several quality checks on it and \
        outputs recommendations, warnings or errors on stderr."
  ] in
  let files =
    Arg.(value & pos_all (existing_filename_dirname_or_dash) [] &
         info ~docv:"FILES" []
           ~doc:"Name of the opam files to check, or directory containing \
                 them. Current directory if unspecified")
  in
  let normalise =
    mk_flag ["normalise"]
      "Output a normalised version of the opam file to stdout"
  in
  let short =
    mk_flag ["short";"s"]
      "Only print the warning/error numbers, space-separated, if any"
  in
  let warnings =
    mk_opt ["warnings";"W"] "WARNS"
      "Select the warnings to show or hide. $(i,WARNS) should be a \
       concatenation of $(b,+N), $(b,-N), $(b,+N..M), $(b,-N..M) to \
       respectively enable or disable warning or error number $(b,N) or \
       all warnings with numbers between $(b,N) and $(b,M) inclusive.\n\
       All warnings are enabled by default, unless $(i,WARNS) starts with \
       $(b,+), which disables all but the selected ones."
      warn_selector []
  in
  let package =
    mk_opt ["package"] "PKG"
      "Lint the current definition of the given package instead of specifying \
       an opam file directly."
      Arg.(some package) None
  in
  let lint global_options files package normalise short warnings_sel =
    apply_global_options global_options;
    let opam_files_in_dir d =
      match OpamPinned.files_in_source d with
      | [] ->
        OpamConsole.warning "No opam files found in %s"
          (OpamFilename.Dir.to_string d);
        []
      | l ->
        List.map (fun (_name,f) -> Some f) l
    in
    let files = match files, package with
      | [], None -> (* Lookup in cwd if nothing was specified *)
        opam_files_in_dir (OpamFilename.cwd ())
      | files, None ->
        List.map (function
            | None -> [None] (* this means '-' was specified for stdin *)
            | Some (OpamFilename.D d) ->
              opam_files_in_dir d
            | Some (OpamFilename.F f) ->
              [Some (OpamFile.make f)])
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
           match OpamPinned.orig_opam_file opam with
           | None -> raise Not_found
           | some -> [some]
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
    let err =
      List.fold_left (fun err opam_f ->
          try
            let warnings,opam =
              match opam_f with
              | Some f -> OpamFileTools.lint_file f
              | None ->
                OpamFileTools.lint_channel
                  (OpamFile.make (OpamFilename.of_string "-")) stdin
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
                   (OpamStd.Option.to_string
                      (fun f -> OpamFile.to_string f ^ ": ")
                      opam_f)
                   (OpamConsole.colorise `green "Passed."))
            else
              msg "%s%s\n%s\n"
                (OpamStd.Option.to_string (fun f -> OpamFile.to_string f ^ ": ")
                   opam_f)
                (if failed then OpamConsole.colorise `red "Errors."
                 else OpamConsole.colorise `yellow "Warnings.")
                (OpamFileTools.warns_to_string warnings);
            if normalise then
              OpamStd.Option.iter (OpamFile.OPAM.write_to_channel stdout) opam;
            err || failed
          with
          | Parsing.Parse_error
          | OpamLexer.Error _
          | OpamPp.Bad_format _ ->
            msg "File format error\n";
            true)
        false files
    in
    if err then OpamStd.Sys.exit_because `False
  in
  Term.(const lint $global_options $files $package $normalise $short $warnings),
  term_info "lint" ~doc ~man

(* CLEAN *)
let clean_doc = "Cleans up opam caches"
let clean =
  let doc = clean_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Cleans up opam caches, reclaiming some disk space. If no options are \
        specified, the default is $(b,--logs --download-cache \
        --switch-cleanup)."
  ] in
  let dry_run =
    mk_flag ["dry-run"]
      "Print the removal commands, but don't execute them"
  in
  let download_cache =
    mk_flag ["c"; "download-cache"]
      "Clear the cache of downloaded files (\\$OPAMROOT/download-cache), as \
       well as the obsolete \\$OPAMROOT/archives, if that exists."
  in
  let repos =
    mk_flag ["unused-repositories"]
      "Clear any configured repository that is not used by any switch nor the \
       default."
  in
  let repo_cache =
    mk_flag ["r"; "repo-cache"]
      "Clear the repository cache. It will be rebuilt by the next opam command \
       that needs it."
  in
  let logs =
    mk_flag ["logs"] "Clear the logs directory."
  in
  let switch =
    mk_flag ["s";"switch-cleanup"]
      "Run the switch-specific cleanup: clears backups, build dirs, \
       uncompressed package sources of non-dev packages, local metadata of \
       previously pinned packages, etc."
  in
  let all_switches =
    mk_flag ["a"; "all-switches"]
      "Run the switch cleanup commands in all switches. Implies $(b,--switch-cleanup)"
  in
  let clean global_options dry_run
      download_cache repos repo_cache logs switch all_switches =
    apply_global_options global_options;
    let logs, download_cache, switch =
      if logs || download_cache || repos || repo_cache || switch || all_switches
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
        OpamFilename.cleandir d
    in
    let rmdir d =
      if dry_run then
        OpamConsole.msg "rm -rf \"%s\"\n"
          (OpamFilename.Dir.to_string d)
      else
        OpamFilename.rmdir d
    in
    let switches =
      if all_switches then OpamGlobalState.switches gt
      else if switch then match OpamStateConfig.get_switch_opt () with
        | Some s -> [s]
        | None -> []
      else []
    in
    if switches <> [] then
      (OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
       List.iter (fun sw ->
           OpamSwitchState.with_ `Lock_write gt ~rt ~switch:sw @@ fun st ->
           OpamConsole.msg "Cleaning up switch %s\n" (OpamSwitch.to_string sw);
           cleandir (OpamPath.Switch.backup_dir root sw);
           cleandir (OpamPath.Switch.build_dir root sw);
           cleandir (OpamPath.Switch.remove_dir root sw);
           let pinning_overlay_dirs =
             List.map
               (fun nv -> OpamPath.Switch.Overlay.package root sw nv.name)
               (OpamPackage.Set.elements st.pinned)
           in
           List.iter (fun d ->
               if not (List.mem d pinning_overlay_dirs) then rmdir d)
             (OpamFilename.dirs (OpamPath.Switch.Overlay.dir root sw));
           let installed_dev_dirs =
             OpamPackage.Set.fold (fun nv acc ->
                 if OpamSwitchState.is_dev_package st nv
                 then OpamSwitchState.source_dir st nv :: acc
                 else acc)
               st.installed []
           in
           OpamFilename.dirs (OpamPath.Switch.sources_dir root sw) |>
           List.iter (fun d ->
               if not (List.mem d installed_dev_dirs) then rmdir d))
         switches);
    if repos then
      (OpamFilename.with_flock `Lock_write (OpamPath.repos_lock gt.root)
       @@ fun _lock ->
       let repos_config =
         OpamFile.Repos_config.safe_read (OpamPath.repos_config gt.root)
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
               OpamFile.Switch_config.safe_read
                 (OpamPath.Switch.switch_config root sw)
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
           rmdir (OpamRepositoryPath.create root r))
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
       rmdir (OpamPath.archives_dir root);
       cleandir (OpamRepositoryPath.download_cache root));
    if logs then
      (OpamConsole.msg "Clearing logs\n";
       cleandir (OpamPath.log root))
  in
  Term.(const clean $global_options $dry_run $download_cache $repos $repo_cache
        $logs $switch $all_switches),
  term_info "clean" ~doc ~man

(* HELP *)
let help =
  let doc = "Display help about opam and opam commands." in
  let man = [
    `S "DESCRIPTION";
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
      | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
      | `Ok t -> `Help (man_format, Some t) in

  Term.(ret (const help $Term.man_format $Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default =
  let doc = "source-based package management" in
  let man = [
    `S "DESCRIPTION";
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
    `S "COMMANDS";
    `S "COMMAND ALIASES";
  ] @  help_sections
  in
  let usage global_options =
    apply_global_options global_options;
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
  Term.(const usage $global_options),
  Term.info "opam"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:global_option_section
    ~doc
    ~man

let admin =
  let doc = "Use 'opam admin' instead (abbreviation not supported)" in
  Term.(ret (const (`Error (true, doc)))),
  Term.info "admin" ~doc:OpamAdminCommand.admin_command_doc
    ~man:[`S "SYNOPSIS";
          `P doc]

let commands = [
  init;
  list ();
  make_command_alias (list ~force_search:true ()) ~options:" --search" "search";
  show; make_command_alias show "info";
  install;
  remove; make_command_alias remove "uninstall";
  reinstall;
  update; upgrade;
  config; var; exec; env;
  repository; make_command_alias repository "remote";
  switch;
  pin (); make_command_alias (pin ~unpin_only:true ()) ~options:" remove" "unpin";
  source;
  lint;
  clean;
  admin;
  help;
]
