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
      OpamConsole.warning "Obsolete OPAM self-upgrade package v.%s found, \
                           not using it (current system version is %s)."
        (OpamVersion.to_string update_version)
        (OpamVersion.to_string OpamVersion.current)
    else (
      if OpamVersion.git () <> None then
        OpamConsole.warning "Using OPAM self-upgrade to %s while the system \
                             OPAM is a development version (%s)"
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
      "OPAM will replace itself with a newer binary found \
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
    if not options.safe_mode && Unix.getuid () = 0 then
      OpamConsole.warning "Running as root is not recommended";
    options, self_upgrade_status
  in
  Term.(pure self_upgrade $ no_self_upgrade $ global_options)

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
let init_doc = "Initialize OPAM state."
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
        $(b,global-variables:)."
  ] in
  let compiler =
    mk_opt ["c";"compiler"] "VERSION" "Set the compiler to install"
      Arg.(some string) None
  in
  let no_compiler =
    mk_flag ["bare"]
      "Initialise the OPAM state, but don't setup any compiler switch yet."
  in
  let repo_name =
    let doc = Arg.info ~docv:"NAME" ~doc:"Name of the repository." [] in
    Arg.(value & pos ~rev:true 1 repository_name OpamRepositoryName.default & doc) in
  let repo_url =
    let doc = Arg.info ~docv:"ADDRESS" ~doc:"Address of the repository." [] in
    Arg.(value & pos ~rev:true 0 (some string) None & doc) in
  let no_setup   = mk_flag ["n";"no-setup"]   "Do not update the global and user configuration options to setup OPAM." in
  let auto_setup = mk_flag ["a";"auto-setup"] "Automatically setup all the global and user configuration options for OPAM." in
  let config_file =
    mk_opt ["config"] "FILE"
      "Use the given init config file (default is ~/.opamrc or /etc/opamrc, \
       if present)"
      Arg.(some & OpamArg.filename) None
  in
  let no_config_file =
    mk_flag ["default-config"]
      "Use the built-in default configuration, bypassing any opamrc file found \
       on the system"
  in
  let bypass_checks =
    mk_flag ["bypass-checks"]
      "Skip checks on required or recommended tools, \
       and assume everything is fine"
  in
  let init global_options
      build_options repo_kind repo_name repo_url
      no_setup auto_setup shell dot_profile_o
      compiler no_compiler config_file no_config_file bypass_checks =
    apply_global_options global_options;
    apply_build_options build_options;
    let config_file =
      if no_config_file then None else
        match config_file with
        | Some f -> Some (OpamFile.make f)
        | None -> OpamPath.init_config_file ()
    in
    let init_config = match config_file with
      | None -> None
      | Some cf ->
         try
           let r = OpamFile.InitConfig.read cf in
           OpamConsole.note "Will configure defaults from %s"
             (OpamFile.to_string cf);
           Some r
         with e ->
           OpamConsole.error
             "Error in configuration file, fix it or use '--default-config' or \
             '--config FILE':";
           OpamConsole.errmsg "%s" (Printexc.to_string e);
           OpamStd.Sys.exit 10
    in
    let repo =
      OpamStd.Option.map (fun url ->
        let repo_url = OpamUrl.parse ?backend:repo_kind url in
        let repo_root =
          OpamRepositoryPath.create (OpamStateConfig.(!r.root_dir))
            repo_name
        in
        { repo_root; repo_name; repo_url; repo_priority = 0 })
        repo_url
    in
    let update_config =
      if no_setup then `no
      else if auto_setup then `yes
      else `ask in
    let dot_profile = init_dot_profile shell dot_profile_o in
    let gt, rt, default_compiler =
      OpamClient.init
        ?init_config ?repo ~bypass_checks
        shell dot_profile update_config
    in
    if not no_compiler &&
      OpamFile.Config.installed_switches gt.config = [] then
      match compiler with
      | Some comp ->
         let packages =
           OpamSwitchCommand.guess_compiler_package rt comp
         in
         OpamSwitchCommand.switch_with_autoinstall
           gt ~packages (OpamSwitch.of_string comp)
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
            OpamSwitchCommand.switch_with_autoinstall
              gt ~packages (OpamSwitch.of_string "default")
         |> ignore
         | None ->
            OpamConsole.note
              "No compiler selected, and no available default switch found: \
             no switch has been created.\n\
             Use 'opam switch create <compiler>' to get started."
  in
  Term.(pure init
          $global_options $build_options $repo_kind_flag $repo_name $repo_url
          $no_setup $auto_setup $shell_opt $dot_profile_flag $compiler $no_compiler
          $config_file $no_config_file $bypass_checks),
  term_info "init" ~doc ~man

(* LIST *)
let list_doc = "Display the list of available packages."
let list =
  let doc = list_doc in
  let selection_docs = "PACKAGE SELECTION" in
  let display_docs = "OUTPUT FORMAT" in
  let man = [
    `S "DESCRIPTION";
    `P "List selections of opam packages.";
    `P "Without argument, the command displays the list of currently installed \
        packages. With pattern arguments, lists all available packages \
        matching one of the patterns.";
    `P ("See section $(b,"^selection_docs^") for all the ways to select the \
         packages to be displayed, and section $(b,"^display_docs^") to \
         customise the output format.");
    `P "For a more detailed description of packages, see $(b,opam show). For \
        extended search capabilities within the packages' metadata, see \
        $(b,opam search)."
  ] in
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
                (this calls the solver and may be more costly)";
        OpamListCommand.Compiler, info ~docs ["base"]
          ~doc:"List only the immutable base of the current switch (i.e. \
                compiler packages)";
        OpamListCommand.Pinned, info ~docs ["pinned"]
          ~doc:"List only the pinned packages";
      ])
  in
  let depends_on =
    let doc =
      "List only packages that depend on one of (comma-separated) $(docv)."
    in
    Arg.(value & opt (list atom) [] &
         info ~doc ~docs:selection_docs ~docv:"PACKAGES" ["depends-on"])
  in
  let required_by =
    let doc = "List only the dependencies of (comma-separated) $(docv)." in
    Arg.(value & opt (list atom) [] &
         info ~doc ~docs:selection_docs ~docv:"PACKAGES" ["required-by"])
  in
  let resolve =
    let doc =
      "Restrict to a solution to install (comma-separated) $(docv), $(i,i.e.) \
       a consistent set of packages including those. This is subtly different \
       from `--required-by --recursive`, which is more predictable and can't \
       fail, but lists all dependencies independently without ensuring \
       consistency. \
       Without `--installed`, the answer is self-contained and independent of \
       the current installation. With `--installed', it's computed from the \
       set of currently installed packages. \
       `--no-switch` further makes the solution independent from the \
       currently pinned packages, architecture, and compiler version. \
       The combination with `--depopts' is not supported."
    in
    Arg.(value & opt (list atom) [] &
         info ~doc ~docs:selection_docs ~docv:"PACKAGES" ["resolve"])
  in
  let recursive =
    mk_flag ["recursive"] ~section:selection_docs
      "With `--depends-on' and `--required-by', display all transitive \
       dependencies rather than just direct dependencies." in
  let depopts =
    mk_flag ["depopts"]  ~section:selection_docs
      "Include optional dependencies in dependency requests."
  in
  let nobuild =
    mk_flag ["nobuild"]  ~section:selection_docs
      "Exclude build dependencies (they are included by default)."
  in
  let dev =
    mk_flag ["dev"]  ~section:selection_docs
      "Include development packages in dependencies."
  in
  let doc_flag =
    mk_flag ["doc"] ~section:selection_docs
      "Include doc-only dependencies."
  in
  let test =
    mk_flag ["t";"test"] ~section:selection_docs
      "Include test-only dependencies."
  in
  let repos =
    mk_opt ["repos"] "REPOS" ~section:selection_docs
      "Include only packages that took their origin from one of the given \
       repositories (unless $(i,no-switch) is also specified, this excludes \
       pinned packages)."
      Arg.(some & list & repository_name) None
  in
  let field_match =
    mk_opt_all ["field-match"] "FIELD:PATTERN" ~section:selection_docs
      "Filter packages with a match for $(i,PATTERN) on the given $(i,FIELD)"
      Arg.(pair ~sep:':' string string)
  in
  let has_flag =
    mk_opt_all ["has-flag"] "FLAG" ~section:selection_docs
      ("Only include packages which have the given flag set. Package flags are \
        one of: "^
       (OpamStd.List.concat_map " "
          (Printf.sprintf "$(b,%s)" @* string_of_pkg_flag)
          all_package_flags))
      ((fun s -> match pkg_flag_of_string s with
          | Pkgflag_Unknown s ->
            `Error ("Invalid package flag "^s^", must be one of "^
                    OpamStd.List.concat_map " " string_of_pkg_flag
                      all_package_flags)
          | f -> `Ok f),
       fun fmt flag ->
         Format.pp_print_string fmt (string_of_pkg_flag flag))
  in
  let has_tag =
    mk_opt_all ["has-tag"] "TAG" ~section:selection_docs
      "Only includes packages which have the given tag set"
      Arg.string
  in
  let no_switch =
    mk_flag ["no-switch"] ~section:selection_docs
      "List what is available from the repositories, without consideration for \
       the current (or any other) switch (installed or pinned packages, etc.)"
  in
  let depexts =
    mk_opt ["e";"external"] "TAGS" ~section:display_docs
      "Instead of displaying the packages, display their external dependencies \
       that are associated with any subset of the given $(i,TAGS) (OS, \
       distribution, etc.). This excludes other display options. \
       Common tags include `debian', `x86', `osx', `homebrew', `source'... \
       Without $(i,TAGS), display the tags and all associated external \
       dependencies. \
       Rather than using this directly, you should probably head for the \
       `depext' plugin, that can infer your system's tags and handle \
       the system installations. Run `opam depext'."
      Arg.(some & list string) None in
  let print_short =
    mk_flag ["short";"s"] ~section:display_docs
      "Don't print a header, and sets the default columns to $(b,name) only. \
       If you need package versions included, use $(b,--columns=package) \
       instead"
  in
  let sort =
    mk_flag ["sort";"S"] ~section:display_docs
      "Sort the packages in dependency order (i.e. an order in which they \
       could be individually installed.)"
  in
  let columns =
    mk_opt ["columns"] "COLUMNS" ~section:display_docs
      (Printf.sprintf "Select the columns to display among: %s.\n\
                       The default is $(b,name) when $(i,--short) is present \
                       and %s otherwise."
         (OpamStd.List.concat_map ", " (fun (_,f) -> Printf.sprintf "$(b,%s)" f)
            OpamListCommand.field_names)
         (OpamStd.List.concat_map ", "
            (fun f -> Printf.sprintf "$(b,%s)" (OpamListCommand.string_of_field f))
            OpamListCommand.default_list_format))
      Arg.(some & list opamlist_column) None
  in
  let all_versions = mk_flag ["all-versions"] ~section:selection_docs
      "Normally, when multiple versions of a package match, only one is shown \
       in the output (the installed one, the pinned-to one, or, failing that, \
       the highest one available or the highest one). This flag disables this \
       behaviour and shows all matching versions. This also changes the \
       default display format to include package versions instead of just \
       package names (including when --short is set). This is automatically \
       turned on when a single non-pattern package name is provided on the \
       command-line."
  in
  let normalise = mk_flag ["normalise"] ~section:display_docs
      "Print the values of opam fields normalised"
  in
  let wrap = mk_flag ["wrap"] ~section:display_docs
      "Wrap long lines, the default being to truncate when displaying on a \
       terminal, or to keep as is otherwise"
  in
  let separator =
    Arg.(value & opt string " " & info ["separator"]
           ~docv:"STRING" ~docs:display_docs
           ~doc:"Set the column-separator string")
  in
  let list global_options state_selector field_match
      depends_on required_by resolve recursive depopts no_switch
      depexts nobuild dev doc test repos has_flag has_tag
      print_short sort columns all_versions normalise wrap separator
      packages =
    apply_global_options global_options;
    let no_switch =
      no_switch || OpamStateConfig.(!r.current_switch) = None
    in
    let all_versions =
      all_versions ||
      state_selector = [] && match packages with
      | [single] ->
        (try ignore (OpamPackage.Name.of_string single); true
         with Failure _ -> false)
      | _ -> false
    in
    let state_selector =
      if state_selector = [] then
        if no_switch then Empty
        else if
          depends_on = [] && required_by = [] && resolve = [] &&
          packages = [] && field_match = [] && has_flag = [] && has_tag = []
        then Atom OpamListCommand.Installed
        else Or (Atom OpamListCommand.Installed,
                 Atom OpamListCommand.Available)
      else OpamFormula.ands (List.map (fun x -> Atom x) state_selector)
    in
    let dependency_toggles = {
      OpamListCommand.
      recursive; depopts; build = not nobuild; test; doc; dev
    } in
    let pattern_toggles ?(exact=true) field = {
      OpamListCommand.
      exact;
      case_sensitive = false;
      fields = [field];
      glob = true;
      ext_fields = false;
    } in
    let filter =
      OpamFormula.ands
        (state_selector ::
         List.map (fun x -> Atom x)
           ((match depends_on with [] -> [] | deps ->
                [OpamListCommand.Depends_on (dependency_toggles, deps)]) @
            (match required_by with [] -> [] | rdeps ->
                [OpamListCommand.Required_by (dependency_toggles, rdeps)]) @
            (match resolve with [] -> [] | deps ->
                [OpamListCommand.Solution (dependency_toggles, deps)]) @
            (if no_switch then [] else
             match repos with None -> [] | Some repos ->
               [OpamListCommand.From_repository repos]) @
            (List.map (fun (field,patt) ->
                 OpamListCommand.Pattern
                   (pattern_toggles ~exact:false field, patt))
                field_match) @
            (List.map (fun flag -> OpamListCommand.Flag flag) has_flag) @
            (List.map (fun tag -> OpamListCommand.Tag tag) has_tag)) @
         [OpamFormula.ors
            (List.map (fun patt ->
                 match OpamStd.String.cut_at patt '.' with
                 | None ->
                   Atom (OpamListCommand.Pattern (pattern_toggles "name", patt))
                 | Some (name, version) ->
                   OpamFormula.ands
                     [Atom (OpamListCommand.Pattern
                              (pattern_toggles "name", name));
                      Atom (OpamListCommand.Pattern
                              (pattern_toggles "version", version))])
                packages)])
    in
    let format =
      match columns with
      | Some c -> c
      | None ->
        let cols =
          if print_short then [OpamListCommand.Name]
          else OpamListCommand.default_list_format
        in
        if all_versions then
          List.map (function
              | OpamListCommand.Name -> OpamListCommand.Package
              | c -> c)
            cols
        else cols
    in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    let st =
      let rt = OpamRepositoryState.load `Lock_none gt in
      if no_switch then OpamSwitchState.load_virtual ?repos_list:repos gt rt
      else OpamSwitchState.load `Lock_none gt rt (OpamStateConfig.get_switch ())
    in

    if not print_short && filter <> OpamFormula.Empty then
      OpamConsole.msg "# Packages matching: %s\n"
        (OpamListCommand.string_of_formula filter);
    let all = OpamPackage.Set.union st.packages st.installed in
    let results =
      OpamListCommand.filter ~base:all st filter
    in
    match depexts with
    | None ->
      OpamListCommand.display st
        ~format
        ~dependency_order:sort
        ~header:(not print_short)
        ~all_versions
        ~wrap:(if wrap then `Wrap "\\ " else `Truncate)
        ~separator
        ~normalise
        results
    | Some tags_list ->
      OpamListCommand.print_depexts st results tags_list
  in
  Term.(pure list $global_options $state_selector $field_match
        $depends_on $required_by $resolve $recursive $depopts
        $no_switch $depexts $nobuild $dev $doc_flag $test $repos
        $has_flag $has_tag
        $print_short $sort $columns $all_versions
        $normalise $wrap $separator
        $pattern_list),
  term_info "list" ~doc ~man

(* SEARCH *)
let search =
  let doc = "Search into the package list." in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of available packages that match one of \
        the package patterns specified as arguments.";
    `P "Unless the $(b,--short) flag is used, the output format is the same as the \
        $(b,opam list) command. It displays one package per line, and each line \
        contains the name of the package, the installed version or -- if the package \
        is not installed, and a short description.";
    `P "The full description can be obtained by doing $(b,opam show <package>).";
  ] in
  let installed =
    mk_flag ["i";"installed"] "Search among installed packages only." in
  let case_sensitive =
    mk_flag ["c";"case-sensitive"] "Force the search in case sensitive mode." in
  let owns_file =
    let doc =
      "Finds installed packages responsible for installing the given file"
    in
    Arg.(value & opt (some OpamArg.filename) None & info ~doc ["owns-file"])
  in
  let search global_options print_short installed installed_roots
      case_sensitive owns_file pkgs =
    apply_global_options global_options;
    match owns_file with
    | None ->
      let filter = match installed, installed_roots with
        | _, true -> `roots
        | true, _ -> `installed
        | _       -> `all in
      let order = `normal in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamListCommand.list gt ~print_short ~filter ~order
        ~exact_name:false ~case_sensitive pkgs;
      `Ok ();
    | Some file ->
      if installed || installed_roots || case_sensitive then
        `Error (true, "options conflicting with --owns-file")
      else
        OpamGlobalState.with_ `Lock_none @@ fun gt ->
        let root = OpamStateConfig.(!r.root_dir) in
        let switch =
          try
            List.find (fun sw ->
                OpamFilename.remove_prefix (OpamPath.Switch.root root sw) file
                <> OpamFilename.to_string file)
              (OpamFile.Config.installed_switches gt.config)
          with Not_found ->
            OpamConsole.error_and_exit
              "The specified file does not seem to belong to an opam switch of \
               the current opam root (%s)"
              (OpamFilename.Dir.to_string root)
        in
        let rel_name =
          OpamFilename.remove_prefix (OpamPath.Switch.root root switch) file
        in
        let matching_change_files =
          List.filter (fun change_f ->
              OpamFilename.check_suffix change_f ".changes" &&
              let changes =
                OpamFile.Changes.safe_read (OpamFile.make change_f)
              in
              OpamStd.String.Map.exists
                (fun f -> function
                   | OpamDirTrack.Removed -> false
                   | _ -> rel_name = f)
                changes)
            (OpamFilename.files (OpamPath.Switch.install_dir root switch))
        in
        List.iter (fun f ->
            OpamConsole.msg "%s\n"
              OpamFilename.(Base.to_string (basename (chop_extension f))))
          matching_change_files;
        `Ok ()
  in
  Term.ret @@
  Term.(pure search $global_options
    $print_short_flag $installed $installed_roots_flag $case_sensitive
    $owns_file
    $pattern_list),
  term_info "search" ~doc ~man

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
        ~doc:"Get package information from the given FILE instead of from \
              known packages. This implies $(b,--raw) unless $(b,--fields) \
              is used. Only raw opam-file fields can be queried."
        ["file"] in
    Arg.(value & opt (some existing_filename_or_dash) None & doc) in
  let normalise = mk_flag ["normalise"]
      "Print the values of opam fields normalised (no newlines, no implicit \
       brackets)"
  in
  let pkg_info global_options fields show_empty raw where
      list_files file normalise packages =
    apply_global_options global_options;
    match file, packages with
    | None, [] ->
      `Error (true, "required argument PACKAGES is missing")
    | Some _, _::_ ->
      `Error (true,
              "arguments PACKAGES and `--files' can't be specified together")
    | None, pkgs ->
      let fields, show_empty =
        if list_files then
          fields @ [OpamListCommand.(string_of_field Installed_files)],
          show_empty
        else fields, show_empty || fields <> []
      in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamListCommand.info gt
        ~fields ~raw_opam:raw ~where ~normalise ~show_empty pkgs;
      `Ok ()
    | Some f, [] ->
      let opam = match f with
        | Some f -> OpamFile.OPAM.read (OpamFile.make f)
        | None -> OpamFile.OPAM.read_from_channel stdin
      in
      OpamFile.OPAM.print_errors opam;
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
          (pure pkg_info $global_options $fields $show_empty $raw $where $list_files
           $file $normalise $atom_list)),
  term_info "show" ~doc ~man


(* CONFIG *)
let config_doc = "Display configuration options for packages."
let config =
  let doc = config_doc in
  let commands = [
    "env", `env, [],
    "Returns the bindings for the environment variables set in the current \
     switch, e.g. PATH, in a format intended to be evaluated by a shell. With \
     $(i,-v), add comments documenting the reason or package of origin for \
     each binding. This is most usefully used as $(b,eval `opam config env`) \
     to have further shell commands be evaluated in the proper opam context. \
     Can also be accessed through $(b,opam env).";
    "revert-env", `revert_env, [],
    "Reverts environment changes made by opam, e.g. $(b,eval `opam config \
     revert-env`) undoes what $(b,eval `opam config env`) did.";
    "setup", `setup, [],
    "Configure global and user parameters for OPAM. Use $(b, opam config \
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
     anyvariable expansion.";
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
    `P "This command uses OPAM state to output information on how to use \
        installed libraries, update the $(b,PATH), and substitute \
        variables used in OPAM packages.";
    `P "Apart from $(b,opam config env), most of these commands are used \
        by OPAM internally, and are of limited interest for the casual \
        user.";
  ] @ mk_subdoc commands in

  let command, params = mk_subcommands commands in
  let all_doc         = "Enable all the global and user configuration options." in
  let global_doc      = "Enable all the global configuration options." in
  let user_doc        = "Enable all the user configuration options." in
  let profile_doc     = "Modify ~/.profile (or ~/.zshrc, etc., depending on your shell) to \
                         setup an OPAM-friendly environment when starting a new shell." in
  let no_complete_doc = "Do not load the auto-completion scripts in the environment." in
  let dot_profile_doc = "Select which configuration file to update (default is ~/.profile)." in
  let list_doc        = "List the current configuration." in
  let sexp_doc        = "Display environment variables as an s-expression" in
  let inplace_path_doc= "When updating the PATH variable, replace any pre-existing OPAM path \
                         in-place rather than putting the new path in front. This means programs \
                         installed in OPAM that were shadowed will remain so after \
                         $(b,opam config env)" in
  let profile         = mk_flag ["profile"]        profile_doc in
  let no_complete     = mk_flag ["no-complete"]    no_complete_doc in
  let all             = mk_flag ["a";"all"]        all_doc in
  let user            = mk_flag ["u";"user"]       user_doc in
  let global          = mk_flag ["g";"global"]     global_doc in
  let list            = mk_flag ["l";"list"]       list_doc in
  let sexp            = mk_flag ["sexp"]           sexp_doc in
  let inplace_path    = mk_flag ["inplace-path"]   inplace_path_doc in

  let config global_options
      command shell sexp inplace_path
      dot_profile_o list all global user
      profile no_complete params =
    apply_global_options global_options;
    match command, params with
    | Some `env, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      if OpamStateConfig.(!r.current_switch) = None then `Ok () else
        OpamSwitchState.with_ `Lock_none gt @@ fun st ->
        `Ok (OpamConfigCommand.env st
               ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish) ~inplace_path)
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
      `Ok (OpamConfigCommand.exec gt ~inplace_path c)
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
          Depends
      in
      let dump oc = OpamSolver.dump_universe opam_univ oc in
      (match params with
       | [] -> `Ok (dump stdout)
       | [file] -> let oc = open_out file in dump oc; close_out oc; `Ok ()
       | _ -> bad_subcommand commands ("config", command, params))
    | Some `report, [] -> (
        let print label fmt = Printf.printf ("# %-15s "^^fmt^^"\n") label in
        Printf.printf "# OPAM config report\n";
        print "opam-version" "%s " (OpamVersion.to_string (OpamVersion.full ()));
        print "self-upgrade" "%s"
          (if self_upgrade_status global_options = `Running then
             OpamFilename.prettify (fst (self_upgrade_exe (OpamStateConfig.(!r.root_dir))))
           else "no");
        print "os" "%s" (OpamStd.Sys.os_string ());
        try
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          OpamSwitchState.with_ `Lock_none gt @@ fun state ->
          let external_solver =
            OpamSolverConfig.external_solver_command
              ~input:"$in" ~output:"$out" ~criteria:"$criteria" in
          print "external-solver" "%s"
            (OpamStd.Option.to_string ~none:"no" (String.concat " ")
               external_solver);
          if external_solver <> None then
            print "criteria" "%s" (OpamSolverConfig.criteria `Default);
          let nprint label n =
            if n <> 0 then [Printf.sprintf "%d (%s)" n label]
            else [] in
          print "jobs" "%d" (Lazy.force OpamStateConfig.(!r.jobs));
          print "repositories" "%s"
            (let repos = state.switch_repos.repositories in
             let has_default, nhttp, nlocal, nvcs =
               OpamRepositoryName.Map.fold
                 (fun _ {repo_url = url; _} (dft, nhttp, nlocal, nvcs) ->
                   let dft = dft || url = OpamInitDefaults.repository_url in
                   match url.OpamUrl.backend with
                   | `http -> dft, nhttp+1, nlocal, nvcs
                   | `rsync -> dft, nhttp, nlocal+1, nvcs
                   | _ -> dft, nhttp, nlocal, nvcs+1)
                 repos (false,0,0,0)
              in
              String.concat ", "
                (Printf.sprintf "%d%s (http)" nhttp
                   (if has_default then "*" else "") ::
                 nprint "local" nlocal @
                 nprint "version-controlled" nvcs)
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
          (* !X fixme: find a way to do thit without the package index
             let index_file =
             OpamFile.to_string (OpamPath.package_index state.switch_global.root)
             in
             let u = Unix.gmtime (Unix.stat index_file).Unix.st_mtime in
             Unix.(print "last-update" "%04d-%02d-%02d %02d:%02d"
                (1900 + u.tm_year) (1 + u.tm_mon) u.tm_mday
                u.tm_hour u.tm_min);
          *)
          `Ok ()
        with e -> print "read-state" "%s" (Printexc.to_string e); `Ok ())
    | command, params -> bad_subcommand commands ("config", command, params)
  in

  Term.ret (
    Term.(pure config
          $global_options $command $shell_opt $sexp
          $inplace_path
          $dot_profile_flag $list $all $global $user
          $profile $no_complete
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
    Term.(pure print_var $global_options $package $varname)
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
  let inplace_path_doc=
    "When updating the PATH variable, replace any \
     pre-existing OPAM path in-place rather than putting \
     the new path in front. This means programs installed \
     in OPAM that were shadowed will remain so after \
     $(b,opam config env)" in
  let inplace_path    = mk_flag ["inplace-path"] inplace_path_doc in
  let exec global_options inplace_path cmd =
    apply_global_options global_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamConfigCommand.exec gt ~inplace_path cmd
  in
  Term.(pure exec $global_options $inplace_path $cmd),
  term_info "exec" ~doc ~man

(* ENV *)
let env_doc = "Executes a command in the proper opam environment"
let env =
  let doc = env_doc in
  let man = [
    `S "DESCRIPTION";
    `P "Returns the bindings for the environment variables set in the current \
        switch, e.g. PATH, in a format intended to be evaluated by a shell. \
        With $(i,-v), add comments documenting the reason or package of origin \
        for each binding. This is most usefully used as $(b,eval `opam env`) \
        to have further shell commands be evaluated in the proper opam \
        context.";
    `P "This is a shortcut, and equivalent to $(b,opam config env).";
  ] in
  let revert =
    mk_flag ["revert"]
      "Output the environment with updates done by opam reverted instead."
  in
  let inplace_path_doc=
    "When updating the PATH variable, replace any \
     pre-existing OPAM path in-place rather than putting \
     the new path in front. This means programs installed \
     in OPAM that were shadowed will remain so after \
     $(b,opam config env)" in
  let inplace_path = mk_flag ["inplace-path"] inplace_path_doc in
  let sexp =
    mk_flag ["sexp"]
      "Display environment variables as an s-expression rather than in shell \
       format"
  in
  let env global_options shell sexp inplace_path revert =
    apply_global_options global_options;
    match revert with
    | false ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      if OpamStateConfig.(!r.current_switch) <> None then
        OpamSwitchState.with_ `Lock_none gt @@ fun st ->
        OpamConfigCommand.env st
          ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish) ~inplace_path
    | true ->
      OpamConfigCommand.print_eval_env
        ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish)
        (OpamEnv.add [] [])
  in
  Term.(pure env $global_options $shell_opt $sexp $inplace_path $revert),
  term_info "env" ~doc ~man

(* INSTALL *)
let install_doc = "Install a list of packages."
let install =
  let doc = install_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command installs one or more packages to the currently selected \
        compiler. To install packages for another compiler, you need to switch \
        compilers using $(b,opam switch). You can remove installed packages with \
        $(b,opam remove), and list installed packages with $(b,opam list -i). \
        See $(b,opam pin) as well to understand how to manage package versions.";
    `P "This command makes OPAM use the dependency solver to compute the \
        transitive closure of dependencies to be installed, and will also handle \
        conflicts. If the dependency solver returns more than one \
        solution, OPAM will arbitrarily select the first one. If dependencies \
        are to be installed, OPAM will confirm if the installation should \
        proceed.";
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
  let upgrade =
    Arg.(value & flag & info ["u";"upgrade"]
           ~doc:"Upgrade the packages if already installed, rather than \
                 ignoring them") in
  let restore =
    Arg.(value & flag & info ["restore"]
           ~doc:"Attempt to restore packages that were marked for installation \
                 but have been removed due to errors") in
  let install
      global_options build_options add_to_roots deps_only upgrade restore
      atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    if atoms = [] && not restore then
      `Error (true, "required argument PACKAGES is missing")
    else
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    let atoms =
      if restore then
        let to_restore = OpamPackage.Set.diff st.installed_roots st.installed in
        if OpamPackage.Set.is_empty to_restore then
          OpamConsole.msg "No packages to restore found\n"
        else
          OpamConsole.msg "Packages to be restored: %s\n"
            (OpamPackage.Name.Set.to_string
               (OpamPackage.names_of_packages to_restore));
        atoms @
        List.map OpamSolution.atom_of_package
          (OpamPackage.Set.elements to_restore)
      else atoms
    in
    if atoms = [] then `Ok () else
      (ignore @@ OpamClient.install st atoms add_to_roots ~deps_only ~upgrade;
       `Ok ())
  in
  Term.ret
    Term.(pure install $global_options $build_options
          $add_to_roots $deps_only $upgrade $restore $atom_list),
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
  ] in
  let autoremove =
    mk_flag ["a";"auto-remove"]
      "Remove all the packages which have not been explicitly installed and \
       which are not necessary anymore. It is possible to prevent the removal of an \
       already-installed package by running $(b,opam install <pkg>). This flag \
       can also be set using the $(b,\\$OPAMAUTOREMOVE) configuration variable." in
  let force =
    mk_flag ["force"]
      "Execute the remove commands of given packages directly, even if they are \
       not considered installed by OPAM." in
  let remove global_options build_options autoremove force atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    ignore @@ OpamClient.remove st ~autoremove ~force atoms
  in
  Term.(pure remove $global_options $build_options $autoremove $force $atom_list),
  term_info "remove" ~doc ~man

(* REINSTALL *)
let reinstall =
  let doc = "Reinstall a list of packages." in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes the given packages and the ones \
        that depend on them, and reinstalls the same versions."
  ] in
  let reinstall global_options build_options atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    ignore @@ OpamClient.reinstall st atoms
  in
  Term.(pure reinstall $global_options $build_options $nonempty_atom_list),
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
  let update global_options jobs names repos_only dev_only all upgrade =
    apply_global_options global_options;
    OpamStateConfig.update
      ?jobs:OpamStd.Option.Op.(jobs >>| fun j -> lazy j)
      ();
    OpamClientConfig.update ();
    OpamGlobalState.with_ `Lock_write @@ fun gt ->
    let rt =
      OpamClient.update gt
        ~repos_only:(repos_only && not dev_only)
        ~dev_only:(dev_only && not repos_only)
        ~all
        names
    in
    if upgrade then
      OpamSwitchState.with_ `Lock_write gt ~rt @@ fun st ->
      OpamConsole.msg "\n";
      ignore @@ OpamClient.upgrade st []
    else
      OpamConsole.msg "Now run 'opam upgrade' to apply any package updates.\n"
  in
  Term.(pure update $global_options $jobs_flag $name_list
        $repos_only $dev_only $all $upgrade),
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
  ] in
  let fixup =
    mk_flag ["fixup"]
      "Recover from a broken state (eg. missing dependencies, two conflicting \
       packages installed together...). This requires that you have an \
       external solver installed (aspcud, cudf-services.irill.org, ...)" in
  let upgrade global_options build_options fixup atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    if fixup then
      if atoms <> [] then
        `Error (true, Printf.sprintf "--fixup doesn't allow extra arguments")
      else
        OpamSwitchState.with_ `Lock_write gt @@ fun st ->
        ignore @@ OpamClient.fixup st;
        `Ok ()
    else
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      ignore @@ OpamClient.upgrade st atoms;
      `Ok ()
  in
  Term.(ret (pure upgrade $global_options $build_options $fixup $atom_list)),
  term_info "upgrade" ~doc ~man

(* REPOSITORY *)
let repository_doc = "Manage OPAM repositories."
let repository =
  let doc = repository_doc in
  let scope_section = "SCOPE SPECIFICATION" in
  let commands = [
    "add", `add, ["NAME"; "[ADDRESS]"],
    "Adds under $(i,NAME) the repository at address $(i,ADDRESS) to the list \
     of configured repositories, if not already registerd, and sets this \
     repository for use in the current switch (or the specified scope). \
     $(i,ADDRESS) is required if the repository name is not already \
     registered, and is otherwise an error if different from the registered \
     address.";
    "remove", `remove, ["NAME..."],
    "Unselects the given repositories so that they will not be used to get \
     package definitions anymore. With $(b,--all), makes opam forget about \
     these repositories completely.";
    "set-repos", `set_repos, ["NAME..."],
    "Explicitely selects the list of repositories to look up package \
     definitions from, in the specified priority order (overriding previous \
     selection and ranks), according to the specified scope.";
    "set-url", `set_url, ["NAME"; "ADDRESS"],
    "Updates the URL associated with a given repository name";
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
    `S scope_section;
    `P "These flags allow to choose what selections are changed by $(b,add), \
        $(b,remove), $(b,set-repos). If no flag in this section is specified \
        the updated selections default to the current switch. Multiple scopes \
        can be selected, e.g. $(b,--this-switch --set-default)."
  ] @ mk_subdoc ~defaults:["","list"] commands in
  let command, params = mk_subcommands commands in
  let scope =
    let scope_info flags doc = Arg.info ~docs:scope_section ~doc flags in
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
        (scope_info ["on-switches"]
           "Act on the selections of the given list of switches")
    in
    let switches =
      Term.(pure (List.map (fun s -> `Switch (OpamSwitch.of_string s)))
            $ Arg.value switches)
    in
    Term.(pure (fun l1 l2 -> match l1@l2 with [] -> [`Current_switch] | l -> l)
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
    let command, rank = match command, params, rank with
      | Some `priority, [rank], 1 ->
        (try Some `add, int_of_string rank
         with Failure _ ->
           OpamConsole.error_and_exit "Invalid rank specification %S" rank)
      | Some `priority, [], rank -> Some `add, rank
      | command, _, rank -> command, rank
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
              OpamConsole.error_and_exit "No switch %s found"
                (OpamSwitch.to_string sw)
            else if List.mem sw acc then acc
            else acc @ [sw]
          | `Current_switch ->
            match OpamStateConfig.(!r.current_switch) with
            | None ->
              OpamConsole.warning "No switch is currently set, maybe you meant \
                                   '--set-default' ?";
              acc
            | Some sw ->
              if List.mem sw acc then acc
              else acc @ [sw])
        [] scope
    in
    match command, params with
    | Some `add, [name; url] ->
      let name = OpamRepositoryName.of_string name in
      let url = OpamUrl.parse ?backend:kind url in
      OpamRepositoryState.with_ `Lock_write gt (fun rt ->
          let rt = OpamRepositoryCommand.add rt name url in
          let _rt =
            OpamUpdate.repositories rt [OpamRepositoryState.get_repo rt name]
          in ());
      let _gt =
        OpamRepositoryCommand.update_selection gt ~global ~switches
          (update_repos name)
      in
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
        `Ok ()
      else
        `Ok ()
    | Some `add, [name] ->
      let name = OpamRepositoryName.of_string name in
      OpamRepositoryState.with_ `Lock_none gt (fun rt ->
          check_for_repos rt [name]
            (OpamConsole.error_and_exit
               "No configured repository '%s' found, you must specify an URL"));
      let _gt =
        OpamRepositoryCommand.update_selection gt ~global ~switches
          (update_repos name)
      in
      `Ok ()
    | Some `set_url, [name; url] ->
      let name = OpamRepositoryName.of_string name in
      let url = OpamUrl.parse ?backend:kind url in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
      let rt = OpamRepositoryCommand.set_url rt name url in
      let _rt =
        OpamUpdate.repositories rt [OpamRepositoryState.get_repo rt name]
      in
      `Ok ()
    | Some `set_repos, names ->
      let names = List.map OpamRepositoryName.of_string names in
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let _gt =
        OpamRepositoryCommand.update_selection gt ~global ~switches
          (fun _ -> names)
      in
      `Ok ()
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
      if not short && scope = [] then
        OpamConsole.note
          "Use '--all' to see all configured repositories independently of \
           what is selected in the current switch";
      OpamRepositoryCommand.list rt ~global ~switches ~short;
      `Ok ()
    | command, params -> bad_subcommand commands ("repository", command, params)
  in
  Term.ret
    Term.(pure repository $global_options $command $repo_kind_flag
          $print_short_flag $scope $rank $params),
  term_info "repository" ~doc ~man

(* SWITCH *)
let switch_doc = "Manage multiple installation of compilers."
let switch =
  let doc = switch_doc in
  let commands = [
    "create", `install, ["SWITCH"; "[COMPILER]"],
    "Create a new switch, and install the given compiler there. $(i,SWITCH) \
     can be a plain name, or a directory, absolute or relative. $(i,COMPILER), \
     if omitted, defaults to $(i,SWITCH) unless $(b,--packages) or \
     $(b,--empty) is specified.";
    "set", `set, ["SWITCH"],
    "Set the currently active switch, among the installed switches.";
    "remove", `remove, ["SWITCH"], "Remove the given switch from disk.";
    "export", `export, ["FILE"],
    "Save the current switch state to a file.";
    "import", `import, ["FILE"],
    "Import a saved switch state. If $(b,--switch) is specified and doesn't \
     point to an existing switch, the switch will be created for the import.";
    "reinstall", `reinstall, ["SWITCH"],
    "Reinstall the given compiler switch and all its packages.";
    "list", `list, [],
    "Lists installed switches.";
    "list-available", `list_available, ["[PATTERN]"],
    "Lists base packages that can be used to create a new switch, i.e. \
     packages with the $(i,compiler) flag set. Only standard versions are \
     shown by default if no pattern is supplied, use $(b,--all) to show all.";
    "show", `current, [], "Prints the name of the current switch.";
    "set-base", `set_compiler, ["NAMES"],
    "Sets the packages forming the immutable base for the selected switch, \
     overriding the current setting. The packages must be installed already.";
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
         $(b,--switch) is specified.");
    `P "$(b,opam switch set) sets the default switch globally, but it is also \
        possible to select a switch in a given shell session, using the \
        environment. For that, use $(i,eval `opam config env \
        --switch=SWITCH`).";
  ] @ mk_subdoc ~defaults:["","list";"SWITCH","set"] commands in

  let command, params = mk_subcommands_with_default commands in
  let no_switch =
    mk_flag ["no-switch"]
      "Don't automatically select newly installed switches" in
  let all =
    mk_flag ["a";"all"]
      "With $(b,list-available), show all available compilers, not only \
       official releases. This is the default if a pattern is supplied" in
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
       instead of the default. You can configure new repositories in advance \
       using $(i,opam repository add --no-select) and then create a switch \
       using them with this option. See $(i,opam repository) for more \
       details. This option also affects $(i,list-available)."
      Arg.(some (list repository_name)) None
  in
  let descr =
    mk_opt ["description"] "STRING"
      "Attach the given description to a switch when creating it. Use the \
       $(i,set-description) subcommand to modify the description of an \
       existing switch."
      Arg.(some string) None
  in
  let switch
      global_options build_options command print_short all
      no_switch packages empty descr repos params =
    apply_global_options global_options;
    apply_build_options build_options;
    let packages =
      match packages, empty with
      | None, true -> Some []
      | packages, _ -> packages
    in
    let guess_compiler_package gt s =
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      OpamSwitchCommand.guess_compiler_package ?repos rt s
    in
    let compiler_packages gt switch compiler_opt =
      match packages, compiler_opt with
      | None, None -> guess_compiler_package gt switch
      | _ ->
        OpamStd.Option.Op.(
          ((compiler_opt >>| guess_compiler_package gt) +! []) @
          packages +! [])
    in
    let param_compiler = function
      | [] -> None
      | [comp] -> Some comp
      | args ->
        OpamConsole.error_and_exit "Invalid extra arguments %s"
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
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      let all_compilers = OpamSwitchCommand.get_compiler_packages ?repos rt in
      let st = OpamSwitchState.load_virtual ?repos_list:repos gt rt in
      let compilers =
        if all || pattlist <> [] then all_compilers else
        let is_main_comp_re =
          Re.(compile (seq [bos; rep1 (alt [digit; char '.']); eos]))
        in
        OpamPackage.Set.filter
          (fun nv ->
             Re.(execp is_main_comp_re (OpamPackage.version_to_string nv)))
          all_compilers
      in
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
      let format = OpamListCommand.([ Name; Version; Synopsis; ]) in
      let order nv1 nv2 =
        if nv1.version = nv2.version
        then OpamPackage.Name.compare nv1.name nv2.name
        else OpamPackage.Version.compare nv1.version nv2.version
      in
      OpamListCommand.display st ~header:true ~format ~dependency_order:false
        ~all_versions:true ~order compilers;
      if all || pattlist <> [] then `Ok () else
      let unshown  = OpamPackage.Set.Op.(all_compilers -- compilers) in
      if not (OpamPackage.Set.is_empty unshown) then
        OpamConsole.msg "# %d more patched or experimental compilers, use \
                         '--all' to show\n"
          (OpamPackage.Set.cardinal unshown);
      `Ok ()
    | Some `install, switch::params ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let _gt, st =
        OpamSwitchCommand.install gt
          ?synopsis:descr ?repos
          ~update_config:(not no_switch)
          ~packages:(compiler_packages gt switch (param_compiler params))
          (OpamSwitch.of_string switch)
      in
      ignore (OpamSwitchState.unlock st);
      `Ok ()
    | Some `export, [filename] ->
      OpamSwitchCommand.export
        (if filename = "-" then None
         else Some (OpamFile.make (OpamFilename.of_string filename)));
      `Ok ()
    | Some `import, [filename] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      let switch = OpamStateConfig.get_switch () in
      let installed_switches = OpamFile.Config.installed_switches gt.config in
      let is_new_switch = not (List.mem switch installed_switches) in
      let gt =
        if is_new_switch then
          OpamGlobalState.with_write_lock gt @@ fun gt ->
          OpamSwitchAction.create_empty_switch gt ?repos switch
        else gt
      in
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
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
    | Some `reinstall, [switch] ->
      let switch = OpamSwitch.of_string switch in
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
         let names = List.map OpamPackage.Name.of_string packages in
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         let _st = OpamSwitchCommand.set_compiler st names in
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
  Term.(ret (pure switch
             $global_options $build_options $command
             $print_short_flag
             $all $no_switch
             $packages $empty $descr $repos $params)),
  term_info "switch" ~doc ~man

(* PIN *)
let pin_doc = "Pin a given package to a specific version or source."
let pin ?(unpin_only=false) () =
  let doc = pin_doc in
  let commands = [
    "list", `list, [], "Lists pinned packages.";
    "add", `add, ["PACKAGE"; "TARGET"],
    "Pins package $(i,PACKAGE) to $(i,TARGET), which may be a version, a path, \
     or a URL. \
     $(i,PACKAGE) can be omitted if $(i,TARGET) is a local path containing a \
     package description with a name. $(i,TARGET) can be replaced by \
     `--dev-repo' if a package by that name is already known. Otherwise, if \
     $(i,TARGET) is $(i,-) or is omitted, the currently defined source package \
     archive is used, if any, and the package is pinned as a virtual package \
     (without any source) otherwise. \
     OPAM will infer the kind of pinning from the format of $(i,TARGET), using \
     $(b,path) pinning by default, unless you use an explicit $(b,--kind) \
     option. \
     Pins to version control systems may target a specific branch or commit \
     using $(b,#branch) e.g. $(b,git://host/me/pkg#testing). When they don't, \
     in the special case of version-controlled pinning to a local path, OPAM \
     will use \"mixed mode\": it will only use version-controlled files, but \
     at their current, on-disk version. \
     If $(i,PACKAGE) is not a known package name, a new package by that name \
     will be locally created. \
     The package version may be specified by using the format \
     $(i,NAME).$(i,VERSION) for $(PACKAGE), in the source opam file, or with \
     $(b,edit).";
    "remove", `remove, ["NAMES...|TARGET"],
    "Unpins packages $(b,NAMES), restoring their definition from the \
     repository, if any. With a $(b,TARGET), unpins everything that is \
     currently pinned to that target.";
    "edit", `edit, ["NAME"],
    "Opens an editor giving you the opportunity to \
     change the package definition that OPAM will locally use for package \
     $(b,NAME), including its version and source URL. \
     The chosen editor is determined from environment variables \
     $(b,OPAM_EDITOR), $(b,VISUAL) or $(b,EDITOR), in order.";
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
    `P "If no target (or $(i,-)) is specified, the package is pinned to its \
        current source archive. The package name can also be omitted if the \
        target is a directory containing a valid package definition (this \
        allows to do e.g. $(i,opam pin add .) from a source directory.";
    `P "If $(i,PACKAGE) has the form $(i,name.version), the pinned package \
        will be considered as version $(i,version) by opam.";
    `P "The default subcommand is $(i,list) if there are no further arguments, \
        and $(i,add) otherwise if unambiguous.";
  ] @ mk_subdoc ~defaults:["","list"] commands in
  let command, params =
    if unpin_only then
      Term.pure (Some `remove),
      Arg.(value & pos_all string [] & Arg.info [])
    else
      mk_subcommands_with_default commands in
  let edit =
    mk_flag ["e";"edit"] "With $(opam pin add), edit the opam file as with \
                          `opam pin edit' after pinning." in
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
  let guess_names path =
    OpamStd.List.filter_map
      (fun (nameopt, f) -> match nameopt with
         | None -> OpamFile.OPAM.(name_opt (read f))
         | some -> some)
      (OpamPinned.files_in_source path)
  in
  let pin_target kind target =
    let looks_like_version_re =
      Re.(compile @@ seq [bos; digit; rep @@ diff any (set "/\\"); eos])
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
    match kind with
    | Some `version -> `Version (OpamPackage.Version.of_string target)
    | Some (#OpamUrl.backend as k) -> `Source (OpamUrl.parse ~backend:k target)
    | Some `none -> `None
    | Some `auto -> auto ()
    | None when OpamClientConfig.(!r.pin_kind_auto) -> auto ()
    | None -> `Source (OpamUrl.parse ~handle_suffix:false target)
  in
  let pin global_options kind edit no_act dev_repo print_short command params =
    apply_global_options global_options;
    let action = not no_act in
    match command, params with
    | Some `list, [] | None, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      OpamClient.PIN.list st ~short:print_short;
      `Ok ()
    | Some `remove, arg ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_write gt @@ fun st ->
      let to_unpin = match arg with
        | [target] ->
          let url = OpamUrl.of_string target in
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
        | _ -> []
      in
      let to_unpin, errs =
        if to_unpin <> [] then to_unpin, [] else
          List.fold_left (fun (names,errs) n -> match (fst package_name) n with
              | `Ok name -> name::names,errs
              | `Error e -> names,e::errs)
            ([],[]) arg
      in
      (match errs with
       | [] ->
         ignore @@ OpamClient.PIN.unpin st ~action to_unpin;
         `Ok ()
       | es -> `Error (false, String.concat "\n" es))
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
       | `Source url when OpamUrl.local_dir url <> None ->
         (* arg is a directory, lookup an opam file *)
         (match guess_names (OpamFilename.Dir.of_string arg) with
          | [] ->
            `Error (false, Printf.sprintf
                      "No valid package description found at path %s.\n\
                       Please supply a package name \
                       (e.g. `opam pin add NAME PATH')"
                      (OpamUrl.base_url url))
          | names ->
            match names with
            | _::_::_ as n when
                not @@
                OpamConsole.confirm
                  "This will pin the following packages: %s. Continue ?"
                  (OpamStd.List.concat_map ", " OpamPackage.Name.to_string n)
              -> OpamStd.Sys.exit 2
            | names ->
              OpamGlobalState.with_ `Lock_none @@ fun gt ->
              OpamSwitchState.with_ `Lock_write gt @@ fun st ->
              let st =
                List.fold_left (fun st name ->
                    OpamPinCommand.source_pin st name ~edit (Some url))
                  st names
              in
              if action then
                (OpamConsole.msg "\n";
                 ignore @@
                 OpamClient.upgrade_t
                   ~strict_upgrade:false ~auto_install:true ~ask:true
                   (List.map (fun n -> n, None) names) st);
              `Ok ())
       | _ ->
         (* arg is a package, guess target *)
         match (fst package) arg with
         | `Ok (name,version) ->
           OpamGlobalState.with_ `Lock_none @@ fun gt ->
           OpamSwitchState.with_ `Lock_write gt @@ fun st ->
           if not (OpamPackage.has_name st.packages name) &&
              command <> Some `add then
             (* Don't do implicit command on non-existing packages *)
             bad_subcommand commands ("pin", command, params)
           else
             (ignore @@ OpamClient.PIN.pin st name ?version ~edit ~action `None;
              `Ok ())
         | `Error _ ->
           if command = Some `add then
             `Error (false, Printf.sprintf
                       "%s is not a valid directory or package name"
                       arg)
           else bad_subcommand commands ("pin", command, params))
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
    Term.(pure pin
          $global_options $kind $edit $no_act $dev_repo $print_short_flag
          $command $params),
  term_info "pin" ~doc ~man

(* SOURCE *)
let source_doc = "Get the source of an OPAM package."
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
        OpamConsole.error_and_exit "No package matching %s found."
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
      OpamConsole.error_and_exit
        "Directory %s already exists. Please remove it or use option `--dir'"
        (Dir.to_string dir);
    let opam = OpamSwitchState.opam t nv in
    if dev_repo then (
      match OpamFile.OPAM.dev_repo opam with
      | None ->
        OpamConsole.error_and_exit
          "Version-controlled repo for %s unknown \
           (\"dev-repo\" field missing from metadata)"
          (OpamPackage.to_string nv)
      | Some url ->
        mkdir dir;
        let text =
          OpamProcess.make_command_text (OpamPackage.name_to_string nv)
            (OpamUrl.string_of_backend (url.OpamUrl.backend))
        in
        match
          OpamProcess.Job.run
            (OpamProcess.Job.with_text text
               (OpamRepository.pull_url nv dir [] [url]))
        with
        | Not_available u -> OpamConsole.error_and_exit "%s is not available" u
        | Result _ | Up_to_date _ ->
          OpamConsole.formatted_msg
            "Successfully fetched %s development repo to ./%s/\n"
            (OpamPackage.name_to_string nv) (OpamPackage.name_to_string nv)
    ) else (
      OpamConsole.formatted_msg "Downloading archive of %s...\n"
        (OpamPackage.to_string nv);
      match OpamProcess.Job.run (OpamAction.download_package t nv) with
      | `Error _ -> OpamConsole.error_and_exit "Download failed"
      | `Successful s ->
        (match OpamProcess.Job.run (OpamAction.extract_package t s nv dir) with
         | None ->
           OpamConsole.formatted_msg "Successfully extracted to %s\n"
             (Dir.to_string dir);
         | Some e ->
           OpamConsole.warning "Some errors extracting to %s: %s\n"
             (Dir.to_string dir) (Printexc.to_string e));
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
  Term.(pure source
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
    mk_opt ["warnings";"w"] "WARNS"
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
           OpamConsole.error_and_exit "No opam file found for %s%s"
             (OpamPackage.Name.to_string (fst pkg))
             (match snd pkg with None -> ""
                               | Some v -> "."^OpamPackage.Version.to_string v))
      | _::_, Some _ ->
        OpamConsole.error_and_exit
          "--package and a file argument are incompatible"
    in
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
                 OpamConsole.msg "%s\n"
                   (OpamStd.List.concat_map " " (fun (n,_,_) -> string_of_int n)
                      warnings))
            else if warnings = [] then
              OpamConsole.msg "%s%s\n"
                (OpamStd.Option.to_string (fun f -> OpamFile.to_string f ^ ": ")
                   opam_f)
                (OpamConsole.colorise `green "Passed.")
            else
              OpamConsole.msg "%s%s\n%s\n"
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
            OpamConsole.msg "File format error\n";
            true)
        false files
    in
    if err then OpamStd.Sys.exit 1
  in
  Term.(pure lint $global_options $files $package $normalise $short $warnings),
  term_info "lint" ~doc ~man


(* HELP *)
let help =
  let doc = "Display help about OPAM and OPAM commands." in
  let man = [
    `S "DESCRIPTION";
    `P "Prints help about OPAM commands.";
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

  Term.(ret (pure help $Term.man_format $Term.choice_names $topic)),
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
    `S "OPTIONS";
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
       \n\
       See 'opam help <command>' for more information on a specific command.\n"
      init_doc list_doc show_doc install_doc remove_doc update_doc
      upgrade_doc config_doc repository_doc switch_doc pin_doc in
  Term.(pure usage $global_options),
  Term.info "opam"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:global_option_section
    ~doc
    ~man

let make_command_alias cmd ?(options="") name =
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s%s)." orig options in
  let man = [
    `S "DESCRIPTION";
    `P (Printf.sprintf "$(b,$(mname) %s) is an alias for $(b,$(mname) %s%s)."
          name orig options);
    `P (Printf.sprintf "See $(b,$(mname) %s --help) for details."
          orig);
    `S "OPTIONS";
  ] @ help_sections
  in
  term,
  Term.info name
    ~docs:"COMMAND ALIASES"
    ~doc ~man

let commands = [
  init;
  list; search;
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
  help;
]

(* Handle git-like plugins *)
let check_and_run_external_commands () =
  let plugin_prefix = "opam-" in
  match Array.to_list Sys.argv with
  | [] | [_] -> ()
  | _ :: name :: args ->
    if
      not (OpamStd.String.starts_with ~prefix:"-" name)
      && List.for_all (fun (_,info) ->
          not (OpamStd.String.starts_with ~prefix:name (Term.name info)))
        commands
    then
      (* No such command, check if there is a matching plugin *)
      let command = plugin_prefix ^ name in
      OpamStd.Config.init ();
      OpamFormatConfig.init ();
      let root_dir = OpamStateConfig.opamroot () in
      let has_init = OpamStateConfig.load_defaults root_dir <> None in
      let env =
        if has_init then (
          OpamStateConfig.init ~root_dir ();
          match OpamStateConfig.(!r.current_switch) with
          | None -> Unix.environment ()
          | Some sw ->
            env_array (OpamEnv.full_with_path ~force_path:false root_dir sw)
        ) else
          Unix.environment ()
      in
      if OpamSystem.command_exists ~env command then
        let argv = Array.of_list (command :: args) in
        raise (OpamStd.Sys.Exec (command, argv, env))
      else if has_init then
        (* Look for a corresponding package *)
        match OpamStateConfig.(!r.current_switch) with
        | None -> ()
        | Some sw ->
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          OpamSwitchState.with_ `Lock_none gt ~switch:sw @@ fun st ->
          let prefixed_name = plugin_prefix ^ name in
          let candidates =
            OpamPackage.packages_of_names
              (Lazy.force st.available_packages)
              (OpamPackage.Name.Set.of_list @@
               List.map OpamPackage.Name.of_string [ prefixed_name; name ])
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
             exit 1)
          else if OpamPackage.Set.is_empty plugins then
            (OpamConsole.error
               "%s is not a known command or plugin (package %s does \
                not have the 'plugin' flag set)."
               name
               (OpamPackage.to_string (OpamPackage.Set.max_elt candidates));
             exit 1)
          else if
            OpamConsole.confirm "OPAM plugin \"%s\" is not installed. \
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
                  None ~deps_only:false ~upgrade:false
              );
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
    match Term.eval_choice ~catch:false default commands with
    | `Error _ -> exit 1
    | _        -> exit 0
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
    let exit_code = ref 1 in
    begin match e with
      | OpamStd.Sys.Exit i ->
        exit_code := i;
        if (OpamConsole.debug ()) && i <> 0 then
          Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e)
      | OpamSystem.Internal_error _ ->
        Printf.eprintf "%s" (Printexc.to_string e)
      | OpamSystem.Process_error result ->
        Printf.eprintf "%s Command %S failed:\n%s\n"
          (OpamConsole.colorise `red "[ERROR]")
          (try List.assoc "command" result.OpamProcess.r_info with
           | Not_found -> "")
          (Printexc.to_string e);
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
      | Sys.Break
      | OpamParallel.Errors (_, (_, Sys.Break)::_, _) ->
        exit_code := 130
      | Sys_error e when e = "Broken pipe" ->
        (* workaround warning 52, this is a fallback (we already handle the
           signal) and there is no way around at the moment *)
        exit_code := 141
      | Failure msg ->
        Printf.eprintf "Fatal error: %s\n" msg;
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
      | _ ->
        Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
        Printf.eprintf "%s" (OpamStd.Exn.pretty_backtrace e);
    end;
    exit !exit_code

let json_out () =
  match OpamStateConfig.(!r.json_out) with
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
  run default commands
