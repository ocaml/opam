(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
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

let apply_global_options (options,_self_upgrade) = apply_global_options options
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
    `P "The $(b,init) command creates a fresh client state. This initializes OPAM \
        configuration in $(i,~/.opam) (or the given $(b,--root)) and configures \
        the initial remote package repository.";
    `P "Once the fresh client has been created, OPAM will ask the user if he wants \
        $(i,~/.profile) (or $(i,~/.zshrc), etc. depending on his shell) and $(i,~/.ocamlinit) \
        to be updated. \
        If $(b,--auto-setup) is used, OPAM will modify the configuration files automatically, \
        without asking the user. If $(b,--no-setup) is used, OPAM will *NOT* modify \
        anything outside of $(i,~/.opam).";
    `P "Additional repositories can be added later by using the $(b,opam repository) command.";
    `P "The state of repositories can be synchronized by using $(b,opam update).";
    `P "The user and global configuration files can be setup later by using $(b,opam config setup).";
  ] in
  let compiler =
    mk_opt ["c";"compiler"] "VERSION" "Set the compiler to install"
      Arg.string "system"
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
    Arg.(value & pos ~rev:true 0 string
           (OpamUrl.to_string OpamRepositoryBackend.default_url) & doc) in
  let no_setup   = mk_flag ["n";"no-setup"]   "Do not update the global and user configuration options to setup OPAM." in
  let auto_setup = mk_flag ["a";"auto-setup"] "Automatically setup all the global and user configuration options for OPAM." in
  let init global_options
      build_options repo_kind repo_name repo_url
      no_setup auto_setup shell dot_profile_o
      compiler no_compiler =
    apply_global_options global_options;
    apply_build_options build_options;
    let repo_priority = 0 in
    let repo_url = OpamUrl.parse ?backend:repo_kind repo_url in
    let repository = {
      repo_root = OpamRepositoryPath.create (OpamStateConfig.(!r.root_dir)) repo_name;
      repo_name; repo_url; repo_priority } in
    let update_config =
      if no_setup then `no
      else if auto_setup then `yes
      else `ask in
    let dot_profile = init_dot_profile shell dot_profile_o in
    let gt, rt = OpamClient.init repository shell dot_profile update_config in
    if not no_compiler &&
       OpamFile.Config.installed_switches gt.config = [] then
      let packages =
        OpamSwitchCommand.guess_compiler_package rt compiler
      in
      OpamSwitchCommand.switch gt ~packages (OpamSwitch.of_string compiler)
  in
  Term.(pure init
    $global_options $build_options $repo_kind_flag $repo_name $repo_url
    $no_setup $auto_setup $shell_opt $dot_profile_flag $compiler $no_compiler),
  term_info "init" ~doc ~man

(* LIST *)
let list_doc = "Display the list of available packages."
let list =
  let doc = list_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of installed packages when called \
        without argument, or the list of available packages matching the \
        given pattern.";
    `P "Unless the $(b,--short) switch is used, the output format displays one \
        package per line, and each line contains the name of the package, the \
        installed version or -- if the package is not installed, and a short \
        description. In color mode, root packages (eg. manually installed) are \
        underlined.";
    `P "The full description can be obtained by doing $(b,opam show <package>). \
        You can search through the package descriptions using the $(b,opam search) \
        command."
  ] in
  let all =
    mk_flag ["a";"all"]
      "List all the packages which can be installed on the system. This is \
       the default when a query argument is supplied." in
  let installed =
    mk_flag ["i";"installed"]
      "List installed packages only. This is the the default when no argument \
       is supplied. With `--resolve', means \"compute a solution from the \
       currently installed packages\" instead." in
  let unavailable =
    mk_flag ["A";"unavailable"]
      "List all packages, even those which can't be installed on the system" in
  let sort = mk_flag ["sort";"S"] "Sort the packages in dependency order." in
  let depends_on =
    let doc = "List only packages that depend on one of (comma-separated) $(docv)." in
    Arg.(value & opt (list atom) [] & info ~doc ~docv:"PACKAGES" ["depends-on"])
  in
  let required_by =
    let doc = "List only the dependencies of (comma-separated) $(docv)." in
    Arg.(value & opt (list atom) [] & info ~doc ~docv:"PACKAGES" ["required-by"])
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
       `--unavailable` further makes the solution independent from the \
       currently pinned packages, architecture, and compiler version. \
       The combination with `--depopts' is not supported."
    in
    Arg.(value & opt (list atom) [] & info ~doc ~docv:"PACKAGES" ["resolve"])
  in
  let recursive =
    mk_flag ["recursive"]
      "With `--depends-on' and `--required-by', display all transitive \
       dependencies rather than just direct dependencies." in
  let depopts =
    mk_flag ["depopts"] "Include optional dependencies in dependency requests."
  in
  let dev =
    mk_flag ["dev"] "Include development packages in dependencies."
  in
  let depexts =
    mk_opt ["e";"external"] "TAGS" ~vopt:(Some [])
      "Instead of displaying the packages, display their external dependencies \
       that are associated with any subset of the given $(i,TAGS) (OS, \
       distribution, etc.). \
       Common tags include `debian', `x86', `osx', `homebrew', `source'... \
       Without $(i,TAGS), display the tags and all associated external \
       dependencies. \
       Rather than using this directly, you should probably head for the \
       `depext' plugin, that can infer your system's tags and handle \
       the system installations. Run `opam depext'."
      Arg.(some & list string) None in
  let list global_options print_short all installed
      installed_roots unavailable sort
      depends_on required_by resolve recursive depopts depexts dev
      packages =
    apply_global_options global_options;
    let filter =
      match unavailable, all, installed, installed_roots with
      | true,  false, false, false -> Some `all
      | false, true,  false, false -> Some `installable
      | false, false, true,  false -> Some `installed
      | false, false, _,     true  -> Some `roots
      | false, false, false, false ->
        if depends_on = [] && required_by = [] && resolve = [] && packages = []
        then Some `installed else Some `installable
      | _ -> None
    in
    let order = if sort then `depends else `normal in
    match filter, (depends_on, required_by, resolve) with
    | Some filter, (depends, [], [] | [], depends, [] | [], [], depends) ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamListCommand.list gt
        ~print_short ~filter ~order
        ~exact_name:true ~case_sensitive:false
        ~depends ~reverse_depends:(depends_on <> [])
        ~resolve_depends:(resolve <> [])
        ~recursive_depends:recursive
        ~depopts ?depexts ~dev
        packages;
      `Ok ()
    | None, _ ->
      `Error (true, "Conflicting filters: only one of --all, --installed and \
                     --installed-roots may be given at a time")
    | _ ->
      (* That would be fairly doable with a change of interface if needed *)
      `Error (true, "Sorry, only one of --depends-on, --required-by and \
                     --resolve are allowed at a time")
  in
  Term.ret
    Term.(pure list $global_options
          $print_short_flag $all $installed $installed_roots_flag
          $unavailable $sort
          $depends_on $required_by $resolve $recursive $depopts $depexts $dev
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
        ~doc:"Only display these fields. You can specify multiple fields by \
              separating them with commas. In addition, fields from the raw \
              package definition can be printed by suffixing their names with \
              ':' character."
        ["f";"field"] in
    Arg.(value & opt (list string) [] & doc) in
  let raw =
    mk_flag ["raw"] "Print the raw opam file for this package" in
  let where =
    mk_flag ["where"]
      "Print the location of the opam file used for this package" in
  let file =
    let doc =
      Arg.info
        ~docv:"FILE"
        ~doc:"Get package information from the given FILE instead of from \
              known packages. This implies $(b,--raw) unless $(b,--fields) \
              is used. Only raw opam-file fields can be queried."
        ["file"] in
    Arg.(value & opt (some existing_filename_or_dash) None & doc) in
  let pkg_info global_options fields raw where file packages =
    apply_global_options global_options;
    match file, packages with
    | None, [] ->
      `Error (true, "required argument PACKAGES is missing")
    | Some _, _::_ ->
      `Error (true,
              "arguments PACKAGES and `--files' can't be specified together")
    | None, pkgs ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamListCommand.info gt ~fields ~raw_opam:raw ~where pkgs;
      `Ok ()
    | Some f, [] ->
      let opam = match f with
        | Some f -> OpamFile.OPAM.read (OpamFile.make f)
        | None -> OpamFile.OPAM.read_from_channel stdin
      in
      if where then
        (OpamConsole.msg "%s\n"
           (match f with Some f -> OpamFilename.(Dir.to_string (dirname f))
                       | None -> ".");
         `Ok ())
      else
      let opam_content_list = OpamFile.OPAM.to_list opam in
      let get_field f =
        let f = OpamStd.String.remove_suffix ~suffix:":" f in
        try OpamListCommand.mini_field_printer (List.assoc f opam_content_list)
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
          (pure pkg_info $global_options $fields $raw $where $file $atom_list)),
  term_info "show" ~doc ~man


(* CONFIG *)
let config_doc = "Display configuration options for packages."
let config =
  let doc = config_doc in
  let commands = [
    "env", `env, [],
    "Return the environment variables PATH, MANPATH, OCAML_TOPLEVEL_PATH \
     and CAML_LD_LIBRARY_PATH according to the currently selected \
     compiler. The output of this command is meant to be evaluated by a \
     shell, for example by doing $(b,eval `opam config env`).";
    "setup", `setup, [],
    "Configure global and user parameters for OPAM. Use $(b, opam config setup) \
     to display more options. Use $(b,--list) to display the current configuration \
     options. You can use this command to automatically update: (i) user-configuration \
     files such as ~/.profile and ~/.ocamlinit; and (ii) global-configuration files \
     controlling which shell scripts are loaded on startup, such as auto-completion. \
     These configuration options can be updated using: $(b,opam config setup --global) \
     to setup the global configuration files stored in $(b,~/.opam/opam-init/) and \
     $(b,opam config setup --user) to setup the user ones. \
     To modify both the global and user configuration, use $(b,opam config setup --all).";
    "exec", `exec, ["[--] COMMAND"; "[ARG]..."],
    "Execute $(i,COMMAND) with the correct environment variables. \
     This command can be used to cross-compile between switches using \
     $(b,opam config exec --switch=SWITCH -- COMMAND ARG1 ... ARGn). \
     Opam expansion takes place in command and args. If no switch is \
     present on the command line or in the OPAMSWITCH environment \
     variable, OPAMSWITCH is not set in $(i,COMMAND)'s environment.";
    "var", `var, ["VAR"],
    "Return the value associated with variable $(i,VAR). Package variables can \
     be accessed with the syntax $(i,pkg:var).";
    "list", `list, ["[PACKAGE]..."],
    "Without argument, prints a documented list of all available variables. With \
     $(i,PACKAGE), lists all the variables available for these packages. Use \
     $(i,-) to include global configuration variables for this switch.";
    "set", `set, ["VAR";"VALUE"],
    "Set the given global opam variable for the current switch. Warning: \
     changing a configured path will not move any files! This command does \
     not perform any variable expansion.";
    "unset", `unset, ["VAR"],
    "Unset the given global opam variable for the current switch. Warning: \
     unsetting built-in configuration variables can cause problems!";
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
    "Outputs the current available package universe in PEF format.";
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
  let ocamlinit_doc   = "Modify ~/.ocamlinit to make `#use \"topfind\"` works in the toplevel." in
  let profile_doc     = "Modify ~/.profile (or ~/.zshrc, etc., depending on your shell) to \
                         setup an OPAM-friendly environment when starting a new shell." in
  let no_complete_doc = "Do not load the auto-completion scripts in the environment." in
  let no_eval_doc     = "Do not install `opam-switch-eval` to switch & eval using a single command." in
  let dot_profile_doc = "Select which configuration file to update (default is ~/.profile)." in
  let list_doc        = "List the current configuration." in
  let sexp_doc        = "Display environment variables as an s-expression" in
  let inplace_path_doc= "When updating the PATH variable, replace any pre-existing OPAM path \
                         in-place rather than putting the new path in front. This means programs \
                         installed in OPAM that were shadowed will remain so after \
                         $(b,opam config env)" in
  let profile         = mk_flag ["profile"]        profile_doc in
  let ocamlinit       = mk_flag ["ocamlinit"]      ocamlinit_doc in
  let no_complete     = mk_flag ["no-complete"]    no_complete_doc in
  let no_switch_eval  = mk_flag ["no-switch-eval"] no_eval_doc in
  let all             = mk_flag ["a";"all"]        all_doc in
  let user            = mk_flag ["u";"user"]       user_doc in
  let global          = mk_flag ["g";"global"]     global_doc in
  let list            = mk_flag ["l";"list"]       list_doc in
  let sexp            = mk_flag ["sexp"]           sexp_doc in
  let inplace_path    = mk_flag ["inplace-path"]   inplace_path_doc in

  let config global_options
      command shell sexp inplace_path
      dot_profile_o list all global user
      profile ocamlinit no_complete no_switch_eval
      params =
    apply_global_options global_options;
    match command, params with
    | Some `env, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      `Ok (OpamConfigCommand.env st
             ~csh:(shell=`csh) ~sexp ~fish:(shell=`fish) ~inplace_path)
    | Some `setup, [] ->
      let user        = all || user in
      let global      = all || global in
      let profile     = user  || profile in
      let ocamlinit   = user  || ocamlinit in
      let completion    = global && not no_complete in
      let switch_eval = global && not no_switch_eval in
      let dot_profile = init_dot_profile shell dot_profile_o in
      if list then
        `Ok (OpamConfigCommand.setup_list shell dot_profile)
      else if profile || ocamlinit || completion || switch_eval then
        let dot_profile = if profile then Some dot_profile else None in
        OpamGlobalState.with_ `Lock_write @@ fun gt ->
        `Ok (OpamConfigCommand.setup gt
               ?dot_profile ~ocamlinit ~switch_eval ~completion ~shell
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
               \    --ocamlinit          %s\n\
               \    --profile            %s\n\
               \    --dot-profile FILE   %s\n\
                \n\
                Global configuration\n\
               \    -g,--global          %s\n\
               \    --no-complete        %s\n\
               \    --no-switch-eval     %s\n\n"
               list_doc all_doc
               user_doc ocamlinit_doc profile_doc dot_profile_doc
               global_doc no_complete_doc no_eval_doc)
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
    | Some `pef, _params ->
      failwith "!X todo"
(*
      let opam_state = OpamSwitchState.load_full_compat "config-universe"
          (OpamStateConfig.get_switch ()) in
      let dump oc = OpamState.dump_state opam_state oc in
      (match params with
       | [] -> `Ok (dump stdout)
       | [file] -> let oc = open_out file in dump oc; close_out oc; `Ok ()
       | _ -> bad_subcommand commands ("config", command, params))
*)
    | Some `cudf, params ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchState.with_ `Lock_none gt @@ fun opam_state ->
      let opam_univ = OpamSwitchState.universe opam_state Depends in
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
                   let dft = dft || url = OpamRepositoryBackend.default_url in
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
          $profile $ocamlinit $no_complete $no_switch_eval
          $params)
  ),
  term_info "config" ~doc ~man

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
  let install
      global_options build_options add_to_roots deps_only upgrade atoms =
    apply_global_options global_options;
    apply_build_options build_options;
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_write gt @@ fun st ->
    ignore @@ OpamClient.install st atoms add_to_roots ~deps_only ~upgrade
  in
  Term.(pure install $global_options $build_options
        $add_to_roots $deps_only $upgrade $nonempty_atom_list),
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
  let update global_options jobs names repos_only dev_only upgrade =
    apply_global_options global_options;
    OpamStateConfig.update
      ?jobs:OpamStd.Option.Op.(jobs >>| fun j -> lazy j)
      ();
    OpamClientConfig.update ();
    OpamGlobalState.with_ `Lock_write @@ fun gt ->
    let st =
      OpamClient.update gt
        ~repos_only:(repos_only && not dev_only)
        ~dev_only:(dev_only && not repos_only)
        ~no_stats:true
        names
    in
    if upgrade then
      OpamSwitchState.with_write_lock st @@ fun st ->
      OpamConsole.msg "\n";
      ignore @@ OpamClient.upgrade st []
    else
      OpamConsole.msg "Now run 'opam upgrade' to apply any package updates.\n"
  in
  Term.(pure update $global_options $jobs_flag $name_list
        $repos_only $dev_only $upgrade),
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
  let commands = [
    "add", `add, ["NAME"; "ADDRESS"],
    "Add the repository at address $(i,ADDRESS) to the list of repositories \
     used by OPAM, under $(i,NAME). It will have highest priority unless \
     $(b,--priority) is specified.";
    "remove", `remove, ["NAME"],
    "Remove the repository $(i,NAME) from the list of configured repositories";
    "list", `list, [],
    "List all repositories used by OPAM.";
    "priority", `priority, ["NAME"; "RANK"],
    "Change the rank of repository named $(i,NAME) to $(i,RANK). 1 is first, \
     negative is from the end with -1 being last. When a package with the same \
     name and version appears in two configured repositories, the definition \
     from the repository with the lower rank is used.";
    "set-url", `set_url, ["NAME"; "ADDRESS"],
    "Change the URL associated with $(i,NAME)";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command is used to manage OPAM repositories. To synchronize OPAM \
        with the last versions of the packages available in remote \
        repositories, use $(b,opam update).";
  ] @ mk_subdoc ~defaults:["","list"] commands in

  let command, params = mk_subcommands commands in
  let priority =
    mk_opt ["p";"priority"]
      "RANK" "In case of conflicting package definitions from multiple \
              repositories, the definition from the lowest-ranked repository \
              is used. $(i,RANK) is 1 for first, etc., and -1 for last, -2 for \
              before last, etc."
      Arg.(some int) None in

  let repository global_options command kind priority short params =
    apply_global_options global_options;
    match command, params with
    | Some `add, [name;url] ->
      let name = OpamRepositoryName.of_string name in
      let url = OpamUrl.parse ?backend:kind url in
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
      let gt, rt = OpamRepositoryCommand.add gt rt name url ~priority in
      let _gt = OpamGlobalState.unlock gt in
      let _rt =
        OpamUpdate.repositories rt [OpamRepositoryState.get_repo rt name]
      in
      `Ok ()
    | (None | Some `list), [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      `Ok (OpamRepositoryCommand.list rt ~short)
    | Some `priority, [name; p] ->
      let name = OpamRepositoryName.of_string name in
      let priority =
        try int_of_string p
        with Failure _ -> OpamConsole.error_and_exit "%s is not an integer." p in
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let _gt = OpamRepositoryCommand.priority gt name ~priority in
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
    | Some `remove, [name] ->
      let name = OpamRepositoryName.of_string name in
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
      let _gt, _rt = OpamRepositoryCommand.remove gt rt name in
      `Ok ()
    | command, params -> bad_subcommand commands ("repository", command, params)
  in
  Term.ret
    Term.(pure repository $global_options $command $repo_kind_flag $priority
          $print_short_flag $params),
  term_info "repository" ~doc ~man

(* SWITCH *)
let switch_doc = "Manage multiple installation of compilers."
let switch =
  let doc = switch_doc in
  let commands = [
    "install", `install, ["SWITCH"],
    "Install the given compiler. The command fails if the switch is \
     already installed (e.g. it will not transparently switch to the \
     installed compiler switch, as with $(b,set)).";
    "set", `set, ["SWITCH"],
    "Set the currently active switch, installing it if needed.";
    "remove", `remove, ["SWITCH"], "Remove the given compiler.";
    "export", `export, ["FILE"],
    "Save the current switch state to a file.";
    "import", `import, ["FILE"], "Import a saved switch state.";
    "reinstall", `reinstall, ["SWITCH"],
    "Reinstall the given compiler switch. This will also reinstall all \
     packages.";
    "list", `list, [],
    "List compilers. \
     By default, lists installed and `standard' compilers. Use `--all' to get \
     the list of all installable compilers.\n\
     The first column displays the switch name (if any), the second one \
     the switch state (C = current, I = installed, -- = not installed), \
     the third one the compiler name and the last one the compiler \
     description. To switch to an already installed compiler alias (with \
     state = I), use $(b,opam switch <name>). If you want to use a new \
     compiler <comp>, use $(b,opam switch <comp>): this will download, \
     compile and create a fresh and independent environment where new \
     packages can be installed. If you want to create a new compiler alias \
     (for instance because you already have this compiler version installed), \
     use $(b,opam switch <name> --alias-of <comp>). In case \
     <name> and <comp> are the same, this is equivalent to $(b,opam switch \
     <comp>).";
    "show", `current, [], "Show the current compiler.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command allows one to switch between different compiler versions, \
        installing the compiler if $(b,opam switch) is used to switch to that \
        compiler for the first time. The different compiler versions are \
        totally independent from each other, meaning that OPAM maintains a \
        separate state (e.g. list of installed packages...) for each.";
    `P "See the documentation of $(b,opam switch list) to see the compilers \
        which are available, and how to switch or to install a new one."
  ] @ mk_subdoc ~defaults:["","list";"SWITCH","set"] commands in

  let command, params = mk_subcommands_with_default commands in
  let alias_of =
    mk_opt ["A";"alias-of"]
      "COMP" "The name of the compiler description which will be aliased."
      Arg.(some string) None in
  let no_switch =
    mk_flag ["no-switch"]
      "Only install the compiler switch, without switching to it. If the compiler \
       switch is already installed, then do nothing." in
  let installed =
    mk_flag ["i";"installed"] "List installed compiler switches only." in
  let all =
    mk_flag ["a";"all"]
      "List all the compilers which can be installed on the system." in
  let packages =
    mk_opt ["packages"] "PKGS"
      "When installing a switch, explicitely define the set of packages to set \
       as the switch base (a.k.a. \"compiler\")."
      Arg.(some (list atom)) None in
  let empty =
    mk_flag ["empty"]
      "Allow creating an empty (without compiler) switch." in

  let switch global_options
      build_options command alias_of print_short installed all
      no_switch packages empty params =
    apply_global_options global_options;
    apply_build_options build_options;
    let packages =
      match packages, empty with
      | None, true -> Some []
      | packages, _ -> packages
    in
    let guess_compiler_package gt s =
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      OpamSwitchCommand.guess_compiler_package rt s
    in
    let compiler_packages gt switch =
      match packages, alias_of with
      | Some pkgs, None -> pkgs
      | None, Some al -> guess_compiler_package gt al
      | None, None -> guess_compiler_package gt switch
      | Some _, Some _ ->
        OpamConsole.error_and_exit
          "Options --alias-of and --packages are incompatible"
    in
    match command, params with
    | None      , []
    | Some `list, [] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchCommand.list gt ~print_short ~installed ~all;
      `Ok ()
    | Some `install, [switch] ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamSwitchCommand.install gt
        ~update_config:(not no_switch)
        ~packages:(compiler_packages gt switch)
        (OpamSwitch.of_string switch);
      `Ok ()
    | Some `export, [filename] ->
      OpamSwitchCommand.export
        (if filename = "-" then None
         else Some (OpamFile.make (OpamFilename.of_string filename)));
      `Ok ()
    | Some `import, [filename] ->
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchCommand.import gt
        (OpamStateConfig.get_switch ())
        (if filename = "-" then None
         else Some (OpamFile.make (OpamFilename.of_string filename)));
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
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamSwitchCommand.reinstall gt
        (OpamSwitch.of_string switch);
      `Ok ()
    | Some `current, [] ->
      OpamSwitchCommand.show ();
      `Ok ()
    | Some `set, [switch]
    | Some `default switch, [] ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      let switch_name = OpamSwitch.of_string switch in
      let is_installed =
        List.mem switch_name (OpamFile.Config.installed_switches gt.config)
      in
      let packages =
        if is_installed then [] else compiler_packages gt switch
      in
      if no_switch then
        if is_installed then
          OpamConsole.msg "Switch already installed, nothing to do.\n"
        else
          OpamSwitchCommand.install gt ~update_config:false
            ~packages switch_name
      else
        OpamSwitchCommand.switch gt ~packages switch_name;
      `Ok ()
    | command, params -> bad_subcommand commands ("switch", command, params)
  in
  Term.(ret (pure switch
             $global_options $build_options $command
             $alias_of $print_short_flag
             $installed $all $no_switch $packages $empty $params)),
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
    "remove", `remove, ["NAMES"],
    "Unpins packages $(b,NAMES), restoring their definition from the \
     repository, if any.";
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
  let guess_name path =
    let open OpamFilename.Op in
    if not (OpamFilename.exists_dir path) then raise Not_found else
    let opamf = OpamFile.make (path / "opam" // "opam") in
    let opamf : OpamFile.OPAM.t OpamFile.t =
      if OpamFile.exists opamf then opamf else OpamFile.make (path // "opam")
    in
    if OpamFile.exists opamf then
      try match OpamFile.OPAM.(name_opt (read opamf)) with
        | Some name -> name
        | None -> raise Not_found
      with e -> OpamStd.Exn.fatal e; raise Not_found
    else
    match
      Array.fold_left (fun acc f ->
          if OpamStd.String.ends_with ~suffix:".opam" f then
            if acc = None then
              Some (OpamStd.String.remove_suffix ~suffix:".opam" f)
            else raise Not_found (* multiple .opam files *)
          else acc)
        None (Sys.readdir (OpamFilename.Dir.to_string path))
    with
    | Some base -> OpamPackage.Name.of_string base
    | None -> raise Not_found
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
    | Some `remove, names ->
      let names,errs =
        List.fold_left (fun (names,errs) n -> match (fst package_name) n with
            | `Ok name -> name::names,errs
            | `Error e -> names,e::errs)
          ([],[]) names
      in
      (match errs with
       | [] ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         ignore @@ OpamClient.PIN.unpin st ~action names;
         `Ok ()
       | es -> `Error (false, String.concat "\n" es))
    | Some `edit, [n]  ->
      (match (fst package_name) n with
       | `Ok name ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
         ignore @@ OpamClient.PIN.edit st ~action name;
         `Ok ()
       | `Error e -> `Error (false, e))
    | Some `add, [nv] | Some `default nv, [] when dev_repo ->
      (match (fst package) nv with
       | `Ok (name,version) ->
         OpamGlobalState.with_ `Lock_none @@ fun gt ->
         OpamSwitchState.with_ `Lock_write gt @@ fun st ->
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
         (try
            let name = guess_name (OpamFilename.Dir.of_string arg) in
            OpamGlobalState.with_ `Lock_none @@ fun gt ->
            OpamSwitchState.with_ `Lock_write gt @@ fun st ->
            ignore @@ OpamClient.PIN.pin st name ~edit ~action (`Source url);
            `Ok ()
          with Not_found ->
            `Error (false, Printf.sprintf
                      "No valid package description found at path %s.\n\
                       Please supply a package name \
                       (e.g. `opam pin add NAME PATH')"
                      (OpamUrl.base_url url)))
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
               (OpamRepository.pull_url nv dir None [url]))
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
        (try OpamAction.extract_package t s nv with Failure _ -> ());
        move_dir
          ~src:(OpamPath.Switch.build t.switch_global.root t.switch nv)
          ~dst:dir;
        OpamConsole.formatted_msg "Successfully extracted to %s\n"
          (Dir.to_string dir);
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
  let file = Arg.(value & pos 0 existing_filename_dirname_or_dash
                    (Some (OpamFilename.D (OpamFilename.cwd ()))) &
                  info ~docv:"FILE" []
                    ~doc:"Name of the opam file to check, or directory \
                          containing it. Current directory if unspecified")
  in
  let normalise =
    mk_flag ["normalise"]
      "Output a normalised version of the opam file to stdout"
  in
  let short =
    mk_flag ["short";"s"]
      "Only print the warning/error numbers, space-separated, if any"
  in
  let lint global_options file normalise short =
    apply_global_options global_options;
    let opam_f = match file with
      | Some (OpamFilename.D d) ->
        Some (OpamFile.make OpamFilename.Op.(d // "opam"))
      | Some (OpamFilename.F f) -> Some (OpamFile.make f)
      | None -> None
    in
    try
      let warnings,opam =
        match opam_f with
        | Some f -> OpamFileTools.lint_file f
        | None ->
          OpamFileTools.lint_channel
            (OpamFile.make (OpamFilename.of_string "-")) stdin
      in
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
        OpamConsole.msg "%s found%s:\n%s\n"
          (if failed then "Errors" else "Warnings")
          (OpamStd.Option.to_string (fun f -> " in "^OpamFile.to_string f)
             opam_f)
          (OpamFileTools.warns_to_string warnings);
      if normalise then
        OpamStd.Option.iter (OpamFile.OPAM.write_to_channel stdout) opam;
      if failed then OpamStd.Sys.exit 1
    with
    | Parsing.Parse_error
    | Lexer_error _
    | OpamFormat.Bad_format _ ->
      OpamConsole.msg "File format error\n";
      OpamStd.Sys.exit 1
  in
  Term.(pure lint $global_options $file $normalise $short),
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
  let doc = "source-based OCaml package management" in
  let man = [
    `S "DESCRIPTION";
    `P "OPAM is a package manager for OCaml. It uses the powerful mancoosi \
        tools to handle dependencies, including support for version \
        constraints, optional dependencies, and conflict management.";
    `P "It has support for different remote repositories such as HTTP, rsync, git, \
        darcs and mercurial. It handles multiple OCaml versions concurrently, and is \
        flexible enough to allow you to use your own repositories and packages \
        in addition to the central ones it provides.";
    `P "Use either $(b,opam <command> --help) or $(b,opam help <command>) \
        for more information on a specific command.";
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
  ] in
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
  config;
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
          if OpamPackage.Set.is_empty candidates then
            (* !X FIXME: here we loaded a full switch state and discard it, to go on
               if the command was a valid _prefix_ of an opam command. We should
               assume prefixes can't be plugins! Or at least not auto-installable
               ones *)
            ()
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
