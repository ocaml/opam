(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open Cmdliner

(* Global options *)
type global_options = {
  debug  : bool;
  verbose: bool;
  quiet  : bool;
  switch : string option;
  yes    : bool;
  root   : string;
  no_base_packages: bool;
}

let create_global_options debug verbose quiet switch yes root no_base_packages =
  { debug; verbose; quiet; switch; yes; root; no_base_packages }

let set_global_options o =
  OpamGlobals.debug    := !OpamGlobals.debug || o.debug;
  OpamGlobals.verbose  := (not o.quiet) && (!OpamGlobals.verbose || o.verbose);
  OpamGlobals.switch   := o.switch;
  OpamGlobals.root_dir := OpamSystem.real_path o.root;
  OpamGlobals.yes      := !OpamGlobals.yes || o.yes;
  OpamGlobals.no_base_packages := !OpamGlobals.no_base_packages || o.no_base_packages

(* Build options *)
type build_options = {
  keep_build_dir: bool;
  make          : string option;
  no_checksums  : bool;
  build_test    : bool;
  build_doc     : bool;
  dryrun        : bool;
  cudf_file     : string option;
  fake          : bool;
}

let create_build_options keep_build_dir make no_checksums build_test build_doc dryrun cudf_file fake =
  { keep_build_dir; make; no_checksums; build_test; build_doc; dryrun; cudf_file; fake }

let set_build_options b =
  OpamGlobals.keep_build_dir := !OpamGlobals.keep_build_dir || b.keep_build_dir;
  OpamGlobals.no_checksums   := !OpamGlobals.no_checksums || b.no_checksums;
  OpamGlobals.build_test     := !OpamGlobals.build_test || b.build_test;
  OpamGlobals.build_doc      := !OpamGlobals.build_doc || b.build_doc;
  OpamGlobals.dryrun         := !OpamGlobals.dryrun || b.dryrun;
  OpamGlobals.cudf_file      := b.cudf_file;
  OpamGlobals.fake           := b.fake;
  match b.make with
  | None   -> ()
  | Some s -> OpamGlobals.makecmd := lazy s

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "FURTHER DOCUMENTATION";
  `P (Printf.sprintf "See %s." OpamGlobals.default_repository_address);

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas.gazagnaire@ocamlpro.com>"; `Noblank;
  `P "Frederic Tuong      <tuong@users.gforge.inria.fr>"; `Noblank;
  `P "Vincent Bernardoff  <vb@luminar.eu.org>"; `Noblank;
  `P "Guillem Rieu        <guillem.rieu@ocamlpro.com>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/OCamlPro/opam/issues.";
]

(* Converters *)
let pr_str = Format.pp_print_string

let repository_name =
  let parse str = `Ok (OpamRepositoryName.of_string str) in
  let print ppf name = pr_str ppf (OpamRepositoryName.to_string name) in
  parse, print

let repository_address =
  let parse str = `Ok (OpamRepository.repository_address str) in
  let print ppf address = pr_str ppf (OpamFilename.Dir.to_string address) in
  parse, print

let filename =
  let parse str = `Ok (OpamFilename.of_string str) in
  let print ppf filename = pr_str ppf (OpamFilename.to_string filename) in
  parse, print

let compiler =
  let parse str = `Ok (OpamCompiler.of_string str) in
  let print ppf comp = pr_str ppf (OpamCompiler.to_string comp) in
  parse, print

let package_name =
  let parse str = `Ok (OpamPackage.Name.of_string str) in
  let print ppf pkg = pr_str ppf (OpamPackage.Name.to_string pkg) in
  parse, print

let enum_with_default sl =
  let parse, print = Arg.enum sl in
  let parse s =
    match parse s with
    | `Ok _ as x -> x
    | _ -> `Ok (`default s) in
  parse, print

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

let mk_opt ?section flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt conv default & doc)

let mk_subdoc ?(names="COMMANDS") commands =
  `S names ::
  List.map (fun (cs,_,d) ->
    let bold s = Printf.sprintf "$(b,%s)" s in
    let cmds = String.concat ", " (List.map bold cs) in
    `I (cmds, d)
  ) commands

let mk_subcommands_aux ?(name="COMMAND") my_enum commands default =
  let command =
    let doc =
      Arg.info ~docv:name ~doc:
        (Printf.sprintf
           "Name of the sub-command. See the $(b,%sS) section for more info.%s"
           name
           (match default with
           | None   -> ""
           | Some d -> " " ^ d))
        [] in
    let commands =
      List.fold_left
        (fun acc (cs,f,_) -> List.map (fun c -> c,f) cs @ acc)
        [] commands in
    Arg.(value & pos 0 (some & my_enum commands) None & doc) in
  let params =
    let doc = Arg.info ~doc:"Optional parameters." [] in
    Arg.(value & pos_right 0 string [] & doc) in
  command, params

let mk_subcommands ?name commands =
  mk_subcommands_aux ?name Arg.enum commands None

let mk_subcommands_with_default ?name commands default =
  mk_subcommands_aux ?name enum_with_default commands (Some default)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

let arg_list name doc conv =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(value & pos_all conv [] & doc)

(* Common flags *)
let print_short_flag =
  mk_flag ["s";"short"]
    "Output the names separated by one whitespace instead of using the usual formatting."

let installed_only_flag =
  mk_flag ["i";"installed"] "List installed packages only."

let repo_kind_flag =
  let kinds = [
    (* main kinds *)
    "http" , `http;
    "local", `local;
    "git"  , `git;
    "darcs"  , `darcs;

    (* aliases *)
    "wget" , `http;
    "curl" , `http;
    "rsync", `local;
  ] in
  mk_opt ["k";"kind"]
    "KIND" "Specify the kind of the repository to be set (the main ones \
            are 'http', 'local', 'git' or 'darcs')."
    Arg.(some (enum kinds)) None

let pattern_list =
  arg_list "PATTERNS" "List of package patterns." Arg.string

let package_list =
  arg_list "PACKAGES" "List of package names." package_name

let repository_list =
  arg_list "REPOSITORIES" "List of repository names." repository_name

let param_list =
  arg_list "PARAMS" "List of parameters." Arg.string

(* Options common to all commands *)
let global_options =
  let section = global_option_section in
  let debug =
    mk_flag ~section ["debug"]
      "Print debug message on stdout. \
       This is equivalent to setting $(b,\\$OPAMDEBUG) to a value greater or equal to 2."  in
  let verbose =
    mk_flag ~section ["v";"verbose"]
      "Be more verbose. \
       This is equivalent to setting either $(b,\\$OPAMDEBUG) to a value greater or equal to 1 \
       or $(b,\\$OPAMVERBOSE) to a non-empty string." in
  let quiet =
    mk_flag ~section ["q";"quiet"] "Be quiet when installing a new compiler." in
  let switch =
    mk_opt ~section ["s";"switch"]
      "SWITCH" "Use $(docv) as the current compiler switch. \
                This is equivalent to setting $(b,\\$OPAMSWITCH) to $(i,SWITCH)."
      Arg.(some string) !OpamGlobals.switch in
  let yes =
    mk_flag ~section ["y";"yes"]
      "Disable interactive mode and answer yes \
       to all questions that would otherwise be \
       asked to the user. \
       This is equivalent to setting $(b,\\$OPAMYES) to a non-empty string." in
  let root =
    mk_opt ~section ["r";"root"]
      "ROOT" "Use $(docv) as the current root path. \
              This is equivalent to setting $(b,\\$OPAMROOT) to $(i,ROOT)."
      Arg.string !OpamGlobals.root_dir in
  let no_base_packages =
    mk_flag ~section ["no-base-packages"]
      "Do not install base packages (useful for testing purposes). \
       This is equivalent to setting $(b,\\$OPAMNOBASEPACKAGES) to a non-empty string." in
  Term.(pure create_global_options $debug $verbose $quiet $switch $yes $root $no_base_packages)

(* Options common to all build commands *)
let build_options =
  let keep_build_dir =
    mk_flag ["b";"keep-build-dir"]
      "Keep the build directory. \
       This is equivalent to setting $(b,\\$OPAMKEEPBUILDIR) to a non-empty string." in
  let no_checksums =
    mk_flag ["n";"no-checksums"]
      "Do not verify the checksum of downloaded archives.\
       This is equivalent to setting $(b,\\$OPAMNOCHECKSUMS) to a non-empty string." in
  let build_test =
    mk_flag ["t";"build-test"]
      "Build and $(b,run) the package unit-tests. \
       This is equivalent to setting $(b,\\$OPAMBUILDTEST) to a non-empty string." in
  let build_doc =
    mk_flag ["d";"build-doc"]
      "Build the package documentation. \
       This is equivalent to setting $(b,\\$OPAMBUILDDOC) to a non-empty string." in
  let make =
    mk_opt ["m";"make"] "MAKE"
      "Use $(docv) as the default 'make' command."
      Arg.(some string) None in
  let dryrun =
    mk_flag ["dry-run"]
      "Simply call the solver without actually performing any build/install operations." in
  let cudf_file =
    mk_opt ["cudf"] "FILENAME"
      "Save the CUDF request sent to the solver to $(docv)-<n>.cudf."
      Arg.(some string) None in
  let fake =
    mk_flag ["fake"]
      "WARNING: This option is fo testing purposes only! Using this option without care is \
       the best way to corrupt your current compiler environement. When using this option \
       OPAM will run a dry-run of the solver and then fake the build and install commands" in

  Term.(pure create_build_options $keep_build_dir $make $no_checksums $build_test $build_doc $dryrun $cudf_file $fake)

let guess_repository_kind kind address =
  match kind with
  | None  ->
    let address = OpamFilename.Dir.to_string address in
    if Sys.file_exists address then
      `local
    else if OpamMisc.starts_with ~prefix:"git" address
        || OpamMisc.ends_with ~suffix:"git" address then
      `git
    else
      `http
  | Some k -> k

(* INIT *)
let init_doc = "Initialize OPAM state."
let init =
  let doc = init_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,init) command creates a fresh client state.  This initializes OPAM \
        configuration in $(i,~/.opam) and configures a default package repository.";
    `P "Additional repositories can be added later by using the $(b,opam repository) command.";
    `P "The local cache of a repository state can be updated by using $(b,opam update).";
  ] in
  let cores = mk_opt ["j";"jobs"] "JOBS" "Number of jobs to use when building packages." Arg.int OpamGlobals.default_jobs in
  let compiler =
    mk_opt ["comp"] "VERSION" "Which compiler version to use." compiler OpamCompiler.default in
  let repo_name =
    let doc = Arg.info ~docv:"NAME" ~doc:"Name of the repository." [] in
    Arg.(value & pos ~rev:true 1 repository_name OpamRepositoryName.default & doc) in
  let repo_address =
    let doc = Arg.info ~docv:"ADDRESS" ~doc:"Address of the repository." [] in
    Arg.(value & pos ~rev:true 0 repository_address OpamRepository.default_address & doc) in
  let init global_options build_options repo_kind repo_name repo_address compiler cores =
    set_global_options global_options;
    set_build_options build_options;
    let repo_kind = guess_repository_kind repo_kind repo_address in
    let repo_priority = 0 in
    let repository = { repo_name; repo_kind; repo_address; repo_priority } in
    OpamClient.init repository compiler cores in
  Term.(pure init $global_options $build_options $repo_kind_flag $repo_name $repo_address $compiler $cores),
  term_info "init" ~doc ~man

(* LIST *)
let list_doc = "Display the list of available packages."
let list =
  let doc = list_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of available packages, or the list of \
         installed packages if the $(b,--installed) switch is used.";
    `P "Unless the $(b,--short) switch is used, the output format displays one \
        package per line, and each line contains the name of the package, the \
        installed version or -- if the package is not installed, and a short \
        description.";
    `P " The full description can be obtained by doing $(b,opam info <package>). \
         You can search through the package descriptions using the $(b,opam search) command."
  ] in
  let list global_options print_short installed_only packages =
    set_global_options global_options;
    OpamClient.list ~print_short ~installed_only packages in
  Term.(pure list $global_options $print_short_flag $installed_only_flag $pattern_list),
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
    `P "The full description can be obtained by doing $(b,opam info <package>).";
  ] in
  let case_sensitive =
    mk_flag ["c";"case-sensitive"] "Force the search in case sensitive mode." in
  let search global_options print_short installed_only case_sensitive packages =
    set_global_options global_options;
    OpamClient.list ~print_short ~installed_only ~name_only:false ~case_sensitive packages in
  Term.(pure search $global_options $print_short_flag $installed_only_flag $case_sensitive $pattern_list),
  term_info "search" ~doc ~man

(* INFO *)
let info_doc = "Display information about specific packages."
let info =
  let doc = info_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the information block for the selected \
        package(s).";
    `P "The information block consists in the name of the package, \
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
        ~doc:"Only display these fields. You can specify multiple fields by separating them with commas."
        ["f";"field"] in
    Arg.(value & opt (list string) [] & doc) in

  let pkg_info global_options fields packages =
    set_global_options global_options;
    OpamClient.info ~fields packages in
  Term.(pure pkg_info $global_options $fields $pattern_list),
  term_info "info" ~doc ~man


(* CONIG *)
let config_doc = "Display configuration options for packages."
let config =
  let doc = config_doc in
  let commands = [
    ["env"]     , `env     , "returns the environment variables PATH, MANPATH, OCAML_TOPLEVEL_PATH \
                              and CAML_LD_LIBRARY_PATH according to the current selected \
                              compiler. The output of this command is meant to be evaluated by a \
                              shell, for example by doing $(b,eval `opam config env`).";
    ["var"]     , `var     , "returns the value associated with the given variable. If the variable \
                              contains a colon such as $(i,pkg:var), then the left element will be \
                              understood as the package in which the variable is defined. \
                              The variable resolution is done as follows: first, OPAM will check whether \
                              $(b,\\$var) exists; for package variables, it will look for $(b,\\$pkg_var). \
                              If the variable is not found, OPAM will then check whether the variable is \
                              implicit. There are two global implicit variables: $(i,ocaml-version) and \
                              $(i,preinstalled) and two implicit variables per package: $(i,pkg:installed) \
                              which is either $(b,\"true\") or $(b,\"false\"), and $(i,pkg:enable) which is \
                              either $(b,\"enable\") or $(b,\"disable\"). Finally, OPAM will look into \
                              its global and package config files to find whether these variables exist.";
    ["list"]    , `list    , "returns the list of all variables defined in the listed packages. It is possible \
                              to filter the list of variables by giving package names (use $(b,globals) to get \
                              the list of global variables). No parameter means displaying all the variables.";
    ["subst"]   , `subst   , "substitutes variables in the given files. The strings $(i,%{var}%) are \
                              replaced by the value of the variable $(i,var) (see the documentation associated \
                              to $(b,opam config var)).";
    ["includes"], `includes, "returns include options.";
    ["bytecomp"], `bytecomp, "returns bytecode compile options.";
    ["asmcomp"] , `asmcomp , "returns assembly compile options.";
    ["bytelink"], `bytelink, "returns bytecode linking options.";
    ["asmlink"] , `asmlink , "returns assembly compile options.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command uses opam state to output information on how to use \
        installed libraries, updating the user’s $PATH, and substitute \
        variables used in opam packages.";
    `P "Apart from $(b,opam config env), most of these commands are used \
        by opam internally, and thus are of limited interest for the casual \
        user.";
  ] @ mk_subdoc ~names:"DOMAINS" commands in

  let command, params = mk_subcommands ~name:"DOMAIN" commands in
  let is_rec = mk_flag  ["R";"rec"] "Recursive query." in
  let csh    = mk_flag  ["c";"csh"] "Use csh-compatible output mode." in
  let env    =
    mk_opt ["e"] "" "Backward-compatible option, equivalent to $(b,opam config env)." Arg.string "" in

  let config global_options command env is_rec csh params =
    set_global_options global_options;
    let mk ~is_byte ~is_link =
      CCompil {
        conf_is_rec  = is_rec;
        conf_is_link = is_link;
        conf_is_byte = is_byte;
        conf_options = List.map OpamVariable.Section.Full.of_string params;
      } in
    let cmd =
      match command with
      | None           -> if env="nv" then CEnv csh else OpamGlobals.error_and_exit "Missing subcommand"
      | Some `env      -> CEnv csh
      | Some `list     -> CList (List.map OpamPackage.Name.of_string params)
      | Some `var      -> CVariable (OpamVariable.Full.of_string (List.hd params))
      | Some `subst    -> CSubst (List.map OpamFilename.Base.of_string params)
      | Some `includes -> CIncludes (is_rec, List.map OpamPackage.Name.of_string params)
      | Some `bytecomp -> mk ~is_byte:true  ~is_link:false
      | Some `bytelink -> mk ~is_byte:true  ~is_link:true
      | Some `asmcomp  -> mk ~is_byte:false ~is_link:false
      | Some `asmlink  -> mk ~is_byte:false ~is_link:true in
    OpamClient.config cmd in

  Term.(pure config $global_options $command $env $is_rec $csh $params),
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
    `P "This command will make opam use the dependency solver to compute the \
        transitive closure of dependencies to be installed, and will handle \
        conflicts as well. If the dependency solver returns more than one \
        solution, opam will arbitraty select the first one. If dependencies \
        are to be installed, opam will ask if the installation should really \
        be performed.";
  ] in
  let install global_options build_options packages =
    set_global_options global_options;
    set_build_options build_options;
    let packages = OpamPackage.Name.Set.of_list packages in
    OpamClient.install packages in
  Term.(pure install $global_options $build_options $package_list),
  term_info "install" ~doc ~man

(* REMOVE *)
let remove_doc = "Remove a list of packages."
let remove =
  let doc = remove_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes (i.e. uninstall) one or more packages currently \
        installed in the currently selected compiler. To remove packages \
        installed in another compiler, you need to switch compilers using \
        $(b,opam switch) or use the $(b,--switch) flag. This command is the \
        inverse of $(b,opam-install).";
  ] in
  let remove global_options build_options packages =
    set_global_options global_options;
    set_build_options build_options;
    let packages = OpamPackage.Name.Set.of_list packages in
    OpamClient.remove packages in
  Term.(pure remove $global_options $build_options $package_list),
  term_info "remove" ~doc ~man

(* REINSTALL *)
let reinstall =
  let doc = "Reinstall a list of packages." in
  let man = [
    `S "DESCRIPTION";
    `P "This command does removes the given packages, reinstall them and \
        recompile the right package dependencies."
  ] in
  let reinstall global_options build_options packages =
    set_global_options global_options;
    set_build_options build_options;
    let packages = OpamPackage.Name.Set.of_list packages in
    OpamClient.reinstall packages in
  Term.(pure reinstall $global_options $build_options $package_list),
  term_info "reinstall" ~doc ~man

(* UPDATE *)
let update_doc = "Update the list of available packages."
let update =
  let doc = update_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command updates each repository that has been previously set up \
        by the $(b,opam init) or $(b,opam repository) commands. The list of packages \
        that can be upgraded will be printed out, and the user can use \
        $(b,opam upgrade) to upgrade those.";
  ] in
  let update global_options repositories =
    set_global_options global_options;
    OpamClient.update repositories in
  Term.(pure update $global_options $repository_list),
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
  let upgrade global_options build_options names =
    set_global_options global_options;
    set_build_options build_options;
    let packages = OpamPackage.Name.Set.of_list names in
    OpamClient.upgrade packages in
  Term.(pure upgrade $global_options $build_options $package_list),
  term_info "upgrade" ~doc ~man

(* UPLOAD *)
let upload =
  let doc = "Upload a package to an OPAM repository." in
  let man = [
    `S "DESCRIPTION";
    `P "This command uploads an already built package to a remote repository, \
        if the remote repository is not read-only.";
  ] in
  let opam =
    mk_opt ["opam"]
      "FILE" "Specify the .opam file that will be uploaded to repo://packages/name.version/opam"
       Arg.(some filename) None in
  let descr =
    mk_opt ["descr"]
      "FILE" "Specify the .descr file that will be uploaded to repo://packages/name.version/descr"
      Arg.(some filename) None in
  let archive =
    mk_opt ["archive"]
      "FILE" "Specify the archive that will be uploaded to repo://archives/name.version+opam.tar.gz"
      Arg.(some filename) None in
  let repo =
    let doc = Arg.info ~docv:"REPO" ~doc:"Specify the repository to upload to." [] in
    Arg.(required & pos 0 (some repository_name) None & doc) in
  let upload global_options opam descr archive repo =
    set_global_options global_options;
    let upl_opam = match opam with
      | None   -> OpamGlobals.error_and_exit "missing OPAM file"
      | Some s -> s in
    let upl_descr = match descr with
      | None   -> OpamGlobals.error_and_exit "missing description file"
      | Some s -> s in
    let upl_archive = match archive with
      | None   -> OpamGlobals.error_and_exit "missing archive file"
      | Some s -> s in
    OpamClient.upload { upl_opam; upl_descr; upl_archive } repo in
  Term.(pure upload $global_options $opam $descr $archive $repo),
  term_info "upload" ~doc ~man

(* REPOSITORY *)
let repository_doc = "Manage OPAM repositories."
let repository name =
  let doc = repository_doc in
  let commands = [
    ["add"]        , `add     , "Add the repository $(b,name) available at address \
                                 $(b,address) to the list of repositories used by OPAM, \
                                 with priority $(b,priority). \
                                 The repository priority can be optionally specified with \
                                 $(b,--priority), otherwise the new repository has a higher \
                                 priority then any other existing repositories. \
                                 The kind of the repository can be specified with the \
                                 $(b,--kind) option, otherwise it will be determined \
                                 automatically.";
    ["remove"]     , `remove  , "Remove the repository named $(b,name) from the list of \
                                 repositories used by OPAM.";
    ["list"]       , `list    , "List all repositories used by OPAM.";
    ["priority"]   , `priority, "Change the priority of repository named $(b,name) to \
                                $(b,priority).";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command is used to manage OPAM repositories. To synchronize OPAM \
        with the last versions of the packages available in remote \
        repositories, $(b,opam update) should be used.";
  ] @ mk_subdoc commands in

  let command, params = mk_subcommands commands in
  let priority =
    mk_opt ["p";"priority"]
      "INT" "Set the repository priority (bigger is better)"
      Arg.(some int) None in

  let repository global_options command kind priority params =
    set_global_options global_options;
    let add name address =
      let name = OpamRepositoryName.of_string name in
      let address = OpamRepository.repository_address address in
      let kind = guess_repository_kind kind address in
      RAdd (name, kind, address, priority) in
    let cmd = match command, params with
      | None          , []
      | Some `list    , []              -> RList
      | Some `priority, [name; p] ->
        RPriority (OpamRepositoryName.of_string name, int_of_string p)
       | Some `remove , [name]          -> RRm (OpamRepositoryName.of_string name)
      | Some `add     , [name; address] -> add name address
      | _ -> OpamGlobals.error_and_exit "Too many parameters" in
    OpamClient.remote cmd in

  Term.(pure repository $global_options $command $repo_kind_flag $priority $params),
  term_info name  ~doc ~man

(* THOMAS: we keep 'opam remote' for backward compatibity *)
let remote = repository "remote"
let repository = repository "repository"

(* SWITCH *)
let switch_doc = "Manage multiple installation of compilers."
let switch =
  let doc = switch_doc in
  let commands = [
    ["install"]      , `install  , "Install the given compiler. The commands fails if the package is \
                                    already installed (e.g. it will not transparently switch to the \
                                    installed compiler switch, as $(b,opam switch <name>) does).";
    ["remove"]       , `remove   , "Remove the given compiler.";
    ["export"]       , `export   , "Export the list installed package to a file.";
    ["import"]       , `import   , "Install the packages from a file.";
    ["reinstall"]    , `reinstall, "Reinstall the given compiler switch. This will also try reinstall the \
                                    installed packages.";
    ["list"]         , `list     , "List the available compilers. \
                                    The first column displays the switch name (if any), the second one \
                                    the switch state (C = current, I = installed, -- = not installed), \
                                    the third one the compiler name and the last one the compiler \
                                    description. To switch to an already installed compiler alias (with \
                                    state = I), use $(b,opam switch <name>). If you want to use a new \
                                    compiler <comp>, use $(b,opam switch <comp>): this will download, \
                                    compile and create a fresh and independant environment where new packages can be installed. \
                                    If you want to create a new compiler alias (for instance because you already have \
                                    this compiler version installed), use $(b,opam switch <name> --alias-of <comp>). In case \
                                    <name> and <comp> are the same, this is equivalent to $(b,opam switch <comp>).";
    ["show"]          , `current  , "Show the current compiler.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command allows to switch between different compiler versions, \
        installing the compiler if $(b,opam switch) is used to switch to that \
        compiler for the first time. The different compiler versions are \
        totally independant from each other, meaning that OPAM maintains a \
        separate state (e.g. list of installed packages...) for each.";
    `P "See the documentation of $(b,opam switch list) to see the compilers which \
        are available, and how to switch or to install a new one."
  ] @ mk_subdoc commands in

  let command, params = mk_subcommands_with_default commands
    "If a compiler switch is given instead of an usual command, this command will switch to \
     the given compiler. You will then need to run $(b,eval `opam config env`) to update your \
     environment variables." in
  let alias_of =
    mk_opt ["a";"alias-of"]
      "COMP" "The name of the compiler description which will be aliased."
      Arg.(some string) None in
  let filename =
    mk_opt ["f";"filename"]
      "FILENAME" "The name of the file to export to/import from."
      Arg.(some filename) None in
  let switch global_options build_options command alias_of filename print_short installed_only params =
    set_global_options global_options;
    set_build_options build_options;
    let no_alias_of () =
      if alias_of <> None then
        OpamGlobals.error_and_exit "invalid -alias-of option" in
    let mk_comp alias = match alias_of with
      | None      -> OpamCompiler.of_string alias
      | Some comp -> OpamCompiler.of_string comp in
    match command, params with
    | None      , []
    | Some `list, [] ->
        no_alias_of ();
        OpamClient.switch_list ~print_short ~installed_only
    | Some `install, [switch] ->
        OpamClient.switch_install global_options.quiet (OpamSwitch.of_string switch) (mk_comp switch)
    | Some `export, [] ->
        no_alias_of ();
        OpamClient.switch_export filename
    | Some `import, [] ->
        no_alias_of ();
        OpamClient.switch_import filename
    | Some `remove, switches ->
        no_alias_of ();
        List.iter (fun switch -> OpamClient.switch_remove (OpamSwitch.of_string switch)) switches
    | Some `reinstall, [switch] ->
        no_alias_of ();
        OpamClient.switch_reinstall (OpamSwitch.of_string switch)
    | Some `current, [] ->
        no_alias_of ();
        OpamClient.switch_current ()
    | Some `default switch, [] ->
        (match alias_of with
        | None -> OpamClient.switch global_options.quiet (OpamSwitch.of_string switch)
        | _    ->
          OpamClient.switch_install global_options.quiet
            (OpamSwitch.of_string switch) (mk_comp switch))
    | _, l -> OpamGlobals.error_and_exit "too many arguments (%d)" (List.length l) in

  Term.(pure switch $global_options $build_options $command $alias_of $filename $print_short_flag $installed_only_flag $params),
  term_info "switch" ~doc ~man

(* PIN *)
let pin_doc = "Pin a given package to a specific version."
let pin =
  let doc = pin_doc in
  let man = [
    `S "DESCRIPTION";
    `P "This command will 'pin' a package to a specific version, or use a \
        specific source path for installing and upgrading the package. Using \
        $(b,opam pin <package> none) will undo the 'pinned' status of \
        <package>.";
    `P "It is possible to pin a package to a specific git commit/tag/branch \
        with $(b,opam pin <package> </path/to/git>#<commit>).";
    `P "To list all the currently pinned packages, call the $(b,opam pin) \
        without arguments or use $(b,--list)."
  ] in

  let package =
    let doc = Arg.info ~docv:"PACKAGE" ~doc:"Package name." [] in
    Arg.(value & pos 0 (some string) None & doc) in
  let pin_option =
    let doc =
      Arg.info ~docv:"PIN" ~doc:
        "Specific version, local path, git or darcs url to pin the package to, \
         or 'none' to unpin the package." [] in
    Arg.(value & pos 1 (some string) None & doc) in
  let list = mk_flag ["l";"list"] "List the currently pinned packages." in
  let kind =
    let doc = Arg.info ~docv:"KIND" ~doc:"Force the kind of pinning." ["k";"kind"] in
    let kinds = [
      "git"    , `git;
      "darcs"  , `darcs;
      "version", `version;
      "local"  , `local;
      "rsync"  , `local;
    ] in
    Arg.(value & opt (some & enum kinds) None & doc) in
  let force = mk_flag ["f";"force"] "Disable consistency checks." in

  let pin global_options force kind list package pin =
    set_global_options global_options;
    if list then
      OpamClient.pin_list ()
    else match package, pin with
    | None  , None   -> OpamClient.pin_list ()
    | Some n, Some p ->
      let pin = {
        pin_package = OpamPackage.Name.of_string n;
        pin_option  = pin_option_of_string ?kind:kind p
      } in
      OpamClient.pin ~force pin
    | _ -> OpamGlobals.error_and_exit "Wrong arguments" in

  Term.(pure pin $global_options $force $kind $list $package $pin_option),
  term_info "pin" ~doc ~man

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
    | None       -> `Help (`Pager, Some "help")
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
  let doc = "a Package Manager for OCaml" in
  let man = [
    `S "DESCRIPTION";
    `P "OPAM is a package manager for OCaml. It uses the powerful mancoosi \
        tools to handle dependencies, including support for version \
        constraints, optional dependencies, and conflicts management.";
    `P "It has support for different repository backends such as HTTP, rsync, git \
        and darcs. It handles multiple OCaml versions concurrently, and is \
        flexible enough to allow you to use your own repositories and packages \
        in addition of the ones it provides.";
    `P "Use either $(b,opam <command> --help) or $(b,opam help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage _ =
    OpamGlobals.msg
      "usage: opam [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
      \n\
      The most commonly used opam commands are:\n\
      \    init         %s\n\
      \    list         %s\n\
      \    info         %s\n\
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
      init_doc list_doc info_doc install_doc remove_doc update_doc
      upgrade_doc config_doc repository_doc switch_doc pin_doc in
  Term.(pure usage $global_options),
  Term.info "opam"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = [
  init;
  list; search; info;
  install; remove; reinstall;
  update; upgrade;
  config;
  remote; repository;
  switch;
  pin;
  upload;
  help;
]

let () =
  Sys.catch_break true;
  try
    match Term.eval_choice ~catch:false default commands with
    | `Error _ -> exit 1
    | _        ->
      if !OpamGlobals.print_stats then (
        OpamFile.print_stats ();
        OpamState.print_stats ();
      );
      exit 0
  with
  | OpamGlobals.Exit 0 -> ()
  | e ->
    OpamGlobals.error "'%s' failed." (String.concat " " (Array.to_list Sys.argv));
    let exit_code = ref 1 in
    begin match e with
    | OpamGlobals.Exit i -> exit_code := i
    | OpamSystem.Internal_error _
    | OpamSystem.Process_error _ -> Printf.eprintf "%s\n" (Printexc.to_string e);
    | _ -> Printf.fprintf stderr "Fatal error: exception %s\n" (Printexc.to_string e)
    end;
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace ());
    exit !exit_code
