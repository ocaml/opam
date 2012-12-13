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

(*

(* opam remote [-list|-add <url>|-rm <url>] *)
let remote =
  let kind = ref None in
  let command : [`add|`list|`rm|`priority] option ref = ref None in
  let set c () = command := Some c in
  let add name address priority =
    let name = OpamRepositoryName.of_string name in
    let kind = guess_repository_kind !kind address in
    let address = OpamRepository.repository_address address in
    OpamClient.remote (RAdd (name, kind, address, priority)) in
{
  name     = "remote";
  usage    = "[-list|add <name> <address>|rm <name>]";
  synopsis = "Manage remote servers";
  help     = "";
  specs    = [
    ("-list" , Arg.Unit (set `list), " List the repositories");
    ("-add"  , Arg.Unit (set `add) , " Add a new repository");
    ("-rm"   , Arg.Unit (set `rm)  , " Remove a remote repository");
    ("-kind" , Arg.String (fun s -> kind := Some s) , " (optional) Specify the repository kind");
    ("-priority", Arg.Unit (set `priority) , " Set the repository priority (higher is better)");
  ];
  anon;
  main     = parse_args (fun args ->
    match !command, args with
    | Some `priority, [name; p]    ->
      OpamClient.remote (RPriority (OpamRepositoryName.of_string name, int_of_string p))
    | Some `list, []                -> OpamClient.remote RList
    | Some `rm,   [ name ]          -> OpamClient.remote (RRm (OpamRepositoryName.of_string name))
    | Some `add , [ name; address ] -> add name address None
    | Some `add ,
      [ name; address; priority ]   -> add name address (Some (int_of_string priority))
    | None, _  -> bad_argument "remote" "Command missing [-list|-add|-rm]"
    | _        -> bad_argument "remote" "Wrong arguments")
}

(* opam switch [-clone] OVERSION *)
let switch =
  let alias_of = ref "" in
  let command = ref `switch in
  let set c () =
    if !command <> `switch then
      bad_argument "switch" "two many sub-commands";
    command := c in
  let no_alias_of () =
    if !alias_of <> "" then
      bad_argument "switch" "invalid -alias-of option" in
  let mk_comp alias = match !alias_of with
    | ""   -> OpamCompiler.of_string alias
    | comp -> OpamCompiler.of_string comp in
{
  name     = "switch";
  usage    = "[compiler-name]";
  synopsis = "Manage multiple installation of compilers";
  help     = "";
  specs    = [
    ("-alias-of"        , Arg.Set_string alias_of        , " Compiler name");
    ("-no-base-packages", Arg.Clear OpamGlobals.base_packages, " Do not install the base packages");
    ("-install"         , Arg.Unit (set `install)        , " Install the given compiler");
    ("-remove"          , Arg.Unit (set `remove)         , " Remove the given compiler");
    ("-export"          , Arg.String (fun s -> set (`export s) ()), " Export the libraries installed with the given alias");
    ("-import"          , Arg.String (fun s -> set (`import s) ()), " Import the libraries installed with the given alias");
    ("-reinstall"       , Arg.Unit (set `reinstall)      , " Reinstall the given compiler");
    ("-list"            , Arg.Unit (set `list)           , " List the available compilers");
    ("-current"         , Arg.Unit (set `current)        , " Display the current compiler");
  ];
  anon;
  main     = parse_args (function args ->
    match !command, args with
    | `install, [switch] ->
        OpamClient.switch_install !quiet (OpamSwitch.of_string switch) (mk_comp switch)
    | `export f, [] ->
        no_alias_of ();
        OpamClient.switch_export (OpamFilename.of_string f)
    | `import f, [] ->
        no_alias_of ();
        OpamClient.switch_import (OpamFilename.of_string f)
    | `remove, switches ->
        no_alias_of ();
        List.iter (fun switch -> OpamClient.switch_remove (OpamSwitch.of_string switch)) switches
    | `reinstall, [switch] ->
        no_alias_of ();
        OpamClient.switch_reinstall (OpamSwitch.of_string switch)
    | `list, [] ->
        no_alias_of ();
        OpamClient.switch_list ()
    | `current, [] ->
        no_alias_of ();
        OpamClient.switch_current ()
    | `switch, [switch] ->
        begin match !alias_of with
          | "" -> OpamClient.switch !quiet (OpamSwitch.of_string switch)
          | _  -> OpamClient.switch_install !quiet (OpamSwitch.of_string switch) (mk_comp switch)
        end
    | _ -> bad_argument "switch" "too many arguments"
  )
}

(* opam pin [-list|<package> <version>|<package> <path>] *)
let pin =
  let list = ref false in
  let kind = ref None in
  let set_kind s = kind := Some s in
{
  name     = "pin";
  usage    = "<package> [<version>|<url>|none]";
  synopsis = "Pin a given package to a specific version";
  help     = "";
  specs    = [
    ("-list", Arg.Set list       , " List the current status of pinned packages");
    ("-kind", Arg.String set_kind, " Force the pin action (options are: 'git', 'rsync', 'version'");
  ];
  anon;
  main     = parse_args (function
    | [] when !list    -> OpamClient.pin_list ()
    | [name; arg]      -> OpamClient.pin { pin_package = OpamPackage.Name.of_string name; pin_arg = pin_option_of_string ?kind:!kind arg }
    | _                -> bad_argument "pin" "Wrong arguments")
}

*)

(* Global options *)
type global_options = {
  debug  : bool;
  verbose: bool;
  switch : string option;
  yes    : bool;
  root   : string;
}

let create_global_options debug verbose switch yes root =
  { debug; verbose; switch; yes; root }

let set_global_options o =
  OpamGlobals.debug    := o.debug;
  OpamGlobals.verbose  := o.verbose;
  OpamGlobals.switch   := o.switch;
  OpamGlobals.root_dir := OpamSystem.real_path o.root

(* Build options *)
type build_options = {
  keep_build_dir: bool;
  make          : string option;
  no_checksums  : bool;
}

let create_build_options keep_build_dir make no_checksums =
  { keep_build_dir; make; no_checksums }

let set_build_options b =
  OpamGlobals.keep_build_dir   := b.keep_build_dir;
  OpamGlobals.verify_checksums := not b.no_checksums;
  match b.make with
  | None   -> ()
  | Some s -> OpamGlobals.makecmd := lazy s

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
  `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
  `P "Use `$(mname) help environment' for help on environment variables.";
  `S "BUGS"; `P "Check bug reports at https://github.com/OCamlPro/opam/issues.";]

(* Converters *)
let pr_str = Format.pp_print_string

let repository_name =
  let parse str = `Ok (OpamRepositoryName.of_string str) in
  let print ppf name = pr_str ppf (OpamRepositoryName.to_string name) in
  parse, print

let repository_address =
  let parse str = `Ok (OpamFilename.raw_dir str) in
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

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

let mk_opt ?section flags value doc conv default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt conv default & doc)

let mk_subdoc commands =
  `S "COMMANDS" ::
  List.map (fun (cs,_,d) ->
    let bold s = Printf.sprintf "$(i,%s)" s in
    let cmds = String.concat ", " (List.map bold cs) in
    `I (cmds, d)
  ) commands

let mk_subcommands commands =
  let command =
    let doc = Arg.info ~docs:"COMMAND" ~doc:"Name of the sub-command." [] in
    let commands =
      List.fold_left
        (fun acc (cs,f,_) -> List.map (fun c -> c,f) cs @ acc)
        [] commands in
    Arg.(required & pos 0 (some & enum commands) None & doc) in
  let params =
    let doc = Arg.info ~doc:"Optional parameters." [] in
    Arg.(value & pos_left 0 string [] & doc) in
  command, params

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

let arg_list name doc conv =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(value & pos_all conv [] & doc)

(* Common flags *)
let print_short_flag =
  mk_flag ["s";"short"]
    "Output the names of packages separated by one whitespace \
     instead of using the usual formatting."

let installed_only_flag =
  mk_flag ["i";"installed"] "List installed packages only."

let pattern_list =
  arg_list "PATTERNS" "List of package patterns." Arg.string

let package_list =
  arg_list "PACKAGES" "List of package names." package_name

let repository_list =
  arg_list "REPOSITORIES" "List of repository names." repository_name

let help copts man_format cmds topic = match topic with
  | None       -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
    let topics = "topics" :: cmds in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t -> `Help (man_format, Some t)

(* Options common to all commands *)
let global_options =
  let section = global_option_section in
  let debug = mk_flag ~section ["debug"] "Print debug message on stdout."  in
  let verbose =
    let quiet =
      let doc = Arg.info ["quiet"] ~docs:section ~doc:"Suppress informational output." in
      false, doc in
    let verbose =
      let doc = Arg.info ["verbose"] ~docs:section ~doc:"Give verbose output." in
      true, doc in
    Arg.(last & vflag_all [false] [quiet; verbose]) in
  let switch =
    mk_opt ~section ["s";"switch"]
      "SWITCH" "Use $(docv) as the current compiler switch."
      Arg.(some string) !OpamGlobals.switch in
  let yes =
    mk_flag ~section ["y";"yes"]
      "Disable interactive mode and answer yes \
       to all questions that would otherwise be\
       asked to the user."  in
  let root =
    mk_opt ~section ["r";"root"]
      "ROOT" "Use $(docv) as the current root path."
      Arg.string !OpamGlobals.root_dir in
  Term.(pure create_global_options $debug $verbose $switch $yes $root)

(* Options common to all build commands *)
let build_options =
  let keep_build_dir = mk_flag ["k";"keep-build-dir"] "Keep the build directory." in
  let no_checksums =
    mk_flag ["n";"no-checksums"]   "Do not verify the checksum of downloaded archives." in
  let make =
    mk_opt ["m";"makecmd";"make"] "MAKE"
      "Use $(docv) as the default 'make' command."
      Arg.(some string) None in
  Term.(pure create_build_options $keep_build_dir $make $no_checksums)

let guess_repository_kind kind address =
  match kind with
  | None  ->
    let address = OpamFilename.Dir.to_string address in
    if Sys.file_exists address then
      "local"
    else if OpamMisc.starts_with ~prefix:"git" address
        || OpamMisc.ends_with ~suffix:"git" address then
      "git"
    else
      OpamGlobals.default_repository_kind
  | Some k -> k

(* INIT *)
let init =
  let doc = "Initialize opam." in
  let man = [
    `S "DESCRIPTION";
    `P "The init command creates a fresh client state, that is initialize opam
        configuration in ~/.opam and setup a default repository.";
    `P "Additional repositories can later be added by using the $(b,opam remote) command.";
    `P "The local cache of a repository state can be updated by using $(b,opam update).";
  ] in
  let cores = mk_opt ["j";"cores"] "CORES" "Number of process to use when building packages." Arg.int 1 in
  let compiler =
    mk_opt ["c";"comp"] "VERSION" "Which compiler version to use." compiler OpamCompiler.default in
  let repo_kind =
    let kinds = List.map (fun x -> x,x) [ "http";"rsync"; "git" ] in
    mk_opt ["kind"] "KIND" "Specify the kind of the repository to be set." Arg.(some (enum kinds)) None in
  let repo_name =
    let doc = Arg.info ~docv:"NAME" ~doc:"Name of the repository." [] in
    Arg.(value & pos ~rev:true 1 repository_name OpamRepositoryName.default & doc) in
  let repo_address =
    let doc = Arg.info ~docv:"ADDRESS" ~doc:"Address of the repository." [] in
    Arg.(value & pos ~rev:true 0 repository_address OpamRepository.default_address & doc) in
  let init global_options repo_kind repo_name repo_address compiler cores =
    set_global_options global_options;
    let repo_kind = guess_repository_kind repo_kind repo_address in
    let repo_priority = 0 in
    let repository = { repo_name; repo_kind; repo_address; repo_priority } in
    OpamClient.init repository compiler cores in
  Term.(pure init $global_options $repo_kind $repo_name $repo_address $ compiler $cores),
  term_info "init" ~doc ~man

(* LIST *)
let list =
  let doc = "Display the list of available packages." in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of available packages, or the list of
         installed packages if the $(i,-installed) switch is used.";
    `P "Unless the $(i,-short) switch is used, the output format displays one
        package per line, and each line contains the name of the package, the
        installed version or -- if the package is not installed, and a short
        description.";
    `P " The full description can be obtained by doing $(b,opam info <package>).
         You can search into the package list with the $(b,opam search) command."
  ] in
  let list global_options print_short installed_only packages =
    set_global_options global_options;
    OpamClient.list ~print_short ~installed_only packages in
  Term.(pure list $global_options $print_short_flag $installed_only_flag $pattern_list),
  term_info "list" ~doc ~man

(* SEARCH *)
let search =
  let doc = "Search into the package list" in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the list of available packages that match one of
        the package patterns specified as arguments.";
    `P "Unless the -$(i,short) flag is used, the output format is the same as the
        $(b,opam list) command. It displays one package per line, and each line
        contains the name of the package, the installed version or -- if the package
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
let info =
  let doc = "Display information about specific packages" in
  let man = [
    `S "DESCRIPTION";
    `P "This command displays the information block for the selected
        package(s).";
    `P "The information block consists in the name of the package,
        the installed version if this package is installed in the current
        selected compiler, the list of available (installable) versions, and a
        complete description.";
    `P "$(b,opam list) can be used to display the list of
        available packages as well as a short description for each.";
  ] in
  let pkg_info global_options packages =
    set_global_options global_options;
    OpamClient.info packages in
  Term.(pure pkg_info $global_options $pattern_list),
  term_info "info" ~doc ~man


(* CONFIG *)
let config =
  let doc = "Display configuration options for packages" in
  let commands = [
    ["env"]     , `env     , "returns the environment variables PATH, MANPATH, OCAML_TOPLEVEL_PATH
                            and CAML_LD_LIBRARY_PATH according to the current selected
                            compiler. The output of this command is meant to be evaluated by a
                            shell, for example by doing $(b,eval `opam config -env`).";
    ["var"]     , `var     , "returns the value associated with the given variable.";
    ["list"]    , `list    , "returns the list of all variables defined in the listed packages (no package = all).";
    ["subst"]   , `subst   , "substitutes variables in the given files.";
    ["includes"], `includes, "returns include options.";
    ["bytecomp"], `bytecomp, "returns bytecode compile options.";
    ["asmcomp"] , `asmcomp , "returns assembly compile options.";
    ["bytelink"], `bytelink, "returns bytecode linking options.";
    ["asmlink"] , `asmlink , "returns assembly compile options.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "This command uses opam state to output information on how to use
        installed libraries, updating the user’s $PATH, and substitute
        variables used in opam packages.";
    `P "Apart from $(b,opam config -env), most of these commands are used
        by opam internally, and thus are of limited interest for the casual
        user.";
  ] @ mk_subdoc commands in

  let command, params = mk_subcommands commands in
  let is_rec = mk_flag  ["r";"rec"] "Recursive query." in
  let csh    = mk_flag  ["c";"csh"] "Use csh-compatible output mode." in

  let config global_options command is_rec csh params =
    set_global_options global_options;
    let mk is_link is_byte =
      CCompil {
        conf_is_rec  = is_rec;
        conf_is_link = is_link;
        conf_is_byte = is_byte;
        conf_options = List.map OpamVariable.Section.Full.of_string params;
      } in
    let cmd = match command with
      | `env      -> CEnv csh
      | `list     -> CList
      | `var      -> CVariable (OpamVariable.Full.of_string (List.hd params))
      | `subst    -> CSubst (List.map OpamFilename.Base.of_string params)
      | `includes -> CIncludes (is_rec, List.map OpamPackage.Name.of_string params)
      | `bytecomp -> mk true false
      | `bytelink -> mk true true
      | `asmcomp  -> mk false false
      | `asmlink  -> mk false true in
    OpamClient.config cmd in

  Term.(pure config $global_options $command $is_rec $csh $params),
  term_info "config" ~doc ~man

(* INSTALL *)
let install =
  let doc = "Install a list of packages" in
  let man = [
    `S "DESCRIPTION";
    `P "This command installs one or more packages to the currently selected
        compiler. To install packages for another compiler, you need to switch
        compilers using $(b,opam switch). You can remove installed packages with
        $(b,opam remove), and list installed packages with $(b,opam list -i).
        See $(b,opam pin) as well to understand how to manage package versions.";
    `P "This command will make opam use the dependency solver to compute the
        transitive closure of dependencies to be installed, and will handle
        conflicts as well. If the dependency solver returns more than one
        solution, opam will arbitraty select the first one. If dependencies
        are to be installed, opam will ask if the installation should really
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
let remove =
  let doc = "Remove a list of packages" in
  let man = [
    `S "DESCRIPTION";
    `P "This command removes (i.e. uninstall) one or more packages currently
        installed in the currently selected compiler. To remove packages
        installed in another compiler, you need to switch compilers using
        $(b,opam switch) or use the $(b,--switch) flag. This command is the
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
  let doc = "Reinstall a list of packages" in
  let man = [
    `S "DESCRIPTION";
    `P "This command does removes the given packages, reinstall them and
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
let update =
  let doc = "Update the list of available packages" in
  let man = [
    `S "DESCRIPTION";
    `P "This command updates each repository that has been previously set up
        by the $(b,opam init) or $(b,opam remote) commands. The list of packages
        that can be upgraded will be printed out, and the user can use
        $(b,opam upgrade) to upgrade those.";
  ] in
  let update global_options build_options repositories =
    set_global_options global_options;
    OpamClient.update repositories in
  Term.(pure update $global_options $build_options $repository_list),
  term_info "update" ~doc ~man

(* UPGRADE *)
let upgrade =
  let doc ="Upgrade the installed package to latest version" in
  let man = [
    `S "DESCRIPTION";
    `P "This command upgrades the installed packages to their latest available
        versions. More precisely, this command calls the dependency solver to
        find a consistent state where $(i,most) of the installed packages are
        upgraded to their latest versions.";
  ] in
  let upgrade global_options names =
    set_global_options global_options;
    let packages = OpamPackage.Name.Set.of_list names in
    OpamClient.upgrade packages in
  Term.(pure upgrade $global_options $package_list),
  term_info "upgrade" ~doc ~man

(* UPLOAD *)
let upload =
  let doc = "Upload a package to an OPAM repository" in
  let man = [
    `S "DESCRIPTION";
    `P "This command uploads an already built package to a remote repository,
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
    mk_opt ["repo";"repository"]
      "REPO" "Specify the repository to upload to. Defaults to the default repository."
      Arg.(some repository_name) None in
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

(* HELP *)
let help =
  let doc = "display help about opam and opam commands" in
  let man = [
    `S "DESCRIPTION";
     `P "Prints help about opam commands"
]
  in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  Term.(ret (pure help $ global_options $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default =
  let doc = "a Package Manager for OCaml" in
  let man = [
    `S "DESCRIPTION";
    `P "OPAM is a package manager for OCaml. It uses the powerful mancoosi
        tools to handle dependencies, including support for version
        constraints, optional dependencies, and conflicts management.";
    `P "It has support for different repository backends such as HTTP, rsync and
        git. It handles multiple OCaml versions concurrently, and is flexible
        enough to allow you to use your own repositories and packages in
        addition of the ones it provides.";
  ] @  help_sections
  in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ global_options)),
  Term.info "opam"
    ~version:(OpamVersion.to_string OpamVersion.current)
    ~sdocs:global_option_section
    ~doc
    ~man

let cmds = [
  init;
  list; search; info;
  install; remove; reinstall;
  update; upgrade;
  config;
  help;
]

let () =
  Sys.catch_break true;
  Printexc.register_printer (function
    | Unix.Unix_error (e,fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s" Sys.argv.(0) fn msg (Unix.error_message e) in
      Some error
    | _ -> None);
  try
    match Term.eval_choice ~catch:false default cmds with
    | `Error _ -> exit 1
    | _        -> exit 0
  with
  | OpamGlobals.Exit 0 -> ()
  | e ->
    OpamGlobals.error "  '%s' failed.\n" (String.concat " " (Array.to_list Sys.argv));
    match e with
    | OpamGlobals.Exit i -> exit i
    | e ->
      let bt = Printexc.get_backtrace () in
      let bt = if bt = "" then "" else Printf.sprintf "    at\n %s\n" bt in
      Printf.fprintf stderr "Fatal error: exception %s\n%s%!"
        (Printexc.to_string e) bt;
      exit 2
