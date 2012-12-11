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

let global_args = [
  "--makecmd"   , Arg.String (fun s -> OpamGlobals.makecmd := lazy s),
    Printf.sprintf " Set the 'make' program used when compiling packages";
  "--no-checksums", Arg.Clear OpamGlobals.verify_checksums, " Do not verify checksums on download";
  "--keep-build-dir", Arg.Set OpamGlobals.keep_build_dir, " Keep the build directory";
]

(* opam install <PACKAGE>+ *)
let install = {
  name     = "install";
  usage    = "[package]+";
  synopsis = "Install a list of packages";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    if names <> [] then
      let names = List.map OpamPackage.Name.of_string names in
      OpamClient.install (OpamPackage.Name.Set.of_list names)
    else OpamGlobals.error_and_exit "You need to specify at least one package to install."
  )
}

(* opam reinstall <PACKAGE>+ *)
let reinstall = {
  name     = "reinstall";
  usage    = "[package]+";
  synopsis = "Reinstall a list of packages";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    let names = List.map OpamPackage.Name.of_string names in
    OpamClient.reinstall (OpamPackage.Name.Set.of_list names)
  )
}

(* opam update *)
let update = {
  name     = "update";
  usage    = "[repo]*";
  synopsis = "Update the list of available packages";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    OpamClient.update (List.map OpamRepositoryName.of_string names)
  )
}

(* opam upgrade *)
let upgrade = {
  name     = "upgrade";
  usage    = "[package]*";
  synopsis = "Upgrade the installed package to latest version";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    let names = List.map OpamPackage.Name.of_string names in
    OpamClient.upgrade (OpamPackage.Name.Set.of_list names);
  )
}

(* opam upload PACKAGE *)
let upload =
  let opam = ref "" in
  let descr = ref "" in
  let archive = ref "" in
  let repo = ref "" in
{
  name     = "upload";
  usage    = "";
  synopsis = "Upload a package to an OPAM repository";
  help     = "";
  specs    = [
    ("-opam"   , Arg.Set_string opam   , " specify the OPAM file to upload");
    ("-descr"  , Arg.Set_string descr  , " specify the description file to upload");
    ("-archive", Arg.Set_string archive, " specify the archive file to upload");
    ("-repo"   , Arg.Set_string repo   , " (optional) specify the repository to upload")
  ];
  anon = noanon "upload";
  main     = (function () ->
    if !opam = "" then
      bad_argument "upload" "missing OPAM file";
    if !descr = "" then
      bad_argument "upload" "missing description file";
    if !archive = "" then
      bad_argument "upload" "missing archive file";
    let upl_opam = OpamFilename.of_string !opam in
    let upl_descr = OpamFilename.of_string !descr in
    let upl_archive = OpamFilename.of_string !archive in
    let repo = if !repo = "" then None else Some (OpamRepositoryName.of_string !repo) in
    OpamClient.upload { upl_opam; upl_descr; upl_archive } repo)
}

(* opam remove PACKAGE *)
let remove = {
  name     = "remove";
  usage    = "[package]+";
  synopsis = "Remove a list of packages";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    if names <> [] then
      let names = List.map OpamPackage.Name.of_string names in
      OpamClient.remove (OpamPackage.Name.Set.of_list names)
    else OpamGlobals.error_and_exit "You need to specify at least one package to remove."
  )
}

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

let commands = [
  init;
  list;
  search;
  info;
  config;
  install;
  reinstall;
  update;
  upgrade;
  upload;
  remove;
  remote;
  switch;
  pin;
]

let f () =
  Sys.catch_break true;
  Printexc.register_printer (function
    | Unix.Unix_error (e,fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s" Sys.argv.(0) fn msg (Unix.error_message e) in
      Some error
    | _ -> None);
  List.iter SubCommand.register commands;
  try ArgExt.parse ~man_fun:
        (fun cmd -> ignore (Sys.command ("man opam-" ^ cmd))) global_args
  with
  | OpamGlobals.Exit 0 -> ()
  | e ->
    OpamGlobals.error "  '%s' failed\n" (String.concat " " (Array.to_list Sys.argv));
    match e with
    | Bad (cmd, msg) ->
        ArgExt.pp_print_help (ArgExt.SubCommand cmd) Format.err_formatter global_args ();
        OpamGlobals.error "%s" msg;
        exit 1;
    | Failure ("no subcommand defined" as s) ->
        ArgExt.pp_print_help ArgExt.NoSubCommand Format.err_formatter global_args ();
        OpamGlobals.error "%s" s;
        exit 2
    | OpamGlobals.Exit i -> exit i
    | e ->
      let bt = Printexc.get_backtrace () in
      let bt = if bt = "" then "" else Printf.sprintf "    at\n %s\n" bt in
      Printf.fprintf stderr "Fatal error: exception %s\n%s%!"
        (Printexc.to_string e) bt;
      exit 2

let global_args = [
  "--no-checksums", Arg.Clear OpamGlobals.verify_checksums, " Do not verify checksums on download";
  "--keep-build-dir", Arg.Set OpamGlobals.keep_build_dir, " Keep the build directory";
]

*)
type global_options = {
  debug  : bool;
  verbose: bool;
  switch : string option;
  yes    : bool;
  root   : string;
}

let set_global_options o =
  OpamGlobals.debug    := o.debug;
  OpamGlobals.verbose  := o.verbose;
  OpamGlobals.switch   := o.switch;
  OpamGlobals.root_dir := OpamSystem.real_path o.root

let help copts man_format cmds topic = match topic with
  | None       -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
    let topics = "topics" :: cmds in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t -> `Help (man_format, Some t)

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

(* Options common to all commands *)
let create_global_options debug verbose switch yes root =
  { debug; verbose; switch; yes; root }

let global_options =
  let docs = global_option_section in
  let debug =
    let doc = Arg.info ~docs ~doc:"Print debug message on stdout." ["debug"] in
    Arg.(value & flag & doc) in
  let verbose =
    let quiet =
      let doc = Arg.info ["quiet"] ~docs ~doc:"Suppress informational output." in
      false, doc in
    let verbose =
      let doc = Arg.info ["verbose"] ~docs ~doc:"Give verbose output." in
      true, doc in
    Arg.(last & vflag_all [false] [quiet; verbose]) in
  let switch =
    let doc = Arg.info ~docs ~doc:"Overwrite the compiler switch name." ["switch"] in
    Arg.(value & opt (some string) !OpamGlobals.switch & doc) in
  let yes =
    let doc = Arg.info ~docs ~doc:"Disable interactive mode and answer yes \
                               to all questions that would otherwise be\
                               asked to the user." ["yes"] in
    Arg.(value & flag & doc) in
  let root =
    let doc = Arg.info ~docs ~doc:"Change the root path." ["root"]  in
    Arg.(value & opt string !OpamGlobals.root_dir & doc) in
  Term.(pure create_global_options $ debug $ verbose $ switch $ yes $ root)

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

let compiler: compiler Arg.converter =
  let parse str = `Ok (OpamCompiler.of_string str) in
  let print ppf comp = pr_str ppf (OpamCompiler.to_string comp) in
  parse, print

(* Helpers *)
let mk_info title doc flags =
  Arg.info ~docv:title ~doc flags

let mk_flag title doc flags =
  Arg.(value & flag & mk_info title doc flags)

let mk_opt title doc conv default flags =
  let doc = Arg.info ~docv:title ~doc flags in
  Arg.(value & opt conv default & doc)

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

(* Commands *)

let print_short_flag =
  mk_flag "SHORT" "Output the names of packages separated by one whitespace \
                   instead of using the usual formatting." ["s";"short"]

let installed_only_flag =
  mk_flag "INSTALLED" "List installed packages only." ["i";"installed"]

let arg_list name doc =
  let doc = Arg.info ~docv:name ~doc [] in
  Arg.(value & pos_all string [] & doc)

let all_packages =
  arg_list "PACKAGE" "List of package patterns."

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
  let cores = mk_opt "CORES" "Number of cores." Arg.int 1 ["j";"cores"] in
  let compiler =
    mk_opt "COMPILER" "Which compiler version to use." compiler OpamCompiler.default ["c";"comp"] in
  let repo_kind =
    let kinds = List.map (fun x -> x,x) [ "http";"rsync"; "git" ] in
    mk_opt "KIND" "Specify the kind of the repository to be set." Arg.(some (enum kinds)) None ["kind"] in
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
  Term.(pure list $global_options $print_short_flag $installed_only_flag $all_packages),
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
    mk_flag "CASE-SENSITIVE" "Force the search in case sensitive mode." ["c";"case-sensitive"] in
  let search global_options print_short installed_only case_sensitive packages =
    set_global_options global_options;
    OpamClient.list ~print_short ~installed_only ~name_only:false ~case_sensitive packages in
  Term.(pure search $global_options $print_short_flag $installed_only_flag $case_sensitive $all_packages),
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
  let client_info global_options packages =
    set_global_options global_options;
    OpamClient.info packages in
  Term.(pure client_info $global_options $all_packages),
  term_info "info" ~doc ~man

(* CONFIG *)

(* opam config [-r [-I|-bytelink|-asmlink] PACKAGE+ *)
let config =
  let doc = "Display configuration options for packages" in
  let man = [
    `S "DESCRIPTION";
    `P "This command uses opam state to output information on how to use
        installed libraries, updating the user’s $PATH, and substitute
        variables used in opam packages.";
    `P "Apart from $(b,opam config -env), most of these commands are used
        by opam internally, and thus are of limited interest for the casual
        user.";
  ] in
  let options = arg_list "OPTIONS" "List of options." in
  let is_rec  = mk_flag  "REC"     "Recursive query."                ["r";"rec"] in
  let csh     = mk_flag  "CSH"     "Use csh-compatible output mode." ["c";"csh"] in

  let compile_options =
    let incl is_rec _ packages =
      CIncludes (is_rec, List.map OpamPackage.Name.of_string packages) in
    let mk is_link is_byte is_rec csh packages =
      CCompil {
        conf_is_rec  = is_rec;
        conf_is_link = is_link;
        conf_is_byte = is_byte;
        conf_options = List.map OpamVariable.Section.Full.of_string packages;
      } in
    let bytecomp = mk true false in
    let bytelink = mk true true in
    let asmcomp  = mk false false in
    let asmlink  = mk false true in
    [
      incl    , mk_info "INCLUDES" "Return include options."          ["I"];
      bytecomp, mk_info "BYTECOMP" "Return bytecode compile options." ["bytecomp"];
      asmcomp , mk_info "ASMCOMP"  "Return assembly compile options." ["asmcomp"];
      bytelink, mk_info "BYTELINK" "Return bytecode linking options." ["bytelink"];
      asmlink , mk_info "ASMLINK"  "Return assembly compile options." ["asmlink"];
    ] in

  let var_options =
    let list _ _ _ = CList in
    let var is_rec _ options = CVariable (OpamVariable.Full.of_string (List.hd options)) in
    let subs is_rec _ options = CSubst (List.map OpamFilename.Base.of_string options) in
    [
      list, mk_info "LIST"  "Return the list of all variables defined in the \
                             listed packages (no package = all)."                  ["l";"list";"list-vars"];
      var , mk_info "VAR"   "Return the value associated with the given variable." ["v";"var"];
      subs, mk_info "SUBST" "Substitute variables in the given files."             ["s";"subst"];
    ] in

  let display_env _ csh _ = CEnv csh in
  let env =
    display_env,
    mk_info "ENVIRONMENT"
      "Return the environment variables PATH, MANPATH, OCAML_TOPLEVEL_PATH \
      and CAML_LD_LIBRARY_PATH according to the current selected \
      compiler. The output of this command is meant to be evaluated by a \
      shell, for example by doing $(b,eval `opam config -env`)." ["e";"env"] in

  let config_option =
    Arg.(value & vflag display_env (env :: compile_options @ var_options)) in

  let config global_options option is_rec csh options =
    set_global_options global_options;
    OpamClient.config (option is_rec csh options) in

  Term.(pure config $global_options $config_option $is_rec $csh$ options),
  term_info "config" ~doc ~man

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
