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
open SubCommand

let ano_args = ref []
let anon s =
  ano_args := s :: !ano_args

exception Bad of string * string

let bad_argument cmd fmt =
  Printf.ksprintf (fun msg -> raise (Bad (cmd, msg))) fmt

let noanon cmd s =
  raise (Bad (cmd, s ^ " is not expected"))

(* Useful for switch, which can overwrite the default verbose flag *)
let quiet = ref false

let set_root_dir dir =
  OpamGlobals.root_dir := OpamSystem.real_path dir

let set_switch switch =
  OpamGlobals.switch := Some switch

let global_args = [
  "--debug"     , Arg.Set OpamGlobals.debug   , " Print internal debug messages (very verbose)";
  "--verbose"   , Arg.Set OpamGlobals.verbose , " Display the output of subprocesses";
  "--quiet"     , Arg.Clear quiet         , " Do not display the output of subprocesses";
  "--version"   , Arg.Unit OpamVersion.message, " Display version information";
  "--switch"    , Arg.String set_switch       , " Use the given alias instead of looking into the config file";
  "--yes"       , Arg.Set OpamGlobals.yes     , " Answer yes to all questions";
  "--makecmd"   , Arg.String (fun s -> OpamGlobals.makecmd := lazy s),
    Printf.sprintf " Set the 'make' program used when compiling packages";
  "--root"      , Arg.String set_root_dir,
    (Printf.sprintf " Change root path (default is %s)" OpamGlobals.default_opam_dir);
  "--no-checksums", Arg.Clear OpamGlobals.verify_checksums, " Do not verify checksums on download";
  "--keep-build-dir", Arg.Set OpamGlobals.keep_build_dir, " Keep the build directory";
]

let parse_args fn () =
  fn (List.rev !ano_args)

let guess_repository_kind kind address =
  match kind with
  | None  ->
      if Sys.file_exists address then
        "rsync"
      else if OpamMisc.starts_with ~prefix:"git" address
           || OpamMisc.ends_with ~suffix:"git" address then
        "git"
      else
        OpamGlobals.default_repository_kind
  | Some k -> k

(* opam init [-kind $kind] $repo $adress *)
let init =
  let kind = ref None in
  let comp = ref None in
  let cores = ref OpamGlobals.default_cores in
  let repo_priority = 0 in
{
  name     = "init";
  usage    = "";
  synopsis = "Initial setup";
  help     = "Create the initial config files";
  specs    = [
    ("-comp" , Arg.String (fun s -> comp := Some (OpamCompiler.of_string s)), " Which compiler version to use");
    ("-cores", Arg.Set_int cores   , " Set the number of cores");
    ("-kind" , Arg.String (fun s -> kind := Some s) , " Set the repository kind");
    ("-no-base-packages", Arg.Clear OpamGlobals.base_packages, " Do not install the base packages");
  ];
  anon;
  main     =
    parse_args (function
    | [] ->
        OpamClient.init OpamRepository.default !comp !cores
    | [address] ->
        let repo_name = OpamRepositoryName.default in
        let repo_kind = guess_repository_kind !kind address in
        let repo_address = OpamRepository.repository_address address in
        let repo = { repo_name; repo_kind; repo_address; repo_priority } in
        OpamClient.init repo !comp !cores
    | [name; address] ->
        let repo_name = OpamRepositoryName.of_string name in
        let repo_kind = guess_repository_kind !kind address in
        let repo_address = OpamRepository.repository_address address in
        let repo = { repo_name; repo_address; repo_kind; repo_priority } in
        OpamClient.init repo !comp !cores
    | _ -> bad_argument "init" "Need a repository name and address")
}

(* opam list [PACKAGE_REGEXP]* *)
let list =
  let print_short = ref false in
  let installed_only = ref false in
{
  name     = "list";
  usage    = "<package-regexp>*";
  synopsis = "Display the list of available packages";
  help     = "";
  specs    = [
    ("-short"    , Arg.Set print_short   , " Minimize the output by displaying only package name");
    ("-installed", Arg.Set installed_only, " Display only the list of installed packages");
  ];
  anon;
  main     =
    parse_args (function args ->
      let print_short = !print_short in
      let installed_only = !installed_only in
      OpamClient.list ~print_short ~installed_only args
    )
}

(* opam search [PACKAGE_REGEXP]* *)
let search =
  let print_short = ref false in
  let installed_only = ref false in
  let case_sensitive = ref false in
{
  name     = "search";
  usage    = "<package-regexp>*";
  synopsis = "Search into the package list";
  help     = "";
  specs    = [
    ("-short"    , Arg.Set print_short   , " Minimize the output by displaying only package name");
    ("-installed", Arg.Set installed_only, " Display only the list of installed packages");
    ("-case-sensitive", Arg.Set case_sensitive, " Force the search in case sensitive (insensitive by default)");
  ];

  anon;
  main     =
    parse_args (function args ->
      let print_short = !print_short in
      let installed_only = !installed_only in
      let case_sensitive = !case_sensitive in
      OpamClient.list ~print_short ~installed_only ~name_only:false ~case_sensitive args
    )
}

(* opam info [PACKAGE] *)
let info = {
  name     = "info";
  usage    = "[package]+";
  synopsis = "Display information about specific packages";
  help     = "";
  specs    = [];
  anon;
  main     =
    parse_args (function
    | [] -> bad_argument "info" "Missing package argument"
    | l  -> OpamClient.info l)
}

(* opam config [-r [-I|-bytelink|-asmlink] PACKAGE+ *)
let config =
let has_cmd = ref false in
let is_rec = ref false in
let is_link = ref false in
let is_byte = ref false in
let bytecomp () =  has_cmd := true; is_byte := true ; is_link := false in
let bytelink () =  has_cmd := true; is_byte := true ; is_link := true in
let asmcomp  () =  has_cmd := true; is_byte := false; is_link := false in
let asmlink  () =  has_cmd := true; is_byte := false; is_link := true in
let command = ref None in
let csh = ref false in
let set cmd () =
  has_cmd := true;
  command := Some cmd in
let specs = [
  ("-r"        , Arg.Set is_rec       , " Recursive search");
  ("-I"        , Arg.Unit (set `I)    , " Display include options");
  ("-bytecomp" , Arg.Unit bytecomp    , " Display bytecode compile options");
  ("-asmcomp"  , Arg.Unit asmcomp     , " Display native compile options");
  ("-bytelink" , Arg.Unit bytelink    , " Display bytecode link options");
  ("-asmlink"  , Arg.Unit asmlink     , " Display native link options");
  ("-list-vars", Arg.Unit (set `List) , " Display the contents of all available variables");
  ("-var"      , Arg.Unit (set `Var)  , " Display the content of a variable");
  ("-subst"    , Arg.Unit (set `Subst), " Substitute variables in files");
  ("-env"      , Arg.Unit (set `Env)  , " Display the compiler environment variables");
  ("-c"        , Arg.Set csh          , " Use csh-compatible output mode");
] in
let mk options =
  if not !has_cmd then
    bad_argument "config"
      "Wrong options (has_cmd=%b is_rec=%b,is_link=%b,is_byte=%b)"
      !has_cmd !is_rec !is_link !is_byte
  else
    CCompil {
      conf_is_rec  = !is_rec;
      conf_is_link = !is_link;
      conf_is_byte = !is_byte;
      conf_options = List.map OpamVariable.Section.Full.of_string options;
    } in
{
  name     = "config";
  usage    = "[...]+";
  synopsis = "Display configuration options for packages";
  help     = "";
  specs;
  anon;
  main = function () ->
    let names = List.rev !ano_args in
    let config = match !command with
      | Some `Env   -> CEnv !csh
      | Some `I     -> CIncludes (!is_rec, List.map OpamPackage.Name.of_string names)
      | Some `List  -> CList
      | Some `Var when List.length names = 1
                    -> CVariable (OpamVariable.Full.of_string (List.hd names))
      | Some `Var   ->
          bad_argument "config" "-var takes exactly one parameter"
      | Some `Subst -> CSubst (List.map OpamFilename.Base.of_string names)
      | None        -> mk names in
    OpamClient.config config
}

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

let () =
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
