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

open Utils
open Types
open Path
open Solver
open Client
open SubCommand

let version () =
  Printf.printf "\
%s version %s

Copyright (C) 2012 OCamlPro - INRIA

This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.argv.(0) Globals.version;
  exit 0

let ano_args = ref []
let anon s =
  ano_args := s :: !ano_args

exception Bad of string * string

let bad_argument cmd fmt =
  Printf.ksprintf (fun msg -> raise (Bad (cmd, msg))) fmt

let noanon cmd s =
  raise (Bad (cmd, s ^ " is not expected"))

let () = Globals.root_path := Globals.default_opam_path

(* Useful for switch, which can overwrite the default verbose flag *)
let quiet = ref false

let global_args = [
  "--debug"     , Arg.Set Globals.debug, " Print more debug messages";
  "--verbose"   , Arg.Set Globals.verbose, " Display the stdout/stderr of subprocesses";
  "--quiet"     , Arg.Clear quiet, " Not display the stdout/stderr of subprocesses";
  "--version"   , Arg.Unit version,      " Display version information";
  "--yes"       , Arg.Set Globals.yes,   " Answer yes to all questions";
  "--makecmd"   , Arg.Set_string Globals.makecmd, 
    Printf.sprintf " Set the 'make' program used when compiling packages (default is %s)" !Globals.makecmd;
  "--root"      , Arg.Set_string Globals.root_path,
    (Printf.sprintf " Change root path (default is %s)" Globals.default_opam_path);
  "--no-checksums", Arg.Clear Globals.verify_checksums, " Do not verify checksums on download";
]

let parse_args fn () =
  fn (List.rev !ano_args)

let guess_repository_kind kind address =
  match kind with
  | None  ->
      if Sys.file_exists address then
        "rsync"
      else if Utils.starts_with ~prefix:"git" address
           || Utils.ends_with ~suffix:"git" address then
        "git"
      else
        Globals.default_repository_kind
  | Some k -> k

(* opam init [-kind $kind] $repo $adress *)
let init = 
  let kind = ref None in
  let comp = ref None in
  let cores = ref Globals.default_cores in
{
  name     = "init";
  usage    = "";
  synopsis = "Initial setup";
  help     = "Create the initial config files";
  specs    = [
    ("-comp" , Arg.String (fun s -> comp := Some (OCaml_V.of_string s)), " Which compiler version to use");
    ("-cores", Arg.Set_int cores   , " Set the nomber of cores");
    ("-kind" , Arg.String (fun s -> kind := Some s) , " Set the repository kind");
    ("-no-base-packages", Arg.Clear Globals.base_packages, " Do not install the base packages");
  ];
  anon;
  main     =
    parse_args (function
    | [] ->
        Client.init Repository.default !comp !cores
    | [address] ->
        let name = Globals.default_repository_name in
        let kind = guess_repository_kind !kind address in
        let repo = Repository.create ~name ~address ~kind in
        Client.init repo !comp !cores
    | [name; address] ->
        let kind = guess_repository_kind !kind address in
        let repo = Repository.create ~name ~address ~kind in
        Client.init repo !comp !cores
    | _ -> bad_argument "init" "Need a repository name and address")
}

(* opam list [PACKAGE_REGEXP]* *)
let list = 
  let short = ref false in
  let installed = ref false in
{
  name     = "list";
  usage    = "<package-regexp>*";
  synopsis = "Display the list of available packages";
  help     = "";
  specs    = [
    ("-short"    , Arg.Set short    , " Minimize the output by displaying only package name");
    ("-installed", Arg.Set installed, " Display only the list of installed packages");
  ];
  anon;
  main     = 
    parse_args (function args ->
      let print_short = !short in
      let installed_only = !installed in
      let name_only = true in
      Client.list ~print_short ~installed_only ~name_only args
    )
}

(* opam search [PACKAGE_REGEXP]* *)
let search =
  let short = ref false in
  let installed = ref false in
{
  name     = "search";
  usage    = "<package-regexp>*";
  synopsis = "Search into the package list";
  help     = "";
  specs    = [
    ("-short"    , Arg.Set short    , " Minimize the output by displaying only package name");
    ("-installed", Arg.Set installed, " Display only the list of installed packages");
  ];

  anon;
  main     = 
    parse_args (function args ->
      let print_short = !short in
      let installed_only = !installed in
      let name_only = false in
      Client.list ~print_short ~installed_only ~name_only args
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
    | l  -> List.iter (fun name -> Client.info (N.of_string name)) l)
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
] in
let mk options =
  if not !has_cmd then
    bad_argument "config"
      "Wrong options (has_cmd=%b is_rec=%b,is_link=%b,is_byte=%b)"
      !has_cmd !is_rec !is_link !is_byte
  else
    Compil {
      is_rec  = !is_rec;
      is_link = !is_link;
      is_byte = !is_byte;
      options = List.map Full_section.of_string options;
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
      | Some `Env   -> Env
      | Some `I     -> Includes (!is_rec, List.map N.of_string names)
      | Some `List  -> List_vars
      | Some `Var when List.length names = 1
                    -> Variable (Full_variable.of_string (List.hd names))
      | Some `Var   ->
          bad_argument "config" "-var takes exactly one parameter"
      | Some `Subst -> Subst (List.map Basename.of_string names)
      | None        -> mk names in
    Client.config config
}

(* opam install <PACKAGE>+ *)
let install = {
  name     = "install";
  usage    = "[package]+";
  synopsis = "Install a list of package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    if names <> [] then
      let names = List.map N.of_string names in
      Client.install (N.Set.of_list names)
    else Globals.error_and_exit "You need to specify at least one package to install."
  )
}

(* opam reinstall <PACKAGE>+ *)
let reinstall = {
  name     = "reinstall";
  usage    = "[package]+";
  synopsis = "Reinstall a list of package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    let names = List.map N.of_string names in
    Client.reinstall (N.Set.of_list names)
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
  main     = parse_args Client.update
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
    let names = List.map N.of_string names in
    Client.upgrade (N.Set.of_list names);
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
  synopsis = "Upload a package to the server";
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
    let opam = Filename.of_string !opam in
    let descr = Filename.of_string !descr in
    let archive = Filename.of_string !archive in
    let repo = if !repo = "" then None else Some !repo in
    Client.upload { opam; descr; archive } repo)
}

(* opam remove PACKAGE *)
let remove = {
  name     = "remove";
  usage    = "[package]+";
  synopsis = "Remove a list of package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (fun names ->
    if names <> [] then
      let names = List.map N.of_string names in
      Client.remove (N.Set.of_list names)
    else Globals.error_and_exit "You need to specify at least one package to remove."
  )
}

(* opam remote [-list|-add <url>|-rm <url>] *)
let remote = 
  let kind = ref None in
  let command : [`add|`list|`rm] option ref = ref None in
  let set c () = command := Some c in
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
  ];
  anon;
  main     = parse_args (fun args ->
    match !command, args with
    | Some `list, []                -> Client.remote List
    | Some `rm,   [ name ]          -> Client.remote (Rm name)
    | Some `add , [ name; address ] ->
        let kind = guess_repository_kind !kind address in
        Client.remote (Add (Repository.create ~name ~kind ~address))
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
    | ""   -> OCaml_V.of_string alias
    | comp -> OCaml_V.of_string comp in
{
  name     = "switch";
  usage    = "[compiler-name]";
  synopsis = "Manage multiple installation of compilers";
  help     = "";
  specs    = [
    ("-alias-of"        , Arg.Set_string alias_of        , " Compiler name");
    ("-no-base-packages", Arg.Clear Globals.base_packages, " Do not install the base packages");
    ("-install"         , Arg.Unit (set `install)        , " Install the given compiler");
    ("-remove"          , Arg.Unit (set `remove)         , " Remove the given compiler");
    ("-clone"           , Arg.Unit (set `clone)          , " Clone the content of the given alias");
    ("-reinstall"       , Arg.Unit (set `reinstall)      , " Reinstall the given compiler");
    ("-list"            , Arg.Unit (set `list)           , " List the available compilers");
    ("-current"         , Arg.Unit (set `current)        , " Display the current compiler");
  ];
  anon;
  main     = parse_args (function args ->
    match !command, args with
    | `install, [alias] ->
        Client.compiler_install !quiet (Alias.of_string alias) (mk_comp alias)
    | `clone, [alias] ->
        no_alias_of ();
        Client.compiler_clone  (Alias.of_string alias)
    | `remove, aliases ->
        no_alias_of ();
        List.iter (fun alias -> Client.compiler_remove (Alias.of_string alias)) aliases
    | `reinstall, [alias] ->
        no_alias_of ();
        Client.compiler_reinstall (Alias.of_string alias)
    | `list, [] ->
        no_alias_of ();
        Client.compiler_list ()
    | `current, [] ->
        no_alias_of ();
        Client.compiler_current ()
    | `switch, [alias] ->
        begin match !alias_of with
          | "" -> Client.compiler_switch !quiet (Alias.of_string alias)
          | _  -> Client.compiler_install !quiet (Alias.of_string alias) (mk_comp alias)
        end
    | _ -> bad_argument "switch" "too many arguments"
  )
}

(* opam pin [-list|<package> <version>|<package> <path>] *)
let pin =
  let list = ref false in
{
  name     = "pin";
  usage    = "<package> [<version>|<url>|none]";
  synopsis = "Pin a given package to a specific version";
  help     = "";
  specs    = [
    ("-list" ,   Arg.Set list,    " List the current status of pinned packages");
  ];
  anon;
  main     = parse_args (function
    | [] when !list    -> Client.pin_list ()
    | [name; arg]      -> Client.pin { pin_package = N.of_string name; pin_arg = pin_option_of_string arg }
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
  List.iter SubCommand.register commands;
  try ArgExt.parse ~man:"opam" global_args
  with e ->
    Globals.error "  '%s' failed\n" (String.concat " " (Array.to_list Sys.argv));
    match e with
    | Bad (cmd, msg) ->
        ArgExt.pp_print_help (ArgExt.SubCommand cmd) Format.err_formatter global_args ();
        Globals.error "%s" msg;
        exit 1;
    | Failure ("no subcommand defined" as s) ->
        ArgExt.pp_print_help ArgExt.NoSubCommand Format.err_formatter global_args ();
        Globals.error "%s" s;
        exit 2
    | Globals.Exit i -> exit i
    | e -> raise e
