(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

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
    Sys.argv.(0) Globals.version

let ano_args = ref []
let anon s =
  ano_args := s :: !ano_args

exception Bad of string * string

let bad_argument cmd fmt =
  Printf.kprintf (fun msg -> raise (Bad (cmd, msg))) fmt

let noanon cmd s =
  raise (Bad (cmd, s ^ " is not expected"))

let () = Globals.root_path := Globals.default_opam_path

let global_args = [
  "--debug"  , Arg.Set Globals.debug, " Print more debug messages";
  "--version", Arg.Unit version,      " Display version information";
  "--root"   , Arg.Set_string Globals.root_path,
  (Printf.sprintf " Change root path (default is %s)" Globals.default_opam_path)
]

let parse_args fn () =
  fn (List.rev !ano_args)

(* opam init [-kind $kind] $repo $adress *)
let kind = ref Globals.default_repository_kind
let init = {
  name     = "init";
  usage    = "";
  synopsis = "Initial setup";
  help     = "Create the initial config files";
  specs    = [
    ("--kind", Arg.Set_string kind, " Set the repository kind")
  ];
  anon;
  main     =
    parse_args (function
    | [] ->
        Client.init Repository.default
    | [name; address]  ->
        Client.init (Repository.create ~name ~address ~kind:!kind)
    | _ -> bad_argument "init" "Need a repository name and address")
}

(* opam list *)
let list = {
  name     = "list";
  usage    = "";
  synopsis = "Display information about all available packages";
  help     = "";
  specs    = [];
  anon     = noanon "list";
  main     = Client.list;
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
let has_cmd = ref false
let is_rec = ref false
let is_link = ref false
let is_byte = ref false
let bytecomp () =  has_cmd := true; is_byte := true ; is_link := false
let bytelink () =  has_cmd := true; is_byte := true ; is_link := true
let asmcomp  () =  has_cmd := true; is_byte := false; is_link := false
let asmlink  () =  has_cmd := true; is_byte := false; is_link := true
let command = ref None
let set cmd () =
  has_cmd := true;
  command := Some cmd
let specs = [
  ("-r"        , Arg.Set is_rec       , " Recursive search (large)");
  ("-I"        , Arg.Unit (set `I)    , " Display native compile options");
  ("-bytecomp" , Arg.Unit bytecomp    , " Display bytecode compile options");
  ("-asmcomp"  , Arg.Unit asmcomp     , " Display native link options");
  ("-bytelink" , Arg.Unit bytelink    , " Display bytecode link options");
  ("-asmlink"  , Arg.Unit asmlink     , " Display native link options");
  ("-list-vars", Arg.Unit (set `List) , " Display the contents of all available variables");
  ("-var"      , Arg.Unit (set `Var)  , " Display the content of a variable");
  ("-subst"    , Arg.Unit (set `Subst), " Substitute variables in files");
]
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
    }
let config = {
  name     = "config";
  usage    = "[...]+";
  synopsis = "Display configuration options for packages";
  help     = "";
  specs;
  anon;
  main = function () ->
    let names = List.rev !ano_args in
    let config = match !command with
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

(* opam install PACKAGE *)
let install = {
  name     = "install";
  usage    = "[package]+";
  synopsis = "Install a package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter (fun name -> Client.install (N.of_string name)))
}

(* opam update *)
let update = {
  name     = "update";
  usage    = "[package]+";
  synopsis = "Update the installed package to latest version";
  help     = "";
  specs    = [];
  anon     = noanon "update";
  main     = Client.update;
}

(* opam upgrade *)
let upgrade = {
  name     = "upgrade";
  usage    = "";
  synopsis = "Upgrade the list of available package";
  help     = "";
  specs    = [];
  anon     = noanon "upgrade";
  main     = Client.upgrade;
}

(* opam upload PACKAGE *)
let opam = ref ""
let descr = ref ""
let archive = ref ""
let repo = ref ""
let upload = {
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
  usage    = "";
  synopsis = "Remove a package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter (fun n -> Client.remove (N.of_string n)));
}

(* opam remote [-list|-add <url>|-rm <url>] *)
let kind = ref Globals.default_repository_kind
let command : [`add|`list|`rm] option ref = ref None
let set c () = command := Some c
let remote = {
  name     = "remote";
  usage    = "[-list|add <name> <address>|rm <name>]";
  synopsis = "Manage remote servers";
  help     = "";
  specs    = [
    ("-list" , Arg.Unit (set `list), " List the repositories");
    ("-add"  , Arg.Unit (set `add) , " Add a new repository");
    ("-rm"   , Arg.Unit (set `rm)  , " Remove a remote repository");
    ("--kind", Arg.Set_string kind , " (optional) Specify the repository kind");
  ];
  anon;
  main     = parse_args (fun args ->
    match !command, args with
    | Some `list, []                -> Client.remote List
    | Some `rm,   [ name ]          -> Client.remote (Rm name)
    | Some `add , [ name; address ] ->
        Client.remote (Add (Repository.create ~name ~kind:!kind ~address))
    | None, _  -> bad_argument "remote" "Command missing [-list|-add|-rm]"
    | _        -> bad_argument "remote" "Wrong arguments")
}

let switch = {
  name     = "switch";
  usage    = "[compiler-name]";
  synopsis = "Switch to an other compiler version";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (function
    |[]      -> bad_argument "switch" "Compiler name is missing"
    | [name] -> Client.switch (OCaml_V.of_string name)
    | _      -> bad_argument "switch" "Too many compiler names")
}

let commands = [
  init;
  list;
  info;
  config;
  install;
  update;
  upgrade;
  upload;
  remove;
  remote;
  switch;
]

let () =
  List.iter SubCommand.register commands;
  try ArgExt.parse global_args
  with e ->
    Globals.error "'%s' failed" (String.concat " " (Array.to_list Sys.argv));
    match e with
    | Bad (cmd, msg) ->
        ArgExt.pp_print_help (ArgExt.SubCommand cmd) Format.err_formatter global_args ();
        Globals.error "%s" msg
    | Failure ("no subcommand defined" as s) ->
        ArgExt.pp_print_help ArgExt.NoSubCommand Format.err_formatter global_args ();
        Globals.error "%s" s
    | Globals.Error -> exit 1
    | e -> raise e
