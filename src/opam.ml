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
let recursive = ref false
let command = ref None
let set c   () = command := Some c
let set_rec () = recursive := true
let specs = [
  ("-r"       , Arg.Unit set_rec        , " Recursive search (large)");
  ("-I"       , Arg.Unit (set `Include) , " Display native compile options");
  ("-bytecomp", Arg.Unit (set `Bytecomp), " Display bytecode compile options");
  ("-asmcomp" , Arg.Unit (set `Asmcomp) , " Display native link options");
  ("-bytelink", Arg.Unit (set `Bytelink), " Display bytecode link options");
  ("-asmlink" , Arg.Unit (set `Asmlink) , " Display native link options");
]
let args n =
  (* XXX: big hack *)
  let nv = NV.of_string n in
  NV.name nv, V.to_string (NV.version nv)
let mk options =
  Compil { recursive = !recursive; options }
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
      | None   -> 
          bad_argument
            "config"  "Missing command [%s]" 
            (String.concat "|" (List.map (fun (s,_,_) -> s) specs))
      | Some `Include  -> mk (Includes (List.map N.of_string names))
      | Some `Bytecomp -> mk (Bytecomp (List.map args names))
      | Some `Bytelink -> mk (Bytelink (List.map args names))
      | Some `Asmcomp  -> mk (Asmcomp (List.map args names))
      | Some `Asmlink  -> mk (Asmlink (List.map args names)) in
    Client.config config
}

(* ocp-get install PACKAGE *)
let install = {
  name     = "install";
  usage    = "[package]+";
  synopsis = "Install a package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter (fun name -> Client.install (N.of_string name)))
}

(* ocp-get update *)
let update = {
  name     = "update";
  usage    = "[package]+";
  synopsis = "Update the installed package to latest version";
  help     = "";
  specs    = [];
  anon     = noanon "update";
  main     = Client.update;
}

(* ocp-get upgrade *)
let upgrade = {
  name     = "upgrade";
  usage    = "";
  synopsis = "Upgrade the list of available package";
  help     = "";
  specs    = [];
  anon     = noanon "upgrade";
  main     = Client.upgrade;
}

(* ocp-get upload PACKAGE *)
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
    let repo = if !repo = "" then None else Some (Repository.of_string !repo) in
    Client.upload { opam; descr; archive } repo)
}

(* ocp-get remove PACKAGE *)
let remove = {
  name     = "remove";
  usage    = "";
  synopsis = "Remove a package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter (fun n -> Client.remove (N.of_string n)));
}

(* ocp-get remote [-list|-add <url>|-rm <url>] *)
let remote =
  let git_repo = ref false in
  {
  name     = "remote";
  usage    = "[list|add <url>|add-git <url>|rm <url>]";
  synopsis = "Manage remote servers";
  help     = "";
  specs    = [
    ("--git-repo", Arg.Set git_repo, " Consider the given url as a git repository")
  ];
  anon;
  main     = parse_args (function
    | [ "list" ]     -> Client.remote List
    | [ "add"; url ] -> Client.remote (Add url)
    | [ "rm" ; url ] -> Client.remote (Rm url)
    | _              -> bad_argument "remote" "action missing")
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
  Globals.log "CLIENT" "Root path is %s" !Globals.root_path;
  List.iter SubCommand.register commands;
  try ArgExt.parse global_args
  with
  | Bad (cmd, msg) ->
    ArgExt.pp_print_help (ArgExt.SubCommand cmd) Format.err_formatter global_args ();
    Printf.eprintf "ERROR: %s\n%!" msg
  | Failure ("no subcommand defined" as s) ->
    ArgExt.pp_print_help ArgExt.NoSubCommand Format.err_formatter global_args ();
    Printf.eprintf "ERROR: %s\n%!" s
