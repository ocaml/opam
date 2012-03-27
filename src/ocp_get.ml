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

open Namespace.Namespace
open Path
open Server
open Solver
open Client
open SubCommand
open Uri

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

let bad_argument () =
  raise (Arg.Bad "Invalid argument")

let noanon s =
  raise (Arg.Bad (s ^ ": Invalid argument"))

let () = Globals.root_path := Globals.default_opam_path

let global_args = [
  "--debug"  , Arg.Set Globals.debug, " Print more debug messages";
  "--version", Arg.Unit version,      " Display version information";

  "--root"   , Arg.Set_string Globals.root_path,
  (Printf.sprintf " Change root path (default is %s)" Globals.default_opam_path)
]

let parse_args fn () =
  fn (List.rev !ano_args)

(* ocp-get init [HOSTNAME[:PORT]]*)
let init =
  let git_repo = ref false in
  {
  name     = "init";
  usage    = "";
  synopsis = "Initial setup";
  help     = "Create the initial config files";
  specs    = [
    ("--git-repo", Arg.Set git_repo, "Get in sync with a git repository of packages")
  ];
  anon;
  main     =
    parse_args (function
    | []            -> Client.init [url ~port:Globals.default_port Globals.default_hostname]
    | [ host]       ->
        if !git_repo then
          Client.init [url ~uri:Git host]
        else
          Client.init [url ~port:Globals.default_port host]
    | [ host; port] ->
        let port =
          try int_of_string port
          with _ -> failwith (port ^ " is not a valid port") in
        Client.init [url ~port host]
    | _ -> bad_argument ())
}

(* ocp-get list *)
let list = {
  name     = "list";
  usage    = "";
  synopsis = "Display information about all available packages";
  help     = "";
  specs    = [];
  anon     = noanon;
  main     = Client.list;
}

(* ocp-get info [PACKAGE] *)
let info = {
  name     = "info";
  usage    = "[package]+";
  synopsis = "Display information about specific packages";
  help     = "";
  specs    = [];
  anon;
  main     =
    parse_args (function
    | [] -> raise (Arg.Bad "Missing package argument")
    | l  -> List.iter (fun name -> Client.info (Name name)) l)
}

(* ocp-get config [R] [Include|Bytelink|Asmlink] PACKAGE *)
let config =
  let recursive = ref false in
  let command = ref None in
  let set_include  () = command := Some Client.Include in
  let set_asmlink  () = command := Some Client.Asmlink in
  let set_bytelink () = command := Some Client.Bytelink in
  {
    name     = "config";
    usage    = "[package]+";
    synopsis = "Display configuration options for packages";
    help     = "";
    specs    = [
      ("-r", Arg.Set recursive           , " Recursive search");
      ("-I", Arg.Unit set_include        , " Display include options");
      ("-bytelink", Arg.Unit set_bytelink, " Display bytecode link options");
      ("-asmlink" , Arg.Unit set_asmlink , " Display native link options");
    ];
    anon;
    main =
      function () ->
        let names = List.rev !ano_args in
        let command = match !command with
        | None   -> raise (Arg.Bad "Missing command [-I|-asmlink|-bytelink]")
        | Some c -> c in
        Client.config !recursive command (List.map (fun n -> Name n) names);
  }

(* ocp-get install PACKAGE *)
let install = {
  name     = "install";
  usage    = "[package]+";
  synopsis = "Install a package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter (fun name -> Client.install name))
}

(* ocp-get update *)
let update = {
  name     = "update";
  usage    = "[package]+";
  synopsis = "Update the installed package to latest version";
  help     = "";
  specs    = [];
  anon     = noanon;
  main     = Client.update;
}

(* ocp-get upgrade *)
let upgrade = {
  name     = "upgrade";
  usage    = "";
  synopsis = "Upgrade the list of available package";
  help     = "";
  specs    = [];
  anon     = noanon;
  main     = Client.upgrade;
}

(* ocp-get upload PACKAGE *)
let upload = {
  name     = "upload";
  usage    = "";
  synopsis = "Upload a package to the server";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter Client.upload);
}

(* ocp-get remove PACKAGE *)
let remove = {
  name     = "remove";
  usage    = "";
  synopsis = "Remove a package";
  help     = "";
  specs    = [];
  anon;
  main     = parse_args (List.iter (fun n -> Client.remove (Name n)));
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
]

let () =
  Globals.log "CLIENT" "Root path is %s" !Globals.root_path;
  List.iter SubCommand.register commands;
  ArgExt.parse global_args
