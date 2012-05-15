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

open Sys
open Unix
open File

let default_root_path = Filename.concat Globals.home ".opam-server"

let usage =
  Printf.sprintf "%s -p <port> [--debug]" Sys.argv.(0)

let nice_exit () =
  Printf.eprintf "%s\n%!" usage;
  exit 2

let version () =
  Printf.printf "\
%s version %s

Copyright (C) 2012 OCamlPro - INRIA

This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.argv.(0) Globals.version

let port  = ref Protocol.default_port
let host  = ref (inet_addr_of_string "127.0.0.1")
let set_host h =
  try host := inet_addr_of_string h
  with exn -> raise (Arg.Bad ("invalid [-i] IP: " ^ h))
  
let _ =
  Globals.root_path := default_root_path

let args = Arg.align [
  "-p"       , Arg.Set_int port     , " Set up the listening port (default: 9999)";
  "-i"       , Arg.String set_host  , " Set up the listening IP address (default: "
                                      ^(Unix.string_of_inet_addr !host)^")";
  "--debug"  , Arg.Set Globals.debug, " Print more debug messages";
  "--version", Arg.Unit version     , " Display version information";

  "--root"   , Arg.Set_string Globals.root_path,
  (Printf.sprintf " Change root path (default is %s)" default_root_path)
]

let _ = Arg.parse args (fun s -> Printf.eprintf "%s: Unknown\n" s) usage

(* Each time a client request is coming, a process is forked to run
   this function. Hence, we really need to init the random generator
   with a new seed, otherwise, all forked processes will have the same
   id (the first random number with the given seed). *)
let process_connection stdin stdout =
  Random.self_init();
  let id = string_of_int (Random.int 1024) in
  Daemon.process (stdin, stdout) (Daemon.process_request id)

let run_server () =
  let addr = ADDR_INET (!host, !port) in
  if !Globals.debug then
    Globals.msg "Root path is %s.\nListening on port %d (%s) ...\n%!"
      !Globals.root_path !port (string_of_inet_addr !host);
  establish_server process_connection addr

let _ =
  handle_unix_error run_server ()
