open Sys
open Unix
open File
open Server

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

let debug = ref false

let port  = ref Globals.default_port
let set_port p = port := p

let args = Arg.align [
  "-p"       , Arg.Int set_port, " Set up the listening port (default is 9999)";
  "--debug"  , Arg.Set debug,    " Print more debug messages";
  "--version", Arg.Unit version, " Display version information";
]

let _ = Arg.parse args (fun s -> Printf.eprintf "%s: Unknown\n" s) usage

let server fn =
  let host = (gethostbyname(gethostname ())).h_addr_list.(0) in 
  let addr = ADDR_INET (host, !port) in
  if !debug then
    Printf.printf "Listening on port %d (%s) ...\n%!"
      !port (string_of_inet_addr host);

  establish_server fn addr

let fn stdin stdout =
  let print s = Printf.eprintf "%s\n%!" s in
  match (input_value stdin : api) with
  | GetList t                     -> print "getList"
  | GetOpam (t, name_version)     -> print "GetOpam"
  | GetArchive (t, opam)          -> print "GetArchive"
  | NewArchive (t, opam, archive) -> print "NewArchive"

let _ =
  handle_unix_error server fn

















