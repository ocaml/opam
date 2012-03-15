open Namespace.Namespace
open Path
open Server
open Solver
open Client

let usage =
  Printf.sprintf "%s [init|info|config|install|update|upgrade|upload|remove]" Sys.argv.(0)

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

let port  = ref Globals.default_port
let ano_args = ref []
let () = Globals.root_path := Globals.default_opam_path

let args = Arg.align [
  "--debug"  , Arg.Set Globals.debug, " Print more debug messages";
  "--version", Arg.Unit version,      " Display version information";

  "--root"   , Arg.Set_string Globals.root_path,
  (Printf.sprintf " Change root path (default is %s)" Globals.default_opam_path)
]

let _ = Arg.parse args (fun s -> ano_args := s :: !ano_args) usage

let () =
  Globals.log "CLIENT" "Root path is %s" !Globals.root_path;

  let error msg =
    Printf.eprintf "%s\n" msg;
    nice_exit () in

  let err l = error (String.concat " " l) in

  match List.rev !ano_args with
  | [] -> nice_exit ()

  (* ocp-get init [HOSTNAME[:PORT]]*)
  | ["init"]             -> Client.init (url Globals.default_hostname Globals.default_port)
  | ["init"; host]       -> Client.init (url host Globals.default_port)
  | ["init"; host; port] ->
      let port =
        try int_of_string port
        with _ -> failwith (port ^ " is not a valid port") in
      Client.init (url host port)
      
  (* ocp-get info [PACKAGE] *)
  | ["info"]       -> Client.info None
  | ["info"; name] -> Client.info (Some (Name name))

  (* ocp-get config [-r] [-dir|-bytelink|-asmlink] PACKAGE *)
  | "config" :: l_arg ->
    let is_rec, l_arg = 
      match l_arg with
        | "-r" :: l_arg -> true, l_arg
        | _ -> false, l_arg in
    let opt, name = 
      match l_arg with
        | [ "-dir" ; name ] -> Client.Dir, name
        | [ "-bytelink" ; name ] -> Client.Bytelink, name
        | [ "-asmlink" ; name ] -> Client.Asmlink, name
        | _ -> err l_arg in
    Client.config is_rec opt (Name name)
     
  (* ocp-get install PACKAGE *)
  | ["install"; name] -> Client.install (Name name)
      
  (* ocp-get update *)
  | ["update"] -> Client.update ()
      
  (* ocp-get upgrade *)
  | ["upgrade"] -> Client.upgrade ()
      
  (* ocp-get upload PACKAGE *)
  | ["upload"; s] -> Client.upload s
    
  (* ocp-get remove PACKAGE *)
  | ["remove"; name] -> Client.remove (Name name)
      
  | l -> err l
