(* Script to check that a given repository is well-typed (or well-parsed) *)
open Types
open Printf
open File

let () =
  let usage = Printf.sprintf "Usage: %s" Sys.argv.(0) in
  let specs = [] in
  let ano x =
    Printf.eprintf "%s: invalid argument" x in
  Arg.parse specs ano usage

let () = 
  let t = Path.R.of_dirname (Dirname.cwd ()) in
  NV.Set.iter (fun nv -> 
    Globals.msg "Processing %s\n" (NV.to_string nv);

    (** Descr *)
    let descr = Path.R.descr t nv in
    Descr.write descr (Descr.read descr);

    (** OPAM *)
    let opam = Path.R.opam t nv in
    OPAM.write opam (OPAM.read opam);

    (** URL *)
    let url = Path.R.url t nv in
    if Filename.exists url then (
      URL.write url (URL.read url);
    );

    (** Dot_install *)
    let dot_install = Path.R.files t nv // (N.to_string (NV.name nv) ^ ".install") in
    if Filename.exists dot_install then (
      Dot_install.Raw.write dot_install (Dot_install.Raw.read dot_install);
    );

    (** Comp *)
    (* TODO *)

  ) (Path.R.available_packages t)
