(* Upload script for OPAM server repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

open Types
open Protocol
open Unix

let local_repo = Path.R.of_dirname (Dirname.cwd ())
let upload_path = Path.R.upload_dir local_repo
let upload_repo = Path.R.of_dirname upload_path

let remote_address =
  try inet_addr_of_string Sys.argv.(1)
  with _ ->
    (gethostbyname Sys.argv.(1)).h_addr_list.(0)

let log fmt = Globals.log Sys.argv.(0) fmt

let () =
  let packages = Path.R.available_packages upload_repo in
  let upload_package nv =
    let opam = File.OPAM.read (Path.R.opam upload_repo nv) in
    let descr = File.Descr.read (Path.R.descr upload_repo nv) in
    let archive = Filename.read (Path.R.archive upload_repo nv) in
    let name = File.OPAM.name opam in
    if Key.exists local_repo name then (
      log "Package %s exists, updating a new version" (N.to_string name);
      let key = Key.read local_repo name in
      Client.new_version remote_address opam descr archive key
    ) else (
      let key = Client.new_package remote_address opam descr archive in
      Key.write local_repo name key
    ) in
  NV.Set.iter upload_package packages
