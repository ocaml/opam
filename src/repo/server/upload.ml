(* Upload script for OPAM server repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

open Types
open Protocol
open Unix

let local_path = Path.R.of_path (Dirname.of_string (Run.cwd ()))
let remote_address =
  try inet_addr_of_string Sys.argv.(1)
  with _ ->
    (gethostbyname Sys.argv.(1)).h_addr_list.(0)

let log fmt = Globals.log Sys.argv.(0) fmt

let () =
  let opams = Filename.list (Path.R.upload_opam_dir local_path) in
  let descrs = Filename.list (Path.R.upload_descr_dir local_path) in
  let archives = Filename.list (Path.R.upload_archives_dir local_path) in
  let rec aux = function
    | []   , []   , []    -> ()
    | o::ol, d::dl, a::al ->
        let opam = File.OPAM.read o in
        let descr = File.Descr.read d in
        let archive = Filename.read a in
        let name = File.OPAM.name opam in
        if Key.exists local_path name then (
          log "Package %s exists, updating a new version" (N.to_string name);
          let key = Key.read local_path name in
          Client.new_version remote_address opam descr archive key
        ) else (
          let key = Client.new_package remote_address opam descr archive in
          Key.write local_path name key
        );
        aux (ol, dl, al)
    | _ -> Globals.error_and_exit "Missing files for upload" in
  aux (opams, descrs, archives)
          
    

