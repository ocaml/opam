(* Download script for OPAM server repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
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

let package = NV.of_string Sys.argv.(2)

let () =
  let archive = Client.get_archive remote_address package in
  Filename.write (Path.R.archive local_path package) archive
