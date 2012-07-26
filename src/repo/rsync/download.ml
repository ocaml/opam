(* Download scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
    exit 1
  )

let log fmt = Globals.log "rsync-download"

open Types
open Repo_helpers

let () =
  let state = make_state () in
  let nv = NV.of_string Sys.argv.(2) in
  Rsync.Archives.make state () nv
