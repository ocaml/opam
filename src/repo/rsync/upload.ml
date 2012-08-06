(* Upload scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )
open Types
open Repo_helpers

let () =
  let state = Repo_helpers.make_state () in
  let updates = Rsync.upload state in
  Globals.msg "%d packages uploaded %s\n" (NV.Set.cardinal updates) (NV.Set.to_string updates)
