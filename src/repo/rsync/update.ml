(* Update scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1;
  )

open Types
open Repo_helpers

let log fmt = Globals.log Sys.argv.(0) fmt

let () =
  let state = Repo_helpers.make_state () in
  let updates = Rsync.get_updates state in
  log "updates=%s" (NV.Set.to_string updates);
  File.Updated.write (Path.R.updated state.local_repo) updates
