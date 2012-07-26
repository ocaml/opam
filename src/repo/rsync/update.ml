(* Update scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1;
  )

open Types
open Repo_helpers

let () =
  let state = Repo_helpers.make_state () in
  let updates = Rsync.Updates.get state () in
  File.Updated.write (Path.R.updated state.local_repo) updates
