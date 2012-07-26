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
  let upload_path = Path.R.upload state.local_repo in
  let upload_repo = Path.R.of_path upload_path in
  let state = {
    local_path = state.remote_path;
    local_repo = state.remote_repo;
    remote_path = upload_path;
    remote_repo = upload_repo;
  } in
  let updates = Rsync.Updates.get state () in
  let _archives = Rsync.Sync.dir state () (Path.R.archive_dir upload_repo) in
  Globals.msg "%d packages uploaded %s\n" (NV.Set.cardinal updates) (NV.Set.to_string updates)
