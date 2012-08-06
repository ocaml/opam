(* Download scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-filename> <force>" Sys.argv.(0);
    exit 1
  )

open Types
open Repo_helpers

let () =
  let state = Repo_helpers.make_download_state () in
  let local_path = Dirname.cwd () in
  let local_repo = Path.R.of_dirname local_path in
  let remote_path = Filename.dirname state.filename in
  let remote_repo = Path.R.of_dirname remote_path in
  let t = { local_path; local_repo; remote_path; remote_repo } in
  let local_file = Repo_helpers.local_of_remote_file t state.filename in
  if state.force || not (Filename.exists local_file) then
    match Rsync.rsync_file t state.filename t.local_path with
    | None       -> exit 1
    | Some (f,_) -> Printf.printf "%s\n%!" (Filename.to_string f)
