(* Download script for git repositories *)

open Types
open Repositories

let git_clone_or_update git_dir d =
  let url = Filename.to_string d.remote_filename in
  (* If the git repo is not already there, then clone it *)
  if not (Dirname.exists git_dir) then (
    let err = Run.command [
      "git" ; "clone" ; url ; NV.to_string d.nv
    ] in
    if err <> 0 then
      Globals.error_and_exit "%s is not a valid git url" url
  ) else (
    Git.git_fetch git_dir;
    Git.git_merge git_dir;
  )

let () =
  let d = Repositories.read_download_info () in
  let local_repo = Path.R.of_dirname d.local_path in
  let git_dir = Path.R.tmp_dir local_repo d.nv in

  git_clone_or_update git_dir d;
  Printf.printf "%s\n%!" (Dirname.to_string git_dir)
