(* Download script for git repositories *)

open Types
open Repositories

let log fmt = Globals.log "git-download" fmt

let git_clone_or_update git_dir d =
  log "git-clone-or-update %s" (Dirname.to_string git_dir); 
  let url = Filename.to_string (Filename.create d.remote_dir d.basename) in
  (* If the git repo is not already there, then clone it *)
  if not (Dirname.exists git_dir) then (
    let err = Run.command [
      "git" ; "clone" ; "-q"; url ; NV.to_string d.nv
    ] in
    if err <> 0 then
      Globals.error_and_exit "%s is not a valid git url" url
  ) else if Git.git_diff git_dir <> [] then (
    Git.git_fetch git_dir;
    Git.git_merge git_dir;
  )

let () =
  let d = Repositories.read_download_info () in
  let git_dir = d.local_dir / NV.to_string d.nv in
  git_clone_or_update git_dir d;
  Printf.printf "%s\n%!" (Dirname.to_string git_dir)
