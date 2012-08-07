(* Download script for git repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote_file> <package> <force>" Sys.argv.(0);
    exit 1
  )

open Types
open Repositories

let git_clone_or_update local_repo nv url =
  let git_dir = Path.R.tmp_dir local_repo nv in
  (* If the git repo is not already there, then clone it *)
  if not (Dirname.exists git_dir) then (
    let err = Run.command [ "git" ; "clone" ; Filename.to_string url ; NV.to_string nv ] in
    if err <> 0 then
      Globals.error_and_exit "%s is not a valid git url" (Filename.to_string url)
  ) else (
    Git.git_fetch git_dir;
    Git.git_merge git_dir;
  )

(*  (* Then run git-archive to get a tar.gz *)
  Dirname.in_dir git_dir (fun () ->
    let tar = NV.to_string nv ^ ".tar" in 
    let err =
      Run.commands [ 
        [ "git" ; "archive" ; "--format=tar" ; "--prefix="^NV.to_string nv^"/" ; "HEAD" ; "-o" ; tar ] ;
        [ "gzip" ; "-f" ; tar ] ;
      ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot run git-archive in %s" (Dirname.to_string git_dir)
  )
*)

let () =
  let local_repo = Path.R.of_dirname (Dirname.cwd ()) in
  let d = Repositories.read_download_info () in
  git_clone_or_update local_repo d.nv d.filename;
  let git_dir = Path.R.tmp_dir local_repo d.nv in
  Printf.printf "%s\n%!" (Dirname.to_string git_dir)
