(* Download script for git repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote_file> <nv> <package>" Sys.argv.(0);
    exit 1
  )

open Types
open Repo_helpers

let git_archive state basename =
  let git_dir = Dirname.cwd () / basename in
  let url = Filename.to_string state.filename in
  (* If the git repo is not already there, then clone it *)
  if not (Dirname.exists git_dir) then (
    let err = Run.command [ "git" ; "clone" ; url ; basename ] in
    if err <> 0 then
      Globals.error_and_exit "%s is not a valid git url" url
  );
  (* Then run git-archive to get a tar.gz *)
  Dirname.in_dir git_dir (fun () ->
    let tar = basename ^ ".tar" in 
    let err =
      Run.commands [ 
        [ "git" ; "archive" ; "--format=tar" ; "--prefix="^basename^"/" ; "HEAD" ; "-o" ; tar ] ;
        [ "gzip" ; "-f" ; tar ] ;
      ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot run git-archive in %s" (Dirname.to_string git_dir)
  )

let () =
  let state = Repo_helpers.make_download_state () in
  let basename = Basename.to_string (Filename.basename state.filename) in
  git_archive state basename;
  let archive = Dirname.cwd () / basename // (basename ^ ".tar.gz") in
  Printf.printf "%s\n%!" (Filename.to_string archive)
