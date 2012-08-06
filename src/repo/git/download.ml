(* Download script for git repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote_file> <package>" Sys.argv.(0);
    exit 1
  )

open Types

let git_archive t state nv url =
  let git_dir = state.git_dir nv in
  (* If the git repo is not already there, then clone it *)
  if not (Dirname.exists git_dir) then (
    Dirname.mkdir state.git_root;
    Dirname.in_dir state.git_root (fun () ->
      let err = Run.command [ "git" ; "clone" ; url ; NV.to_string nv ] in
      if err <> 0 then
        Globals.error_and_exit "%s is not a valid git url" url
    )
  );
  (* Then run git-archive to get a tar.gz *)
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

let () =
  let t = Repo_helpers.make_state () in
  let state = Git.make_state false t in

  (* Run git-archive in the right directory *)
  git_archive t state nv (Dirname.to_string t.remote_path)

  (* and copy the archive at the right place *)
  Dirname.mkdir (Path.R.archives_dir t.local_repo);
  Filename.move
    (state.git_dir nv // ((NV.to_string nv) ^ ".tar.gz"))
    (Path.R.archive t.local_repo nv)
