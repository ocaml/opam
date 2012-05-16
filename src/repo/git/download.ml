(* Download script for git repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
    exit 1
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)
let package = Sys.argv.(2)

let (/) = Filename.concat

let git_archive () =
  let dirname = local_path / "git" / package in
  (* If the git repo is not already there, then clone it *)
  if not (Sys.file_exists dirname) then (
    let url =
      let p = local_path / "url" / package in
      if Sys.file_exists p then
        Run.read p
      else
        Globals.error_and_exit "Cannot find %s" p in
    Run.mkdir "git";
    Run.in_dir (local_path / "git") (fun () ->
      let err = Run.command "git clone %s %s" url package in
      if err <> 0 then
        Globals.error_and_exit "%s is not a valid git url" url
    )
  );
  (* Then run git-archive to get a tar.gz *)
  Run.in_dir dirname (fun () ->
    let err =
      Run.command "git archive --format=tar --prefix=%s HEAD | gzip > %s.tar.gz" package package in
    if err <> 0 then
      Globals.error_and_exit "Cannot run git-archive in %s" dirname
  )

let () =
  (* Run git-archive in the right directory *)
  git_archive ();

  (* and copy the archive at the right place *)
  Run.mkdir (local_path / "archive");
  Run.copy
    (local_path / "git" / package / package ^ ".tar.gz")
    (local_path / "archive" / package ^ ".tar.gz")
