(* Download script for git repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
    exit 1
  )

open Types

let local_path = Dirname.cwd ()
let local_repo = Path.R.of_dirname local_path
let remote_address = Sys.argv.(1)
let package = Sys.argv.(2)
let nv = NV.of_string package

let git_root = local_path / "git"
let git_dir = git_root / package

let git_archive () =
  (* If the git repo is not already there, then clone it *)
  if not (Dirname.exists git_dir) then (
    let urls = File.URL.read (Path.R.url local_repo nv) in
    Dirname.mkdir git_root;
    Dirname.in_dir git_root (fun () ->
      let url = match urls with h::_ -> Filename.to_string h | _ -> assert false in
      let err = Run.command [ "git" ; "clone" ; url ; package ] in
      if err <> 0 then
        Globals.error_and_exit "%s is not a valid git url" url
    )
  );
  (* Then run git-archive to get a tar.gz *)
  Dirname.in_dir git_dir (fun () ->
    let tar = package ^ ".tar" in 
    let err =
      Run.commands [ 
        [ "git" ; "archive" ; "--format=tar" ; "--prefix="^package^"/" ; "HEAD" ; "-o" ; tar ] ;
        [ "gzip" ; "-f" ; tar ] ;
      ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot run git-archive in %s" (Dirname.to_string git_dir)
  )

let () =
  (* Run git-archive in the right directory *)
  git_archive ();

  (* and copy the archive at the right place *)
  Dirname.mkdir (Path.R.archives_dir local_repo);
  Filename.move
    (git_dir // (package ^ ".tar.gz"))
    (Path.R.archive local_repo nv)
