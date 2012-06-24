(* Init script for git repositories *)

(* Git repositories should have the following structure:
   - opam/                    contains the OPAM files
   - descr/                   contains the description files
   - url/$name.$version       contains the git url for package
                              $name.version
   - git/$name.$version/      will contain the git repo for the
                              package $name.$version when it will
                              be cloned
*)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

open Types

let remote_address = Sys.argv.(1)
let local_dir = Dirname.cwd ()

let git_clone () =
  let err =
    Run.commands [
      [ "git" ; "init" ] ;
      [ "git" ; "remote" ; "add" ; "origin" ; remote_address ] ;
    ] in
  if err <> 0 then
    Globals.error_and_exit "Cannot clone %s" remote_address

let packages () =
  let all = Filename.rec_list local_dir in
  NV.Set.of_list (Utils.filter_map NV.of_filename all)

let () =
  Run.mkdir "git";
  git_clone ();
  File.Updated.write
    (Path.R.updated (Path.R.of_path local_dir))
    (packages ())
