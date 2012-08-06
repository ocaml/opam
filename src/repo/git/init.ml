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
open Repo_helpers
open Git

let git_init t =
  let repo = Dirname.to_string t.remote_path in
  let err =
    Run.commands [
      [ "git" ; "init" ] ;
      [ "git" ; "remote" ; "add" ; "origin" ; repo ] ;
    ] in
  if err <> 0 then
    Globals.error_and_exit "Cannot clone %s" repo

let () =
  let t = Repo_helpers.make_state () in
  git_init t;
  Dirname.mkdir (Path.R.tmp_root t.local_repo)
