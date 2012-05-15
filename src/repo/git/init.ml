(* Init script for git repositories *)

(* Git repositories should have the following structure:
   - opam/                 contains the OPAM files
   - descr/                contains the description files
   - url/$name.$version    contains the git url for package
                           $name.version
   - git/$name.$version/   will contain the git repo for the
                           package $name.$version when it will
                           be cloned
*)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: opam-git-init <remote-address>";
    exit 1
  )

let remote_address = Sys.argv.(1)

let git_clone () =
  let err =
    Run.command "git clone %s ./" remote_address in
  exit err

let () =
  git_clone ()
