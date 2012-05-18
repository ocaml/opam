(* Init script for git repositories *)

(* Git repositories should have the following structure:
   - opam/                     contains the OPAM files
   - descr/                   contains the description files
   - url/$name.$version       contains the git url for package
                              $name.version
   - packages/$name.$version/ will contain the git repo for the
                              package $name.$version when it will
                              be cloned
*)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

let remote_address = Sys.argv.(1)

let git_clone () =
  let err =
    Run.commands [
      "git init";
      Printf.sprintf "git remote add origin %s" remote_address;
      "git pull origin master"
    ] in
  exit err

let () =
  Run.mkdir "packages";
  git_clone ()
