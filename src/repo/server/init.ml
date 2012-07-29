(* Init script for OPAM server repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

open Types

let t = Path.R.of_dirname (Dirname.cwd ())

let () =
  Dirname.mkdir (Path.R.archives_dir t);
  Dirname.mkdir (Path.R.compilers_dir t);
  Dirname.mkdir (Path.R.packages_dir t)
  
