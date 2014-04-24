let deps = [ "P1" ]

open Ocamlbuild_plugin

let libdir =
  Ocamlbuild_pack.My_unix.run_and_open
    (Printf.sprintf "%s config var lib"
       (Unix.getenv "OPAM"))
    input_line

let includes =
  Printf.sprintf "-I %s/P1 -I %s/P2" libdir libdir

let add_dep p =
  flag ["ocaml"; "compile"] & S[Sh includes]

let _ = dispatch & function
        | After_rules -> List.iter add_dep deps
        | _ -> ()
