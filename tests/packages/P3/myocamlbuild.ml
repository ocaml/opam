let deps = [ "P1" ]

open Ocamlbuild_plugin

let ocp_get pkg =
  Ocamlbuild_pack.My_unix.run_and_open
    (Printf.sprintf "%s config includes -R %s"
       (Unix.getenv "OPAM")
       (Filename.quote pkg))
    input_line

let add_dep p =
  flag ["ocaml"; "compile"] & S[Sh (ocp_get p)]

let _ = dispatch & function
  | After_rules -> List.iter add_dep deps
  | _ -> ()








