open Ocamlbuild_plugin

let ocp_get pkg =
  Ocamlbuild_pack.My_unix.run_and_open
    (Printf.sprintf "ocp-get --root %s config dir %s" 
       (Unix.getenv "OPAM_ROOT")
       (Filename.quote pkg))
    input_line

let _ = dispatch & function
  | After_rules -> ocaml_lib ~extern:true ~dir:(ocp_get "P1") "p1"
  | _ -> ()
