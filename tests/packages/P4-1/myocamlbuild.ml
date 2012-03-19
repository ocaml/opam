open Ocamlbuild_plugin
open Printf

let ocp_get pkg =
  Ocamlbuild_pack.My_unix.run_and_open
    (Printf.sprintf "ocp-get --root %s config dir %s" 
       (Unix.getenv "OPAM_ROOT")
       (Filename.quote pkg))
    input_line

let p n = 
  ocaml_lib ~extern:true ~dir:(ocp_get (sprintf "P%d" n)) (sprintf "p%d" n)

let _ = dispatch & function
  | After_rules -> List.iter p [ 2 ; 3 ]
  | _ -> ()
