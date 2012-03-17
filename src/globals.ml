let debug = ref false

let version = "0.1+dev"

let default_hostname = "opam.ocamlpro.com"
let default_port = 9999

let ocaml_version = Sys.ocaml_version
let opam_version = "1"

let home = Unix.getenv "HOME"
let default_opam_server_path = Filename.concat home ".opam-server"
let default_opam_path = Filename.concat home ".opam"

let root_path = ref default_opam_path

let log section fmt =
  Printf.kprintf (fun str ->
    if !debug then
      Printf.eprintf " %-20s %s\n%!" section str
  ) fmt

let error fmt =
  Printf.kprintf (fun str ->
    Printf.eprintf "ERROR: %s\n%!" str
  ) fmt

let error_and_exit fmt =
  Printf.kprintf (fun str ->
    error "%s" str;
    exit 1
  ) fmt

let msg fmt =
  Printf.kprintf (fun str ->
    Printf.printf "%s%!" str
  ) fmt




















