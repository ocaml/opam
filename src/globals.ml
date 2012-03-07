let debug = ref false

let version = "0.1+dev"

let default_hostname = "opam.ocamlpro.com"
let default_port = 9999

let ocaml_version = Sys.ocaml_version
let opam_version = "1"

let opam_server_path = ".opam-server"
let opam_path = ".opam"

let log section fmt =
  Printf.kprintf (fun str ->
    if !debug then
      Printf.eprintf " %-20s %s\n%!" section str
  ) fmt

let error fmt =
  Printf.kprintf (fun str ->
    Printf.eprintf "ERROR: %s\n%!" str
  ) fmt
