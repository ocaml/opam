(* Download scrip for curl-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
    exit 1
  )

let package = Sys.argv.(2)

open Types

let () =
  let nv = NV.of_string package in
  let state = Repo_helpers.make_state () in
  let t = Curl.make_state state in
  Curl.Archives.make state t nv
