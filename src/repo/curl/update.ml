(* Update scrip for curl repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1;
  )

open Types
open Repo_helpers
open Curl

let () =
  let state = Repo_helpers.make_state () in
  let t = Curl.make_state state in
  let updates = Curl.Updates.get state t in
  File.Updated.write (Path.R.updated state.local_repo) updates;
