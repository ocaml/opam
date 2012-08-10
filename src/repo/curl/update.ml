(* Update scrip for curl repositories *)

open Types
open Repo_helpers
open Curl

let () =
  let state = Repo_helpers.make_state () in
  let updates = Curl.get_updates state in
  File.Updated.write (Path.R.updated state.local_repo) updates;
