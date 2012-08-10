(* Update scrip for rsync-ed repositories *)

open Types
open Repo_helpers

let () =
  let state = Repo_helpers.make_state () in
  let updates = Rsync.get_updates state in
  File.Updated.write (Path.R.updated state.local_repo) updates
