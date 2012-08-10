(* Upload scrip for rsync-ed repositories *)

open Types
open Repo_helpers

let log fmt = Globals.log "rsync-upload" fmt

let () =
  let state = Repo_helpers.make_state () in
  let updates = Rsync.upload state in
  Globals.msg "%d packages uploaded %s\n" (NV.Set.cardinal updates) (NV.Set.to_string updates)
