(* Download scrip for curl-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-filename> <force>" Sys.argv.(0);
    exit 1
  )

open Types
open Repo_helpers

let () =
  let state = Repo_helpers.make_download_state () in
  let basename = Filename.basename state.filename in
  let local_file = Filename.create (Dirname.cwd ()) basename in
  if state.force || not (Filename.exists local_file) then
    match Filename.download state.filename (Dirname.cwd ()) with
    | None   -> exit 1
    | Some f ->
        Printf.printf "%s" (Filename.to_string f)
