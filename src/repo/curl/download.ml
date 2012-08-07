(* Download scrip for curl-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-filename> <nv> <force>" Sys.argv.(0);
    exit 1
  )

open Types
open Repositories

let () =
  let d = Repositories.read_download_info () in
  let basename = Filename.basename d.filename in
  let local_file = Filename.create (Dirname.cwd ()) basename in
  if d.force || not (Filename.exists local_file) then
    match Filename.download d.filename (Dirname.cwd ()) with
    | None   -> exit 1
    | Some f ->
        Printf.printf "%s" (Filename.to_string f)
