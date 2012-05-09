(* Upload scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: opam-rsync-init <remote-address>";
    exit 1
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)

open Printf
let (/) = Filename.concat

let rsync dir =
  let err =
    Run.command "rsync -ar upload/%s/ %s/" dir (remote_address / dir) in
  if err <> 0 then
    Globals.error_and_exit "rsync (%s) command failed" dir

let () =
  List.iter rsync ["opam"; "descr"; "archives" ]
