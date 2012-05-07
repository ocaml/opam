(* Init scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: opam-rsync-init <remote-address>";
    exit 1
  )

let remote_address = Sys.argv.(1)

let rsync dir =
  Run.mkdir "opam";
  let err =
    Run.command "rsync -ar %s%s/ %s/" remote_address dir dir in
  if err <> 0 then
    Globals.error_and_exit "rsync failed with code %d" err

let _ =
  rsync "opam";
  rsync "descr"
