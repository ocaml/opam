(* Upload scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)

let () =
  Globals.error_and_exit "CURL does not support upload"










