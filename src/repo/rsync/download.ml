(* Download scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s <remote-address> <package>" Sys.argv.(0);
    exit 1
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)
let package = Sys.argv.(2)

let (/) = Filename.concat

let () =
  let remote_archive = remote_address / "archives" / package ^ ".tar.gz" in
  let err =
    Run.command [ "rsync" ; "-ar"; remote_archive ; "archives/" ] in
  if err <> 0 then
    Globals.error_and_exit "rsync command failed"
