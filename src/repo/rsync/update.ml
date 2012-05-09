(* Update scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: opam-rsync-init <remote-address>";
    exit 1;
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)

open Types
let (/) = Stdlib_filename.concat

let log = Globals.log "opam-rsync-update"

let rsync ?fn dir =
  let option, filter = match fn with
    | None   -> "" , (fun _ -> None)
    | Some f -> "v",  f in
  let lines =
    Run.read_command_output "rsync -ar%s %s/ %s/" option (remote_address / dir) dir in
  let files = Utils.filter_map filter lines in
  List.iter (fun x -> log "updated: %s" (NV.to_string x)) files;
  List.fold_left (fun set f -> NV.Set.add f set) NV.Set.empty files

let () =
  let opam = rsync ~fn:(fun str -> NV.of_filename (Filename.of_string str)) "opam" in
  let descr = rsync "descr" in
  let updates = NV.Set.union opam descr in
  File.Updated.write
    (Path.R.updated (Path.R.of_path (Dirname.of_string local_path)))
    updates
