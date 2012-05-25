(* Update scrip for rsync-ed repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1;
  )

let local_path = Run.cwd ()
let remote_address = Sys.argv.(1)

open Types
let (/) = Stdlib_filename.concat

let log fmt = Globals.log "opam-rsync-update" fmt

let rsync ?fn dir =
  let option, filter = match fn with
    | None   -> "" , (fun _ -> None)
    | Some f -> "v",  f in
  let lines =
    Run.read_command_output [
      "rsync"; "-ar"^option ; option ; (remote_address / dir) ; dir
    ] in
  let files = Utils.filter_map filter lines in
  List.iter (fun x -> log "updated: %s" (NV.to_string x)) files;
  List.fold_left (fun set f -> NV.Set.add f set) NV.Set.empty files

let _ =    log "FOO1"

let () =
  let fn str = NV.of_filename (Filename.of_string str) in
    log "FOO2";
  let opam = rsync ~fn "opam/" in
    log "FOO3";
  let descr = rsync "descr/" in
  let archives =
    log "FOO";
    let files = Run.files "archives" in
    log "BAR";
    List.fold_left (fun set f -> NV.Set.union (rsync ~fn f) set) NV.Set.empty files in
  let updates = NV.Set.union archives (NV.Set.union opam descr) in
  File.Updated.write
    (Path.R.updated (Path.R.of_path (Dirname.of_string local_path)))
    updates
