(* Update script for OPAM server repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1
  )

open Types
open Protocol
open Unix

let log fmt = Globals.log Sys.argv.(0) fmt

let local_path = Path.R.of_dirname (Dirname.cwd ())
let remote_address =
  try inet_addr_of_string Sys.argv.(1)
  with _ ->
    (gethostbyname Sys.argv.(1)).h_addr_list.(0)

let () =
  let server_list = Client.get_list remote_address in
  let client_list = Path.R.available_packages local_path in
  let updates = NV.Set.diff server_list client_list in
  log "server-list=%s" (NV.Set.to_string server_list);
  log "client-list=%s" (NV.Set.to_string client_list);
  log "updates=%s" (NV.Set.to_string updates);
  NV.Set.iter (fun nv ->
    (* filter out already existing packages *)
    if not (NV.Set.mem nv client_list) then (
      let opam = Client.get_opam remote_address nv in
      let descr = Client.get_descr remote_address nv in
      File.OPAM.write (Path.R.opam local_path nv) opam;
      File.Descr.write (Path.R.descr local_path nv) descr;
    )
  ) server_list;
  NV.Set.iter (fun nv ->
    if not (NV.Set.mem nv server_list) then (
      Filename.remove (Path.R.opam local_path nv);
      Filename.remove (Path.R.descr local_path nv);
      Filename.remove (Path.R.archive local_path nv);
    )
  ) client_list;
  File.Updated.write (Path.R.updated local_path) updates;

  let compilers = Client.get_compilers remote_address in
  List.iter (fun c ->
    let filename = Path.R.compiler local_path (File.Comp.name c) in
    File.Comp.write filename c
  ) compilers
