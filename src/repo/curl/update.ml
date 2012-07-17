(* Update scrip for curl repositories *)

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <remote-address>" Sys.argv.(0);
    exit 1;
  )

open Types
open Misc

let (++) = NV.Set.union

let get_updates dir =
  let current = Filename.Set.of_list (Filename.list (local_path / dir)) in
  log "current: %s" (Filename.Set.to_string current);
  let to_keep = Filename.Set.filter (fun f ->
    Filename.starts_with (local_path / dir) f 
  ) active_local_files in
  log "to_keep: %s" (Filename.Set.to_string to_keep);
  let to_delete = Filename.Set.diff current to_keep in
  log "to_delete: %s" (Filename.Set.to_string to_delete);
  Filename.Set.iter Filename.remove to_delete;
  download_remote_dir (remote_path / dir)
  

let get_nv_updates dir =
  let l = get_updates dir in
  NV.Set.of_list (Utils.filter_map NV.of_filename (Filename.Set.elements l))

let () =
  let url = get_nv_updates "url" in
  let descr = get_nv_updates "descr" in
  let opam = get_nv_updates "opam" in
  let updates = url ++ descr ++ opam in
  File.Updated.write (Path.R.updated local_repo) updates;

  let _comps = get_updates "compilers" in
  ()
