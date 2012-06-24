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
