(* Download scrip for rsync-ed repositories *)

let check_suffix = Filename.check_suffix

open Types
open Repo_helpers
open Repositories


let is_archive f =
  let f = Filename.to_string f in
  List.exists
    (fun suff -> check_suffix f suff)
    [ "tgz"; "bgz"; "tar.gz"; "tar.bz2" ]

let () =
  let d = Repositories.read_download_info () in
  if not (is_archive d.remote_filename) then (
    let remote_dir = Dirname.of_string (Filename.to_string d.remote_filename) in
    let local_dir = d.local_path / NV.to_string d.nv in
    let _files = Rsync.rsync_dirs remote_dir local_dir in
    Printf.printf "%s\n%!" (Dirname.to_string local_dir)
  ) else (
    let local_file = Filename.create d.local_path (Filename.basename d.remote_filename) in
    let _file = Rsync.rsync_file d.remote_filename local_file in
    Printf.printf "%s\n%!" (Filename.to_string local_file)
  )
