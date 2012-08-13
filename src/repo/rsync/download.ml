(* Download scrip for rsync-ed repositories *)

let check_suffix = Filename.check_suffix
module F = Filename

open Types
open Repo_helpers
open Repositories


let is_archive f =
  let f = Basename.to_string f in
  List.exists
    (fun suff -> check_suffix f suff)
    [ "tgz"; "bgz"; "tar.gz"; "tar.bz2" ]

let () =
  let d = Repositories.read_download_info () in
  if not (is_archive d.basename) then (
    let remote_dir = d.remote_dir / Basename.to_string d.basename in
    let local_dir = d.local_dir / NV.to_string d.nv in
    let _files = Rsync.rsync_dirs remote_dir local_dir in
    Printf.printf "%s\n%!" (Dirname.to_string local_dir)
  ) else (
    let remote_file = Filename.create d.remote_dir d.basename in
    let local_file = d.local_dir // F.basename (Basename.to_string d.basename) in
    let _file = Rsync.rsync_file remote_file local_file in
    Printf.printf "%s\n%!" (Filename.to_string local_file)
  )
