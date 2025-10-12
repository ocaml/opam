(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Dir = struct
  type t = OpamFilename.Dir.t

  let of_dir = Fun.id
  let to_dir = Fun.id
  let to_string = OpamFilename.Dir.to_string

  let quarantine repo_root = OpamFilename.raw_dir (to_string repo_root ^ ".new")
  let with_tmp = OpamFilename.with_tmp_dir
  let backup ~tmp_dir repo_root =
    let open OpamFilename.Op in
    tmp_dir / OpamFilename.Base.to_string (OpamFilename.basename_dir repo_root)

  let cwd = OpamFilename.cwd
  let in_dir = OpamFilename.in_dir
  let exists = OpamFilename.exists_dir
  let remove = OpamFilename.rmdir
  let move = OpamFilename.move_dir
  let copy = OpamFilename.copy_dir
  let copy_except_vcs = OpamFilename.copy_dir_except_vcs
  let is_symlink = OpamFilename.is_symlink_dir
  let patch = OpamFilename.patch
  let make_empty = OpamFilename.mkdir
  let dirs = OpamFilename.dirs
  let is_empty = OpamFilename.dir_is_empty
  let dirname = OpamFilename.dirname_dir

  let repo repo_root = OpamFilename.Op.(repo_root // "repo" |> OpamFile.make)

  module Op = struct
    let (/) d s = OpamFilename.Op.(d / s)
    let (//) d s = OpamFilename.Op.(d // s)
  end

end

module Tar = struct
  type t = OpamFilename.t

  let of_file = Fun.id
  let to_file = Fun.id
  let to_string = OpamFilename.to_string

  let backup ~tmp_dir tar =
    OpamFilename.create tmp_dir (OpamFilename.basename tar)

  let exists = OpamFilename.exists
  let remove = OpamFilename.remove
  let extract_in = OpamFilename.extract_in
  let download_as = OpamDownload.download_as
  let copy = OpamFilename.copy
  let move = OpamFilename.move
  let is_symlink = OpamFilename.is_symlink
end

let make_tar_gz_job = OpamFilename.make_tar_gz_job
let extract_in_job = OpamFilename.extract_in_job

type t =
  | Dir of Dir.t
  | Tar of Tar.t

let quarantine = function
  | Dir dir -> Dir (Dir.quarantine dir)
  | Tar tar -> Tar (OpamFilename.raw (Tar.to_string tar ^ ".new"))

let remove = function
  | Dir dir -> Dir.remove dir
  | Tar tar -> Tar.remove tar

let is_empty = function
  | Dir dir -> Dir.is_empty dir
  | Tar _tar -> None

let make_empty = function
  | Dir dir -> Dir.make_empty dir
  | Tar _tar -> () (* Creating an empty tar file doesn't make sense *)

let dirname = function
  | Dir dir -> OpamFilename.dirname_dir (Dir.to_dir dir)
  | Tar tar -> OpamFilename.dirname (Tar.to_file tar)

let basename = function
  | Dir dir -> OpamFilename.basename_dir (Dir.to_dir dir)
  | Tar tar -> OpamFilename.basename (Tar.to_file tar)

let to_string = function
  | Dir dir -> Dir.to_string dir
  | Tar tar -> Tar.to_string tar

let copy ~src ~dst =
  let open OpamProcess.Job.Op in
  match src, dst with
  | Dir src, Dir dst -> Dir.copy ~src ~dst; Done None
  | Tar src, Tar dst -> Tar.copy ~src ~dst; Done None
  | Tar src, Dir dst -> OpamFilename.extract_in_job src dst
  | Dir src, Tar dst -> OpamFilename.make_tar_gz_job dst src

let move ~src ~dst =
  let open OpamProcess.Job.Op in
  match src, dst with
  | Dir src, Dir dst -> Dir.move ~src ~dst; Done None
  | Tar src, Tar dst -> Tar.move ~src ~dst; Done None
  | Tar _, Dir _
  | Dir _, Tar _ ->
    copy ~src ~dst @@+ function
    | None -> remove src; Done None
    | Some exn -> Done (Some exn)

let is_symlink = function
  | Dir dir -> Dir.is_symlink dir
  | Tar tar -> Tar.is_symlink tar

let patch ~allow_unclean patch = function
  | Dir dir -> Dir.patch ~allow_unclean patch dir
  | Tar _ -> assert false (* TODO *)

let delayed_read_repo = function
  | Dir dir ->
    let repo_file_path = Dir.repo dir in
    let read () = OpamFile.Repo.safe_read repo_file_path in
    (OpamFile.exists repo_file_path, read)
  | Tar tar ->
    let repo_content =
      let exception Found of string in
      try
        OpamTar.fold_reg_files (fun () fname content ->
            if fname = "repo" then
              raise (Found content))
          () (Unix.openfile (Tar.to_string tar) [Unix.O_RDONLY] 0);
        None
      with Found content -> Some content
    in
    let read () =
      match repo_content with
      | None -> OpamFile.Repo.empty
      | Some content ->
        try OpamFile.Repo.read_from_string content
        with _ -> OpamFile.Repo.empty
    in
    (Option.is_some repo_content, read)
