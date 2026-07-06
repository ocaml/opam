(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025-2026 Kate Deplaix                                    *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

module Dir = struct
  type t = dirname

  let of_dir = Fun.id
  let to_dir = Fun.id
  let to_string = OpamFilename.Dir.to_string

  let quarantine repo_root = OpamFilename.raw_dir (to_string repo_root ^ ".new")
  let with_tmp = OpamFilename.with_tmp_dir
  let backup ~inn repo_root =
    let open OpamFilename.Op in
    inn / OpamFilename.Base.to_string (OpamFilename.basename_dir repo_root)

  let cwd = OpamFilename.cwd
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

  module Op = OpamFilename.Op

  module Path = OpamRepositoryPath.Make (struct
      type root = t
      type file = filename
      type dir = dirname
      type 'a typed_file = 'a OpamFile.t
      let root root name =
        let open Op in
        root / OpamRepositoryPathName.repo_d / OpamRepositoryName.to_string name
      let absolute root f = Op.(root // f)
      let absolute_dir root d = Op.(root / OpamFilename.Dir.to_string d)
      let dir_of_string = OpamFilename.raw_dir
      let to_typed_file = OpamFile.make
      module Op = Op
    end)

end

let make_tar_gz = OpamTar.create ~flat:false ~except_vcs:false
let extract_in_job = OpamFilename.extract_in_job

type t =
  | Dir of Dir.t

let quarantine = function
  | Dir dir -> Dir (Dir.quarantine dir)

let backup ~inn = function
  | Dir dir -> Dir (Dir.backup ~inn dir)

let remove = function
  | Dir dir -> Dir.remove dir

let is_empty = function
  | Dir dir -> Dir.is_empty dir

let make_empty = function
  | Dir dir -> Dir.make_empty dir

let dirname = function
  | Dir dir -> OpamFilename.dirname_dir (Dir.to_dir dir)

let basename = function
  | Dir dir -> OpamFilename.basename_dir (Dir.to_dir dir)

let to_string = function
  | Dir dir -> Dir.to_string dir

let remove_prefix rr file =
  match rr with
  | Dir dir ->
    OpamFilename.remove_prefix dir file
    |> OpamFilename.Unix.of_string

let remove_prefix_dir rr d =
  match rr with
  | Dir dir ->
    OpamFilename.remove_prefix_dir dir d
    |> OpamFilename.Unix.Dir.of_string

let string_of_backend = function
  | Dir _ -> "dir"

let copy ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.copy ~src ~dst

let move ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.move ~src ~dst

let exists = function
  | Dir dir -> Dir.exists dir

let is_symlink = function
  | Dir dir -> Dir.is_symlink dir

let patch ~allow_unclean patch = function
  | Dir dir -> Dir.patch ~allow_unclean patch dir

let delayed_read_repo = function
  | Dir dir ->
    let repo_file_path =
      OpamFilename.Op.(dir // OpamRepositoryPathName.repo_f)
      |> OpamFile.make
    in
    let read () = OpamFile.Repo.safe_read repo_file_path in
    (OpamFile.exists repo_file_path, read)
