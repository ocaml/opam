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

module Tgz = struct
  type t = OpamFilename.t

  let of_file = Fun.id
  let to_file = Fun.id
  let to_string = OpamFilename.to_string

  let quarantine tar = OpamFilename.raw (to_string tar ^ ".new")
  let backup ~inn tar =
    OpamFilename.create inn (OpamFilename.basename tar)

  let exists = OpamFilename.exists
  let remove = OpamFilename.remove
  let extract_in = OpamFilename.extract_in
  let download_as = OpamDownload.download_as
  let copy = OpamFilename.copy
  let move = OpamFilename.move
  let is_symlink = OpamFilename.is_symlink

  let fold = OpamTar.fold_reg_files

  let patch ~allow_unclean patch_source tar =
    let patch_source =
      match patch_source with
      | `Patch_file f -> `Patch_file (OpamFilename.to_string f)
      | `Patch_diffs _ as d -> d
    in
    OpamTar.patch ~allow_unclean patch_source tar

  let filter_files cond t =
    fold (fun acc file content ->
        if cond file then (file,content)::acc else acc)
      [] t

  let is_empty t =
    if exists t then
      let empty =
        try
          fold (fun _ _ _ -> raise Exit) true t
        with Exit -> false
      in
      Some empty
    else None

  module Path = OpamRepositoryPath.Make (struct
      type root = t
      type file = unix_filename
      type dir = unix_dirname
      type 'a typed_file = 'a OpamFile.t
      module Op = OpamFilename.Unix.Op
      let root root name =
        let open OpamFilename.Op in
        of_file (root / OpamRepositoryPathName.repo_d
                 // (OpamRepositoryName.to_string name ^ ".tar.gz"))
      let absolute _root f = OpamFilename.Unix.of_string f
      let absolute_dir _root d = d
      let dir_of_string = OpamFilename.Unix.Dir.of_string
      let to_typed_file f = OpamFile.make (OpamFilename.Unix.to_filename f)
    end)

end

let make_tar_gz = OpamTar.create ~flat:true ~except_vcs:true
let extract_in_job = OpamFilename.extract_in_job

type t =
  | Dir of Dir.t
  | Tgz of Tgz.t

let quarantine = function
  | Dir dir -> Dir (Dir.quarantine dir)
  | Tgz tgz -> Tgz (Tgz.quarantine tgz)

let backup ~inn = function
  | Dir dir -> Dir (Dir.backup ~inn dir)
  | Tgz tgz -> Tgz (Tgz.backup ~inn tgz)

let remove = function
  | Dir dir -> Dir.remove dir
  | Tgz tgz -> Tgz.remove tgz

let is_empty = function
  | Dir dir -> Dir.is_empty dir
  | Tgz tgz -> Tgz.is_empty tgz

let make_empty = function
  | Dir dir -> Dir.make_empty dir
  | Tgz _tar -> () (* Creating an empty tgz file doesn't make sense *)

let dirname = function
  | Dir dir -> OpamFilename.dirname_dir (Dir.to_dir dir)
  | Tgz tgz -> OpamFilename.dirname (Tgz.to_file tgz)

let basename = function
  | Dir dir -> OpamFilename.basename_dir (Dir.to_dir dir)
  | Tgz tgz -> OpamFilename.basename (Tgz.to_file tgz)

let to_string = function
  | Dir dir -> Dir.to_string dir
  | Tgz tgz -> Tgz.to_string tgz

let remove_prefix rr file =
  match rr with
  | Dir dir ->
    OpamFilename.remove_prefix dir file
    |> OpamFilename.Unix.of_string
  | Tgz _ -> OpamFilename.Unix.of_filename file

let remove_prefix_dir rr d =
  match rr with
  | Dir dir ->
    OpamFilename.remove_prefix_dir dir d
    |> OpamFilename.Unix.Dir.of_string
  | Tgz _ -> OpamFilename.Unix.Dir.of_dir d

let string_of_backend = function
  | Dir _ -> "dir"
  | Tgz _ -> "tgz"

let copy ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.copy ~src ~dst
  | Tgz src, Tgz dst -> Tgz.copy ~src ~dst
  | Tgz src, Dir dst -> OpamFilename.extract_in src dst
  | Dir src, Tgz dst -> make_tar_gz dst src

let move ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.move ~src ~dst
  | Tgz src, Tgz dst -> Tgz.move ~src ~dst
  | Tgz _, Dir _
  | Dir _, Tgz _ ->
    copy ~src ~dst;
    remove src

let exists = function
  | Dir dir -> Dir.exists dir
  | Tgz tgz -> Tgz.exists tgz

let is_symlink = function
  | Dir dir -> Dir.is_symlink dir
  | Tgz tgz -> Tgz.is_symlink tgz

let patch ~allow_unclean patch = function
  | Dir dir -> Dir.patch ~allow_unclean patch dir
  | Tgz tgz -> Tgz.patch ~allow_unclean patch tgz

let read_file (type a) (module R : OpamFile.IO_FILE with type t = a)
    ?(safe=false) repo_root
  : ?filename:a OpamFile.t -> string -> a =
  let rd =
    if safe then R.safe_read_from_string
    else R.read_from_string
  in
  rd ~loc:(to_string repo_root)

let delayed_read_repo = function
  | Dir dir ->
    let repo_file_path =
      OpamFilename.Op.(dir // OpamRepositoryPathName.repo_f)
      |> OpamFile.make
    in
    let read () = OpamFile.Repo.safe_read repo_file_path in
    (OpamFile.exists repo_file_path, read)
  | Tgz tgz ->
    let repo = OpamFilename.Unix.of_string OpamRepositoryPathName.repo_f in
    let repo_content =
      let exception Found of string in
      try
        Tgz.fold (fun () fname content ->
            if OpamFilename.Unix.equal fname repo then
              raise (Found content))
          () (Tgz.to_file tgz);
        None
      with Found content -> Some content
    in
    let read () =
      match repo_content with
      | None -> OpamFile.Repo.empty
      | Some content ->
        let filename = OpamFile.make (OpamFilename.Unix.to_filename repo) in
        let loc = OpamFilename.to_string tgz in
        OpamFile.Repo.safe_read_from_string ~loc ~filename content
    in
    (Option.is_some repo_content, read)

let remove_both root name =
  remove (Tgz (Tgz.Path.root root name));
  remove (Dir (Dir.Path.root root name))

let on_dir f = function
  | Dir dir -> f dir
  | Tgz tgz ->
    OpamFilename.with_tmp_dir (fun dir ->
        Tgz.extract_in tgz dir;
        let repo_dir = Dir.of_dir dir in
        let res = f repo_dir in
        make_tar_gz tgz repo_dir;
        res)

let root_exists root name =
  exists (Tgz (Tgz.Path.root root name))
  || exists (Dir (Dir.Path.root root name))
