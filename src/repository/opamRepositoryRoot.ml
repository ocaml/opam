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

module Tar = struct
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

  let archives : (OpamHash.t, string OpamFilename.Unix.Map.t) Hashtbl.t = Hashtbl.create 8
  let unload_repo_tars () = Hashtbl.clear archives

  let fold f x tar =
    (* TAR TOQUESTION : do we need to have a sha256 ? md5 have collision, will it
       really happen irl ? *)
    let hash = OpamHash.compute ~kind:`SHA256 (OpamFilename.to_string tar) in
    match Hashtbl.find_opt archives hash with
    | Some contents ->
      OpamFilename.Unix.Map.fold (fun filename content acc ->
          f acc filename content)
        contents x
    | None ->
      let result, map =
        OpamTar.fold_reg_files (fun (acc, map) file content ->
            f acc file content,
            OpamFilename.Unix.Map.add file content map)
          (x, OpamFilename.Unix.Map.empty) tar
      in
      Hashtbl.add archives hash map;
      result

  let files t =
    fold (fun files file _ -> file::files) [] t
  let ls t =
    OpamStd.Format.itemize OpamFilename.Unix.to_string (files t)

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
      Some (match files t with | [] -> true | _ -> false)
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
  | Tar of Tar.t

let quarantine = function
  | Dir dir -> Dir (Dir.quarantine dir)
  | Tar tar -> Tar (Tar.quarantine tar)

let backup ~inn = function
  | Dir dir -> Dir (Dir.backup ~inn dir)
  | Tar tar -> Tar (Tar.backup ~inn tar)

let remove = function
  | Dir dir -> Dir.remove dir
  | Tar tar -> Tar.remove tar

let is_empty = function
  | Dir dir -> Dir.is_empty dir
  | Tar tar -> Tar.is_empty tar

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

let remove_prefix file = function
  | Dir dir ->
    OpamFilename.remove_prefix dir file
    |> OpamFilename.raw
  | Tar _ -> file

let remove_prefix_dir d = function
  | Dir dir ->
    OpamFilename.remove_prefix_dir dir d
    |> OpamFilename.raw_dir
  | Tar _ -> d

let is_tar = function
  | Dir _ -> false
  | Tar _ -> true

let is_dir = function
  | Dir _ -> true
  | Tar _ -> false

let ls = function
  | Dir dir ->
    OpamFilename.rec_files dir
    |> OpamStd.Format.itemize OpamFilename.to_string
  | Tar tar -> Tar.ls tar


let copy ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.copy ~src ~dst
  | Tar src, Tar dst -> Tar.copy ~src ~dst
  | Tar src, Dir dst -> OpamFilename.extract_in src dst
  | Dir src, Tar dst -> make_tar_gz dst src


let move ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.move ~src ~dst
  | Tar src, Tar dst -> Tar.move ~src ~dst
  | Tar _, Dir _
  | Dir _, Tar _ ->
    copy ~src ~dst;
    remove src

let exists = function
  | Dir dir -> Dir.exists dir
  | Tar tar -> Tar.exists tar

let is_symlink = function
  | Dir dir -> Dir.is_symlink dir
  | Tar tar -> Tar.is_symlink tar

let patch ~allow_unclean patch = function
  | Dir dir -> Dir.patch ~allow_unclean patch dir
  | Tar tar -> Tar.patch ~allow_unclean patch tar

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
  | Tar tar ->
    let repo_content =
      let exception Found of string in
      let repo = OpamFilename.Unix.of_string OpamRepositoryPathName.repo_f in
      try
        Tar.fold (fun () fname content ->
            if OpamFilename.Unix.equal fname repo then
              raise (Found content))
          () (Tar.to_file tar);
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

let remove_both root name =
  remove (Tar (Tar.Path.root root name));
  remove (Dir (Dir.Path.root root name))

let on_dir f = function
  | Dir dir -> f dir
  | Tar tar ->
    OpamFilename.with_tmp_dir (fun dir ->
        Tar.extract_in tar dir;
        let repo_dir = Dir.of_dir dir in
        let res = f repo_dir in
        make_tar_gz tar repo_dir;
        res)
