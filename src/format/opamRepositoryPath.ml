(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2026 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*    Copyright 2026 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamFilename.Op
open OpamTypes

(* Repository Paths *)

module type PATH = sig
  type repo_root
  type repo_dirname
  type 'a typed_file

  val root: dirname -> repository_name -> repo_root
  val repo: repo_root -> OpamFile.Repo.t typed_file
  val packages_dir: repo_root -> repo_dirname
  val packages: repo_root -> string option -> package -> repo_dirname
  val opam:
    repo_root -> string option -> package -> OpamFile.OPAM.t typed_file
  val files: repo_root -> string option -> package -> repo_dirname
  val descr:
    repo_root -> string option -> package -> OpamFile.Descr_legacy.t typed_file
  val url:
    repo_root -> string option -> package -> OpamFile.URL_legacy.t typed_file
end

module type PATH_REPR = sig
  type root
  type file
  type dir
  type 'a typed_file
  val root : dirname -> repository_name -> root
  val absolute : root -> string -> file
  val absolute_dir : root -> dir -> dir
  val dir_of_string : string -> dir
  val to_typed_file : file -> 'a typed_file
  module Op : sig
    val (/): dir -> string -> dir
    val (//): dir -> string -> file
  end
end

module Make (I : PATH_REPR) : PATH
  with type repo_root = I.root
   and type repo_dirname = I.dir
   and type 'a typed_file = 'a I.typed_file
= struct
  open I.Op
  type repo_root = I.root
  type repo_dirname = I.dir
  type 'a typed_file = 'a I.typed_file

  let root = I.root
  let repo root =
    I.to_typed_file (I.absolute root OpamRepositoryPathName.repo_f)
  let packages_dir root =
    I.absolute_dir root (I.dir_of_string OpamRepositoryPathName.packages_d)

  let packages root prefix nv =
    let pkg_dir = I.dir_of_string OpamRepositoryPathName.packages_d in
    let d =
      match prefix with
      | None -> pkg_dir / OpamPackage.to_string nv
      | Some p -> pkg_dir / p / OpamPackage.to_string nv
    in
    I.absolute_dir root d

  let opam root prefix nv =
    I.to_typed_file
      (packages root prefix nv // OpamRepositoryPathName.opam_f)
  let files root prefix nv =
    packages root prefix nv / OpamRepositoryPathName.files_d
  let descr root prefix nv =
    I.to_typed_file (packages root prefix nv // "descr")
  let url root prefix nv =
    I.to_typed_file (packages root prefix nv // "url")
end

(* Other paths *)

let download_cache root = root / OpamRepositoryPathName.download_cache_d

let pin_cache_dir =
  let dir =
    lazy (OpamSystem.mk_temp_dir ~prefix:"opam-pin-cache" ()
          |> OpamFilename.Dir.of_string )
  in
  fun () -> Lazy.force dir

let pin_cache u =
  pin_cache_dir () /
  String.sub
    (OpamHash.contents @@
     OpamHash.compute_from_string ~kind:`SHA512 @@
     OpamUrl.to_string u)
    0 16

(* URL paths *)

module Remote = struct
  (** URL, not FS paths *)
  open OpamUrl.Op

  let repo root_url =
    root_url / OpamRepositoryPathName.repo_f

  let packages_url root_url =
    root_url / OpamRepositoryPathName.packages_d

  let archive root_url nv =
    root_url / "archives" / (OpamPackage.to_string nv ^ "+opam.tar.gz")
end
