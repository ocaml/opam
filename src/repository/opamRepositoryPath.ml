(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamFilename.Op

let root t = t.repo_root

let update_cache t = root t // "update.cache"

let create root name = root / "repo" / OpamRepositoryName.to_string name

let repo t = root t // "repo"

let raw_config root name =
  root / "repo" / OpamRepositoryName.to_string name // "config"

let config t = root t // "config"

let packages_dir t = root t / "packages"

let packages t prefix nv =
  match prefix with
  | None   -> packages_dir t / OpamPackage.to_string nv
  | Some p -> packages_dir t / p / OpamPackage.to_string nv

let opam t prefix nv = packages t prefix nv // "opam"

let descr t prefix nv = packages t prefix nv // "descr"

let url t prefix nv = packages t prefix nv // "url"

let files t prefix nv = packages t prefix nv / "files"

let archives_dir t = root t / "archives"

let archive t nv = archives_dir t // (OpamPackage.to_string nv ^ "+opam.tar.gz")

let upload_dir t = root t / "upload"

let compilers_dir t = root t / "compilers"

let compiler_comp t prefix c =
  match prefix with
  | None   -> compilers_dir t // (OpamCompiler.to_string c ^ ".comp")
  | Some p -> compilers_dir t / p // (OpamCompiler.to_string c ^ ".comp")

let compiler_descr t prefix c =
  match prefix with
  | None   -> compilers_dir t // (OpamCompiler.to_string c ^ ".descr")
  | Some p -> compilers_dir t / p // (OpamCompiler.to_string c ^ ".descr")

module Remote = struct
  (** URL, not FS paths *)
  open OpamUrl.Op

  let repo t =
    t.repo_url / "repo"

  let packages_url t =
    t.repo_url / "packages"

  let archive t nv =
    t.repo_url / "archives" / (OpamPackage.to_string nv ^ "+opam.tar.gz")

  let compilers_url t =
    t.repo_url / "compilers"
end
