(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamFilename.Op

let root t = t.repo_root

let update_cache t = root t // "update.cache"

let create root name = root / "repo" / OpamRepositoryName.to_string name

let repo t = root t // "repo" |> OpamFile.make

let packages_dir t = root t / "packages"

let packages t prefix nv =
  match prefix with
  | None   -> packages_dir t / OpamPackage.to_string nv
  | Some p -> packages_dir t / p / OpamPackage.to_string nv

let opam t prefix nv = packages t prefix nv // "opam" |> OpamFile.make

let descr t prefix nv = packages t prefix nv // "descr" |> OpamFile.make

let url t prefix nv = packages t prefix nv // "url" |> OpamFile.make

let files t prefix nv = packages t prefix nv / "files"

let archives_dir t = root t / "archives"

let archive t nv = archives_dir t // (OpamPackage.to_string nv ^ "+opam.tar.gz")

let upload_dir t = root t / "upload"

module Remote = struct
  (** URL, not FS paths *)
  open OpamUrl.Op

  let repo t =
    t.repo_url / "repo"

  let packages_url t =
    t.repo_url / "packages"

  let archive t nv =
    t.repo_url / "archives" / (OpamPackage.to_string nv ^ "+opam.tar.gz")
end
