(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Types

let log fmt = Globals.log "PATH" fmt

let available dir =
  let files = Filename.list dir in
  let files = List.filter (fun f -> Filename.check_suffix f ".opam") files in
  List.fold_left (fun set file ->
    match NV.of_filename file with
    | None    -> set
    | Some nv -> NV.Set.add nv set
  ) NV.Set.empty files

module G = struct

  type t = dirname (* [$opam/] *)

  let create opam = opam

  let root opam = opam

  let dirname_of_nv nv = Dirname.of_string (NV.to_string nv)

  let config t = t // "config"

  let opam_dir t = t / "opam"

  let opam t nv = opam_dir t // NV.to_string nv

  let available t = available (opam_dir t)

  let descr_dir t = t / "descr"

  let descr t nv = descr_dir t // NV.to_string nv

  let archive_dir t = t / "archive"

  let archive t nv = archive_dir t // (NV.to_string nv ^ ".tar.gz")

  let repo_index t = t / "repo" // "index"

end

module C = struct

  type t = dirname (* [$opam/$oversion/ *)

  let create global oversion =
    let root = G.root global in
    root / OCaml_V.to_string oversion

  let lib t n = t / "lib" / N.to_string n

  let bin t = t / "bin"

  let installed t = t // "installed"

  let build_dir t = t / "build"

  let build t nv = build_dir t / NV.to_string nv

  let install_dir t = t / "install"

  let install t n = install_dir t // (N.to_string n ^ ".install")

  let reinstall t = t // "reinstall"

  let config_dir t = t / "config"

  let config t n = config_dir t // (N.to_string n ^ ".config")

end


module R = struct

  type t = dirname (* [$opam/repo/$repo/] *)

  let create global r =
    G.root global / "repo" / Repository.name r

  let of_path path = path
    
  let root t = t

  let config t = t // "config"

  let opam_dir t = t / "opam"

  let available t = available (opam_dir t)

  let opam t nv = opam_dir t // (NV.to_string nv ^ ".opam")

  let descr_dir t = t / "descr"

  let descr t nv = descr_dir t // (NV.to_string nv)

  let archive_dir t = t / "archives"

  let archive t nv = archive_dir t // (NV.to_string nv ^ ".tar.gz")

  let updated t = t // "updated"

  let upload_dir t = t / "upload"

  let upload t nv = upload_dir t / NV.to_string nv

  let upload_opam t nv = upload t nv // (NV.to_string nv ^ ".opam")

  let upload_descr t nv = upload t nv // NV.to_string nv

  let upload_archive t nv = upload t nv // (NV.to_string nv ^ ".tar.gz")

end
