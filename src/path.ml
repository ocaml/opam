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

(** Global state *)
module type GLOBAL = sig

  (** Contains [$opam] *)
  type t

  val create: dirname -> t

  (** Root dir: [$opam/] *)
  val root: t -> dirname

  (** Main configuration file: [$opam/config] *)
  val config: t -> filename

  (** OPAM files: [$opam/opam/$NAME.$VERSION.opam] *)
  val opam: t -> NV.t -> filename

  (** List all the available packages: [$opam/opam/$NAME.$VERSION.opam] *)
  val available: t -> NV.Set.t

  (** Description file: [$opam/descr/$NAME.$VERSION] *)
  val descr: t -> NV.t -> filename

  (** Archives files: [$opam/archives/$NAME.$VERSION.tar.gz] *)
  val archive: t -> NV.t -> filename

  (** OPAM files folder: [$opam/opam/] *)
  val opam_dir: t -> dirname

  (** Description files folder: [$opam/descr/] *)
  val descr_dir: t -> dirname

  (** Archives files folder: [$opam/archives/] *)
  val archive_dir: t -> dirname

  (** Return the repository index: [$opam/repo/index] *)
  val repo_index: t -> filename
end

module G : GLOBAL = struct
  type t = dirname

  let create opam = opam

  let root opam = opam

  let dirname_of_nv nv = Dirname.of_string (NV.to_string nv)

  let config t = t // b "config"

  let opam_dir t = t / d "opam"

  let opam t nv = opam_dir t // b (NV.to_string nv)

  let available t =
    let files = Filename.list (opam_dir t) in
    let files = List.filter (fun f -> Filename.check_suffix f ".opam") files in
    List.fold_left (fun set file -> NV.Set.add (NV.of_file file) set) NV.Set.empty files

  let descr_dir t = t / d "descr"

  let descr t nv = descr_dir t // b (NV.to_string nv)

  let archive_dir t = t / d "archive"

  let archive t nv = archive_dir t // b (NV.to_string nv ^ ".tar.gz")

  let repo_index t = t / d "repo" // b "index"

end

(** Compiler-version related state *)
module type COMPILER = sig

  (** Contains [$opam] and [$OVERSION] *)
  type t
    
  val create: G.t -> OCaml_V.t -> t

  (** Installed libraries for the package: [$opam/$OVERSION/lib/NAME] *)
  val lib: t -> N.t -> dirname

  (** Installed binaries: [$opam/$OVERSION/bin] *)
  val bin: t -> dirname

  (** List of installed packages with their version: [$opam/$OVERSION/installed] *)
  val installed: t -> filename

  (** Tempory folders used to decompress the corresponding archives:
      [$opam/$OVERSION/build/$NAME-$VERSION] *)
  val build: t -> NV.t -> dirname

  (** Tempory folder: [$opam/$OVERSION/build] *)
  val build_dir: t -> dirname

  (** Installed files for a given package: [$opam/$OVERSION/install/$NAME.install] *)
  val install: t -> N.t -> filename

  (** Installed files: [$opam/$OVERSION/install/] *)
  val install_dir: t -> dirname
    
  (** Packages to reinstall on next upgrade: [$opam/$OVERSION/reinstall]  *)
  val reinstall: t -> filename

  (** Compile and link flags for a given package:
      [$opam/$OVERSION/config/$NAME.config] *)
  val config: t -> N.t -> filename

  (** Configuration folder: [$opam/$OVERSION/config] *)
  val config_dir: t -> dirname

end

module C : COMPILER = struct

  type t = dirname

  let create global oversion =
    let root = G.root global in
    root / d (OCaml_V.to_string oversion)

  let lib t n = t / d "lib" / d (N.to_string n)

  let bin t = t / d "bin"

  let installed t = t // b "installed"

  let build_dir t = t / d "build"

  let build t nv = build_dir t / d (NV.to_string nv)

  let install_dir t = t / d "install"

  let install t n = install_dir t // b (N.to_string n ^ ".install")

  let reinstall t = t // b "reinstall"

  let config_dir t = t / d "config"

  let config t n = config_dir t // b (N.to_string n ^ ".config")

end

module type REPOSITORY = sig
  type t

  val create: G.t -> repository -> t

  (** Return the repository folder: [$opam/repo/$repo] *)
  val root: t -> dirname

  (** Return the repository kind: [$opam/repo/$repo/kind] *)
  val kind: t -> filename

  (** Return the repository address: [$opam/repo/$repo/address] *)
  val address: t -> filename

  (** Return the OPAM file for a given package:
      [$opam/repo/$repo/opam/$NAME.$VERSION.opam] *)
  val opam: t -> NV.t -> filename

  (** Return the OPAM folder: [$opam/repo/$repo/opam/] *)
  val opam_dir: t -> dirname

  (** Return the description file for a given package:
      [$opam/repo/$repo/descr/$NAME.VERSION] *)
  val descr: t -> NV.t -> filename

  (** Return the description folder *)
  val descr_dir: t -> dirname

  (** Return the archive for a giben package:
      [$opam/repo/$repo/archives/$NAME.$VERSION.tar.gz *)
  val archive: t -> NV.t -> filename

  (** Return the archive folder: [$opam/repo/$repo/archives/] *)
  val archive_dir: t -> dirname

  (** Return the list of updated packages:
      [$opam/repo/$repo/updated] *)
  val updated: t -> filename

  (** Return the upload folder for a given version:
      [$opam/repo/$repo/upload/$NAME.$VERSION/] *)
  val upload: t -> NV.t -> dirname

  (** Return the upload folder for OPAM files:
      [$opam/repo/$repo/upload/$NAME.$VERSION/$NAME.$VERSION.opam]*)
  val upload_opam: t -> NV.t -> filename

  (** Return the upload folder for descr files:
      [$opam/repo/$repo/upload/$NAME.$VERSION/$NAME.$VERSION] *)
  val upload_descr: t -> NV.t -> filename

  (** Return the upload folder for archive files:
      [$opam/repo/$repo/upload/$NAME.$VERSION/$NAME.$VERSION.tar.gz] *)
  val upload_archive: t -> NV.t -> filename

  (** Return the upload folder: [$opam/repo/$repo/upload] *)
  val upload_dir: t -> dirname

end

module R : REPOSITORY = struct
  type t = {
    root: dirname; (* [$opam/] *)
    repo: dirname; (* [$opam/repo/$repo] *)
  }

  let create global r =
    let root = G.root global in
    {
      root;
      repo = root / d "repo" / d (Repository.name r);
    }

  let root t = t.repo

  let kind t = t.repo // b "kind"

  let address t = t.repo // b "address"

  let opam_dir t = t.repo / d "opam"

  let opam t nv = opam_dir t // b (NV.to_string nv ^ ".opam")

  let descr_dir t = t.repo / d "descr"

  let descr t nv = descr_dir t // b (NV.to_string nv)

  let archive_dir t = t.repo / d "archives"

  let archive t nv = archive_dir t // b (NV.to_string nv ^ ".tar.gz")

  let updated t = t.repo // b "updated"

  let upload_dir t = t.repo / d "upload"

  let upload t nv = upload_dir t / d (NV.to_string nv)

  let upload_opam t nv = upload t nv // b (NV.to_string nv ^ ".opam")

  let upload_descr t nv = upload t nv // b (NV.to_string nv)

  let upload_archive t nv = upload t nv // b (NV.to_string nv ^ ".tar.gz")

end


















