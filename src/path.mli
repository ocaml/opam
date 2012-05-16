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

(** Typed filename management. *)

open Types

(** Global state *)
module G : sig

  (** Contains {i $opam} *)
  type t

  (** Create a global path *)
  val create: unit -> t

  (** Root dir: {i $opam/} *)
  val root: t -> dirname

  (** Main configuration file: {i $opam/config} *)
  val config: t -> filename

  (** OPAM files: {i $opam/opam/$NAME.$VERSION.opam} *)
  val opam: t -> NV.t -> filename

  (** List all the available packages:
      {i $opam/opam/$NAME.$VERSION.opam} *)
  val available: t -> NV.Set.t

  (** List all the available packages:
      {i $opam/opam/$NAME.$VERSION.opam} *)
  val available_versions: t -> N.t -> V.Set.t

  (** Description file: {i $opam/descr/$NAME.$VERSION} *)
  val descr: t -> NV.t -> filename

  (** Archives files: {i $opam/archives/$NAME.$VERSION.tar.gz} *)
  val archive: t -> NV.t -> filename

  (** OPAM files folder: {i $opam/opam/} *)
  val opam_dir: t -> dirname

  (** Description files folder: {i $opam/descr/} *)
  val descr_dir: t -> dirname

  (** Archives files folder: {i $opam/archives/} *)
  val archive_dir: t -> dirname

  (** Return the repository index: {i $opam/repo/index} *)
  val repo_index: t -> filename

end

(** Compiler-version related state *)
module C : sig

  (** Contains {i $opam} and {i $OVERSION} *)
  type t

  (** Create a compiler path *)
  val create: OCaml_V.t -> t

  (** Root dir: {i $opam/$oversion} *)
  val root: t -> dirname
   
  (** Library path for a given package:
      {i $opam/$OVERSION/lib/NAME} *)
  val lib: t -> N.t -> dirname

  (** Library path: {i $opam/$OVERSION/lib/} *)
  val lib_dir: t -> dirname

  (** Installed binaries: {i $opam/$OVERSION/bin} *)
  val bin: t -> dirname

  (** List of installed packages with their version:
      {i $opam/$OVERSION/installed} *)
  val installed: t -> filename

  (** Tempory folders used to decompress the corresponding archives:
      {i $opam/$OVERSION/build/$NAME-$VERSION} *)
  val build: t -> NV.t -> dirname

  (** Tempory folder: {i $opam/$OVERSION/build} *)
  val build_dir: t -> dirname

  (** Tempory location of install files:
      {i $opam/$OVERSION/build/$NAME.$VERSION/$NAME.install} *)
  val build_install: t -> NV.t -> filename

  (** Tempory location of config files: {i
      $opam/$OVERSION/build/$NAME.$VERSION/$NAME.config} *)
  val build_config: t -> NV.t -> filename

  (** Installed files for a given package:
      {i $opam/$OVERSION/install/$NAME.install} *)
  val install: t -> N.t -> filename

  (** Installed files: {i $opam/$OVERSION/install/} *)
  val install_dir: t -> dirname
    
  (** Packages to reinstall on next upgrade:
      {i $opam/$OVERSION/reinstall} *)
  val reinstall: t -> filename

  (** Compile and link flags for a given package:
      {i $opam/$OVERSION/config/$NAME.config} *)
  val config: t -> N.t -> filename

  (** Configuration folder: {i $opam/$OVERSION/config} *)
  val config_dir: t -> dirname

end

module R : sig

  type t

  (** Create a repository path *)
  val create: repository -> t

  (** Transform a directory name into a repository path *)
  val of_path: dirname -> t

  (** Return the repository folder: {i $opam/repo/$repo} *)
  val root: t -> dirname

  (** Return the repository config: {i $opam/repo/$repo/config} *)
  val config: t -> filename

  (** Return the OPAM file for a given package:
      {i $opam/repo/$repo/opam/$NAME.$VERSION.opam} *)
  val opam: t -> NV.t -> filename

  (** Return the OPAM folder: {i $opam/repo/$repo/opam/} *)
  val opam_dir: t -> dirname

  (** List all the available packages:
      {i $opam/repo/$repo/$NAME.$VERSION.opam} *)
  val available: t -> NV.Set.t

  (** List all the available versions for a given package:
      {i $opam/repo/$repo/$name.$VERSION.opam} *)
  val available_versions: t -> N.t -> V.Set.t

  (** Return the description file for a given package:
      {i $opam/repo/$repo/descr/$NAME.VERSION} *)
  val descr: t -> NV.t -> filename

  (** Return the description folder *)
  val descr_dir: t -> dirname

  (** Return the archive for a giben package:
      {i $opam/repo/$repo/archives/$NAME.$VERSION.tar.gz} *)
  val archive: t -> NV.t -> filename

  (** Return the archive folder: {i $opam/repo/$repo/archives/} *)
  val archive_dir: t -> dirname

  (** Return the list of updated packages:
      {i $opam/repo/$repo/updated} *)
  val updated: t -> filename

  (** Return the upload folder for a given version:
      {i $opam/repo/$repo/upload/} *)
  val upload: t -> dirname

  (** Return the upload folder for OPAM files:
      {i $opam/repo/$repo/upload/opam/}*)
  val upload_opam_dir: t -> dirname

  (** Return the upload folder for descr files:
      {i $opam/repo/$repo/upload/descr/} *)
  val upload_descr_dir: t -> dirname

  (** Return the upload folder for archive files:
      {i $opam/repo/$repo/upload/archives/} *)
  val upload_archives_dir: t -> dirname

  (** Return the upload folder for OPAM files:
      {i $opam/repo/$repo/upload/opam/$NAME.$VERSION.opam}*)
  val upload_opam: t -> NV.t -> filename

  (** Return the upload folder for descr files:
      {i $opam/repo/$repo/upload/descr/$NAME.$VERSION} *)
  val upload_descr: t -> NV.t -> filename

  (** Return the upload folder for archive files:
      {i $opam/repo/$repo/upload/archives/$NAME.$VERSION.tar.gz} *)
  val upload_archives: t -> NV.t -> filename

end
