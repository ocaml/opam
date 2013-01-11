(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** The various paths where OPAM configuration files are stored. *)

open OpamTypes

(** {2 Global paths} *)

(** Type of path root *)
type t = dirname

(** Default root path *)
val default: unit -> t

(** Root dir: {i $opam/} *)
val root: t -> dirname

(** lock file *)
val lock: t -> filename

(** Main configuration file: {i $opam/config} *)
val config: t -> filename

(** Compiler aliases *)
val aliases: t -> filename

(** OPAM files: {i $opam/opam/$NAME.$VERSION.opam} *)
val opam: t -> package -> filename

(** Compiler files: {i $opam/compilers/$OVERSION.comp} *)
val compiler: t -> compiler -> filename

(** Compiler description files: {i $opam/compilers/$OVERSION.descr} *)
val compiler_descr: t -> compiler -> filename

(** Compiler files: {i $opam/compilers/} *)
val compilers_dir: t -> dirname

(** Description file: {i $opam/descr/$NAME.$VERSION} *)
val descr: t -> package -> filename

(** Archives files: {i $opam/archives/$NAME.$VERSION.tar.gz} *)
val archive: t -> package -> filename

(** OPAM files folder: {i $opam/opam/} *)
val opam_dir: t -> dirname

(** Description files folder: {i $opam/descr/} *)
val descr_dir: t -> dirname

(** Archives files folder: {i $opam/archives/} *)
val archives_dir: t -> dirname

(** Return the repository index: {i $opam/repo/index} *)
val repo_index: t -> filename

(** Switch related paths *)
module Switch: sig

  (** Root dir: {i $opam/$OVERSION} *)
  val root: t -> switch -> dirname

  (** lock file *)
  val lock: t -> switch -> filename

  (** Library path for a given package:
      {i $opam/$OVERSION/lib/NAME} *)
  val lib: t -> switch -> name -> dirname

  (** Library path: {i $opam/$OVERSION/lib/} *)
  val lib_dir: t -> switch -> dirname

  (** DLL paths *)
  val stublibs: t -> switch -> dirname

  (** toplevel path: {i $opam/$OVERSION/lib/toplevel} *)
  val toplevel: t -> switch -> dirname

  (** Documentation path for a given package:
      {i $opam/$OVERSION/doc/NAME} *)
  val doc: t -> switch -> name -> dirname

  (** Documentation path: {i $opam/$OVERSION/doc/} *)
  val doc_dir: t -> switch -> dirname

  (** Shared directory: {i $opam/$OVERSION/share} *)
  val share_dir: t -> switch -> dirname

  (** Share directory for a given package: {i $opam/$OVERSION/share/$package} *)
  val share: t -> switch -> name -> dirname

  (** Man pages path: {i $opam/$OVERSION/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)
  val man_dir: ?num:string -> t -> switch -> dirname

  (** Installed binaries: {i $opam/$OVERSION/bin} *)
  val bin: t -> switch -> dirname

  (** List of installed packages with their version:
      {i $opam/$OVERSION/installed} *)
  val installed: t -> switch -> filename

  (** List of packages expliciterly installed by the user: {i
      $opam/$switch/installed.roots} *)
  val installed_roots: t -> switch -> filename

  (** Tempory folders used to decompress and compile
      the corresponding archives:
      {i $opam/$OVERSION/build/$NAME-$VERSION} *)
  val build: t -> switch -> package -> dirname

  (** Tempory folders used to decompress and compile
      the OCaml compiler:
      {i $opam/$OVERSION/build/_} *)
  val build_ocaml: t -> switch -> dirname

  (** Tempory folder: {i $opam/$OVERSION/build} *)
  val build_dir: t -> switch -> dirname

  (** Tempory location of install files:
      {i $opam/$OVERSION/build/$NAME.$VERSION/$NAME.install} *)
  val build_install: t -> switch -> package -> filename

  (** Tempory location of config files: {i
      $opam/$OVERSION/build/$NAME.$VERSION/$NAME.config} *)
  val build_config: t -> switch -> package -> filename

  (** Installed files for a given package:
      {i $opam/$OVERSION/install/$NAME.install} *)
  val install: t -> switch -> name -> filename

  (** Installed files: {i $opam/$OVERSION/install/} *)
  val install_dir: t -> switch -> dirname

  (** Packages to reinstall on next upgrade:
      {i $opam/$OVERSION/reinstall} *)
  val reinstall: t -> switch -> filename

  (** Compile and link flags for a given package:
      {i $opam/$OVERSION/config/$NAME.config} *)
  val config: t -> switch -> name -> filename

  (** Configuration folder: {i $opam/$OVERSION/config} *)
  val config_dir: t -> switch -> dirname

  (** Pinned package file *)
  val pinned: t -> switch -> filename

  (** Build dir for pinned packages *)
  val pinned_dir: t -> switch -> name -> dirname

end

(** Repository paths *)
module Repository: sig

  (** Get the directory root *)
  val root: repository_root -> dirname

  (** lock file *)
  val lock: repository_root -> filename

   (** Return the repository folder: {i $opam/repo/$repo} *)
  val create: repository_root -> repository_name -> repository_root

  (** Return the version file *)
  val version: repository_root -> filename

  (** Return the repository config: {i $opam/repo/$repo/config} *)
  val config: repository_root -> filename

  (** Packages folder: {i $opam/repo/$repo/packages} *)
  val packages_dir: repository_root -> dirname

  (** Package folder: {i $opam/repo/$repo/packages/$NAME.$VERSION} *)
  val package: repository_root -> package -> dirname

  (** Return the OPAM file for a given package:
      {i $opam/repo/$repo/packages/$NAME.$VERSION/opam} *)
  val opam: repository_root -> package -> filename

  (** Return the description file for a given package:
      {i $opam/repo/$repo/packages/$NAME.VERSION/descr} *)
  val descr: repository_root -> package -> filename

  (** Return the archive for a given package:
      {i $opam/repo/$repo/archives/$NAME.$VERSION.tar.gz} *)
  val archive: repository_root -> package -> filename

  (** Return the archive folder: {i $opam/repo/$repo/archives/} *)
  val archives_dir: repository_root -> dirname

  (** Return the list of updated packages:
      {i $opam/repo/$repo/updated} *)
  val updated: repository_root -> filename

  (** Return the upload folder for a given version:
      {i $opam/repo/$repo/upload/} *)
  val upload_dir: repository_root -> dirname

  (** Compiler files: {i $opam/repo/$repo/compilers/$OVERSION.comp} *)
  val compiler: repository_root -> compiler -> filename

  (** Compiler description files: {i $opam/repo/$repo/compilers/$OVERSION.descr} *)
  val compiler_descr: repository_root -> compiler -> filename

  (** Compiler files: {i $opam/repo/$repo/compilers/} *)
  val compilers_dir: repository_root -> dirname

  (** urls {i $opma/repo/$repo/package/$NAME.$VERSION/url} *)
  val url: repository_root -> package -> filename

  (** files {i $opam/repo/$repo/packages/$NAME.$VERSION/files} *)
  val files: repository_root -> package -> dirname

  (** Tempory folder {i $opam/repo/$repo/tmp} *)
  val tmp: repository_root -> dirname

  (** Tempory folder {i $opam/repo/$repo/tmp/$NAME.$VERSION/} *)
  val tmp_dir: repository_root -> package -> dirname

end
