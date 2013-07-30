(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(** The various paths where OPAM configuration files are stored. *)

open OpamTypes

(** {2 Global paths} *)

(** Type of path root *)
type t = dirname

(** Default root path *)
val default: unit -> t

(** State cache *)
val state_cache: t -> filename

(** Update cache *)
val update_cache: t -> filename

(** lock file *)
val lock: t -> filename

(** Main configuration file: {i $opam/config} *)
val config: t -> filename

(** Compiler aliases *)
val aliases: t -> filename

(** Package directroy {i $opam/packages/} *)
val packages_dir: t -> dirname

(** Package sub-directory {i $opam/packages/$NAME/$NAME.$VERSION/} *)
val packages: t -> package -> dirname

(** OPAM files: {i $opam/packages/$NAME/$NAME.$VERSION/opam} *)
val opam: t -> package -> filename

(** URL files: {i $opam/packages/$NAME/$NAME.$VERSION/url} *)
val url: t -> package -> filename

(** Additional files: {i $opam/packages/$NAME/$NAME.$VERSION/files} *)
val files: t -> package -> dirname

(** Tempory folder for dev packages {i $opam/packages.dev/} *)
val dev_packages_dir: t -> dirname

(** Tempory folder for dev packages {i $opam/packages.dev/$NAME.$VERSION/} *)
val dev_packages: t -> package -> dirname

(** Description file: {i $opam/packages/$NAME/$NAME.$VERSION/descr} *)
val descr: t -> package -> filename

(** Archives files: {i $opam/packages/$NAME.$VERSION/$NAME.$VERSION+opam.tar.gz} *)
val archive: t -> package -> filename

(** Compiler files: {i $opam/compilers/$VERSION/$COMP.comp} *)
val compiler_comp: t -> compiler -> filename

(** Compiler description files: {i $opam/compilers/$VERSION/$COMP.descr} *)
val compiler_descr: t -> compiler -> filename

(** Compiler files: {i $opam/compilers/} *)
val compilers_dir: t -> dirname

(** Compiler subdir {i $opam/compilers/$VERSION/$COMP} *)
val compilers: t -> compiler -> dirname

(** Return the repository index: {i $opam/repo/index} *)
val repo_index: t -> filename

(** Init scripts *)
val init: t -> dirname

(** Log dir {i $opam/log} *)
val log: t -> dirname

(** Switch related paths *)
module Switch: sig

  (** Root dir: {i $opam/switches/$switch} *)
  val root: t -> switch -> dirname

  (** lock file: {i $opam/switches/lock} *)
  val lock: t -> switch -> filename

  (** Library path for a given package:
      {i $opam/switches/$switch/lib/$name} *)
  val lib: t -> switch -> name -> dirname

  (** Library path: {i $opam/switches/$switch/lib} *)
  val lib_dir: t -> switch -> dirname

  (** DLL paths *)
  val stublibs: t -> switch -> dirname

  (** toplevel path: {i $opam/switches/$switch/lib/toplevel} *)
  val toplevel: t -> switch -> dirname

  (** Documentation path for a given package:
      {i $opam/switches/$switch/doc/$name} *)
  val doc: t -> switch -> name -> dirname

  (** Documentation path: {i $opam/switches/$switch/doc/} *)
  val doc_dir: t -> switch -> dirname

  (** Shared directory: {i $opam/switches/$switch/share} *)
  val share_dir: t -> switch -> dirname

  (** Share directory for a given package: {i
      $opam/switches/$switch/share/$package} *)
  val share: t -> switch -> name -> dirname

  (** Man pages path: {i $opam/switches/$switch/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)
  val man_dir: ?num:string -> t -> switch -> dirname

  (** Installed binaries: {i $opam/switches/$switch/bin} *)
  val bin: t -> switch -> dirname

  (** List of installed packages with their version:
      {i $opam/switches/$switch/installed} *)
  val installed: t -> switch -> filename

  (** List of packages expliciterly installed by the user: {i
      $opam/switches/$switch/installed.roots} *)
  val installed_roots: t -> switch -> filename

  (** Tempory folders used to decompress and compile
      the corresponding archives:
      {i $opam/switches/$switch/build/$packages} *)
  val build: t -> switch -> package -> dirname

  (** Tempory folders used to decompress and compile the OCaml
      compiler: {i $opam/switches/$switch/build/ocaml} *)
  val build_ocaml: t -> switch -> dirname

  (** Tempory folder: {i $opam/switches/$switch/build} *)
  val build_dir: t -> switch -> dirname

  (** Tempory location of install files: {i
      $opam/switches/$switch/build/$package/$name.install} *)
  val build_install: t -> switch -> package -> filename

  (** Tempory location of config files: {i
      $opam/switches/$switch/build/$packages/$name.config} *)
  val build_config: t -> switch -> package -> filename

  (** Installed files for a given package: {i
      $opam/switches/$switch/install/$name.install} *)
  val install: t -> switch -> name -> filename

  (** Installed files: {i $opam/switches/$switch/install/} *)
  val install_dir: t -> switch -> dirname

  (** Packages to reinstall on next upgrade: {i
      $opam/switches/$switch/reinstall} *)
  val reinstall: t -> switch -> filename

  (** Compile and link flags for a given package: {i
      $opam/switches/$switch/config/$name.config} *)
  val config: t -> switch -> name -> filename

  (** Configuration folder: {i $opam/switches/$switch/config} *)
  val config_dir: t -> switch -> dirname

  (** Pinned package file: {i $opam/switches/$switch/pinned}  *)
  val pinned: t -> switch -> filename

  (** Build dir for all pinned packages: {i
      $opam/switches/$switch/pinned.cache} *)
  val pinned_cache: t -> switch -> dirname

  (** Cached OPAM file: {i
      $opam/switches/$switch/pinned.cache/$name.opam} *)
  val pinned_opam: t -> switch -> name -> filename

  (** Build dir for a given pinned package: {i
      $opam/switches/$switch/pinned.cache/$name/} *)
  val pinned_dir: t -> switch -> name -> dirname

end

(** Repository paths *)
module Repository: sig

  (** Repository local path: {i $opam/repo/<name>} *)
  val create: repository_name -> dirname

  (** Update cache *)
  val update_cache: repository -> filename

  (** Return the version file *)
  val version: repository -> filename

  (** Remote version file *)
  val remote_version: repository -> filename

  (** Return the repository config: {i $opam/repo/$repo/config} *)
  val raw_config: dirname -> repository_name -> filename

  (** Return the repository config: {i $opam/repo/$repo/config} *)
  val config: repository -> filename

  (** Packages folder: {i $opam/repo/$repo/packages} *)
  val packages_dir: repository -> dirname

  (** Remote package files: {i $remote/packages} *)
  val remote_packages_dir: repository -> dirname

  (** Package folder: {i $opam/repo/$repo/packages/XXX/$NAME.$VERSION} *)
  val packages: repository -> string option -> package -> dirname

  (** Return the OPAM file for a given package:
      {i $opam/repo/$repo/packages/XXX/$NAME.$VERSION/opam} *)
  val opam: repository -> string option -> package -> filename

  (** Return the description file for a given package:
      {i $opam/repo/$repo/packages/XXX/$NAME.VERSION/descr} *)
  val descr: repository -> string option -> package -> filename

  (** urls {i $opma/repo/$repo/package/XXX/$NAME.$VERSION/url} *)
  val url: repository -> string option -> package -> filename

  (** files {i $opam/repo/$repo/packages/XXX/$NAME.$VERSION/files} *)
  val files: repository -> string option -> package -> dirname

  (** Return the archive for a given package:
      {i $opam/repo/$repo/archives/$NAME.$VERSION.tar.gz} *)
  val archive: repository -> package -> filename

  (** Remote archive {i $remote/archives/$NAME.$VERSION.tar.gz} *)
  val remote_archive: repository -> package -> filename

  (** Return the archive folder: {i $opam/repo/$repo/archives/} *)
  val archives_dir: repository -> dirname

  (** Return the upload folder for a given version:
      {i $opam/repo/$repo/upload/} *)
  val upload_dir: repository -> dirname

  (** Compiler files: {i $opam/repo/$repo/compilers/} *)
  val compilers_dir: repository -> dirname

  (** Compiler files: {i $opam/repo/$repo/compilers/XXX/$OVERSION.comp} *)
  val compiler_comp: repository -> string option -> compiler -> filename

  (** Compiler description files: {i $opam/repo/$repo/compilers/XXX/$OVERSION.descr} *)
  val compiler_descr: repository -> string option -> compiler -> filename

  (** Remote compiler files: {i $remote/compilers} *)
  val remote_compilers_dir: repository -> dirname

end
