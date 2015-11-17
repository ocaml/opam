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

(** The various paths where OPAM configuration files are stored. *)

open OpamTypes

(** {2 Global paths} *)

(** Type of path root *)
type t = dirname

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

(** Temporary folder for dev packages {i $opam/packages.dev/} *)
val dev_packages_dir: t -> dirname

(** Temporary folder for dev packages {i $opam/packages.dev/$NAME.$VERSION/} *)
val dev_package: t -> package -> dirname

(** Description file: {i $opam/packages/$NAME/$NAME.$VERSION/descr} *)
val descr: t -> package -> filename

(** Archives dir *)
val archives_dir: t -> dirname

(** Archive file: {i $opam/archives/$NAME.$VERSION+opam.tar.gz} *)
val archive: t -> package -> filename

(** Compiler files: {i $opam/compilers/$VERSION/$COMP.comp} *)
val compiler_comp: t -> compiler -> filename

(** Compiler description files: {i $opam/compilers/$VERSION/$COMP.descr} *)
val compiler_descr: t -> compiler -> filename

(** Compiler files: {i $opam/compilers/} *)
val compilers_dir: t -> dirname

(** Compiler subdir {i $opam/compilers/$VERSION/$COMP} *)
val compilers: t -> compiler -> dirname

(** Return the repository index: {i $opam/repo/package-index} *)
val package_index: t -> filename

(** Return the repository index: {i $opam/repo/compiler-index} *)
val compiler_index: t -> filename

(** Init scripts *)
val init: t -> dirname

(** Log dir {i $opam/log} *)
val log: t -> dirname

(** The directory where global backups are stored *)
val backup_dir: t -> dirname

(** Backup file for state export *)
val backup: t -> filename

(** Switch related paths *)
module Switch: sig

  (** Locations of opam internal dirs and files *)

  (** Root dir: {i $opam/$switch} *)
  val root: t -> switch -> dirname

  (** lock file: {i $opam/lock} *)
  val lock: t -> switch -> filename

  (** The directory where backups are stored for this switch *)
  val backup_dir: t -> switch -> dirname

  (** Backup file for state export *)
  val backup: t -> switch -> filename

  (** Switch state: currently installed packages, roots, pinnings, etc. {i
      $opam/$switch/state} *)
  val state: t -> switch -> filename

  (** Temporary folders used to decompress and compile
      the corresponding archives:
      {i $opam/$switch/build/$packages} *)
  val build: t -> switch -> package -> dirname

  (** Temporary folders used to decompress and compile the OCaml
      compiler: {i $opam/$switch/build/ocaml} *)
  val build_ocaml: t -> switch -> dirname

  (** Temporary folder: {i $opam/$switch/build} *)
  val build_dir: t -> switch -> dirname

  (** Temporary location of install files: {i
      $opam/$switch/build/$package/$name.install} *)
  val build_install: t -> switch -> package -> filename

  (** Temporary location of config files: {i
      $opam/$switch/build/$packages/$name.config} *)
  val build_config: t -> switch -> package -> filename

  (** Installed files for a given package: {i
      $opam/$switch/install/$name.install} *)
  val install: t -> switch -> name -> filename

  (** Installed files: {i $opam/$switch/install/} *)
  val install_dir: t -> switch -> dirname

  (** Packages to reinstall on next upgrade: {i
      $opam/$switch/reinstall} *)
  val reinstall: t -> switch -> filename

  (** Configuration folder: {i $opam/$switch/config} *)
  val config_dir: t -> switch -> dirname

  (** Global config for the switch: {i
      $opam/$switch/config/global-config.config} *)
  val global_config: t -> switch -> filename

  (** Build dir for all pinned packages: {i
      $opam/$switch/packages.dev/} *)
  val dev_packages_dir: t -> switch -> dirname

  (** Build dir for a given pinned package: {i
      $opam/$switch/packages.dev/$name.$version/} *)
  val dev_package: t -> switch -> name -> dirname

  (** Cached environment updates. *)
  val environment: t -> switch -> filename

  (** Locations for the visible part of the installation *)

  (** Default config *)
  module Default : sig
    (** Library path for a given package:
        {i $opam/$switch/lib/$name} *)
    val lib: t -> switch -> name -> dirname

    (** Compile and link flags for a given package: {i
        $opam/$switch/lib/$name/opam.config} *)
    val config: t -> switch -> name -> filename

    (** Library path: {i $opam/$switch/lib} *)
    val lib_dir: t -> switch -> dirname

    (** DLL paths *)
    val stublibs: t -> switch -> dirname

    (** toplevel path: {i $opam/$switch/lib/toplevel} *)
    val toplevel: t -> switch -> dirname

    (** Documentation path for a given package:
        {i $opam/$switch/doc/$name} *)
    val doc: t -> switch -> name -> dirname

    (** Documentation path: {i $opam/$switch/doc/} *)
    val doc_dir: t -> switch -> dirname

    (** Shared directory: {i $opam/$switch/share} *)
    val share_dir: t -> switch -> dirname

    (** Share directory for a given package: {i
        $opam/$switch/share/$package} *)
    val share: t -> switch -> name -> dirname

    (** Etc directory: {i $opam/$switch/etc} *)
    val etc_dir: t -> switch -> dirname

    (** Etc directory for a given package: {i
        $opam/$switch/etc/$package} *)
    val etc: t -> switch -> name -> dirname

    (** Man pages path: {i $opam/$switch/man/}. The optional
        [num] argument will add a {i manN } suffix if specified *)
    val man_dir: ?num:string -> t -> switch -> dirname

    (** Installed binaries: {i $opam/$switch/bin} *)
    val bin: t -> switch -> dirname

    (** Installed system binaries: {i $opam/$switch/sbin} *)
    val sbin: t -> switch -> dirname
  end

  (** Actual config handling the global-config.config indirections *)

  (** Package-independent dirs *)

  (** Library path: {i $opam/$switch/lib} *)
  val lib_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** DLL paths *)
  val stublibs: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** toplevel path: {i $opam/$switch/lib/toplevel} *)
  val toplevel: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Documentation path: {i $opam/$switch/doc/} *)
  val doc_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Shared directory: {i $opam/$switch/share} *)
  val share_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Etc directory: {i $opam/$switch/etc} *)
  val etc_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Man pages path: {i $opam/$switch/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)
  val man_dir: ?num:string -> t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Installed binaries: {i $opam/$switch/bin} *)
  val bin: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Installed system binaries: {i $opam/$switch/sbin} *)
  val sbin: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Package dependent dirs *)

  (** Library path for a given package:
      {i $opam/$switch/lib/$name} *)
  val lib: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Documentation path for a given package:
      {i $opam/$switch/doc/$name} *)
  val doc: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Share directory for a given package: {i
      $opam/$switch/share/$package} *)
  val share: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Etc directory for a given package: {i
      $opam/$switch/etc/$package} *)
  val etc: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Compile and link flags for a given package: {i
      $opam/$switch/lib/$name/opam.config} *)
  val config: t -> switch -> OpamFile.Dot_config.t -> name -> filename

  module Overlay: sig
    (** Switch metadata overlay (over the global metadata): {i
        $opam/$switch/overlay/} *)
    val dir: t -> switch -> dirname

    (** Switch metadata overlay (over the global metadata): {i
        $opam/$switch/overlay/$name.$version} *)
    val package: t -> switch -> name -> dirname

    (** OPAM overlay: {i
        $opam/$switch/cache/$name.$version/opam} *)
    val opam: t -> switch -> name -> filename

    (** OPAM temp overlay (for user editing): {i
        $opam/$switch/cache/$name.$version/opam_} *)
    val tmp_opam: t -> switch -> name -> filename

    (** URL overlay: {i
        $opam/$switch/overlay/$name.$version/url} *)
    val url: t -> switch -> name -> filename

    (** Descr orverlay *)
    val descr: t -> switch -> name -> filename

    (** Files overlay *)
    val files: t -> switch -> name -> dirname
  end
end
