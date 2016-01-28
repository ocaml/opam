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

(* Opam Root Directory *)
val root : unit -> dirname

(** State cache *)
val state_cache: unit -> filename

(** Update cache *)
val update_cache: unit -> filename

(** lock file *)
val lock: unit -> filename

(** Main configuration file: {i $opam/config} *)
val config: unit -> filename

(** Compiler aliases *)
val aliases: unit -> filename

(** Package directroy {i $opam/packages/} *)
val packages_dir: unit -> dirname

(** Package sub-directory {i $opam/packages/$NAME/$NAME.$VERSION/} *)
val packages: package -> dirname

(** OPAM files: {i $opam/packages/$NAME/$NAME.$VERSION/opam} *)
val opam: package -> filename

(** URL files: {i $opam/packages/$NAME/$NAME.$VERSION/url} *)
val url: package -> filename

(** Additional files: {i $opam/packages/$NAME/$NAME.$VERSION/files} *)
val files: package -> dirname

(** Temporary folder for dev packages {i $opam/packages.dev/} *)
val dev_packages_dir: unit -> dirname

(** Temporary folder for dev packages {i $opam/packages.dev/$NAME.$VERSION/} *)
val dev_package: package -> dirname

(** Description file: {i $opam/packages/$NAME/$NAME.$VERSION/descr} *)
val descr: package -> filename

(** Archives dir *)
val archives_dir: unit -> dirname

(** Archive file: {i $opam/archives/$NAME.$VERSION+opam.tar.gz} *)
val archive: package -> filename

(** Compiler files: {i $opam/compilers/$VERSION/$COMP.comp} *)
val compiler_comp: compiler -> filename

(** Compiler description files: {i $opam/compilers/$VERSION/$COMP.descr} *)
val compiler_descr: compiler -> filename

(** Compiler files: {i $opam/compilers/} *)
val compilers_dir: unit -> dirname

(** Compiler subdir {i $opam/compilers/$VERSION/$COMP} *)
val compilers: compiler -> dirname

(** Return the repository index: {i $opam/repo/package-index} *)
val package_index: unit -> filename

(** Return the repository index: {i $opam/repo/compiler-index} *)
val compiler_index: unit -> filename

(** Init scripts *)
val init: unit -> dirname

(** Log dir {i $opam/log} *)
val log: unit -> dirname

(** The directory where global backups are stored *)
val backup_dir: unit -> dirname

(** Backup file for state export *)
val backup: unit -> filename

(** Switch related paths *)
module Switch : sig

  (** Locations of opam internal dirs and files *)

  (** Root dir: {i $opam/$switch} *)
  val root: switch -> dirname

  (** lock file: {i $opam/lock} *)
  val lock: switch -> filename

  (** The directory where backups are stored for this switch *)
  val backup_dir: switch -> dirname

  (** Backup file for state export *)
  val backup: switch -> filename

  (** Switch state: currently installed packages, roots, pinnings, etc. {i
      $opam/$switch/state} *)
  val state: switch -> filename

  (** Temporary folders used to decompress and compile
      the corresponding archives:
      {i $opam/$switch/build/$packages} *)
  val build: switch -> package -> dirname

  (** Temporary folders used to decompress and compile the OCaml
      compiler: {i $opam/$switch/build/ocaml} *)
  val build_ocaml: switch -> dirname

  (** Temporary folder: {i $opam/$switch/build} *)
  val build_dir: switch -> dirname

  (** Temporary location of install files: {i
      $opam/$switch/build/$package/$name.install} *)
  val build_install: switch -> package -> filename

  (** Temporary location of config files: {i
      $opam/$switch/build/$packages/$name.config} *)
  val build_config: switch -> package -> filename

  (** Installed files for a given package: {i
      $opam/$switch/install/$name.install} *)
  val install: switch -> name -> filename

  (** Installed files: {i $opam/$switch/install/} *)
  val install_dir: switch -> dirname

  (** Packages to reinstall on next upgrade: {i
      $opam/$switch/reinstall} *)
  val reinstall: switch -> filename

  (** Configuration folder: {i $opam/$switch/config} *)
  val config_dir: switch -> dirname

  (** Global config for the switch: {i
      $opam/$switch/config/global-config.config} *)
  val global_config: switch -> filename

  (** Package-specific configuration file for installed packages: {i
      $opam/$switch/config/$name.config} *)
  val config: switch -> name -> filename

  (** Source dir for all pinned packages: {i
      $opam/$switch/packages.dev/} *)
  val dev_packages_dir: switch -> dirname

  (** Build dir for a given pinned package: {i
      $opam/$switch/packages.dev/$name.$version/} *)
  val dev_package: switch -> name -> dirname

  (** Cached environment updates. *)
  val environment: switch -> filename

  (** Locations for the visible part of the installation *)

  (** Default config *)
  module Default : sig
    (** Library path for a given package:
        {i $opam/$switch/lib/$name} *)
    val lib: switch -> name -> dirname

    (** Library path: {i $opam/$switch/lib} *)
    val lib_dir: switch -> dirname

    (** DLL paths *)
    val stublibs: switch -> dirname

    (** toplevel path: {i $opam/$switch/lib/toplevel} *)
    val toplevel: switch -> dirname

    (** Documentation path for a given package:
        {i $opam/$switch/doc/$name} *)
    val doc: switch -> name -> dirname

    (** Documentation path: {i $opam/$switch/doc/} *)
    val doc_dir: switch -> dirname

    (** Shared directory: {i $opam/$switch/share} *)
    val share_dir: switch -> dirname

    (** Share directory for a given package: {i
        $opam/$switch/share/$package} *)
    val share: switch -> name -> dirname

    (** Etc directory: {i $opam/$switch/etc} *)
    val etc_dir: switch -> dirname

    (** Etc directory for a given package: {i
        $opam/$switch/etc/$package} *)
    val etc: switch -> name -> dirname

    (** Man pages path: {i $opam/$switch/man/}. The optional
        [num] argument will add a {i manN } suffix if specified *)
    val man_dir: ?num:string -> switch -> dirname

    (** Installed binaries: {i $opam/$switch/bin} *)
    val bin: switch -> dirname

    (** Installed system binaries: {i $opam/$switch/sbin} *)
    val sbin: switch -> dirname
  end

  (** Actual config handling the global-config.config indirections *)

  (** Package-independent dirs *)

  (** Library path: {i $opam/$switch/lib} *)
  val lib_dir: switch -> OpamFile.Dot_config.t -> dirname

  (** DLL paths *)
  val stublibs: switch -> OpamFile.Dot_config.t -> dirname

  (** toplevel path: {i $opam/$switch/lib/toplevel} *)
  val toplevel: switch -> OpamFile.Dot_config.t -> dirname

  (** Documentation path: {i $opam/$switch/doc/} *)
  val doc_dir: switch -> OpamFile.Dot_config.t -> dirname

  (** Shared directory: {i $opam/$switch/share} *)
  val share_dir: switch -> OpamFile.Dot_config.t -> dirname

  (** Etc directory: {i $opam/$switch/etc} *)
  val etc_dir: switch -> OpamFile.Dot_config.t -> dirname

  (** Man pages path: {i $opam/$switch/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)
  val man_dir: ?num:string -> switch -> OpamFile.Dot_config.t -> dirname

  (** Installed binaries: {i $opam/$switch/bin} *)
  val bin: switch -> OpamFile.Dot_config.t -> dirname

  (** Installed system binaries: {i $opam/$switch/sbin} *)
  val sbin: switch -> OpamFile.Dot_config.t -> dirname

  (** Package dependent dirs *)

  (** Library path for a given package:
      {i $opam/$switch/lib/$name} *)
  val lib: switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Documentation path for a given package:
      {i $opam/$switch/doc/$name} *)
  val doc: switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Share directory for a given package: {i
      $opam/$switch/share/$package} *)
  val share: switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Etc directory for a given package: {i
      $opam/$switch/etc/$package} *)
  val etc: switch -> OpamFile.Dot_config.t -> name -> dirname

  module Overlay: sig
    (** Switch metadata overlay (over the global metadata): {i
        $opam/$switch/overlay/} *)
    val dir: switch -> dirname

    (** Switch metadata overlay (over the global metadata): {i
        $opam/$switch/overlay/$name.$version} *)
    val package: switch -> name -> dirname

    (** OPAM overlay: {i
        $opam/$switch/cache/$name.$version/opam} *)
    val opam: switch -> name -> filename

    (** OPAM temp overlay (for user editing): {i
        $opam/$switch/cache/$name.$version/opam_} *)
    val tmp_opam: switch -> name -> filename

    (** URL overlay: {i
        $opam/$switch/overlay/$name.$version/url} *)
    val url: switch -> name -> filename

    (** Descr orverlay *)
    val descr: switch -> name -> filename

    (** Files overlay *)
    val files: switch -> name -> dirname
  end
end
