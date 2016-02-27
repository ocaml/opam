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

(** lock file *)
val lock: t -> filename

(** Main configuration file: {i $opam/config} *)
val config: t -> OpamFile.Config.t OpamFile.t

(** Temporary folder for dev packages {i $opam/packages.dev/} *)
val dev_packages_dir: t -> dirname

(** Temporary folder for dev packages {i $opam/packages.dev/$NAME.$VERSION/} *)
val dev_package: t -> package -> dirname

(** Archives dir *)
val archives_dir: t -> dirname

(** Archive file: {i $opam/archives/$NAME.$VERSION+opam.tar.gz} *)
val archive: t -> package -> filename

(** Return the repository index: {i $opam/repo/package-index} *)
val package_index: t -> OpamFile.Package_index.t OpamFile.t

(** Init scripts *)
val init: t -> dirname

(** Log dir {i $opam/log} *)
val log: t -> dirname

(** The directory where global backups are stored *)
val backup_dir: t -> dirname

(** Backup file for state export *)
val backup: t -> switch_selections OpamFile.t

(** Switch related paths *)
module Switch: sig

  (** Locations of opam internal dirs and files *)

  (** The switch prefix: {i $opam/$switch} *)
  val root: t -> switch -> dirname

  (** The subdirectory of the prefix where opam data lives:
      {i $opam/$switch/.opam-switch}*)
  val meta: t -> switch -> dirname

  (** lock file: {i $meta/lock} *)
  val lock: t -> switch -> filename

  (** The directory where backups are stored for this switch *)
  val backup_dir: t -> switch -> dirname

  (** Backup file for state export *)
  val backup: t -> switch -> switch_selections OpamFile.t

  (** Switch selections {i $meta/switch-state} *)
  val selections: t -> switch -> switch_selections OpamFile.t

  (** Temporary folders used to decompress and compile
      the corresponding archives:
      {i $meta/build/$packages} *)
  val build: t -> switch -> package -> dirname

  (** Temporary folders used to decompress and compile the OCaml
      compiler: {i $meta/build/ocaml} *)
  val build_ocaml: t -> switch -> dirname

  (** Temporary folder: {i $meta/build} *)
  val build_dir: t -> switch -> dirname

  (** Temporary location of install files: {i
      $meta/build/$package/$name.install} *)
  val build_install: t -> switch -> package -> OpamFile.Dot_install.t OpamFile.t

  (** Temporary location of config files: {i
      $meta/build/$packages/$name.config} *)
  val build_config: t -> switch -> package -> OpamFile.Dot_config.t OpamFile.t

  (** Installed files for a given package: {i
      $meta/install/$name.install} *)
  val install: t -> switch -> name -> OpamFile.Dot_install.t OpamFile.t

  (** Installed files: {i $meta/install/} *)
  val install_dir: t -> switch -> dirname

  (** Packages to reinstall on next upgrade: {i
      $meta/reinstall} *)
  val reinstall: t -> switch -> OpamFile.PkgList.t OpamFile.t

  (** Configuration folder: {i $meta/config} *)
  val config_dir: t -> switch -> dirname

  (** Global config for the switch: {i
      $meta/config/global-config.config} *)
  val global_config: t -> switch -> OpamFile.Dot_config.t OpamFile.t

  (** Package-specific configuration file for installed packages: {i
      $meta/config/$name.config} *)
  val config: t -> switch -> name -> OpamFile.Dot_config.t OpamFile.t

  (** Source dir for all pinned packages: {i
      $meta/packages.dev/} *)
  val dev_packages_dir: t -> switch -> dirname

  (** Build dir for a given pinned package: {i
      $meta/packages.dev/$name.$version/} *)
  val dev_package: t -> switch -> name -> dirname

  (** Cached environment updates. *)
  val environment: t -> switch -> OpamFile.Environment.t OpamFile.t

  (** Directory where the metadata of installed packages is mirrored.
      {i $meta/packages/} *)
  val installed_opams: t -> switch -> dirname

  (** The mirror of the opam file for the given installed package
      {i $meta/packages/$name.$version.opam} *)
  val installed_opam: t -> switch -> package -> OpamFile.OPAM.t OpamFile.t

  (** Locations for the visible part of the installation *)

  (** Default config *)
  module Default : sig
    (** Library path for a given package:
        {i $prefix/lib/$name} *)
    val lib: t -> switch -> name -> dirname

    (** Library path: {i $prefix/lib} *)
    val lib_dir: t -> switch -> dirname

    (** DLL paths *)
    val stublibs: t -> switch -> dirname

    (** toplevel path: {i $prefix/lib/toplevel} *)
    val toplevel: t -> switch -> dirname

    (** Documentation path for a given package:
        {i $prefix/doc/$name} *)
    val doc: t -> switch -> name -> dirname

    (** Documentation path: {i $prefix/doc/} *)
    val doc_dir: t -> switch -> dirname

    (** Shared directory: {i $prefix/share} *)
    val share_dir: t -> switch -> dirname

    (** Share directory for a given package: {i
        $prefix/share/$package} *)
    val share: t -> switch -> name -> dirname

    (** Etc directory: {i $prefix/etc} *)
    val etc_dir: t -> switch -> dirname

    (** Etc directory for a given package: {i
        $prefix/etc/$package} *)
    val etc: t -> switch -> name -> dirname

    (** Man pages path: {i $prefix/man/}. The optional
        [num] argument will add a {i manN } suffix if specified *)
    val man_dir: ?num:string -> t -> switch -> dirname

    (** Installed binaries: {i $prefix/bin} *)
    val bin: t -> switch -> dirname

    (** Installed system binaries: {i $prefix/sbin} *)
    val sbin: t -> switch -> dirname
  end

  (** Actual config handling the global-config.config indirections *)

  (** Package-independent dirs *)

  (** Library path: {i $prefix/lib} *)
  val lib_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** DLL paths *)
  val stublibs: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** toplevel path: {i $prefix/lib/toplevel} *)
  val toplevel: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Documentation path: {i $prefix/doc/} *)
  val doc_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Shared directory: {i $prefix/share} *)
  val share_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Etc directory: {i $prefix/etc} *)
  val etc_dir: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Man pages path: {i $prefix/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)
  val man_dir: ?num:string -> t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Installed binaries: {i $prefix/bin} *)
  val bin: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Installed system binaries: {i $prefix/sbin} *)
  val sbin: t -> switch -> OpamFile.Dot_config.t -> dirname

  (** Package dependent dirs *)

  (** Library path for a given package:
      {i $prefix/lib/$name} *)
  val lib: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Documentation path for a given package:
      {i $prefix/doc/$name} *)
  val doc: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Share directory for a given package: {i
      $prefix/share/$package} *)
  val share: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  (** Etc directory for a given package: {i
      $prefix/etc/$package} *)
  val etc: t -> switch -> OpamFile.Dot_config.t -> name -> dirname

  module Overlay: sig
    (** Switch metadata overlay (over the global metadata): {i
        $meta/overlay/} *)
    val dir: t -> switch -> dirname

    (** Switch metadata overlay (over the global metadata): {i
        $meta/overlay/$name.$version} *)
    val package: t -> switch -> name -> dirname

    (** OPAM overlay: {i
        $meta/overlay/$name.$version/opam} *)
    val opam: t -> switch -> name -> OpamFile.OPAM.t OpamFile.t

    (** OPAM temp overlay (for user editing): {i
        $meta/overlay/$name.$version/opam_} *)
    val tmp_opam: t -> switch -> name -> OpamFile.OPAM.t OpamFile.t

    (** URL overlay: {i
        $meta/overlay/$name.$version/url} *)
    val url: t -> switch -> name -> OpamFile.URL.t OpamFile.t

    (** Descr orverlay *)
    val descr: t -> switch -> name -> OpamFile.Descr.t OpamFile.t

    (** Files overlay *)
    val files: t -> switch -> name -> dirname
  end
end
