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

(** Defines the file hierarchy in ~/.opam *)

open OpamTypes

(** {2 Global paths} *)

(** Type of path root *)
type t = dirname

(** State cache *)
val state_cache: t -> filename

(** Global lock file for the whole opamroot. Opam should generally read-lock
    this (e.g. initialisation and format upgrades require a write lock) *)
val lock: t -> filename

(** Main configuration file: {i $opam/config} *)
val config: t -> OpamFile.Config.t OpamFile.t

(** The list of configuration files location used by default ({i /etc/opamrc}
    and {i ~/.opamrc}). More general (lower priority) first. *)
val init_config_files: unit -> OpamFile.InitConfig.t OpamFile.t list

(** Lock for updates on the main config file (write lock when changes to
    switches, repositories lists are expected. No lock needed otherwise) *)
val config_lock: t -> filename

(** Archives dir *)
val archives_dir: t -> dirname

(** Archive file: {i $opam/archives/$NAME.$VERSION+opam.tar.gz} *)
val archive: t -> package -> filename

(** Global lock file for the repositories mirrors: {i $opam/repo/lock} *)
val repos_lock: t -> filename

(** Global config file for the repositories mirrors:
    {i $opam/repo/repos-config} *)
val repos_config: t -> OpamFile.Repos_config.t OpamFile.t

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

  (** The name of the subdir of the switch prefix where opam data is stored
      (".opam-switch") *)
  val meta_dirname: string

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

  (** Temporary folders used to decompress the corresponding archives, used only
      for package removal {i $meta/remove/$packages} *)
  val remove: t -> switch -> package -> dirname

  (** Temporary folder: {i $meta/build} *)
  val build_dir: t -> switch -> dirname

  (** Temporary folder: {i $meta/remove} *)
  val remove_dir: t -> switch -> dirname

  (** Installed files for a given package: {i
      $meta/install/$name.install} *)
  val install: t -> switch -> name -> OpamFile.Dot_install.t OpamFile.t

  (** File registering the changes made by the installation of the given package
      {i $meta/install/$name.changes} *)
  val changes: t -> switch -> name -> OpamDirTrack.t OpamFile.t

  (** Installed files: {i $meta/install/} *)
  val install_dir: t -> switch -> dirname

  (** Packages to reinstall on next upgrade: {i
      $meta/reinstall} *)
  val reinstall: t -> switch -> OpamFile.PkgList.t OpamFile.t

  (** Configuration folder: {i $meta/config} *)
  val config_dir: t -> switch -> dirname

  (** Global config for the switch: {i $meta/switch-config} *)
  val switch_config: t -> switch -> OpamFile.Switch_config.t OpamFile.t

  (** Package-specific configuration file for installed packages: {i
      $meta/config/$name.config} *)
  val config: t -> switch -> name -> OpamFile.Dot_config.t OpamFile.t

  (** Clean, uncompressed sources for this switch: {i $meta/sources/} *)
  val sources_dir: t -> switch -> dirname

  (** Clean, uncompressed source directory for this package: {i
      $meta/sources/$name.$version/} *)
  val sources: t -> switch -> package -> dirname

  (** Mirror of the sources for a given pinned package: {i
      $meta/sources/$name/} (without version) *)
  val pinned_package: t -> switch -> name -> dirname

  (** Cached environment updates. *)
  val environment: t -> switch -> OpamFile.Environment.t OpamFile.t

  (** Like [environment], but from the switch prefix dir *)
  val env_relative_to_prefix: dirname -> OpamFile.Environment.t OpamFile.t

  (** Directory where the metadata of installed packages is mirrored.
      {i $meta/packages/} *)
  val installed_opams: t -> switch -> dirname

  (** The mirror of the package definition for the given installed package {i
      $meta/packages/$name.$version/} *)
  val installed_package_dir: t -> switch -> package -> dirname

  (** The mirror of the opam file for the given installed package
      {i $meta/packages/$name.$version/opam} *)
  val installed_opam: t -> switch -> package -> OpamFile.OPAM.t OpamFile.t

  (** Mirror of the extra files attached to the package definitions of installed
      packages
      {i $meta/packages/$name.$version/files/} *)
  val installed_opam_files_dir: t -> switch -> package -> dirname

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

  val get_stdpath:
    t -> switch -> OpamFile.Switch_config.t -> std_path -> dirname

  (** Library path: {i $prefix/lib} *)
  val lib_dir: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** DLL paths *)
  val stublibs: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** toplevel path: {i $prefix/lib/toplevel} *)
  val toplevel: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Documentation path: {i $prefix/doc/} *)
  val doc_dir: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Shared directory: {i $prefix/share} *)
  val share_dir: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Etc directory: {i $prefix/etc} *)
  val etc_dir: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Man pages path: {i $prefix/man/}. The optional
      [num] argument will add a {i manN } suffix if specified *)
  val man_dir: ?num:string -> t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Installed binaries: {i $prefix/bin} *)
  val bin: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Installed system binaries: {i $prefix/sbin} *)
  val sbin: t -> switch -> OpamFile.Switch_config.t -> dirname

  (** Package dependent dirs *)

  (** Library path for a given package:
      {i $prefix/lib/$name} *)
  val lib: t -> switch -> OpamFile.Switch_config.t -> name -> dirname

  (** Documentation path for a given package:
      {i $prefix/doc/$name} *)
  val doc: t -> switch -> OpamFile.Switch_config.t -> name -> dirname

  (** Share directory for a given package: {i
      $prefix/share/$package} *)
  val share: t -> switch -> OpamFile.Switch_config.t -> name -> dirname

  (** Etc directory for a given package: {i
      $prefix/etc/$package} *)
  val etc: t -> switch -> OpamFile.Switch_config.t -> name -> dirname

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

(** Location of package-specific files relative to their build directory *)
module Builddir: sig

  (** package.install file: {i $builddir/$name.install} *)
  val install: dirname -> package -> OpamFile.Dot_install.t OpamFile.t

  (** package.config file: {i $builddir/$name.config} *)
  val config: dirname -> package -> OpamFile.Dot_config.t OpamFile.t

end
