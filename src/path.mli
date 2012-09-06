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

(** Compiler-version related state *)
module C: sig

  (** Contains {i $opam} and {i $OVERSION} *)
  type t

  (** Create a compiler path *)
  val create: Alias.t -> t

  (** Root dir: {i $opam/$OVERSION} *)
  val root: t -> dirname
   
  (** Library path for a given package:
      {i $opam/$OVERSION/lib/NAME} *)
  val lib: t -> N.t -> dirname

  (** Library path: {i $opam/$OVERSION/lib/} *)
  val lib_dir: t -> dirname

  (** DLL paths *)
  val stublibs: t -> dirname

  (** toplevel path: {i $opam/$OVERSION/lib/toplevel} *)
  val toplevel: t -> dirname

  (** Documentation path for a given package:
      {i $opam/$OVERSION/doc/NAME} *)
  val doc: t -> N.t -> dirname

  (** Documentation path: {i $opam/$OVERSION/doc/} *)
  val doc_dir: t -> dirname

  (** Man pages path: {i $opam/$OVERSION/man/} *)
  val man_dir: t -> dirname

  (** Installed binaries: {i $opam/$OVERSION/bin} *)
  val bin: t -> dirname

  (** List of installed packages with their version:
      {i $opam/$OVERSION/installed} *)
  val installed: t -> filename

  (** Tempory folders used to decompress and compile 
      the corresponding archives:
      {i $opam/$OVERSION/build/$NAME-$VERSION} *)
  val build: t -> NV.t -> dirname

  (** Tempory folders used to decompress and compile 
      the OCaml compiler:
      {i $opam/$OVERSION/build/_} *)
  val build_ocaml: t -> dirname

  (** Tempory folder: {i $opam/$OVERSION/build} *)
  val build_dir: t -> dirname

  (** A file containing the env variables in which build command are
      processed: {i $opam/$OVERSION/build/$NAME.$VERSION/$NAME.env} *)
  val build_env: t -> NV.t -> filename

  (** A file containing a copy of the current env variables, before 
      the env variables for the build are set:
      {i $opam/$OVERSION/build/$NAME.$VERSION/$NAME.old.env} *)
  val build_old_env: t -> NV.t -> filename

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

  (** Pinned package file *)
  val pinned: t -> filename
end

(** Global state *)
module G: sig

  (** Contains {i $opam} *)
  type t

  (** Create a global path *)
  val create: unit -> t

  (** Root dir: {i $opam/} *)
  val root: t -> dirname

  (** Main configuration file: {i $opam/config} *)
  val config: t -> filename

  (** Compiler aliases *)
  val aliases: t -> filename

  (** OPAM files: {i $opam/opam/$NAME.$VERSION.opam} *)
  val opam: t -> NV.t -> filename

  (** Compiler files: {i $opam/compilers/$OVERSION.comp} *)
  val compiler: t -> OCaml_V.t -> filename

  (** Compiler files: {i $opam/compilers/} *)
  val compilers_dir: t -> dirname

  (** All the compiler files *)
  val available_compilers: t -> OCaml_V.Set.t

  (** List all the available packages:
      {i $opam/opam/$NAME.$VERSION.opam} *)
  val available_packages: t -> NV.Set.t

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
  val archives_dir: t -> dirname

  (** Return the repository index: {i $opam/repo/index} *)
  val repo_index: t -> filename

  (** Available aliases *)
  val available_aliases: t -> Alias.Set.t

end

(** Repository related *)
module R: sig

  type t

  (** Create a repository path *)
  val create: repository -> t

  (** Transform a directory name into a repository path *)
  val of_dirname: dirname -> t

  (** Create a repository path with the current working directory *)
  val cwd: unit -> t

  (** Return the repository folder: {i $opam/repo/$repo} *)
  val root: t -> dirname

  (** Return the repository config: {i $opam/repo/$repo/config} *)
  val config: t -> filename

  (** Packages folder: {i $opam/repo/$repo/packages} *)
  val packages_dir: t -> dirname

  (** Package folder: {i $opam/repo/$repo/packages/$NAME.$VERSION} *)
  val package: t -> NV.t -> dirname

  (** Return the OPAM file for a given package:
      {i $opam/repo/$repo/packages/$NAME.$VERSION/opam} *)
  val opam: t -> NV.t -> filename

  (** List all the available packages:
      {i $opam/repo/$repo/packages/$NAME.$VERSION/opam} *)
  val available_packages: t -> NV.Set.t

  (** List all the available versions for a given package:
      {i $opam/repo/$repo/packages/[name].$VERSION/opam} *)
  val available_versions: t -> N.t -> V.Set.t

  (** Return the description file for a given package:
      {i $opam/repo/$repo/packages/$NAME.VERSION/descr} *)
  val descr: t -> NV.t -> filename

  (** Return the archive for a given package:
      {i $opam/repo/$repo/archives/$NAME.$VERSION.tar.gz} *)
  val archive: t -> NV.t -> filename

  (** Return the archive folder: {i $opam/repo/$repo/archives/} *)
  val archives_dir: t -> dirname

  (** Return the list of archive files in {i $opam/repo/$repo/archives} *)
  val available_archives: t -> Filename.Set.t

  (** Return the list of updated packages:
      {i $opam/repo/$repo/updated} *)
  val updated: t -> filename

  (** Return the upload folder for a given version:
      {i $opam/repo/$repo/upload/} *)
  val upload_dir: t -> dirname

  (** Compiler files: {i $opam/repo/$repo/compilers/$OVERSION.comp} *)
  val compiler: t -> OCaml_V.t -> filename

  (** Compiler files: {i $opam/repo/$repo/compilers/} *)
  val compilers_dir: t -> dirname

  (** All the compiler files *)
  val available_compilers: t -> OCaml_V.Set.t

  (** urls {i $opma/repo/$repo/package/$NAME.$VERSION/url} *)
  val url: t -> nv -> filename

  (** files {i $opam/repo/$repo/packages/$NAME.$VERSION/files} *)
  val files: t -> nv -> dirname

  (** All files in the file dir *)
  val available_files: t -> nv -> filename list

  (** Tempory folder {i $opam/repo/$repo/tmp/$NAME.$VERSION/} *)
  val tmp_dir: t -> nv -> dirname

  (** Available packages in the temp dir *)
  val available_tmp: t -> NV.Set.t
end
