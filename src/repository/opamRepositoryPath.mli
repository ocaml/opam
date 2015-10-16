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

open OpamTypes

(** Repository local path: {i $opam/repo/<name>} *)
val create: OpamFilename.Dir.t -> repository_name -> dirname

(** Update cache *)
val update_cache: repository -> filename

(** Return the repo file *)
val repo: repository -> filename

(** Return the repository config from the opam root and the repo name:
    {i $opam/repo/$repo/config} *)
val raw_config: dirname -> repository_name -> filename

(** Return the repository config: {i $opam/repo/$repo/config} *)
val config: repository -> filename

(** Packages folder: {i $opam/repo/$repo/packages} *)
val packages_dir: repository -> dirname

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


(** Url constructor for parts of remote repositories, when applicable (http and
    rsync) *)
module Remote: sig
  (** Remote repo file *)
  val repo: repository -> url

  (** Remote package files: {i $remote/packages} *)
  val packages_url: repository -> url

  (** Remote archive {i $remote/archives/$NAME.$VERSION.tar.gz} *)
  val archive: repository -> package -> url

  (** Remote compiler files: {i $remote/compilers} *)
  val compilers_url: repository -> url
end
