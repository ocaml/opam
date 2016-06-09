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

(** Defines the file hierarchy in repositories *)

open OpamTypes

(** Repository local path: {i $opam/repo/<name>} *)
val create: OpamFilename.Dir.t -> repository_name -> dirname

(** Update cache *)
val update_cache: repository -> filename

(** Return the repo file *)
val repo: repository -> OpamFile.Repo.t OpamFile.t

(** Packages folder: {i $opam/repo/$repo/packages} *)
val packages_dir: repository -> dirname

(** Package folder: {i $opam/repo/$repo/packages/XXX/$NAME.$VERSION} *)
val packages: repository -> string option -> package -> dirname

(** Return the OPAM file for a given package:
    {i $opam/repo/$repo/packages/XXX/$NAME.$VERSION/opam} *)
val opam: repository -> string option -> package -> OpamFile.OPAM.t OpamFile.t

(** Return the description file for a given package:
    {i $opam/repo/$repo/packages/XXX/$NAME.VERSION/descr} *)
val descr: repository -> string option -> package -> OpamFile.Descr.t OpamFile.t

(** urls {i $opma/repo/$repo/package/XXX/$NAME.$VERSION/url} *)
val url: repository -> string option -> package -> OpamFile.URL.t OpamFile.t

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


(** Url constructor for parts of remote repositories, when applicable (http and
    rsync) *)
module Remote: sig
  (** Remote repo file *)
  val repo: repository -> url

  (** Remote package files: {i $remote/packages} *)
  val packages_url: repository -> url

  (** Remote archive {i $remote/archives/$NAME.$VERSION.tar.gz} *)
  val archive: repository -> package -> url
end
