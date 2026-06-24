(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2026 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*    Copyright 2026 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the file hierarchy in repositories *)

open OpamTypes

(* {2} Repository paths *)

(** Module type of internal representation of repositories.
    Use the functor [Path]. *)
module type PATH = sig

  (** The type of a repository root, see repository/OpamRepositoryRoot for the
     several types *)
  type repo_root

  (** The type of the returned dirname *)
  type repo_dirname

  (** Type of a filename as possible phantom type *)
  type 'a typed_file

  (** Repository local path: {i $opam/repo/<name>} *)
  val root: dirname -> repository_name -> repo_root

  (** Return the repo file *)
  val repo: repo_root -> OpamFile.Repo.t typed_file

  (** Packages folder: {i $repo/packages} *)
  val packages_dir: repo_root -> repo_dirname

  (** Package folder: {i $repo/packages/XXX/$NAME.$VERSION} *)
  val packages: repo_root -> string option -> package -> repo_dirname

  (** Return the OPAM file for a given package:
      {i $repo/packages/XXX/$NAME.$VERSION/opam} *)
  val opam:
    repo_root -> string option -> package -> OpamFile.OPAM.t typed_file

  (** files {i $repo/packages/XXX/$NAME.$VERSION/files} *)
  val files: repo_root -> string option -> package -> repo_dirname

  (** Return the description file for a given package:
      {i $repo/packages/XXX/$NAME.VERSION/descr} *)
  val descr:
    repo_root -> string option -> package -> OpamFile.Descr_legacy.t typed_file

  (** urls {i $repo/package/XXX/$NAME.$VERSION/url} *)
  val url:
    repo_root -> string option -> package -> OpamFile.URL_legacy.t typed_file

end

(** Representation of pathnames for a specific repository root kind *)
module type PATH_REPR = sig

  (** The repository root *)
  type root

  (** Filenames *)
  type file

  (** Directory names *)
  type dir

  (** Filenames but with phantom types *)
  type 'a typed_file

  (** Repository local path: {i $opam/repo/<name>} *)
  val root : dirname -> repository_name -> root

  (** Returns an absolute filepath that works for the
      targeted repository representation *)
  val absolute : root -> string -> file

  (** Same as [absolute] but for directories *)
  val absolute_dir : root -> dir -> dir

  val dir_of_string : string -> dir
  val to_typed_file : file -> 'a typed_file

  module Op : sig
    val (/): dir -> string -> dir
    val (//): dir -> string -> file
  end

end

(** Instantiate repository paths by giving a filepath representation *)
module Make (I : PATH_REPR) : PATH
  with type repo_root = I.root
   and type repo_dirname = I.dir
   and type 'a typed_file = 'a I.typed_file

(* {2} Other paths *)

(** obsolete, no longer use *)
val repo_tarring: dirname -> repository_name -> filename

(** Prefix where to store the downloaded files cache: {i $opam/download-cache}.
    Warning, this is relative to the opam root, not a repository root. *)
val download_cache: dirname -> dirname

(** Pin global cache, located in temporary directory, cleaned at end of process *)
val pin_cache_dir: unit -> dirname

(** Pin cache for a given download url. *)
val pin_cache: OpamUrl.t -> dirname

(* {2} URL paths *)

(** Url constructor for parts of remote repositories, when applicable (http and
    rsync). Function take the repo's root url. *)
module Remote: sig
  (** Remote repo file *)
  val repo: url -> url

  (** Remote package files: {i $remote/packages} *)
  val packages_url: url -> url

  (** Remote archive {i $remote/archives/$NAME.$VERSION.tar.gz} *)
  val archive: url -> package -> url
end
