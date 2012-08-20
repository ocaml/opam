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

(** Generic repository pluggin *)

(** The following functions are wrapper to the corresponding
    scripts *)

open Types

(** Initialize {i $opam/repo/$repo} *)
val init: repository -> unit

(** Update {i $opam/repo/$repo} *)
val update: repository -> unit

(** Download {i $opam/repo/$repo/archive/$nv.tar.gz} *)
val download: repository -> nv -> unit

(** Upload the content of {i $opam/repo/$repo/upload} to the remote
    repository.*)
val upload: repository -> unit

(** {2 Repository backends} *)

type address = dirname

(** Backend signature *)
module type BACKEND = sig

  (** Initialize an OPAM repository in the current directory. The
      argument is the remote repository address. *)
  val init: address -> unit

  (** Update the OPAM repository in the current directory. Return the
      list of locally updated files. *)
  val update: address -> Filename.Set.t

  (** Download a (remote) archive file, stored on the (remote) OPAM
      repository, in the current repository. Return the local path to
      the downloaded archive.*)
  val download_archive: address -> nv -> filename download

  (** Download a (remote) file and return the local path to the
      downloaded file. As the opposite to [download_archive], the
      downloaded file needs not to be stored on the remote
      repository. If needed, the function can use {i $repo/tmp/$nv/}
      to store transient states between downloads. *)
  val download_file: nv -> filename -> filename download

  (** Download a (remote) directory and return the local path to the
      downloaded directory. If needed, the function can use {i
      $repo/tmp/$nv/} to store transient states between downloads. *)
  val download_dir: nv -> dirname  -> dirname download

  (** Upload the content of the current directory to the directory
      given as argument. Return the local paths corresponding to the
      uploaded local files. *)
  val upload_dir: address:address -> dirname -> Filename.Set.t

end

type kind = string

(** Register a repository backend *)
val register_backend: kind -> (module BACKEND) -> unit

(** [make_archive repo_kind nv] build ./$nv.tar.gz, assuming the
    repository kind is [repo_kind]. *)
val make_archive: nv -> unit
