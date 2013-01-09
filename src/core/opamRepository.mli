(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Mangagement of OPAM repositories. *)

open OpamTypes

exception Unknown_backend

(** Pretty-print *)
val to_string: repository -> string

(** Compare repositories *)
val compare: repository -> repository -> int

(** Default repository *)
val default: repository

(** Default repository address *)
val default_address: address

(** Constructor *)
val repository_address: string -> address

(** Create a dummy local repository *)
val local_repo: unit -> repository_root

(** Initialize {i $opam/repo/$repo} *)
val init: repository -> unit

(** Update {i $opam/repo/$repo} *)
val update: repository -> unit

(** Download {i $opam/repo/$repo/archive/$nv.tar.gz} *)
val download: repository -> package -> unit

(** Upload the content of {i $opam/repo/$repo/upload} to the remote
    repository.*)
val upload: repository -> unit

(** {2 Repository backends} *)

(** Backend signature *)
module type BACKEND = sig

  (** Initialize an OPAM repository in the current directory. The
      argument is the remote repository address. *)
  val init: address:address -> unit

  (** Update the OPAM repository in the current directory. Return the
      list of locally updated files. *)
  val update: address:address -> OpamFilename.Set.t

  (** Download a (remote) archive file, stored on the (remote) OPAM
      repository, in the current repository. Return the local path to
      the downloaded archive.*)
  val download_archive: address:address -> package -> filename download

  (** Download a (remote) file and return the local path to the
      downloaded file: {i $repo/tmp/$package/$filename}. *)
  val download_file: ?checksum:string -> package -> filename -> filename download

  (** Download a (remote) directory and return the local path to the
      downloaded directory: {i $repo/tmp/$package/$dirname}. *)
  val download_dir: package -> ?dst:dirname -> address -> dirname download

  (** Upload the content of the current directory to the directory
      given as argument. Return the local paths corresponding to the
      uploaded local files. *)
  val upload_dir: address:dirname -> address -> OpamFilename.Set.t

end

(** Register a repository backend *)
val register_backend: repository_kind -> (module BACKEND) -> unit

(** Find a backend *)
val find_backend: repository_kind -> (module BACKEND)

(** Copy the additional package files in the current dir *)
val copy_files: repository_root -> package -> OpamFilename.Set.t

(** [make_archive repo_kind nv] build ./$nv.tar.gz, assuming the
    repository kind is [repo_kind].
    By default, the digest that appear in
    {i $NAME.$VERSION/url} is not modified,
    unless [gener_digest = true] is given. *)
val make_archive: ?gener_digest:bool -> ?local_path:dirname -> package -> unit

(** Get the list of packages *)
val packages: repository_root -> package_set

(** Get the list of compilers *)
val compilers: repository_root -> compiler_set

(** Get the available versions for a given compiler *)
val versions: repository_root -> name -> version_set

(** Get the external files associated to a package *)
val files: repository_root -> package -> filename_set
