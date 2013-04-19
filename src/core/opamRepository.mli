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
val default: unit -> repository

(** Create a local repository on a given path *)
val local: dirname -> repository

(** Default repository address *)
val default_address: address

(** Constructor *)
val repository_address: string -> address

(** [get_upstream_update repo packages] checks for upstream changes
    for packages in the [packages] map. Return the packages whose
    contents have changed upstream (ie. either the 'url' file has
    changed, the archive file has changed, or the contents of the
    'files/' sub-directory, or, git and rsync-ed packages, upstream
    contents have changed. *)
val get_upstream_updates:
  repository -> package_repository_state package_map -> package_set

(** [cleanup repo _packages] cleans the tempory files in the given
    repository. [packages] contains all the 'active' package states,
    ie. whose current repository is [repo]. *)
val clean: repository -> package_repository_state package_map -> unit

(** Copy the additional package files in the current dir *)
val copy_files: repository -> package -> dirname -> OpamFilename.Set.t

(** [make_archive ?gener_digest repo package] builds the archive for
    the given [package]. It reads the metada in [repo.repo_root]. By
    default, the digest that appears in {i $NAME.$VERSION/url} is not
    modified, unless [gener_digest] is set. *)
val make_archive: ?gener_digest:bool -> repository -> package -> unit

(** Get the list of packages *)
val packages: repository -> string name_map * package_set

(** Get the list of compilers (and their eventual description file) *)
val compilers: repository -> (filename * filename option) compiler_map

(** Get the external files associated to a package *)
val files: repository -> package -> filename_set

(** Check if a package has a given prefix in the repository *)
val read_prefix: repository -> string name_map

(** Find an eventual prefix in a map *)
val find_prefix: string name_map -> package -> string option

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

(** Update {i $opam/repo/$repo}. *)
val update: repository -> unit

(** Download {i $remote/archives/$nv.tar.gz}. If is not there, then
    download the upstream archive, add the eventual files in it, and
    create {i $nv.tar.gz}. *)
val download: repository -> package -> unit

(** Copy the additional package files in the current dir *)
val copy_files: repository_root -> package -> OpamFilename.Set.t

(** [make_archive repo_kind nv] build ./$nv.tar.gz, assuming the
    repository kind is [repo_kind].
    By default, the digest that appear in
    {i $NAME.$VERSION/url} is not modified,
    unless [gener_digest = true] is given. *)
val make_archive: ?gener_digest:bool -> ?local_path:dirname -> package -> unit

(** Get the list of packages *)
val packages: repository_root -> string name_map * package_set

(** Get the list of compilers (and their eventual description file) *)
val compilers: repository_root -> (filename * filename option) compiler_map

(** Register a repository backend *)
val register_backend: repository_kind -> (module BACKEND) -> unit

(** Find a backend *)
val find_backend: repository_kind -> (module BACKEND)

(** {2 Misc} *)

(** Map-reduce on repositories. *)
val map_reduce: int -> (repository -> 'a) -> ('a -> 'a -> 'a) -> 'a ->
  repository list -> 'a

(** Parallel iteration. *)
val parallel_iter: int -> (repository -> unit) -> repository list -> unit
