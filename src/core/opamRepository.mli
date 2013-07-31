(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(** Mangagement of OPAM repositories. *)

open OpamTypes

include OpamMisc.ABSTRACT with type t := repository

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

(** [get_cache_update repo] checks for upstream changes for
    packages in the cache. Return the packages whose contents have
    changed upstream (ie. either the 'url' file has changed, the
    archive file has changed, or the contents of the 'files/'
    sub-directory, or, git and rsync-ed packages, upstream contents
    have changed. *)
val get_cache_updates: repository -> package_set

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

(** {2 State} *)

(** Compute a compiler state *)
val compiler_state: repository -> compiler -> compiler_repository_state

(** Compute a package state. Raise [Not_found] if no such package
    exists in the repository. *)
val package_state: repository -> string option -> package -> package_repository_state

(** {2 Repository backends} *)

(** Initialize {i $opam/repo/$repo} *)
val init: repository -> unit

(** Update {i $opam/repo/$repo}. *)
val update: repository -> unit

(** Backend signature *)
module type BACKEND = sig

  (** [pull_url package local_dir remote_url] pull the contents of
      [remote_url] into [local_dir]. Can return either a file or a
      directory. *)
  val pull_url: package -> dirname -> string -> generic_file download

  (** [pull_repo] pull the contents of a repository. *)
  val pull_repo: repository -> unit

  (** [pull_archive repo archive] pull [archive] in the given
      repository. *)
  val pull_archive: repository -> filename -> filename download

  (** Return the (optional) revision of a given repository. Only useful
      for VCS backends. *)
  val revision: repository -> version option

end

(** Signature for pull functions *)
type pull_fn = repository_kind -> package -> dirname -> string -> generic_file download

(** Download an url *)
val pull_url: pull_fn

(** Pull and check the resulting digest *)
val pull_and_check_digest: checksum:string -> pull_fn

(** Pull and fix the resultging digest *)
val pull_and_fix_digest: file:filename -> checksum:string -> pull_fn

(** Pull an archive in a repository *)
val pull_archive: repository -> package -> filename download

(** Get the optional revision associated to a backend. *)
val revision: repository -> version option

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
