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

(** Defines on-disk package repositories, synchronised with an upstream *)

open OpamTypes

(** Get the list of packages *)
val packages: repository -> package_set

(** Get the list of packages (and their possible prefix) *)
val packages_with_prefixes: repository -> string option package_map

(** Get the list of all compiler *)
val compilers: repository -> compiler_set

(** Get the list of compilers (and their possible prefix) *)
val compilers_with_prefixes: repository -> string option compiler_map

(** {2 Repository Collection Operations } *)

(** Sort a collection of repositories by priority *)
val sort: repository repository_name_map -> repository list

(** Generate a package index from a collection of repositories *)
val package_index: repository repository_name_map -> (repository_name * string option) package_map

(** Generate a compiler index from a collection of repositories *)
val compiler_index: repository repository_name_map -> (repository_name * string option) compiler_map

(** {2 State} *)

(** Get the package archive checksum off an url file *)
val url_checksum: OpamFilename.t -> checksums

(** Get all the package files *)
val package_files: repository -> string option -> package -> archive:bool ->
  filename list

(** Compute a package state (ie. a list of checksums). If [`partial archive],
    only the checksum of the archive within the url file (instead of the file
    itself), of the files/ subdirectory, and of the archive if set are
    returned. *)
val package_state: repository -> string option -> package ->
  [`all|`partial of bool]
  -> checksums

(** Get all the compiler files *)
val compiler_files: repository -> string option -> compiler -> filename list

(** Compute a compiler state (ie. a list of checksums). *)
val compiler_state: repository -> string option -> compiler -> checksums

(** {2 Repository backends} *)

(** Initialize {i $opam/repo/$repo} *)
val init: repository -> unit OpamProcess.job

(** Update {i $opam/repo/$repo}. *)
val update: repository -> unit OpamProcess.job

(** Error and exit on incompatible version *)
val check_version: repository -> unit OpamProcess.job

(** Download an url. Several mirrors can be provided, in which case they will be
    tried in order in case of an error. *)
val pull_url:
  package -> dirname -> string option -> url list ->
  generic_file download OpamProcess.job

(** Pull and fix the resulting digest *)
val pull_url_and_fix_digest:
  package -> dirname -> string -> filename -> url list ->
  generic_file download OpamProcess.job

(** Pull an archive in a repository *)
val pull_archive: repository -> package -> filename download OpamProcess.job

(** Get the optional revision associated to a backend. *)
val revision: repository -> version option OpamProcess.job

(** [make_archive ?gener_digest repo prefix package] builds the
    archive for the given [package]. By default, the digest that
    appears in {i $NAME.$VERSION/url} is not modified, unless
    [gener_digest] is set. *)
val make_archive: ?gener_digest:bool -> repository -> string option -> package -> unit OpamProcess.job

(** Find a backend *)
val find_backend: repository -> (module OpamRepositoryBackend.S)
val find_backend_by_kind: OpamUrl.backend -> (module OpamRepositoryBackend.S)
