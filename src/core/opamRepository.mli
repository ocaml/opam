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

(** Default repository address*)
val default_address: address

(** Pretty-print *)
val to_string: repository -> string

(** Compare repositories *)
val compare: repository -> repository -> int

(** Default repository *)
val default: unit -> repository

(** Create a local repository on a given path *)
val local: dirname -> repository

(** Get the list of packages *)
val packages: repository -> package_set

(** Get the list of packages (and their eventual prefixes) *)
val packages_with_prefixes: repository -> string option package_map

(** Get the list of all compiler *)
val compilers: repository -> compiler_set

(** Get the list of compilers (and their eventual prefixes) *)
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

(** Backend signature *)
module type BACKEND = sig

  (** [pull_url package local_dir checksum remote_url] pull the contents of
      [remote_url] into [local_dir]. Can return either a file or a
      directory. [checksum] is the optional expected checksum. *)
  val pull_url: package -> dirname -> string option -> address -> generic_file download OpamProcess.job

  (** [pull_repo] pull the contents of a repository. *)
  val pull_repo: repository -> unit OpamProcess.job

  (** [pull_archive repo archive] pull [archive] in the given
      repository. *)
  val pull_archive: repository -> filename -> filename download OpamProcess.job

  (** Return the (optional) revision of a given repository. Only useful
      for VCS backends. *)
  val revision: repository -> version option OpamProcess.job

end

(** Download an url. Several mirrors can be provided, in which case they will be
    tried in order in case of an error. *)
val pull_url: repository_kind ->
  package -> dirname -> string option -> address list ->
  generic_file download OpamProcess.job

(** Pull and fix the resulting digest *)
val pull_url_and_fix_digest: repository_kind ->
  package -> dirname -> string -> filename -> address list ->
  generic_file download OpamProcess.job

(** [check_digest file expected] check that the [file] digest is the
    one [expected]. *)
val check_digest: filename -> string option -> bool

(** Pull an archive in a repository *)
val pull_archive: repository -> package -> filename download OpamProcess.job

(** Get the optional revision associated to a backend. *)
val revision: repository -> version option OpamProcess.job

(** [make_archive ?gener_digest repo prefix package] builds the
    archive for the given [package]. By default, the digest that
    appears in {i $NAME.$VERSION/url} is not modified, unless
    [gener_digest] is set. *)
val make_archive: ?gener_digest:bool -> repository -> string option -> package -> unit OpamProcess.job

(** Register a repository backend *)
val register_backend: repository_kind -> (module BACKEND) -> unit

(** Find a backend *)
val find_backend: repository_kind -> (module BACKEND)

(** {2 Misc} *)

(** Parallel iterations *)
module Parallel: OpamParallel.SIG with type G.V.t = repository
