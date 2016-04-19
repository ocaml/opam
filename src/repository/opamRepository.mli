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

(** Operations on repositories (update, fetch...) based on the different
    backends implemented in separate modules *)

open OpamTypes

(** Get the list of packages *)
val packages: repository -> package_set

(** Get the list of packages (and their possible prefix) *)
val packages_with_prefixes: repository -> string option package_map

(** {2 State} *)

(** Get the package archive checksum off an url file *)
val url_checksum: OpamFile.URL.t OpamFile.t -> checksums

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
  package -> dirname -> string -> OpamFile.URL.t OpamFile.t -> url list ->
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
