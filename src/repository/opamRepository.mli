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

(** Operations on repositories (update, fetch...) based on the different
    backends implemented in separate modules *)

open OpamTypes

(** Get the list of packages *)
val packages: repository -> package_set

(** Get the list of packages (and their possible prefix) *)
val packages_with_prefixes: repository -> string option package_map

(** {2 Repository backends} *)

(** Initialize {i $opam/repo/$repo} *)
val init: dirname -> repository_name -> unit OpamProcess.job

(** Update {i $opam/repo/$repo}. *)
val update: repository -> unit OpamProcess.job

(** Fetch an URL into a directory: if a single file, it will be put in that
    directory, otherwise the given directory is synchronised with the remote
    one. Several mirrors can be provided, in which case they will be tried in
    order, in case of an error.
    All provided hashes are checked in case of a single file; if the hash list
    is non-empty, and a directory is obtained, an error message is printed and
    Not_available returned.
    The [package] argument is only for status message printing. *)
val pull_url:
  package -> ?cache_dir:dirname -> ?cache_urls:url list -> ?silent_hits:bool ->
  dirname -> OpamHash.t list -> url list ->
  generic_file download OpamProcess.job

(** Pull and fix the resulting digest *)
val pull_url_and_fix_digest:
  package -> dirname -> OpamHash.t list -> OpamFile.URL.t OpamFile.t -> url list ->
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
