(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
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
val packages: dirname -> package_set

(** Get the list of packages (and their possible prefix) *)
val packages_with_prefixes: dirname -> string option package_map

(** {2 Repository backends} *)

(** Update {i $opam/repo/$repo}. Raises [Failure] in case the update couldn't be
    achieved. *)
val update: repository -> dirname -> unit OpamProcess.job

(** Fetch an URL and put the resulting tree into the supplied directory. The URL
    must either point to a tree (VCS, rsync) or to a known archive type. In case
    of an archive, the cache is used and supplied the hashes verified, then the
    archive uncompressed. In case of a version-controlled URL, it's checked out,
    or synchronised directly if local and [working_dir] was set. *)
val pull_tree:
  string -> ?cache_dir:dirname -> ?cache_urls:url list -> ?working_dir:bool ->
  ?subpath:string -> dirname -> OpamHash.computable_kind OpamHash.hash list -> url list ->
  string download OpamProcess.job

(** Same as [pull_tree], but for fetching a single file. *)
val pull_file:
  string -> ?cache_dir:dirname -> ?cache_urls:url list -> ?silent_hits:bool ->
  filename -> OpamHash.computable_kind OpamHash.hash list -> url list ->
  unit download OpamProcess.job

(** Same as [pull_file], but without a destination file: just ensures the file
    is present in the cache. *)
val pull_file_to_cache:
  string -> cache_dir:dirname -> ?cache_urls:url list ->
  OpamHash.computable_kind OpamHash.hash list -> url list -> string download OpamProcess.job

(** The file where the file with the given hash is stored under cache at given
    dirname. *)
val cache_file: dirname -> OpamHash.kind OpamHash.hash -> filename

(** Get the optional revision associated to a backend (git hash, etc.). *)
val revision: dirname -> url -> version option OpamProcess.job

(** Get the version-control branch for that url. Only applicable for local,
    version controlled URLs. Returns [None] in other cases. *)
val current_branch: url -> string option OpamProcess.job

(** Returns true if the url points to a local, version-controlled directory that
    has uncommitted changes *)
val is_dirty: ?subpath:string -> url -> bool OpamProcess.job

(** Find a backend *)
val find_backend: repository -> (module OpamRepositoryBackend.S)
val find_backend_by_kind: OpamUrl.backend -> (module OpamRepositoryBackend.S)

(** Prints user messages upon the result of a download *)
val report_fetch_result: package -> string download -> unit download
