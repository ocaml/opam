(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Signature for repository handlers and some helpers for the repository
    type *)

open OpamTypes

(** Type returned by repository updates. *)
type update =
  | Update_full of dirname
  (** No previous known state, the full contents have been put in the given
      temporary directory *)
  | Update_patch of filename
  (** The given patch file corresponds to the update, i.e. applying it to the
      local repository with 'patch -p1' would get it to the upstream state *)
  | Update_empty
  (** The repository is already up to date *)
  | Update_err of exn
  (** Failed to obtain the update *)

(** Backend signature *)
module type S = sig

  val name: OpamUrl.backend

  (** [pull_url local_dir checksum remote_url] pulls the contents of
      [remote_url] into [local_dir].

      Two kinds of results are allowed:

      - a single file was downloaded, in this case it is placed within
        [local_dir] and returned as [Some filename]

      - a directory was retrieved, in this case the contents of [local_dir] have
        been synchronised with its own, and [None] is returned

      [checksum] can be used for retrieval but is NOT checked by this
      function. *)
  val pull_url:
    ?cache_dir:dirname -> dirname -> OpamHash.t option -> url ->
    filename option download OpamProcess.job

  (** [pull_repo_update] fetches the remote update from [url] to the local
      repository at [dirname], but does not apply it, allowing for further
      verifications. The file or directory returned is always temporary and
      should be cleaned up by the caller. *)
  val fetch_repo_update:
    repository_name -> ?cache_dir:dirname -> dirname -> url ->
    update OpamProcess.job

  (** [repo_update_complete dirname url] finalizes the update of the repository
      after verification of the patch returned from [pull_repo_update] with
      [Update_patch file] is applied. Version control systems, e.g. Mercurial,
      that track the state of the working directory automatically use this to
      update internal caches. *)
  val repo_update_complete: dirname -> url -> unit OpamProcess.job

  (** Return the (optional) revision of a given repository. Only useful for VCS
      backends. Is not expected to work with [pull_repo_update], which doesn't
      update the VCS commit information. *)
  val revision: dirname -> version option OpamProcess.job

  (** Like [pull_url], except for locally-bound version control backends, where
      it should get the latest, uncommited source. *)
  val sync_dirty:
    dirname -> url -> filename option download OpamProcess.job

end

(** Pretty-print *)
val to_string: repository -> string
val to_json: repository -> json

(** Compare repositories *)
val compare: repository -> repository -> int

(** Create a local repository on a given path, without remote (only for external
    tools, not to be mistaken for an opam repo with a local url) *)
val local: dirname -> repository

(** [check_digest file expected] check that the [file] digest is the
    one [expected]. *)
val check_digest: filename -> OpamHash.t option -> bool

(** Adds a label to the given job, for the correspondig repository name and
    action *)
val job_text:
  repository_name -> string -> 'a OpamProcess.job -> 'a OpamProcess.job

(** [get_diff parent_dir subdir1 subdir2] computes the diff between the two
    subdirs of [parent_dir], returns None if they are equal, and the
    corresponding patch otherwise.

    Note: this relies on the [diff -ruN] command, a built-in diff may be more
    portable -- in particular, [-u], [-N] are not POSIX, and recursive diffs
    might not be completely reliable. It also assumes text files only, and fails
    otherwise. *)
val get_diff: dirname -> basename -> basename -> filename option OpamProcess.job
