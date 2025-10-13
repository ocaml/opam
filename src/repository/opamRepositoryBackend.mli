(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2019 OCamlPro                                        *)
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
  | Update_full of OpamRepositoryRoot.t
  (** No previous known state, the full contents have been put in the given
      directory *)
  | Update_patch of (filename * Patch.t list)
  (** A patch file that corresponds to the update, i.e. applying it to the
      local repository with 'patch -p1' would get it to the upstream state,
      and its and its list of file-level operations *)
  | Update_empty
  (** The repository is already up to date *)
  | Update_err of exn
  (** Failed to obtain the update *)

(** Backend signature *)
module type S = sig

  val name: OpamUrl.backend

  (** [pull_url ?full_fetch ?cache_dir ?subpath local_dir checksum remote_url]
      pulls the contents of [remote_url] into [local_dir].

      Two kinds of results are allowed:

      - a single file was downloaded, in this case it is placed within
        [local_dir] and returned as [Some filename]

      - a directory was retrieved, in this case the contents of [local_dir] have
        been synchronised with its own, and [None] is returned

      [checksum] can be used for retrieval but is NOT checked by this
      function.

      If [full_fetch] is set to false, VCS repository is retrieved with shallow
      history (by default, full history).
      If [cache_dir] is given, the directory is used by VCS tool as a its cache
      directory.
      If [subpath] is given, only that [subpath] of the url is retrieved. *)
  val pull_url:
    ?full_fetch:bool ->
    ?cache_dir:dirname -> ?subpath:subpath -> dirname -> OpamHash.t option ->
    url -> filename option download OpamProcess.job

  (** [fetch_repo_update] fetches the remote update from [url] to the local
      repository at [dirname], but does not apply it, allowing for further
      verifications. The file or directory returned is always temporary and
      should be cleaned up by the caller. *)
  val fetch_repo_update:
    repository_name -> ?cache_dir:dirname -> OpamRepositoryRoot.t -> url ->
    update OpamProcess.job

  (** [repo_update_complete dirname url] finalizes the update of the repository
      after verification of the patch returned from {!fetch_repo_update} with
      [Update_patch file] is applied. Version control systems, e.g. Mercurial,
      that track the state of the working directory automatically use this to
      update internal caches. *)
  val repo_update_complete: OpamRepositoryRoot.t -> url -> unit OpamProcess.job

  (** Return the (optional) revision of a given repository. Only useful for VCS
      backends. Is not expected to work with [fetch_repo_update], which doesn't
      update the VCS commit information. *)
  val revision: dirname -> string option OpamProcess.job

  (** Like {!pull_url}, except for locally-bound version control backends, where
      it should get the latest, uncommitted source. First, it performs a
      {!pull_url}, then remove deleted files, and finally copy via rsync
      unversioned & modified-uncommitted files. *)
  val sync_dirty:
    ?subpath:subpath -> dirname -> url ->
    filename option download OpamProcess.job

  (** [get_remote_url ?hash dirname] return the distant url of repo [dirname], \
      if found. When [hash] is specified, it checks that this hash (branch or \
      commit) is present in the distant repository and returns the url with \
      this hash. If the hash is absent it returns the remote url with no hash. *)
  val get_remote_url:
    ?hash:string -> dirname -> url option OpamProcess.job
end

(** Pretty-print *)
val to_string: repository -> string
val to_json: repository -> json

(** Compare repositories *)
val compare: repository -> repository -> int

(** [check_digest file expected] check that the [file] digest is the
    one [expected]. *)
val check_digest: filename -> OpamHash.t option -> bool

(** Adds a label to the given job, for the corresponding repository name and
    action *)
val job_text:
  repository_name -> string -> 'a OpamProcess.job -> 'a OpamProcess.job

(** [get_diff parent_dir subdir1 subdir2] computes the diff between the two
    subdirs of [parent_dir], returns None if they are equal, and the
    corresponding patch and the list of file-changes otherwise.

    @raise Stdlib.Failure if an unsupported file type or comparison is
    detected in any of [subdir1] or [subdir2].
    Unsupported file types: symlinks, character devices, block devices,
    named pipes, sockets.
    Unsupported comparison: comparison between regular files and directories. *)
val get_diff_dirs: dirname -> basename -> basename -> (filename * Patch.t list) option

(** [get_diff_tars tar1 tar2] computes the diff between two tar.gz files.
    Returns None if they are equal, and the corresponding
    patch and the list of file-changes otherwise. *)
val get_diff_tars: filename -> filename -> (filename * Patch.t list) option

(** [get_diff_tar_dir tar_file dir] computes the diff between a tar.gz file
    and a directory. Returns None if they are equal, and the corresponding
    patch and the list of file-changes otherwise.

    Treats the tar file as the old state and the directory as the new state. *)
val get_diff_tar_dir: filename -> dirname -> (filename * Patch.t list) option

(** [get_diff_dir_tar dir tar_file] computes the diff between a directory
    and a tar.gz file. Returns None if they are equal, and the corresponding
    patch and the list of file-changes otherwise.

    Treats the directory as the old state and the tar file as the new state. *)
val get_diff_dir_tar: dirname -> filename -> (filename * Patch.t list) option
