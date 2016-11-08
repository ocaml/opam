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

(** Backend signature *)
module type S = sig

  val name: OpamUrl.backend

  (** [pull_url local_dir checksum remote_url] pulls the contents of
      [remote_url] into [local_dir]. Can return either a file or a directory.
      [checksum] is the optional expected checksum, and is here in case it is
      useful for retrieval, but is not expected to be verified by this
      function (and it doesn't apply to directories). *)
  val pull_url:
    dirname -> OpamHash.t option -> url ->
    generic_file download OpamProcess.job

  (** [pull_repo] pulls the contents of a repository, creating or updating the
      contents of the given directory. *)
  val pull_repo: repository_name -> dirname -> url -> unit OpamProcess.job

  (** [pull_archive repo repo_root archive_url] pulls [archive] assuming it
      belongs to the given repository. *)
  val pull_archive: repository_name -> dirname -> url -> filename download OpamProcess.job

  (** Return the (optional) revision of a given repository. Only useful
      for VCS backends. *)
  val revision: dirname -> version option OpamProcess.job

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
