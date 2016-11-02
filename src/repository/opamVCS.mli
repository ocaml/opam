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

(** Layer for handling version control sources through a functor *)

open OpamTypes

(** Each backend should implement this signature. *)
module type VCS = sig

  val name: OpamUrl.backend

  (** Test whether the given repository is correctly initialized. *)
  val exists: dirname -> bool

  (** Init a repository. *)
  val init: dirname -> url -> unit OpamProcess.job

  (** Fetch changes from upstream. This is supposed to put the changes
      in a staging area.
      Be aware that the remote URL might have been changed, so make sure
      to update accordingly. *)
  val fetch: dirname -> url -> unit OpamProcess.job

  (** Reset the master branch of the repository to match the remote
      repository state. *)
  val reset: dirname -> url -> unit OpamProcess.job

  (** Check whether the staging area is empty. Returns true if not (eg. there is
      an update pending) *)
  val diff: dirname -> url -> bool OpamProcess.job

  (** Return the HEAD revision. *)
  val revision: dirname -> string OpamProcess.job

  (** Returns the list of files under version control *)
  val versionned_files: dirname -> string list OpamProcess.job

  (** Returns the absolute directory name for vc data (e.g.
      [.../project/.git]) *)
  val vc_dir: dirname -> dirname
end

(** Create a backend from a [VCS] implementation. *)
module Make(VCS: VCS): OpamRepositoryBackend.S
