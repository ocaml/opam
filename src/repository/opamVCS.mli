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

(** Generic backend for version-control systems. *)

open OpamTypes

(** Each backend should implement this signature. *)
module type VCS = sig

  val name: OpamUrl.backend

  (** Test whether the given repository is correctly initialized. *)
  val exists: repository -> bool

  (** Init a repository. *)
  val init: repository -> unit OpamProcess.job

  (** Fetch changes from upstream. This is supposed to put the changes
      in a staging area.
      Be aware that the remote URL might have been changed, so make sure
      to update accordingly. *)
  val fetch: repository -> unit OpamProcess.job

  (** Reset the master branch of the repository to match the remote
      repository state. *)
  val reset: repository -> unit OpamProcess.job

  (** Check whether the staging area is empty. Returns true if not (eg. there is
      an update pending) *)
  val diff: repository -> bool OpamProcess.job

  (** Return the HEAD revision. *)
  val revision: repository -> string OpamProcess.job

  (** Returns the list of files under version control *)
  val versionned_files: repository -> string list OpamProcess.job

  (** Returns the absolute directory name for vc data (e.g.
      [.../project/.git]) *)
  val vc_dir: repository -> dirname
end

(** Create a backend from a [VCS] implementation. *)
module Make(VCS: VCS): OpamRepositoryBackend.S
