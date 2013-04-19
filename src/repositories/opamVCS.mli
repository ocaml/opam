(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Generic backend for version-control systems. *)

open OpamTypes

(** Each backend should implement this signature. *)
module type VCS = sig

  (** Test whether the given repository is correctly initialized. *)
  val exists: repository -> bool

  (** Init a repository. *)
  val init: repository -> unit

  (** Fetch changes from upstream. This is supposed to put the changes
      in a staging area. *)
  val fetch: repository -> unit

  (** Merge the staging area into the master branch of the
      repository. *)
  val merge: repository -> unit

  (** Check whether the staging area is empty. *)
  val diff: repository -> bool
end

(** Create a backend from a [VCS] implementation. *)
module Make(VCS: VCS): OpamRepository.BACKEND
