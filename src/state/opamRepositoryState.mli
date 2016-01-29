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

open OpamTypes

open OpamStateTypes

(** Caching of repository loading (marshall of all parsed opam files) *)
module Cache : sig
  val save: repos_state -> unit
  val load: unit -> OpamFile.OPAM.t package_map option
  val remove: unit -> unit
end

val load:
  ?save_cache:bool -> ?lock:lock_kind -> global_state -> repos_state

(** Downloads the repository-mirrored package archive into
    $opam/archives/$name.$version+opam.tar.gz if not already there, and
    returns the file name if found either way *)
val download_archive: repos_state -> package -> filename option OpamProcess.job


(** "states" denote a given instance of the metadata for a given package and
    version. "partial" is used to decide that the package needs to be recompiled
    (the boolean indicating the presence of a local repackaged archive),
    "full" is for all metadata including e.g. [descr] *)

(** Global compiler state *)
val compiler_state: repos_state -> checksums compiler_map

(** Repository state *)
val compiler_repository_state: repos_state -> checksums compiler_map

(** Build a map which says in which repository the latest metadata for
    a given compiler is. *)
val compiler_index:
  repos_state -> (repository_name * string option) compiler_map

(** Global package state. *)
val package_state: repos_state -> checksums package_map

(** Global & partial package state. *)
val package_partial_state: package -> archive:bool -> bool * checksums

(** Repository state *)
val package_repository_state: repos_state -> checksums package_map

(** Repository & partial package state. *)
val package_repository_partial_state: repos_state -> package -> archive:bool ->
  bool * checksums

(** Builds a map which says in which repository the latest metadata
    for a given package are. The function respect the bustom
    priorities given by the order of [priorities]. *)
val package_index:
  repos_state -> (repository_name * string option) package_map

(** Get the active repository for a given package *)
val repository_of_package: repos_state -> package -> repository option
