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
module Cache: sig
  val save: repos_state -> unit
  val load: dirname -> OpamFile.OPAM.t package_map option
  val remove: unit -> unit
end

val load:
  ?save_cache:bool -> ?lock:lock_kind -> global_state -> repos_state

(** Downloads the repository-mirrored package archive into
    $opam/archives/$name.$version+opam.tar.gz if not already there, and
    returns the file name if found either way *)
val download_archive: repos_state -> package -> filename option OpamProcess.job

(** Builds a map which says in which repository the latest metadata
    for a given package are. The function respect the bustom
    priorities given by the order of [priorities]. *)
val package_index:
  repos_state -> (repository_name * string option) package_map

(** Get the active repository for a given package *)
val repository_of_package: repos_state -> package -> repository option
