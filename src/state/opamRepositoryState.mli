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

(** loading and handling of the repository state of an opam root (i.e. what is
    in ~/.opam/repo) *)

open OpamTypes

open OpamStateTypes

(** Caching of repository loading (marshall of all parsed opam files) *)
module Cache: sig
  val save: [< rw] repos_state -> unit
  val load: dirname -> OpamFile.OPAM.t package_map repository_name_map option
  val remove: unit -> unit
end

val load: 'a lock -> [< unlocked ] global_state -> 'a repos_state

(** Loads the repository state as [load], and calls the given function while
    keeping it locked (as per the [lock] argument), releasing the lock
    afterwards *)
val with_:
  'a lock -> [< unlocked ] global_state -> ('a repos_state -> 'b) -> 'b

(** Returns the repo of origin and metadata corresponding to a package, if
    found, from a sorted list of repositories (highest priority first) *)
val find_package_opt: 'a repos_state -> repository_name list -> package ->
  (repository_name * OpamFile.OPAM.t) option

(** Given the repos state, and a list of repos to use (highest priority first),
    build a map of all existing package definitions *)
val build_index:
  'a repos_state -> repository_name list -> OpamFile.OPAM.t OpamPackage.Map.t

(** List of repos name ordered by decreasing priority (note: this is at the
    moment included in the repos config, but is intended to move to the switch
    configs) *)
val repos_list: 'a repos_state -> repository_name list

(** Load all the metadata within the local mirror of the given repository,
    without cache *)
val load_repo_opams: repository -> OpamFile.OPAM.t OpamPackage.Map.t

(** Downloads the repository-mirrored package archive into the repo's cache
    ($opam/repo/$repo/archives) ; returns None if not found *)
val download_archive: 'a repos_state -> repository_name list ->
  package -> filename option OpamProcess.job

(** Releases any locks on the given repos_state *)
val unlock: 'a repos_state -> unlocked repos_state

(** Calls the provided function, ensuring a temporary write lock on the given
    repository state*)
val with_write_lock:
  ?dontblock:bool -> 'a repos_state -> (rw repos_state -> 'c) -> 'c
