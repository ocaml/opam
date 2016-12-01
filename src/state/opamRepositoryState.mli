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

(** loading and handling of the repository state of an opam root (i.e. what is
    in ~/.opam/repo) *)

open OpamTypes

open OpamStateTypes

(** Caching of repository loading (marshall of all parsed opam files) *)
module Cache: sig
  val save: [< rw] repos_state -> unit
  val load:
    dirname ->
    (OpamFile.Repo.t repository_name_map *
     OpamFile.OPAM.t package_map repository_name_map)
      option
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

(** Finds a package repository definition from its name (assuming it's in
    ROOT/repos/) *)
val get_repo: 'a repos_state -> repository_name -> repository

(** Load all the metadata within the local mirror of the given repository,
    without cache *)
val load_repo_opams: repository -> OpamFile.OPAM.t OpamPackage.Map.t

(** Releases any locks on the given repos_state *)
val unlock: 'a repos_state -> unlocked repos_state

(** Calls the provided function, ensuring a temporary write lock on the given
    repository state*)
val with_write_lock:
  ?dontblock:bool -> 'a repos_state -> (rw repos_state -> 'b * rw repos_state) ->
  'b * 'a repos_state

(** Writes the repositories config file back to disk *)
val write_config: rw repos_state -> unit
