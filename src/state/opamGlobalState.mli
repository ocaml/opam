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

val load: lock:'a lock -> unit -> 'a global_state

(** The set of all installed packages, in any switch *)
val all_installed: 'a global_state -> package_set

val fold_switches:
  (switch -> switch_selections -> 'a -> 'a) -> 'b global_state -> 'a -> 'a

(** Returns the map of installed instances of the package name towards the list
    of switches they are installed in *)
val installed_versions: 'a global_state -> name -> switch list package_map

(** Releases any locks on the given global_state *)
val unlock: 'a global_state -> unlocked global_state

(** Calls the provided function, ensuring a temporary write lock on the given
    global state*)
val with_write_lock:
  ?dontblock:bool -> 'a global_state -> (rw global_state -> 'c) -> 'c
