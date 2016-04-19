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

(** Loading and querying a switch state *)

open OpamTypes
open OpamStateTypes

val load:
  'a lock -> 'b global_state -> 'c repos_state -> switch -> 'a switch_state

(** Loads the switch state and calls the given function on it, releasing the
    lock afterwards.

    The repository state is automatically loaded if not provided.

    The switch is selected, if not set, using [OpamStateConfig.get_switch] --
    which can fail if no switch is configured.

    Additionally, in case of a write lock, a backup is saved and a message is
    printed on restoring if [f] raised an exception and there were changes.  *)
val with_:
  'a lock -> ?rt:([< unlocked ] repos_state) -> ?switch:switch ->
  [< unlocked ] global_state ->
  ('a switch_state -> 'b) -> 'b

(** Creates a virtual state with all package available and nothing installed.
    Useful for querying and simulating actions when no switch is yet
    configured *)
val load_virtual: 'a global_state -> 'b repos_state -> unlocked switch_state

(** Load the switch's state file, without constructing the package maps: much
    faster than loading the full switch state *)
val load_selections: 'a global_state -> switch -> switch_selections

(** Raw function to compute the availability of all packages, in [opams], given
    the switch configuration and the set of pinned packages. (The result is
    precomputed in global_state.available_packages once the state is loaded) *)
val compute_available_packages:
  'a global_state -> switch -> OpamFile.Dot_config.t ->
  pinned:package_set -> opams:OpamFile.OPAM.t package_map ->
  package_set

(** Releases any locks on the given switch_state *)
val unlock: 'a switch_state -> unlocked switch_state

(** Calls the provided function, ensuring a temporary write lock on the given
    switch state *)
val with_write_lock:
  ?dontblock:bool -> 'a switch_state -> (rw switch_state -> 'c) -> 'c

(** {2 Helpers to access state data} *)

val selections: 'a switch_state -> switch_selections

(** Return the OPAM file for the given package.
    @raise Not_found when appropriate *)
val opam: 'a switch_state -> package -> OpamFile.OPAM.t

(** Return the OPAM file, including URL and descr, for the given package, if
    any *)
val opam_opt: 'a switch_state -> package -> OpamFile.OPAM.t option

(** Return the URL file for the given package *)
val url: 'a switch_state -> package -> OpamFile.URL.t option

(** Return the Descr file for the given package (or an empty descr if none) *)
val descr: 'a switch_state -> package -> OpamFile.Descr.t

(** Return the Descr file for the given package *)
val descr_opt: 'a switch_state -> package -> OpamFile.Descr.t option

(** Returns the full paths of overlay files under the files/ directory *)
val files: 'a switch_state -> package -> filename list

(** Return the installed package's local configuration *)
val package_config: 'a switch_state -> name -> OpamFile.Dot_config.t

(** Check whether a package name is installed *)
val is_name_installed: 'a switch_state -> name -> bool

(** Return the installed package with the right name
    @raise Not_found when appropriate *)
val find_installed_package_by_name: 'a switch_state -> name -> package

(** Return all packages satisfying one of the given atoms from a state *)
val packages_of_atoms: 'a switch_state -> atom list -> package_set

(** Gets the current version of package [name]: pinned version, installed
    version, max available version or max existing version, tried in this order.
    @raise Not_found only if there is no package by this name *)
val get_package: 'a switch_state -> name -> package

(** "dev packages" are any package with an upstream that isn't the usual HTTP,
    and without an archive checksum. These need to be updated from upstream
    independently when installed. It's generally only the case of source-pinned
    packages, but no rule enforces it in opam itself. *)
val is_dev_package: 'a switch_state -> package -> bool

(** The set of all "dev packages" (see [is_dev_package] for a definition) *)
val dev_packages: 'a switch_state -> package_set

(** Put the package data in a form suitable for the solver, pre-computing some
    maps and sets *)
val universe: 'a switch_state -> user_action -> universe

(** {2 Updating} *)

(** Sets the given opam file for the given package, updating the other related
    fields along the way *)
val update_package_metadata:
  package -> OpamFile.OPAM.t -> 'a switch_state -> 'a switch_state

(** Removes the metadata associated to the given package, also updating the
    packages and available sets. *)
val remove_package_metadata: package -> 'a switch_state -> 'a switch_state

(** Like [update_package_metadata], but also ensures the package is pinned to
    the given version. Also marks it for reinstall if changed. *)
val update_pin: package -> OpamFile.OPAM.t -> 'a switch_state -> 'a switch_state

(** {2 User interaction and reporting } *)

(** Returns [true] if the switch of the state is the one set in
    [$OPAMROOT/config], [false] otherwise. This doesn't imply that the switch is
    current w.r.t. either the process or the shell, for that you need to check
    [OpamStateConfig.(!r.switch_from)] *)
val is_switch_globally_set: 'a switch_state -> bool

(** Returns a message about a package or version that couldn't be found *)
val not_found_message: 'a switch_state -> atom -> string

(** Returns a printable explanation why a package is not currently available
    (pinned to an incompatible version, unmet [available:] constraints...) *)
val unavailable_reason: 'a switch_state -> atom -> string
