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

val load: ?lock:lock_kind -> global_state -> repos_state -> switch -> state

(** Loads global, repository and switch state in one go. Using the
    lower-granularity functions is recommended, but this can help for the
    transition. First argument is a debug string (ignored) *)
val load_full_compat: string -> switch -> state

(** Load the switch's state file, without constructing the package maps: much
    faster than loading the full switch state *)
val load_state_file: global_state -> switch -> OpamFile.State.t

val get_switch : state -> switch -> switch_state
(** {2 Helpers to access state data} *)

val state_file: switch_state -> OpamFile.State.t

(** Return the OPAM file for the given package. @raise [Not_found] *)
val opam: switch_state -> package -> OpamFile.OPAM.t

(** Return the OPAM file, including URL and descr, for the given package, if
    any *)
val opam_opt: switch_state -> package -> OpamFile.OPAM.t option

(** Return the URL file for the given package *)
val url: switch_state -> package -> OpamFile.URL.t option

(** Return the Descr file for the given package (or an empty descr if none) *)
val descr: switch_state -> package -> OpamFile.Descr.t

(** Return the Descr file for the given package *)
val descr_opt: switch_state -> package -> OpamFile.Descr.t option

(** Return the files/ directory overlay for the given package, if it exists *)
val files: switch_state -> package -> dirname option

(** Check whether a package name is installed *)
val is_name_installed: switch_state -> name -> bool

(** Return the installed package with the right name
    @raise [Not_found] *)
val find_installed_package_by_name: switch_state -> name -> package

(** Return all packages satisfying one of the given atoms from a state *)
val packages_of_atoms: switch_state -> atom list -> package_set

(** Gets the current version of package [name]: pinned version, installed
    version, max available version or max existing version, tried in this order.
    Raises [Not_found] only if there is no package by this name. *)
val get_package: switch_state -> name -> package

(** "dev packages" are any package with an upstream that isn't the usual HTTP,
    and without an archive checksum. These need to be updated from upstream
    independently when installed. It's generally only the case of source-pinned
    packages, but no rule enforces it in opam itself. *)
val is_dev_package: switch_state -> package -> bool

(** The set of all "dev packages" (see [is_dev_package] for a definition) *)
val dev_packages: switch_state -> package_set

(** Put the package data in a form suitable for the solver, pre-computing some
    maps and sets *)
val universe: state -> user_action -> universe

(** {2 User interaction and reporting } *)

(** Returns [true] if the switch of the state is the one set in
    [$OPAMROOT/config], [false] otherwise. This doesn't imply that the switch is
    current w.r.t. either the process or the shell, for that you need to check
    [OpamStateConfig.(!r.switch_from)] *)
val is_switch_globally_set: state -> bool

(** Returns a message about a package or version that couldn't be found *)
val not_found_message: state -> switch -> atom -> string

(** Returns a printable explanation why a package is not currently available
    (pinned to an incompatible version, unmet [available:] constraints...) *)
val unavailable_reason: state -> switch -> atom -> string
