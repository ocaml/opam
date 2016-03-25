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

(** Client entry-point. *)

open OpamTypes
open OpamStateTypes

(** Initialize the client a consistent state. *)
val init:
  repository -> shell -> filename -> [`ask|`yes|`no] ->
  unlocked repos_state

(** Install the given list of packages. Second argument, if not None, specifies
    that given packages should be added or removed from the roots.
    Third argument installs all dependencies but not the packages themselves *)
val install:
  rw switch_state ->
  atom list -> bool option -> deps_only:bool -> upgrade:bool ->
  rw switch_state

(** Reinstall the given set of packages. *)
val reinstall:
  rw switch_state -> atom list -> rw switch_state

(** Refresh the available packages. *)
val update:
  [< unlocked ] global_state ->
  repos_only:bool -> dev_only:bool -> ?no_stats:bool ->
  string list -> unit

(** Find a consistent state where most of the installed packages are
    upgraded to their latest version, within the given constraints.
    An empty list means upgrade all installed packages. *)
val upgrade: rw switch_state -> atom list -> rw switch_state

(** Recovers from an inconsistent universe *)
val fixup: rw switch_state -> rw switch_state

(** Remove the given list of packages. *)
val remove:
  rw switch_state -> autoremove:bool -> force:bool -> atom list ->
  rw switch_state

module PIN: sig

  (** Set a package pinning. if [pin_option] is [None], set the package defined
      upstream. If [action], prompt for install/reinstall as appropriate after
      pinning. *)
  val pin:
    rw switch_state ->
    OpamPackage.Name.t ->
    ?edit:bool -> ?version:version -> ?action:bool ->
    pin_option option ->
    rw switch_state

  val edit:
    rw switch_state -> ?action:bool -> OpamPackage.Name.t -> rw switch_state

  val unpin:
    rw switch_state ->
    ?action:bool -> OpamPackage.Name.t list -> rw switch_state

  (** List the current pinned packages. *)
  val list: 'a switch_state -> short:bool -> unit

end
