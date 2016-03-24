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
  [< unlocked ] global_state ->
  atom list -> bool option -> deps_only:bool -> upgrade:bool -> unit

(** Reinstall the given set of packages. *)
val reinstall:
  [< unlocked ] global_state -> atom list -> unit

(** Refresh the available packages. *)
val update:
  [< unlocked ] global_state ->
  repos_only:bool -> dev_only:bool -> ?no_stats:bool ->
  string list -> unit

(** Find a consistent state where most of the installed packages are
    upgraded to their latest version, within the given constraints.
    An empty list means upgrade all installed packages. *)
val upgrade: [< unlocked ] global_state -> atom list -> unit

(** Recovers from an inconsistent universe *)
val fixup: [< unlocked ] global_state -> unit

(** Remove the given list of packages. *)
val remove:
  [< unlocked ] global_state -> autoremove:bool -> force:bool -> atom list -> unit

module PIN: sig

  (** Set a package pinning. if [pin_option] is [None], set the package defined
      upstream. If [action], prompt for install/reinstall as appropriate after
      pinning. *)
  val pin:
    [< unlocked ] global_state ->
    OpamPackage.Name.t ->
    ?edit:bool -> ?version:version -> ?action:bool ->
    pin_option option -> unit

  val edit:
    [< unlocked ] global_state -> ?action:bool -> OpamPackage.Name.t -> unit

  val unpin:
    [< unlocked ] global_state ->
    ?action:bool -> OpamPackage.Name.t list -> unit

  (** List the current pinned packages. *)
  val list: [< unlocked ] global_state -> short:bool -> unit -> unit

end
