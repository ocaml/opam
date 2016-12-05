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

(** High-level execution of user-facing functions like install and upgrade, and
    wrappers around the pinning commands *)

open OpamTypes
open OpamStateTypes

(** Initialize the client a consistent state. *)
val init:
  ?init_config:OpamFile.InitConfig.t ->
  ?repo:repository ->
  ?bypass_checks:bool ->
  shell -> filename -> [`ask|`yes|`no] ->
  rw global_state * unlocked repos_state * formula

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

(** Low-level version of [reinstall], bypassing the package name sanitization
    and dev package update, and offering more control *)
val reinstall_t:
  rw switch_state -> ?ask:bool -> ?force:bool -> atom list -> rw switch_state

(** Update the local mirrors for the repositories and/or development packages *)
val update:
  'a global_state ->
  repos_only:bool -> dev_only:bool -> ?all:bool ->
  string list ->
  unlocked repos_state

(** Find a consistent state where most of the installed packages are
    upgraded to their latest version, within the given constraints.
    An empty list means upgrade all installed packages. *)
val upgrade: rw switch_state -> atom list -> rw switch_state

(** Low-level version of [upgrade], bypassing the package name sanitization
    and dev package update, and offering more control *)
val upgrade_t:
  ?strict_upgrade:bool -> ?auto_install:bool -> ?ask:bool ->
  atom list -> rw switch_state -> rw switch_state

(** Recovers from an inconsistent universe *)
val fixup: rw switch_state -> rw switch_state

(** Remove the given list of packages. *)
val remove:
  rw switch_state -> autoremove:bool -> force:bool -> atom list ->
  rw switch_state

module PIN: sig

  (** Set a package pinning. If [action], prompt for install/reinstall as
      appropriate after pinning. *)
  val pin:
    rw switch_state ->
    OpamPackage.Name.t ->
    ?edit:bool -> ?version:version -> ?action:bool ->
    [< `Source of url | `Version of version | `Dev_upstream | `None ] ->
    rw switch_state

  val edit:
    rw switch_state -> ?action:bool -> ?version:version -> OpamPackage.Name.t ->
    rw switch_state

  val unpin:
    rw switch_state ->
    ?action:bool -> OpamPackage.Name.t list -> rw switch_state

  (** List the current pinned packages. *)
  val list: 'a switch_state -> short:bool -> unit

end
