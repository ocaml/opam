(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
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

(** Initialize the client to a consistent state.
    Returns the initial state and, in case a switch is to be created, its
    initial set of packages *)
val init:
  init_config:OpamFile.InitConfig.t ->
  interactive:bool ->
  ?repo:repository ->
  ?bypass_checks:bool ->
  ?dot_profile:filename ->
  ?update_config:bool ->
  ?env_hook:bool ->
  ?completion:bool ->
  ?check_sandbox:bool ->
  ?cygwin_setup: [ `internal of OpamSysPkg.t list | `default_location | `location of dirname | `no ] ->
  ?git_location:(dirname, unit) either ->
  shell ->
  rw global_state * unlocked repos_state * atom list

(* (\** Gets the initial config (opamrc) to be used *\)
 * val get_init_config:
 *   no_default_config_file:bool ->
 *   add_config_file:OpamUrl.t ->
 *   OpamFile.InitConfig.t *)

(** Re-runs the extra tools checks, updates the configuration from [init_config]
   (defaults to {!OpamInitDefaults.init_config}) for the settings that are
   unset, and updates all repositories *)
val reinit:
  ?init_config:OpamFile.InitConfig.t -> interactive:bool -> ?dot_profile:filename ->
  ?update_config:bool -> ?env_hook:bool -> ?completion:bool -> ?inplace:bool ->
  ?check_sandbox:bool -> ?bypass_checks:bool ->
  ?cygwin_setup: [ `internal of OpamSysPkg.t list | `default_location | `location of dirname | `no ] ->
  ?git_location:(dirname, unit) either ->
  OpamFile.Config.t -> shell -> unit

(** Install the given list of packages. [add_to_roots], if given, specifies that
    given packages should be added or removed from the roots. [autoupdate]
    defaults to the list of atoms, and can be used to restrict the atoms which
    are updated if pinned. *)
val install:
  rw switch_state ->
  ?formula:formula ->
  ?autoupdate:atom list -> ?add_to_roots:bool -> ?deps_only:bool ->
  ?ignore_conflicts:bool -> ?assume_built:bool -> ?download_only:bool ->
  ?depext_only:bool -> atom list ->
  rw switch_state

(** Low-level version of {!reinstall}, bypassing the package name sanitization
    and dev package update, and offering more control *)
val install_t:
  rw switch_state ->
  ?ask:bool -> ?ignore_conflicts:bool -> ?depext_only:bool -> ?download_only:bool ->
  atom list -> ?formula:formula ->
  bool option -> deps_only:bool -> assume_built:bool ->
  rw switch_state

(** Check that the given list of packages [atoms] have their dependencies
    satisfied, without calling the solver.
    Returns a map of package names to their set of missing dependencies.

    @param recursive if [true] will check every installed dependencies
    recursively. For example, this is useful if a package currently installed
    has been modified and a dependencies has been added but that package has
    not yet been upgraded.
    Note: if [true], the keys of the resulting map might contain packages
    further down the dependency tree and is no longer guaranteed to contain
    only packages from [atoms]. *)
val check_installed:
  build:bool -> post:bool -> recursive:bool -> rw switch_state -> atom list ->
  OpamPackage.Name.Set.t OpamPackage.Name.Map.t

(** Reinstall the given set of packages. *)
val reinstall:
  rw switch_state -> ?assume_built:bool -> atom list -> rw switch_state

(** Low-level version of {!reinstall}, bypassing the package name sanitization
    and dev package update, and offering more control *)
val reinstall_t:
  rw switch_state -> ?ask:bool -> ?force:bool -> assume_built:bool -> atom list
  -> rw switch_state

(** Update the local mirrors for the repositories and/or development packages.
    Returns [(success, changes, rt)], where [success] is [true] only if all
    updates were successful, [changes] is true if any upstream had updates, and
    [rt] is the updated repository state. *)
val update:
  'a global_state ->
  repos_only:bool -> dev_only:bool -> ?all:bool ->
  string list ->
  bool * bool * unlocked repos_state

(** Upgrade the switch, that is, move packages to their more recent available
    versions. The specified atoms are kept installed (or newly installed after a
    confirmation). The upgrade concerns them only unless [all] is specified. *)
val upgrade:
  rw switch_state ->
  ?formula:formula -> ?check:bool -> ?only_installed:bool ->
  all:bool -> atom list -> rw switch_state

(** Low-level version of {!upgrade}, bypassing the package name sanitization and
   dev package update, and offering more control. [terse] avoids the verbose
   message when we are at a local maximum, but there are possible upgrades *)
val upgrade_t:
  ?strict_upgrade:bool -> ?auto_install:bool -> ?ask:bool -> ?check:bool ->
  ?terse:bool ->
  ?only_installed:bool ->
  all:bool -> atom list -> ?formula:formula ->
  rw switch_state -> rw switch_state

(** Recovers from an inconsistent universe *)
val fixup: ?formula:formula -> rw switch_state -> rw switch_state

(** Remove the given list of packages. *)
val remove:
  rw switch_state -> autoremove:bool -> force:bool ->
  ?formula:formula -> atom list ->
  rw switch_state

module PIN: sig

  (** Set a package pinning. If [action], prompt for install/reinstall as
      appropriate after pinning. *)
  val pin:
    rw switch_state ->
    OpamPackage.Name.t ->
    ?edit:bool -> ?version:version -> ?action:bool -> ?subpath:subpath ->
    ?locked:string ->
    [< `Source of url | `Version of version | `Dev_upstream
    | `Source_version of version * version
    (* the first version is the source one, the second the package one *)
    | `None ] ->
    rw switch_state

  val edit:
    rw switch_state ->
    ?action:bool -> ?version:version -> ?locked:string ->
    OpamPackage.Name.t ->
    rw switch_state

  val url_pins:
    rw switch_state -> ?edit:bool -> ?action:bool -> ?locked:string ->
    ?pre:(pinned_opam -> unit) -> pinned_opam list ->
    rw switch_state

  val unpin:
    rw switch_state ->
    ?action:bool -> OpamPackage.Name.t list -> rw switch_state

  (** List the current pinned packages. *)
  val list: 'a switch_state -> short:bool -> unit

  (** Runs an install/upgrade on the listed packages if necessary.
      [post_pin_action st was_pinned names] takes the set of packages pinned
      beforehand, and a list of newly pinned packages *)
  val post_pin_action: rw switch_state -> package_set -> name list -> rw switch_state

end
