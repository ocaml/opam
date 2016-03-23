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

(** Initialises a new switch with the given name in the given opam root,
    registers it in the global config and returns the updated global state *)
val create_empty_switch:
  ([< rw ] global_state as 'a) -> switch -> 'a

(** Writes the current state file to disk (installed, pinned, root packages etc.).
    Unless [OpamStateConfig.(!r.dryrun)] *)
val write_selections: [< rw ] switch_state -> unit

(** Updates the defined default switch and loads its state *)
val set_current_switch:
  [< rw ] global_state -> lock:'a lock -> switch -> 'a switch_state

(** Create the default global_config structure for a switch, including default
    paths (lib, bin, etc.) *)
val gen_global_config: dirname -> switch -> OpamFile.Dot_config.t

(** (Re-)install the configuration for a given root and switch *)
val install_global_config: dirname -> switch -> OpamFile.Dot_config.t -> unit

(** Add the package metadata to the switch-local cache of installed packages *)
val install_metadata: [< rw ] switch_state -> package -> unit

(** Remove the metadata of the package from the switch-local cache of installed
    packages *)
val remove_metadata: [< rw ] switch_state -> package_set -> unit

(** Update the on-disk set of packages marked to reinstall on the current
    switch *)
val add_to_reinstall:
  ([< rw ] switch_state as 'a) -> unpinned_only:bool -> package_set -> 'a

(** Updates the package selections and switch config to take into account the
    given newly installed package. The updated state is written to disk unless
    [OpamStateConfig.(!r.dry_run)] and returned. *)
val add_to_installed:
  ([< rw ] switch_state as 'a) -> ?root:bool -> package -> 'a

(** Updates the package selections and switch config to take into account the
    removed package. The updated state is written to disk unless
    [OpamStateConfig.(!r.dry_run)] and returned. *)
val remove_from_installed: ([< rw ] switch_state as 'a) -> package -> 'a
