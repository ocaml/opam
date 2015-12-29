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

(** Initialises a new switch with the given name in the given opam root *)
val create_empty_switch: dirname -> ?compiler:compiler -> switch -> unit

(** Creates the given switch, and installs the given compiler *)
val install_compiler: global_state -> quiet:bool -> switch -> compiler -> unit

(** Writes the current state file to disk (installed, pinned, root packages etc.).
    Unless [OpamStateConfig.(!r.dryrun)]. Writes all switches loaded in the current
    state. *)
val write_state_file: state -> unit

(** Update the on-disk set of packages marked to reinstall *)
val add_to_reinstall:
  global_state -> switch -> OpamFile.State.t -> unpinned_only:bool ->
  package_set -> unit

(** Updates the defined default switch and loads its state *)
val set_current_switch: global_state -> switch -> state

(** Create the default global_config structure for a switch, including default
    paths (lib, bin, etc.) *)
val gen_global_config: dirname -> switch -> OpamFile.Dot_config.t

(** (Re-)install the configuration for a given root and switch *)
val install_global_config: dirname -> switch -> OpamFile.Dot_config.t -> unit

(* !X These two are more repository than switch related; remove_metadata
   actually depends on wether installed in other switches *)

(** Stores a copy of the package's metadata in [/packages/NAME/NAME.VERSION].
    Used for installed packages *)
val install_metadata: state -> switch -> package -> unit

(** Remove the metadata copy in [/packages/NAME/NAME.VERSION], after checking
    for each package that it isn't installed in any switch anymore *)
val remove_metadata: state -> switch -> package_set -> unit


