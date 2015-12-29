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

(** {2 Environment handling} *)

(** Get the environment with OPAM specific additions for a specific switch. 
    If [force_path], the PATH is modified to ensure opam dirs are leading. *)
val get_full: force_path:bool -> state -> switch -> env

(** Get only environment modified by OPAM for a specific switch. 
    If [force_path], the PATH is modified to ensure opam dirs are leading. *)
val get_opam: force_path:bool -> state -> switch -> env

(** Update an environment. (Note: depends on the currently defined
    [OpamStateConfig.(!r.root_dir)] to detect existing OPAM-added paths and
    replace them when using the +=, =:, etc. update operators *)
val add: env -> env_update list -> env

(** Check if the shell environment is in sync with the current OPAM switch *)
val is_up_to_date: state -> switch -> bool

(** Returns the current environment updates to configure the current switch with
    its set of installed packages *)
val compute_updates: state -> switch -> env_update list

(** The shell command to run by the user to set his OPAM environment, adapted to
    the current environment (OPAMROOT, OPAMSWITCH variables) and shell (as
    returned by [eval `opam config env`]). Takes root and switch. *)
val eval_string: dirname -> switch -> string


(** {2 Shell and initialisation support} *)

(** NOTE: besides shell init, this still includes a bit of OCaml-specific code
    to register use of [$OCAML_TOPLEVEL_PATH] in [~/.ocamlinit]. *)

(** Details the process to the user, and interactively update the global and
    user configurations. Returns [true] if the update was confirmed and
    successful *)
val update_setup_interactive: state -> switch -> shell -> filename -> bool

(** Display the global and user configuration for OPAM. *)
val display_setup: global_state -> shell -> filename -> unit

(** Update the user configuration. *)
val update_setup:
  state -> switch -> user_config option -> global_config option -> unit

(** Update scripts in ~/.opam/opam-init (subset of [update_setup]) *)
val update_init_scripts: state -> switch -> global:(global_config option) -> unit

(** Print a warning if the environment is not set-up properly.
    (General message) *)
val check_and_print_env_warning: state -> switch -> unit

(** Print a long message with explanations if the environment is not set-up
    properly, and advises to update user's file depending on what has already
    been done automatically according to [user_config] *)
val print_env_warning_at_init: state -> switch -> user_config -> unit
