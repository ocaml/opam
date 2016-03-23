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

(** Get the current environment with OPAM specific additions. If [force_path],
    the PATH is modified to ensure opam dirs are leading. If [opamswitch],
    the OPAMSWITCH environment variable is included (default true). *)
val get_full: ?opamswitch:bool -> force_path:bool -> 'a switch_state -> env

(** Get only environment modified by OPAM. If [force_path], the PATH is modified
    to ensure opam dirs are leading. *)
val get_opam: force_path:bool -> 'a switch_state -> env

(** Update an environment. (Note: depends on the currently defined
    [OpamStateConfig.(!r.root_dir)] to detect existing OPAM-added paths and
    replace them when using the +=, =:, etc. update operators *)
val add: env -> env_update list -> env

(** Check if the shell environment is in sync with the current OPAM switch *)
val is_up_to_date: 'a switch_state -> bool

(** Returns the current environment updates to configure the current switch with
    its set of installed packages *)
val compute_updates: 'a switch_state -> env_update list

(** The shell command to run by the user to set his OPAM environment, adapted to
    the current environment (OPAMROOT, OPAMSWITCH variables) and shell (as
    returned by [eval `opam config env`]) *)
val eval_string: 'a global_state -> switch option -> string

(** Returns the updated contents of the PATH variable for the given opam root
    and switch (set [force_path] to ensure the opam path is leading) *)
val path: force_path:bool -> dirname -> switch -> string

(** Returns the full environment with only the PATH variable updated, as per
    [path] *)
val full_with_path: force_path:bool -> dirname -> switch -> env

(** {2 Shell and initialisation support} *)

(** NOTE: besides shell init, this still includes a bit of OCaml-specific code
    to register use of [$OCAML_TOPLEVEL_PATH] in [~/.ocamlinit]. *)

(** Details the process to the user, and interactively update the global and
    user configurations. Returns [true] if the update was confirmed and
    successful *)
val setup_interactive: dirname -> dot_profile:filename -> shell -> bool

(** Display the global and user configuration for OPAM. *)
val display_setup: dirname -> dot_profile:filename -> shell -> unit

(** Update the user configuration in $HOME for good opam integration. *)
val update_user_setup:
  dirname -> ocamlinit:bool -> ?dot_profile:filename -> shell -> unit

(** Write the generic scripts in ~/.opam/opam-init needed to import state for
    various shells *)
val write_static_init_scripts:
  dirname -> switch_eval:bool -> completion:bool -> unit

(** Update the shell scripts containing the current switch configuration in
    ~/.opam/opam-init ; prints a warning and skips if a write lock on the global
    state can't be acquired (note: it would be better to acquire a write lock
    beforehand, but only when working on the switch selected in
    ~/.opam/config) *)
val write_dynamic_init_scripts: 'a switch_state -> unit

(** Print a warning if the environment is not set-up properly.
    (General message) *)
val check_and_print_env_warning: 'a switch_state -> unit

(** Print a long message with explanations if the environment is not set-up
    properly, and advises to update user's file depending on what has already
    been done automatically according to [user_config] *)
val print_env_warning_at_init:
  'a global_state -> ocamlinit:bool -> ?dot_profile:filename -> shell -> unit
