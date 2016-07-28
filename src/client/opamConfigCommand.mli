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

(** Functions handling the "opam config" subcommand *)

open OpamTypes
open OpamStateTypes

(** Display the current environment. Booleans csh, sexp and fish set an alternative
    output (unspecified if more than one is true, sh-style by default).
    [inplace_path] changes how the PATH variable is updated when there is already
    an opam entry: either at the same rank, or pushed in front. *)
val env:
  'a switch_state -> csh:bool -> sexp:bool -> fish:bool -> inplace_path:bool ->
  unit

(** Like [env] but allows to specify the precise env to print rather than
    compute it from a switch state *)
val print_eval_env: csh:bool -> sexp:bool -> fish:bool -> env -> unit

(** Display the content of all available variables; global summary if the list
    is empty, package name "-" is understood as global configuration *)
val list:
  'a global_state -> name list -> unit

(** Display the content of a given variable *)
val variable: 'a global_state -> full_variable -> unit

(** Substitute files *)
val subst: 'a global_state -> basename list -> unit

(** Prints expansion of variables in string *)
val expand: 'a global_state -> string -> unit

(** Sets or unsets switch config variables *)
val set: full_variable -> string option -> unit

(** Sets or unsets global config variables *)
val set_global: full_variable -> string option -> unit

(** Update the global and user configuration to use OPAM. *)
val setup:
  rw global_state ->
  ?dot_profile:OpamTypes.filename ->
  completion:bool ->
  shell:OpamTypes.shell ->
  user:bool -> global:bool -> unit

(** Display the global and user configuration for OPAM. *)
val setup_list: shell -> filename -> unit

(** Execute a command in a subshell, after variable expansion *)
val exec: [< unlocked ] global_state -> inplace_path:bool -> string list -> unit
