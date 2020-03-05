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

(** Display the current environment. Booleans csh, sexp and fish set an
    alternative output (unspecified if more than one is true, sh-style by
    default). [inplace_path] changes how the PATH variable is updated when there
    is already an opam entry: either at the same rank, or pushed in front. *)
val env:
  'a global_state -> switch ->
  ?set_opamroot:bool -> ?set_opamswitch:bool ->
  csh:bool -> sexp:bool -> fish:bool -> inplace_path:bool ->
  unit

(** Ensures that the environment file exists in the given switch, regenerating
    it, if necessary. *)
val ensure_env: 'a global_state -> switch -> unit

(** Like [env] but allows one to specify the precise env to print rather than
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

(** [set_opt_global gt field_value] updates global config field with value
    (using prefix '+=', '-=', '=') in <opamroot>/config file. Modifiable fields
    are a subset of all defined fields in [OpamFile.Config.t]. [field_value] is
    a string of the form "field[(+=|-=|=)value]". In the case where it contains
    only the field name, field is reverted to its initial value as defined in
    [OpamInitDefaults.init_config], to default value otherwise
    ([OpamFile.Config.empty]).*)
val set_opt_global: rw global_state -> string -> rw global_state

(** As [set_opt_global], [set_opt_switch] updates switch config file in
    <opamroot>/<switch>/.opam-switch/switch-config. *)
val set_opt_switch: rw switch_state -> string -> rw switch_state

(** [set_var_global] and [set_var_switch] update respectively `global-variables`
    field in global config and `variables` field in switch config, by appending
    the new variables to current set *)
val set_var_global:
  rw global_state -> string -> string option -> rw global_state
val set_var_switch:
  rw switch_state -> string -> string option -> rw switch_state

(** Execute a command in a subshell, after variable expansion *)
val exec:
  [< unlocked ] global_state ->
  ?set_opamroot:bool -> ?set_opamswitch:bool -> inplace_path:bool ->
  string list -> unit
