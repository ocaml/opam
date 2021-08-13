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

(** Functions handling the `opam config` subcommand and configuration actions *)

open OpamTypes
open OpamStateTypes

(** {2 `opam config` subcommand and their associated commands } *)

(** Display the current environment. Booleans csh, sexp and fish set an
    alternative output (unspecified if more than one is true, sh-style by
    default). [inplace_path] changes how the PATH variable is updated when there
    is already an opam entry: either at the same rank, or pushed in front. *)
val env:
  'a global_state -> switch ->
  ?set_opamroot:bool -> ?set_opamswitch:bool ->
  csh:bool -> sexp:bool -> fish:bool -> pwsh:bool -> cmd:bool ->
  inplace_path:bool -> unit

(** Ensures that the environment file exists in the given switch, regenerating
    it, if necessary. *)
val ensure_env: 'a global_state -> switch -> unit

(** Like [env] but allows one to specify the precise env to print rather than
    compute it from a switch state *)
val print_eval_env: csh:bool -> sexp:bool -> fish:bool -> pwsh:bool -> cmd:bool -> env -> unit

(** Display the content of all available packages variables *)
val list: 'a switch_state -> name list -> unit

(** Substitute files *)
val subst: 'a global_state -> basename list -> unit

(** Prints expansion of variables in string *)
val expand: 'a global_state -> string -> unit

(** Execute a command in a subshell, after variable expansion *)
val exec:
  [< unlocked ] global_state ->
  set_opamroot:bool -> set_opamswitch:bool -> inplace_path:bool ->
  no_switch:bool ->
  string list -> unit

(** {2 Variables and options display and setting } *)
(** Functions handling `opam var` and `opam option` command *)

(** Given an `opam option` field or field-value argument, detect the scope,
    switch, global or nonexistent field
    (cf. [OpamCommands.Var_Option_Common.var_option]) *)
val get_scope: string -> [> `Switch | `Global | `None of string ]

(** {3 Setting variables and options } *)

(** Update operation type *)
type whole_op  = [ `Overwrite of string | `Revert ]
type append_op = [ `Add of string | `Remove of string ]
type update_op = [ append_op  | whole_op ]

(** Parse an update operation. String is of the form [var[(+=|-=|=)[value]]].
    If 'value' is absent, it is a revert operation.
    Raise [Invalid_argument] if the string is malformed *)
val parse_update: string -> string * update_op

(** As [parse_update] but parse only overwrites and reverts. String is of the
    form [var=[value]]`.
    Raise [Invalid_argument] if the string is malformed *)
val parse_whole: string -> string * whole_op

val whole_of_update_op: update_op -> whole_op

(** [set_opt_global gt field value] updates global config field with update
    value in <opamroot>/config file. Modifiable fields are a subset of all
    defined fields in [OpamFile.Config.t]. On revert, field is reverted to its
    initial value as defined in [OpamInitDefaults.init_config], to default
    value otherwise ([OpamFile.Config.empty]).
    May raise [OpamStd.Sys.Exit 2]. *)
val set_opt_global: rw global_state -> string -> update_op -> rw global_state

(** As [set_opt_global], [set_opt_switch] updates switch config file in
    <opamroot>/<switch>/.opam-switch/switch-config. If switch state is given,
    uses its config and returns it with then new config. Otherwise, loads the
    raw switch state and returns [None].
    Raises [OpamStd.Sys.Exit 50] ([`Configuration_error]) if no switch is set *)
val set_opt_switch:
  'a global_state -> ?st:rw switch_state -> string -> update_op
  -> rw switch_state option

(** [set_var_global] and [set_var_switch] update respectively `global-variables`
    field in global config and `variables` field in switch config, by appending
    the new variables to current set. If switch state is given, uses its
    config and returns it with then new config. Otherwise, loads the raw switch
    state and returns [None].
    Raises [OpamStd.Sys.Exit 2] ([`Bad_argument]) if field is not modifiable *)
val set_var_global:
  rw global_state -> string -> whole_op -> rw global_state

(** Raises [OpamStd.Sys.Exit 50] ([`Configuration_error]) if no switch is set *)
val set_var_switch:
    'a global_state -> ?st:rw switch_state -> string -> whole_op
    -> rw switch_state option


(** {3 Display list of variables and options } *)

(** List switch and/or global fields/sections and their value. If switch state
    is given, uses its config, otherwise loads the raw switch state. *)

val options_list:
  ?st:unlocked switch_state -> 'a global_state -> unit
val options_list_global: 'a global_state -> unit

(** Raises [OpamStd.Sys.Exit 50] ([`Configuration_error]) if no switch is set *)
val options_list_switch:
  ?st:unlocked switch_state -> 'a global_state -> unit

(** List switch and/or global variables and their value. If switch state is
    given, uses its config, otherwise loads the raw switch state. *)

val vars_list:
  ?st:'a switch_state -> 'b global_state -> unit
val vars_list_global: 'a global_state -> unit

(** Raises [OpamStd.Sys.Exit 50] ([`Configuration_error]) if no switch is set *)
val vars_list_switch:
  ?st:'a switch_state -> 'b global_state -> unit


(** {3 Display a variable and option } *)

(** Display [field] name and content in the global / switch configuration.
    If switch state is given, uses its config, otherwise loads the raw switch
    state. *)

val option_show_global: 'a global_state -> string -> unit

(** Raises [OpamStd.Sys.Exit 50] ([`Configuration_error]) if no switch is set *)
val option_show_switch:
  'a global_state -> ?st:unlocked switch_state -> string -> unit

(** Display [var] name and content in the global / switch configuration.
    Look first in the raw switch_state, if not found, uses the given switch
    state or loads one. *)

val var_show_global: 'a global_state -> string -> unit
val var_show_switch:
  'a global_state -> ?st:'b switch_state -> string -> unit
val var_show: 'a global_state -> string -> unit
