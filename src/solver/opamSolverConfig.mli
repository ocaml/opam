(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the solver lib (record, global reference, setter,
    initialisation) *)

type t = private {
  cudf_file: string option;
  solver_timeout: float;
  external_solver: OpamTypes.arg list option Lazy.t;
  best_effort: bool;
  solver_preferences_default: string option Lazy.t;
  solver_preferences_upgrade: string option Lazy.t;
  solver_preferences_fixup: string option Lazy.t;
}

type 'a options_fun =
  ?cudf_file:string option ->
  ?solver_timeout:float ->
  ?external_solver:OpamTypes.arg list option Lazy.t ->
  ?best_effort:bool ->
  ?solver_preferences_default:string option Lazy.t ->
  ?solver_preferences_upgrade:string option Lazy.t ->
  ?solver_preferences_fixup:string option Lazy.t ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

val external_solver_command:
  input:string -> output:string -> criteria:string -> string list option

val criteria: OpamTypes.solver_criteria -> string
