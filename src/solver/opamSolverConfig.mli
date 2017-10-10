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
  solver: (module OpamCudfSolver.S) Lazy.t;
  best_effort: bool;
  solver_preferences_default: string option Lazy.t;
  solver_preferences_upgrade: string option Lazy.t;
  solver_preferences_fixup: string option Lazy.t;
  solver_preferences_best_effort_prefix: string option Lazy.t;
  solver_timeout: float option;
}

type 'a options_fun =
  ?cudf_file:string option ->
  ?solver:(module OpamCudfSolver.S) Lazy.t ->
  ?best_effort:bool ->
  ?solver_preferences_default:string option Lazy.t ->
  ?solver_preferences_upgrade:string option Lazy.t ->
  ?solver_preferences_fixup:string option Lazy.t ->
  ?solver_preferences_best_effort_prefix:string option Lazy.t ->
  ?solver_timeout:float option ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

val call_solver: criteria:string -> Cudf.cudf -> Cudf.preamble option * Cudf.universe

(** Checks if best_effort was set and is supported *)
val best_effort: unit -> bool

val criteria: OpamTypes.solver_criteria -> string
