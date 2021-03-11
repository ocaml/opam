(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2017 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the solver lib (record, global reference, setter,
    initialisation) *)

module E : sig
  type OpamStd.Config.E.t +=
    | BESTEFFORT of bool option
    | BESTEFFORTPREFIXCRITERIA of string option
    | CRITERIA of string option
    | CUDFFILE of string option
    | CUDFTRIM of string option
    | DIGDEPTH of int option
    | EXTERNALSOLVER of string option
    | FIXUPCRITERIA of string option
    | NOASPCUD of bool option
    | PREPRO of bool option
    | SOLVERALLOWSUBOPTIMAL of bool option
    | SOLVERTIMEOUT of float option
    | UPGRADECRITERIA of string option
    | USEINTERNALSOLVER of bool option
    | VERSIONLAGPOWER of int option
    val externalsolver: unit -> string option
end

type t = private {
  cudf_file: string option;
  solver: (module OpamCudfSolver.S) Lazy.t;
  best_effort: bool;
  solver_preferences_default: string option Lazy.t;
  solver_preferences_upgrade: string option Lazy.t;
  solver_preferences_fixup: string option Lazy.t;
  solver_preferences_best_effort_prefix: string option Lazy.t;
  solver_timeout: float option;
  solver_allow_suboptimal: bool;
  cudf_trim: string option;
  dig_depth: int;
  preprocess: bool;
  version_lag_power: int;
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
  ?solver_allow_suboptimal:bool ->
  ?cudf_trim:string option ->
  ?dig_depth:int ->
  ?preprocess:bool ->
  ?version_lag_power:int ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

val call_solver: criteria:string -> Cudf.cudf -> Cudf.preamble option * Cudf.universe

(** Checks if best_effort was set and is supported *)
val best_effort: unit -> bool

val criteria: OpamTypes.solver_criteria -> string
