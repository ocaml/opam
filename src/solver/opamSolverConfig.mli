(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
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

(** Configuration options for the solver lib (record, global reference, setter,
    initialisation) *)

type t = private {
  cudf_file: string option;
  solver_timeout: float;
  external_solver: OpamTypes.arg list option Lazy.t;
  solver_preferences_default: string Lazy.t option;
  solver_preferences_upgrade: string Lazy.t option;
  solver_preferences_fixup: string Lazy.t option;
}

type 'a options_fun =
  ?cudf_file:string option ->
  ?solver_timeout:float ->
  ?external_solver:OpamTypes.arg list option Lazy.t ->
  ?solver_preferences_default:string Lazy.t option ->
  ?solver_preferences_upgrade:string Lazy.t option ->
  ?solver_preferences_fixup:string Lazy.t option ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

val external_solver_command:
  input:string -> output:string -> criteria:string -> string list option

val criteria: OpamTypes.solver_criteria -> string
