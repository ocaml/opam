(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type criteria_def = {
  crit_default: string;
  crit_upgrade: string;
  crit_fixup: string;
  crit_best_effort_prefix: string option;
}

(** Timeout might still return a non-optimal solution *)
exception Timeout of (Cudf.preamble option * Cudf.universe) option

module type S = sig

  val name: string

  (** extra configurable solver parameters *)
  val ext: string option ref

  val is_present: unit -> bool

  val command_name: string option
  (** None means the solver is built-in *)

  val default_criteria: criteria_def

  val preemptive_check: bool
  (** Should be true for solvers that may take a long time to detect that there
      is no solution: in this case, dose's check is run beforehand ; otherwise
      it's only run if the solver returns unsat, to extract the explanations. *)

  val call:
    criteria:string -> ?timeout:float -> Cudf.cudf ->
    Cudf.preamble option * Cudf.universe

end
