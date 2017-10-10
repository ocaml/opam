(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
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

exception Timeout

module type S = sig

  val name: string

  val is_present: bool Lazy.t

  val command_name: string option
  (** None means the solver is built-in *)

  val default_criteria: criteria_def

  val call:
    criteria:string -> ?timeout:float -> Cudf.cudf ->
    Cudf.preamble option * Cudf.universe

end
