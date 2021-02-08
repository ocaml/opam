(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCudfSolverSig

let name = "builtin-dummy-0install-solver"

let is_present () = false

let ext = ref None

let command_name = None

let default_criteria = {
  crit_default = "";
  crit_upgrade = "";
  crit_fixup = "";
  crit_best_effort_prefix = None;
}

let call ~criteria:_ ?timeout:_ _cudf =
  failwith "This opam was compiled without the opam-0install solver built in"
