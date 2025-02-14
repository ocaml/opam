(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCudfSolverSig

module S = struct
  let name = "builtin-dummy-solver"

  let is_present () = false

  let ext = ref None

  let preemptive_check = false

  let command_name = None

  let default_criteria = {
    crit_default = "";
    crit_upgrade = "";
    crit_fixup = "";
    crit_best_effort_prefix = None;
  }

  let call ~criteria:_ ?timeout:_ ?tolerance:_ _cudf =
    failwith "This opam was compiled without a solver built in"
end

let all_backends = [ (module S: S) ]
