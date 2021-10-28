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

let name solver_backend = "builtin-"^Mccs.get_solver_id ~solver:solver_backend ()

let default_criteria = {
  crit_default = "-removed,\
                  -count[avoid-version,changed],\
                  -count[version-lag,request],\
                  -count[version-lag,changed],\
                  -count[missing-depexts,changed],\
                  -changed";
  crit_upgrade = "-removed,\
                  -count[avoid-version,changed],\
                  -count[version-lag,solution],\
                  -count[missing-depexts,changed],\
                  -new";
  crit_fixup = "-changed,\
                -count[avoid-version:,true],\
                -count[version-lag:,false],\
                -count[missing-depexts:,true]";
  crit_best_effort_prefix = Some "+count[opam-query:,false],";
}

let call solver_backend ext ~criteria ?timeout cudf =
  let solver = match solver_backend, ext with
    | `LP _, Some ext -> `LP ext
    | _ -> solver_backend
  in
  match
    Mccs.resolve_cudf
      ~solver
      ~verbose:OpamCoreConfig.(abs !r.debug_level >= 2)
      ?timeout criteria cudf
  with
  | None -> raise Dose_common.CudfSolver.Unsat
  | Some (preamble, univ) -> Some preamble, univ
  | exception Mccs.Timeout -> raise (Timeout None)

let of_backend backend : (module OpamCudfSolverSig.S) =
  (module struct
    let name = name backend
    let ext = ref None
    let is_present () =
      match backend, !ext with
      | `LP "", None -> false
      | `LP cmd, None | `LP _, Some cmd ->
        OpamSystem.resolve_command cmd <> None
      | _ -> true
    let command_name = None
    let default_criteria = default_criteria
    let preemptive_check = true
    let call = call backend !ext
  end)

let all_backends =
  List.map of_backend Mccs.supported_backends
