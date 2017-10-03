(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

include OpamCudfSolverSig

let default_compat_criteria = {
  crit_default = "-removed,-notuptodate,-changed";
  crit_upgrade = "-removed,-notuptodate,-changed";
  crit_fixup = "-changed,-notuptodate";
  crit_best_effort_prefix = None;
}

module type ExternalArg = sig
  val name: string
  val is_present: bool Lazy.t
  val command_name: string
  val command_args: OpamTypes.arg list
  val default_criteria: criteria_def
end

let call_external_solver command ~criteria ?timeout (_, universe,_ as cudf) =
  let solver_in =
    OpamFilename.of_string (OpamSystem.temp_file "solver-in") in
  let solver_out =
    OpamFilename.of_string (OpamSystem.temp_file "solver-out") in
  try
    let _ =
      let oc = OpamFilename.open_out solver_in in
      Cudf_printer.pp_cudf oc cudf;
      close_out oc
    in
    let () =
      let cmd =
        OpamFilter.single_command (fun v ->
            if not (OpamVariable.Full.is_global v) then None else
            match OpamVariable.to_string (OpamVariable.Full.variable v) with
            | "input" -> Some (S (OpamFilename.to_string solver_in))
            | "output" -> Some (S (OpamFilename.to_string solver_out))
            | "criteria" -> Some (S criteria)
            | _ -> None)
          command
      in
      OpamSystem.command
        ~verbose:(OpamCoreConfig.(!r.debug_level >= 2))
        cmd
    in
    OpamFilename.remove solver_in;
    if not (OpamFilename.exists solver_out) then
      raise (Common.CudfSolver.Error "no output")
    else if
      (let ic = OpamFilename.open_in solver_out in
       try
         let i = input_line ic in close_in ic;
         i = "FAIL"
       with End_of_file -> close_in ic; false)
    then
      raise Common.CudfSolver.Unsat
    else
    let r =
      Cudf_parser.load_solution_from_file
        (OpamFilename.to_string solver_out) universe in
    OpamFilename.remove solver_out;
    r
  with e ->
    OpamFilename.remove solver_in;
    OpamFilename.remove solver_out;
    raise e


module External (E: ExternalArg) : S = struct
  let name = E.name

  let is_present = E.is_present

  let command_name = Some E.command_name

  let default_criteria = E.default_criteria

  let call =
    call_external_solver ((CString E.command_name, None) :: E.command_args)
end

module Aspcud_def = struct
  let name = "aspcud"

  let command_name = "aspcud"

  let is_present = lazy (
    match OpamSystem.resolve_command command_name with
    | None -> false
    | Some cmd ->
    try
      match
        OpamSystem.read_command_output ~verbose:false ~allow_stdin:false
          [cmd; "-v"]
      with
      | [] -> false
      | s::_ ->
        match OpamStd.String.split s ' ' with
        | "aspcud"::_::v::_ when OpamVersionCompare.compare v "1.9" >= 0 ->
          OpamConsole.log "SOLVER"
            "Solver is aspcud >= 1.9: using latest version criteria";
          true
        | _ -> false
    with OpamSystem.Process_error _ -> false
  )

  let command_args = [
    CIdent "input", None; CIdent "output", None;
    CIdent "criteria", None
  ]

  let default_criteria =
    {
      crit_default = "-count(removed),\
                      -sum(request,version-lag),\
                      -count(down),\
                      -sum(solution,version-lag),\
                      -count(changed)";
      crit_upgrade = "-count(down),\
                      -count(removed),\
                      -sum(solution,version-lag),\
                      -count(new)";
      crit_fixup = "-count(changed),\
                    -notuptodate(solution),-sum(solution,version-lag)";
      crit_best_effort_prefix = Some "+sum(solution,opam-query),";
    }
end

module Aspcud = External(Aspcud_def)

module Aspcud_old_def = struct
  let name = "aspcud-old"

  let command_name = Aspcud_def.command_name

  let is_present = lazy (OpamSystem.resolve_command command_name <> None)

  let command_args = Aspcud_def.command_args

  let default_criteria = default_compat_criteria
end

module Aspcud_old = External(Aspcud_old_def)

module Mccs_def = struct
  let name = "mccs"

  let command_name = "mccs"

  let is_present = lazy (OpamSystem.resolve_command command_name <> None)

  let command_args = [
    CString "-i", None; CIdent "input", None;
    CString "-o", None; CIdent "output", None;
    CString "-lexagregate[%{criteria}%]", None;
  ]

  let default_criteria =  {
    crit_default = "-removed,\
                    -count[version-lag:,true],\
                    -changed,\
                    -count[version-lag:,false],\
                    -new";
    crit_upgrade = "-removed,\
                    -count[version-lag:,false],\
                    -new";
    crit_fixup = "-changed,-count[version-lag:,false]";
    crit_best_effort_prefix = Some "+count[opam-query:,false],";
  }
end

module Mccs = External(Mccs_def)

module Packup_def = struct
  let name = "packup"

  let command_name = "packup"

  let is_present = lazy (OpamSystem.resolve_command command_name <> None)

  let command_args = [
    CIdent "input", None; CIdent "output", None;
    CString "-u", None; CIdent "criteria", None;
  ]

  let default_criteria = default_compat_criteria
end

module Packup = External(Packup_def)

let make_custom_solver name args criteria =
  (module
    (External (struct
       let command_name = name
       let name = name ^ "-custom"
       let is_present = lazy true
       let command_args = args
       let default_criteria = criteria
     end))
    : S)


let default_solver_selection = [
  (module OpamBuiltinMccs: S);
  (module Aspcud: S);
  (module Mccs: S);
  (module Aspcud_old: S);
  (module Packup: S);
]

let custom_solver cmd = match cmd with
  | [ CIdent name, _ ] | [ CString name, _ ] ->
    (try
       List.find (fun (module S: S) ->
           (S.name = Filename.basename name || S.command_name = Some name)
           && Lazy.force S.is_present)
         default_solver_selection
     with Not_found ->
       OpamConsole.error_and_exit `Configuration_error
         "No installed solver matching the selected '%s' found"
         name)
  | ((CIdent name | CString name), _) :: args ->
    let criteria =
      try
        let corresponding_module =
          List.find (fun (module S: S) ->
              S.command_name =
              Some (Filename.basename name) && Lazy.force S.is_present)
            default_solver_selection
        in
        let module S = (val corresponding_module) in
        S.default_criteria
      with Not_found -> default_compat_criteria
    in
    make_custom_solver name args criteria
  | _ ->
    OpamConsole.error_and_exit `Configuration_error
      "Invalid custom solver command specified."

let solver_of_string s =
  let args = OpamStd.String.split s ' ' in
  (custom_solver
     (List.map (fun a -> OpamTypes.CString a, None) args))

let has_builtin_solver () =
  List.exists
    (fun (module S: S) -> S.command_name = None && Lazy.force S.is_present)
    default_solver_selection

let get_solver ?internal l =
  try
    List.find
      (fun (module S: S) ->
         (internal = None || internal = Some (S.command_name = None)) &&
         Lazy.force S.is_present)
      l
  with Not_found ->
    OpamConsole.error_and_exit `Configuration_error
      "No available solver found. Make sure your solver configuration is \
       correct. %s"
      (if has_builtin_solver () then
         "You can enforce use of the built-in solver with \
          `--use-internal-solver'."
       else
         "This opam has been compiled without a built-in solver, so you need \
          to install and configure an external one. See \
          http://opam.ocaml.org/doc/Install.html#ExternalSolvers for details.")
