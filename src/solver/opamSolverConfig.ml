(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type t = {
  cudf_file: string option;
  solver: (module OpamCudfSolver.S) Lazy.t;
  best_effort: bool;
  (* The following are options because the default can only be known once the
     solver is known, so we set it only if no customisation was made *)
  solver_preferences_default: string option Lazy.t;
  solver_preferences_upgrade: string option Lazy.t;
  solver_preferences_fixup: string option Lazy.t;
  solver_preferences_best_effort_prefix: string option Lazy.t;
}

type 'a options_fun =
  ?cudf_file:string option ->
  ?solver:((module OpamCudfSolver.S) Lazy.t) ->
  ?best_effort:bool ->
  ?solver_preferences_default:string option Lazy.t ->
  ?solver_preferences_upgrade:string option Lazy.t ->
  ?solver_preferences_fixup:string option Lazy.t ->
  ?solver_preferences_best_effort_prefix:string option Lazy.t ->
  'a

let default =
  let solver = lazy (
    OpamCudfSolver.get_solver OpamCudfSolver.default_solver_selection
  ) in
  {
    cudf_file = None;
    solver;
    best_effort = false;
    solver_preferences_default = lazy None;
    solver_preferences_upgrade = lazy None;
    solver_preferences_fixup = lazy None;
    solver_preferences_best_effort_prefix = lazy None;
  }

let setk k t
    ?cudf_file
    ?solver
    ?best_effort
    ?solver_preferences_default
    ?solver_preferences_upgrade
    ?solver_preferences_fixup
    ?solver_preferences_best_effort_prefix
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    cudf_file = t.cudf_file + cudf_file;
    solver = t.solver + solver;
    best_effort = t.best_effort + best_effort;
    solver_preferences_default =
      t.solver_preferences_default + solver_preferences_default;
    solver_preferences_upgrade =
      t.solver_preferences_upgrade + solver_preferences_upgrade;
    solver_preferences_fixup =
      t.solver_preferences_fixup + solver_preferences_fixup;
    solver_preferences_best_effort_prefix =
      t.solver_preferences_best_effort_prefix +
      solver_preferences_best_effort_prefix;
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let with_auto_criteria config =
  let criteria = lazy (
    let module S = (val Lazy.force config.solver) in
    S.default_criteria
  ) in
  set config
    ~solver_preferences_default:
      (lazy (match config.solver_preferences_default with
           | lazy None -> Some (Lazy.force criteria).OpamCudfSolver.crit_default
           | lazy some -> some))
    ~solver_preferences_upgrade:
      (lazy (match config.solver_preferences_upgrade with
           | lazy None -> Some (Lazy.force criteria).OpamCudfSolver.crit_upgrade
           | lazy some -> some))
    ~solver_preferences_fixup:
      (lazy (match config.solver_preferences_fixup with
           | lazy None -> Some (Lazy.force criteria).OpamCudfSolver.crit_fixup
           | lazy some -> some))
    ~solver_preferences_best_effort_prefix:
      (lazy (match config.solver_preferences_best_effort_prefix with
           | lazy None ->
             (Lazy.force criteria).OpamCudfSolver.crit_best_effort_prefix
           | lazy some -> some))
    ()

let initk k =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  let solver =
    let open OpamCudfSolver in
    match env_string "EXTERNALSOLVER" with
    | Some "" -> lazy (get_solver ~internal:true default_solver_selection)
    | Some s -> lazy (solver_of_string s)
    | None ->
      let internal = env_bool "USEINTERNALSOLVER" ++ env_bool "NOASPCUD" in
      lazy (get_solver ?internal default_solver_selection)
  in
  let best_effort =
    env_bool "BESTEFFORT" in
  let criteria =
    env_string "CRITERIA" >>| fun c -> lazy (Some c) in
  let upgrade_criteria =
    (env_string "UPGRADECRITERIA" >>| fun c -> lazy (Some c)) ++ criteria in
  let fixup_criteria =
    env_string "FIXUPCRITERIA" >>| fun c -> (lazy (Some c)) in
  let best_effort_prefix_criteria =
    env_string "BESTEFFORTPREFIXCRITERIA" >>| fun c -> (lazy (Some c)) in
  setk (setk (fun c -> r := with_auto_criteria c; k)) !r
    ~cudf_file:(env_string "CUDFFILE")
    ~solver
    ?best_effort
    ?solver_preferences_default:criteria
    ?solver_preferences_upgrade:upgrade_criteria
    ?solver_preferences_fixup:fixup_criteria
    ?solver_preferences_best_effort_prefix:best_effort_prefix_criteria

let init ?noop:_ = initk (fun () -> ())

let best_effort =
  let r = lazy (
    !r.best_effort &&
    let crit = match Lazy.force !r.solver_preferences_default with
      | Some c -> c
      | None -> failwith "Solver criteria uninitialised"
    in
    let pfx = Lazy.force !r.solver_preferences_best_effort_prefix in
    pfx <> None ||
    OpamStd.String.contains ~sub:"opam-query" crit ||
    (OpamConsole.warning
       "Your solver configuration does not support --best-effort, the option \
        was ignored (you need to specify variable OPAMBESTEFFORTCRITERIA, or \
        set your criteria to maximise the count for cudf attribute \
        'opam-query')";
     false)
  ) in
  fun () -> Lazy.force r

let criteria kind =
  let crit = match kind with
    | `Default -> !r.solver_preferences_default
    | `Upgrade -> !r.solver_preferences_upgrade
    | `Fixup -> !r.solver_preferences_fixup
  in
  let str =
    match Lazy.force crit with
    | Some c -> c
    | None -> failwith "Solver criteria uninitialised"
  in
  if !r.best_effort then
    match !r.solver_preferences_best_effort_prefix with
    | lazy (Some pfx) -> pfx ^ str
    | lazy None -> str
  else str

let call_solver ~criteria cudf =
  let module S = (val Lazy.force (!r.solver)) in
  OpamConsole.log "SOLVER" "Calling solver %s with criteria %s" S.name criteria;
  S.call ~criteria cudf
