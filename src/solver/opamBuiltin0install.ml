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

let log ?level f = OpamConsole.log "0install" ?level f

let name = "builtin-0install"

let ext = ref None

let is_present () = true

let command_name = None

let preemptive_check = false

let default_criteria = {
  crit_default = "-changed,\
                  -count[avoid-version,solution]";
  crit_upgrade = "-count[avoid-version,solution]";
  crit_fixup = "-count[avoid-version,solution]";
  crit_best_effort_prefix = None;
}

let not_relop = function
  | `Eq -> `Neq
  | `Neq -> `Eq
  | `Geq -> `Lt
  | `Gt -> `Leq
  | `Leq -> `Gt
  | `Lt -> `Geq

let keep_installed ~drop_installed_packages request pkgname =
  not drop_installed_packages &&
  not (List.exists (fun (pkg, _) -> String.equal pkg pkgname) request.Cudf.install) &&
  not (List.exists (fun (pkg, _) -> String.equal pkg pkgname) request.Cudf.upgrade) &&
  not (List.exists (fun (pkg, _) -> String.equal pkg pkgname) request.Cudf.remove)

let add_spec pkg req c (pkgs, constraints) =
  let pkgs = (pkg, req) :: pkgs in
  let constraints = match c with
    | None -> constraints
    | Some c -> (pkg, c) :: constraints
  in
  (pkgs, constraints)

let essential spec (pkg, c) = add_spec pkg `Essential c spec
let recommended spec (pkg, c) = add_spec pkg `Recommended c spec

let restricts (pkgs, constraints) (pkg, c) =
  let constraints = match c with
    | None -> (pkg, (`Lt, 1)) :: (pkg, (`Gt, 1)) :: constraints (* pkg < 1 & pkg > 1 is always false *)
    | Some (relop, v) -> (pkg, (not_relop relop, v)) :: constraints
  in
  (pkgs, constraints)

let create_spec ~drop_installed_packages universe request =
  let spec = ([], []) in
  let spec = List.fold_left essential spec request.Cudf.install in
  let spec = List.fold_left essential spec request.Cudf.upgrade in
  let spec = List.fold_left restricts spec request.Cudf.remove in
  Cudf.fold_packages_by_name (fun spec pkgname pkgs ->
      match List.find_opt (fun pkg -> pkg.Cudf.installed) pkgs with
      | Some {Cudf.keep = `Keep_version; version; _} -> essential spec (pkgname, Some (`Eq, version))
      | Some {Cudf.keep = `Keep_package; _} -> essential spec (pkgname, None)
      | Some {Cudf.keep = `Keep_feature; _} -> assert false (* NOTE: Opam has no support for features *)
      | Some {Cudf.keep = `Keep_none; _} ->
          if keep_installed ~drop_installed_packages request pkgname then
            recommended spec (pkgname, None)
          else
            spec
      | None -> spec
    ) spec universe

let reconstruct_universe universe selections =
  Opam_0install_cudf.packages_of_result selections |>
  List.fold_left (fun pkgs (pkg, v) ->
      let pkg = Cudf.lookup_package universe (pkg, v) in
      {pkg with was_installed = pkg.installed; installed = true} :: pkgs
    ) [] |>
  Cudf.load_universe

type options = {
  drop_installed_packages : bool;
  prefer_oldest : bool;
  handle_avoid_version : bool;
  prefer_installed : bool;
}

let parse_criteria criteria =
  let default =
    {
      drop_installed_packages = false;
      prefer_oldest = false;
      handle_avoid_version = false;
      prefer_installed = false;
    }
  in
  let rec parse default (criteria : OpamCudfCriteria.criterion list) =
    match criteria with
    | [] -> default
    | (Plus, Removed, None)::xs ->
      parse {default with drop_installed_packages = true} xs
    | (Plus, Solution, Some "version-lag")::xs ->
      parse {default with prefer_oldest = true} xs
    | (Minus, Solution, Some "avoid-version")::xs ->
      parse {default with handle_avoid_version = true} xs
    | (Minus, Changed, None)::xs ->
      parse {default with prefer_installed = true} xs
    | criterion::xs ->
      OpamConsole.warning
        "Criteria '%s' is not supported by the 0install solver"
        (OpamCudfCriteria.criterion_to_string criterion);
      parse default xs
  in
  parse default (OpamCudfCriteria.of_string criteria)

let call ~criteria ?timeout:_ (preamble, universe, request) =
  let {
    drop_installed_packages;
    prefer_oldest;
    handle_avoid_version;
    prefer_installed;
  } =
    parse_criteria criteria
  in
  let timer = OpamConsole.timer () in
  let pkgs, constraints = create_spec ~drop_installed_packages universe request in
  let context =
    Opam_0install_cudf.create
      ~prefer_oldest ~handle_avoid_version ~prefer_installed
      ~constraints universe
  in
  match Opam_0install_cudf.solve context pkgs with
  | Ok selections ->
    let universe = reconstruct_universe universe selections in
    log "Solution found. Solve took %.2f s" (timer ());
    (Some preamble, universe)
  | Error problem ->
    log "No solution. Solve took %.2f s" (timer ());
    log ~level:3 "%a" (OpamConsole.slog Opam_0install_cudf.diagnostics) problem;
    raise Dose_common.CudfSolver.Unsat
