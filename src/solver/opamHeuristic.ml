(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes

let log fmt = OpamGlobals.log "HEURISTIC" fmt

(* Try to remove a subset of the installed packages from the universe
   and check whether the the resulting universe stays consistent. *)
let rec minimize_universe minimizable universe =
  log "minimize minimizable=%s" (OpamMisc.StringSet.to_string minimizable);
  if OpamMisc.StringSet.is_empty minimizable then
    universe
  else
    let is_removable universe name =
      let b, r = Cudf_checker.is_consistent (OpamCudf.uninstall name universe) in
      (match r with
      | None   -> log "%s is not necessary" name
      | Some r ->
        log "cannot remove %s: %s" name
          (Cudf_checker.explain_reason (r:>Cudf_checker.bad_solution_reason)));
      b in
    let to_remove = OpamMisc.StringSet.filter (is_removable universe) minimizable in
    let minimizable = OpamMisc.StringSet.diff minimizable to_remove in
    if OpamMisc.StringSet.is_empty to_remove then
      universe
    else
      let universe = OpamMisc.StringSet.fold OpamCudf.uninstall to_remove universe in
      minimize_universe minimizable universe

(* Forget about the changes which are not related to the packages we
   are interested in.  We don't have yet computed the transitive
   closure of dependencies: we are processing 'raw' actions which come
   directly from the solver. It shoud be safe to discard
   install/upgrade action outside of our the interesting packages;
   delete actions are using a different code-path, so we arbitrary
   keep it there. *)
let minimize_actions interesting_packages actions =
  List.filter (function
  | To_change (_, p)
  | To_recompile p -> OpamMisc.StringSet.mem p.Cudf.package interesting_packages
  | To_delete _    -> assert false
  ) actions

(* Given a list of bounds, create the least tuple such that the sum of
   components is equal to n.  For instance: init [1;2;1] 3 is
   [0;2;1] *)
let init ~bounds n =
  let rec zero n =
    if n > 0 then
      0 :: zero (n-1)
    else
      [] in
  let rec aux = function
    | 0, []   -> Some []
    | 0, l    -> Some (zero (List.length l))
    | _, []   -> None
    | n, b::t ->
      if n <= b then
        Some (n :: zero (List.length t))
      else match aux (n-b, t) with
      | None   -> None
      | Some l -> Some (b::l) in
  match aux (n, List.rev bounds) with
  | None   -> None
  | Some l -> Some (List.rev l)

  (* Given a list of bounds and a tuple, return the next tuple while
     keeping the sum of components of the tuple constant *)
let rec cst_succ ~bounds k l =
  match l, bounds with
  | [] , []  -> None
  | [n], [b] ->
    if n+1 = k && n < b then
      Some [k]
    else
      None
  | n::nt, b::bt ->
    if n >= k then
      None
    else (
      match cst_succ ~bounds:bt (k-n) nt with
      | Some s -> Some (n::s)
      | None   ->
        if n < b then
          match init ~bounds:bt (k-n-1) with
          | None   -> None
          | Some l -> Some (n+1 :: l)
        else
          None)
  | _ ->
    failwith "Bounds and tuple do not have the same size"

(* Given a list of bounds and a tuple, return the next tuple *)
let succ ~bounds l =
  let k = List.fold_left (+) 0 l in
  match cst_succ ~bounds k l with
  | Some t -> Some t
  | None   ->
    let k = List.fold_left (+) 0 l in
    init ~bounds (k+1)

(* Maximum duration of the state-space exploration. *)
let exploration_timeout = 5.

(* explore the state-space given by an upgrade table.

   - [upgrade_tbl] associate pkg name to pacake constraints, for a
   collection of possible versions.

   - [f] is applied on each possible state of the system, where a
   state is where each pacakge has a fix version. We ensure that we
   apply [f] in increasing order regarding the difference between
   the maximum version and the current version for each
   package. That is, we apply [f] first on the state where all
   package have the maximum version, then on all the states where at
   all the package have their maximum version but one which has the
   second version, etc... *)
let explore f upgrade_tbl =
  let default_conflict = Conflicts (fun _ -> assert false)  in
  let upgrades =
    Hashtbl.fold (fun pkg constrs acc -> (pkg, constrs) :: acc) upgrade_tbl [] in
  let bounds = List.map (fun (_,v) -> Array.length v - 1) upgrades in
  let constrs t =
    List.map2 (fun (_, vs) i -> vs.(i)) upgrades t in
  let t0 = Unix.time () in
  let count = ref 0 in
  let interval = 500 in
  let flush_output () =
    if !count >= interval then OpamGlobals.msg "[%d]\n" !count in
  let rec aux = function
    | None   ->
      log "no better solution found";
      flush_output ();
      default_conflict
    | Some t ->
      let constrs = constrs t in
      (* log "explore %s %s"
        (OpamMisc.string_of_list
           (fun (i,b) -> Printf.sprintf "%d/%d" (i+1) (b+1))
           (List.combine t bounds))
        (OpamFormula.string_of_conjunction OpamCudf.string_of_atom constrs); *)
      incr count;
      let t1 = Unix.time () in
      if !count mod interval = interval - 1 then
        OpamGlobals.msg ".";
      if t1 -. t0 > exploration_timeout then (
        if !count >= interval - 1 then OpamGlobals.msg "T";
        default_conflict
      ) else match f constrs with
      | Success _ as s -> flush_output (); s
      | _              -> aux (succ ~bounds t) in
  aux (init ~bounds 0)

let filter_dependencies universe constrs =
  let filter pkg =
    List.exists (fun (n,v) ->
      n = pkg.Cudf.package
      && match v with
      | None       -> true
      | Some (_,x) -> x=pkg.Cudf.version
    ) constrs in
  let packages = Cudf.get_packages ~filter universe in
  let graph = OpamCudf.Graph.of_universe universe in
  let packages = OpamCudf.Graph.closure graph (OpamCudf.Set.of_list packages) in
  OpamMisc.StringSet.of_list (List.map (fun p -> p.Cudf.package) packages)

(* Try to play all the possible upgrade scenarios ... *)
let resolve universe request =
  match OpamCudf.get_final_universe universe request with

  | Conflicts e ->
    log "resolve: conflict!";
    Conflicts e

  | Success u   ->
    log "resolve: sucess! final-universe=%s" (OpamCudf.string_of_universe u);

    (* Return the version of a given package in the initial
       solution *)
    let initial_version =
      let initial_versions = Hashtbl.create 1024 in
      List.iter (fun pkg -> Hashtbl.add initial_versions pkg.Cudf.package pkg.Cudf.version) (Cudf.get_packages u);
      function name ->
        try Some (Hashtbl.find initial_versions name)
        with Not_found -> None in

    (* [upgrade_tbl] contains the packages which will be tested by the
       brute-force state explorer. In the remaining parts of this
       function, we try to minimize the state to explore for each
       package. This means:

       - if the package has a version constraint in the request,
       that's the only we consider.

       - if the package has no version constraints in the request, or
       if the package does not appear in the initial request, then we
       consider only the versions greater or equals to the one
       proposed by the solver.

    *)
    let upgrade_tbl = Hashtbl.create 1024 in
    let version_constraint =
      let l = request.wish_install @ request.wish_upgrade in
      function name ->
        try List.assoc name l
        with Not_found -> None in
    let add_to_upgrade name =
      if not (Hashtbl.mem upgrade_tbl name) then
        match version_constraint name with
        | Some v -> Hashtbl.add upgrade_tbl name [| (name, Some v) |]
        | None   ->
          match initial_version name with
          | None             -> ()
          | Some min_version ->
            let packages =
              Cudf.get_packages
                ~filter:(fun p -> p.Cudf.package = name && p.Cudf.version >= min_version)
                universe in
            let packages = List.sort (fun p1 p2 -> compare p2.Cudf.version p1.Cudf.version) packages in
            let atoms = List.map (fun p -> p.Cudf.package, Some (`Eq, p.Cudf.version)) packages in
            Hashtbl.add upgrade_tbl name (Array.of_list atoms) in

    (* Compute the set of minimizable packages, eg. the ones which are
       maybe not necessary to get a correct solution (ie. we will
       later check whether an optimal solution without these package
       exists). *)
    let requested_packages = ref OpamMisc.StringSet.empty in
    List.iter (fun (n,_) ->
      requested_packages := OpamMisc.StringSet.add n !requested_packages
    ) (request.wish_upgrade @ request.wish_install);
    let minimizable = ref OpamMisc.StringSet.empty in
    let diff = Common.CudfDiff.diff universe u in
    Hashtbl.iter (fun name s ->
      if not (Common.CudfAdd.Cudf_set.is_empty s.Common.CudfDiff.installed)
      && not (OpamMisc.StringSet.mem name !requested_packages) then
        (* REMARK: non-root packages are 'minimizable' *)
        minimizable := OpamMisc.StringSet.add name !minimizable
    ) diff;

    (* Register the interesting packages (eg. the ones we want to optimize) *)
    let interesting_packages = filter_dependencies universe request.wish_upgrade in
    log "resolve: interesting_packages=%s" (OpamMisc.StringSet.to_string interesting_packages);
    OpamMisc.StringSet.iter (fun n -> add_to_upgrade n) interesting_packages;

    (* We build a new [wish_install] constraint, containing all the
       packages initially installed which do not belong to the list of
       interesting packages. We want these packages to stay unchanged. *)
    let wish_install =
      let installed = Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) universe in
      let installed = List.filter (fun p -> not (Hashtbl.mem upgrade_tbl p.Cudf.package)) installed in
      List.map (fun p ->
        let constr =
          try List.assoc p.Cudf.package request.wish_install
          with Not_found -> None in
        let constr = match constr with
          | None   -> Some (`Eq, p.Cudf.version)
          | Some v -> Some v in
        p.Cudf.package, constr
      ) installed in

    let resolve wish_upgrade =
      let request = { request with wish_install; wish_upgrade } in
      (* log "explore: request=%s" (OpamCudf.string_of_request request); *)
      OpamCudf.get_final_universe universe request in

    match explore resolve upgrade_tbl with
    | Conflicts _ ->
      log "no optimized solution found";
      OpamCudf.resolve universe request
    | Success u   ->
      log "succes=%s" (OpamCudf.string_of_universe u);
      try
        let diff = OpamCudf.Diff.diff universe (minimize_universe !minimizable u) in
        let actions = OpamCudf.actions_of_diff diff in
        let actions = minimize_actions interesting_packages actions in
        Success actions
      with Cudf.Constraint_violation s ->
        OpamGlobals.error_and_exit "constraint violations: %s" s
