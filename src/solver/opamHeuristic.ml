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

type 'a state = 'a list
type 'a state_space = 'a array list

(* Forget about the changes which are not related to the packages we
   are interested in. We don't have yet computed the transitive
   closure of dependencies: we are processing 'raw' actions which come
   directly from the solver. It shoud be safe to discard
   install/upgrade action outside of the interesting names; delete
   actions are using a different code-path, they should not appear
   here. *)
let minimize_actions interesting_names actions =
  let interesting_names = OpamMisc.StringSet.of_list interesting_names in
  List.filter (function
    | To_change (_, p)
    | To_recompile p -> OpamMisc.StringSet.mem p.Cudf.package interesting_names
    | To_delete p    -> OpamGlobals.error_and_exit "delete %s(%d)\n" p.Cudf.package p.Cudf.version
  ) actions

(* Given a list of bounds, create the least tuple such that the sum of
   components is equal to n.  For instance: init [1;2;1] 3 is
   [0;2;1] *)
let init ~bounds n =
  let rec zero acc n =
    if n > 0 then
      zero (0 :: acc) (n-1)
    else
      acc in
  let zero = zero [] in
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

(* Brute-force exploration of a given space-state:

   [is_consistent] is applied on each possible state of the system,
   where a state is where each pacakge has a fix version. We ensure
   that we apply [is_consistent] in increasing order regarding the
   difference between the maximum version and the current version for
   each package. That is, we apply [is_consistent] first on the state
   where all packages have the maximum version, then on all the states
   where all packages have their maximum version but one which has its
   second maximal version, etc... *)
let brute_force ?(verbose=true) is_consistent state_space =

  let bounds = List.map (fun v -> Array.length v - 1) state_space in
  List.iter (fun v -> assert (v >= 0)) bounds;
  let mk_state t =
    List.map2 (fun vs i -> vs.(i)) state_space t in
  let t0 = Unix.time () in
  let count = ref 0 in
  let interval = 500 in
  let flush_output () =
    if verbose && !count >= interval then
      OpamGlobals.msg " an optimal solution has been found after exploring %d states.\n" !count in

  (* XXX: need to ensure this is properly transformed into a
     while-loop. *)
  let rec aux = function
    | None ->
      log "no better solution found";
      flush_output ();
      None
    | Some t ->
      let state = mk_state t in
      incr count;
      let t1 = Unix.time () in
      if verbose && !count mod interval = interval - 1 then
        OpamGlobals.msg ".";
      if t1 -. t0 > exploration_timeout then (
        if verbose && !count >= interval - 1 then
          OpamGlobals.msg
            " brute-force exploration timed-out [%d states, %.2gs].\n\
             You might need to add explicit version constraints to your \
             request to get a better answer.\n"
            !count exploration_timeout;
        None
      ) else
      if is_consistent state then
        Some state
      else
        aux (succ ~bounds t) in

  aux (init ~bounds 0)

(* Explore a given [state_space] to find the optimal solution. Ideally
   the state space should be as small as possible, eg. we rely on
   previous heuristics to reduce its size. *)
let explore ?(verbose=true) universe state_space =
  let packages_of_state state =
    let filter p =
      List.exists (fun s ->
        s.Cudf.package = p.Cudf.package
        && s.Cudf.version = p.Cudf.version
      ) state
    in
    Cudf.get_packages ~filter universe in
  let is_consistent state =
    let packages = packages_of_state state in
    let open Algo.Diagnostic in
    match Algo.Depsolver.edos_coinstall universe packages with
    | { result = Success _ } -> true
    | { result = Failure _ } -> false in
  brute_force ~verbose is_consistent state_space

(* Build a solution from a given space-state. If a package appears
   in the state, the solution has the package installed with the
   given version. If a package does not appear in the state, but is
   installed in the given universe, its version stays the same.
   Otherwise, if a package appears neither in the state nor is
   installed, it will not appear in the resulting solution. *)
exception Not_reachable
let actions_of_state universe state =
  let installed =
    let filter p =
      p.Cudf.installed
      && List.for_all (fun s -> s.Cudf.package <> p.Cudf.package) state in
    let packages = Cudf.get_packages ~filter universe in
    List.rev_map (fun p -> p.Cudf.package, Some (`Eq, p.Cudf.version)) packages in
  let small_universe =
    let filter p =
      p.Cudf.installed
      || List.exists (fun s -> s.Cudf.package = p.Cudf.package) state in
    let packages = Cudf.get_packages ~filter universe in
    Cudf.load_universe packages in
  let state =
    List.map (fun p -> p.Cudf.package, Some (`Eq, p.Cudf.version)) state in
  let request = {
    wish_install = [];
    wish_remove  = [];
    wish_upgrade = state @ installed
  } in
  match OpamCudf.get_final_universe small_universe request with
  | Conflicts _ -> raise Not_reachable
  | Success u   ->
    try
      let diff = OpamCudf.Diff.diff universe u in
      let actions = OpamCudf.actions_of_diff diff in
      let actions = minimize_actions (List.map fst state) actions in
      actions
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "constraint violations: %s" s

(* Find both dependent and reversely dependent packages. *)
let find_interesting_names universe constrs =
  let filter pkg =
    List.exists (fun (n,v) ->
      n = pkg.Cudf.package
      && Cudf.version_matches pkg.Cudf.version v
    ) constrs in
  let depends =
    let packages = Cudf.get_packages ~filter universe in
    OpamCudf.dependencies universe packages in
  let revdepends =
    let filter pkg = pkg.Cudf.installed && filter pkg in
    let packages = Cudf.get_packages ~filter universe in
    OpamCudf.reverse_dependencies universe packages in
  let set = ref OpamMisc.StringSet.empty in
  let add p = set := OpamMisc.StringSet.add p.Cudf.package !set in
  List.iter add depends;
  List.iter add revdepends;
  OpamMisc.StringSet.elements !set

(* Find:
   - all package dependencies; and
   - package reverse dependencies which are installed. *)
let dependencies universe constrs =
  let filter pkg =
    List.exists (fun (n,v) ->
      n = pkg.Cudf.package
      && Cudf.version_matches pkg.Cudf.version v
    ) constrs in
  let packages = Cudf.get_packages ~filter universe in
  let packages = OpamCudf.dependencies universe packages in
  let universe = Cudf.load_universe packages in
  Algo.Depsolver.trim universe

(* [state_space] returns the packages which will be tested by the
   brute-force state explorer. As we try to minimize the state to
   explore for each package, this means:

   - if the package has a version constraint in the request, that's
   the only we consider (eg. return only one element for this package
   name: the version in the constraint)

   - if the package has no version constraints in the request, or if
   the package does not appear in the initial request, then return
   only the versions greater or equals to the one appearing in the
   given universe.
*)
let state_space ?(filters = fun _ -> None) universe interesting_names =

  let universe_packages = Cudf.get_packages universe in

  (* Return the version associated to a given package in the
     universe. *)
  let installed_version_of_name =
    let tbl = Hashtbl.create 1024 in
    List.iter
      (fun p ->
        if p.Cudf.installed then
          Hashtbl.add tbl p.Cudf.package p.Cudf.version)
      universe_packages;
    function name ->
      try Some (Hashtbl.find tbl name)
      with Not_found -> None in

  let state_space = Hashtbl.create 1024 in
  let add_state name =
    if not (Hashtbl.mem state_space name) then
      let filter = filters name in
      let packages = Cudf.lookup_packages universe ~filter name in
      let packages =
        match installed_version_of_name name with
        | None   -> packages
        | Some v -> List.filter (fun p -> p.Cudf.version >= v) packages in
      let packages =
        List.sort (fun p1 p2 -> compare p2.Cudf.version p1.Cudf.version) packages in
      if List.length packages <> 0 then
        Hashtbl.add state_space name (Array.of_list packages) in

  List.iter add_state interesting_names;
  Hashtbl.fold (fun _ states acc -> states :: acc) state_space []

(* Various heuristic to transform a solution checker into an optimized
   solver. *)
let resolve ?(verbose=true) current_universe request =

  if request.wish_remove <> [] then
    OpamCudf.resolve current_universe request

  else match OpamCudf.get_final_universe current_universe request with
    | Conflicts e ->
      log "resolve: conflict!";
      Conflicts e

    | Success result_universe ->
      log "resolve: result-universe=%s"
        (OpamCudf.string_of_universe result_universe);

      (* This first [result_universe] is a consistent solution which
         contains only installed packages fulfilling the initial
         constraints. It is thus not a complete universe and it is not
         guaranteed to be optimal. So we extend the result with all the
         existing packages. *)
      let result_universe =
        let installed = Cudf.get_packages result_universe in
        let current_universe = OpamCudf.uninstall_all current_universe in
        List.fold_left OpamCudf.install current_universe installed in

      let all_wishes = request.wish_install @ request.wish_upgrade in

      (* We remove from the universe all the package versions which are
         not specified on the command-line. For instance:

         $ opam install core.109.13.00

         will cause all versions of core != 109.13.00 to disapear from
         the universe. This is causing [Universe.trim] to remove *a lot*
         of uninstallable packages and will improve the brute-force
         state exploration results.

         Note: We don't want to trim the universe too early because we
         want to keep good error messages in case the solver does not
         find a solution. *)
      let trimed_universe =
        let universe = List.fold_left (fun universe (name, constr) ->
            OpamCudf.remove_all_uninstalled_versions_but name constr universe
          ) result_universe all_wishes in
        let universe = Algo.Depsolver.trim universe in
        dependencies universe all_wishes in
      log "trimed-universe: %s" (OpamCudf.string_of_universe trimed_universe);

      let interesting_names =
        find_interesting_names current_universe request.wish_upgrade in
      log "resolve: interesting-name=%s" (OpamMisc.pretty_list interesting_names);

      let filters name =
        try List.assoc name all_wishes
        with Not_found -> None in

      let state_space = state_space ~filters trimed_universe interesting_names in

      match explore ~verbose current_universe state_space with
      | Some state -> Success (actions_of_state current_universe state)
      | None       ->
        log "no optimized solution found with the current state-space";
        (* XXX: we could try with a different state-space *)
        OpamCudf.resolve current_universe request
