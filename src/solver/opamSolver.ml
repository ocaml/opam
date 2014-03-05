(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2014 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase

let log fmt = OpamGlobals.log "SOLVER" fmt

module Action = OpamActionGraph.MakeAction(OpamPackage)
module ActionGraph = OpamActionGraph.Make(Action)
type solution = (OpamPackage.t, ActionGraph.t) gen_solution

let empty_universe =
  {
    u_packages = OpamPackage.Set.empty;
    u_installed = OpamPackage.Set.empty;
    u_available = OpamPackage.Set.empty;
    u_depends = OpamPackage.Map.empty;
    u_depopts = OpamPackage.Map.empty;
    u_conflicts = OpamPackage.Map.empty;
    u_action = Install OpamPackage.Name.Set.empty;
    u_installed_roots = OpamPackage.Set.empty;
    u_pinned = OpamPackage.Name.Map.empty;
    u_builddeps = OpamPackage.Set.empty;
  }

(* Returns the package with its real version if it has been pinned *)
let real_version universe pkg =
  if OpamPackage.is_pinned pkg then
    let n = OpamPackage.name pkg in
    let v = Lazy.force (OpamPackage.Name.Map.find n universe.u_pinned) in
    OpamPackage.create n v
  else pkg

(* Convert an OPAM formula into a debian formula *)
let atom2debian (n, v) =
  (OpamPackage.Name.to_string n, None),
  match v with
  | None       -> None
  | Some (r,v) -> Some (OpamFormula.string_of_relop r, OpamPackage.Version.to_string v)

(* Get the optional depencies of a package *)
let depopts_of_package universe package =
  let opts =
    try OpamPackage.Map.find package universe.u_depopts
    with Not_found -> Empty in
  OpamFormula.to_dnf opts

let is_installed universe (name,_) =
  OpamPackage.Set.exists (fun pkg ->
      OpamPackage.name pkg = name
    ) universe.u_installed

let find_installed universe (name, _) =
  let pkg = OpamPackage.Set.find (fun pkg ->
      OpamPackage.name pkg = name
    ) universe.u_installed in
  OpamPackage.version pkg

let is_available universe wish_remove (name, _ as c) =
  let version = find_installed universe c in
  OpamPackage.Set.exists (fun pkg ->
      OpamPackage.name pkg = name && OpamPackage.version pkg = version
    ) universe.u_available
  &&
  List.for_all (fun (n, _) -> n <> name) wish_remove

let cudf_versions_map universe =
  let pmap = OpamPackage.to_map universe.u_packages in
  OpamPackage.Name.Map.fold (fun name versions acc ->
      let versions = OpamPackage.Version.Set.elements versions in
      let versions = List.filter ((<>) OpamPackage.Version.pinned) versions in
      let versions = List.sort OpamPackage.Version.compare versions in
      let _, map =
        List.fold_left
          (fun (i,acc) version ->
             let nv = OpamPackage.create name version in
             i + 1, OpamPackage.Map.add nv i acc)
          (1,acc) versions in
      map)
    pmap OpamPackage.Map.empty

let name_to_cudf name =
  Common.CudfAdd.encode (OpamPackage.Name.to_string name)

let atom2cudf version_map (name,cstr) =
  name_to_cudf name, match cstr with
  | None -> None
  | Some (op,v) ->
    try
      let cv = OpamPackage.Map.find (OpamPackage.create name v) version_map in
      Some (op, cv)
    with Not_found ->
      (* The version for comparison doesn't exist: match to the closest
         existing version according to the direction of the comparison *)
      match op with
      | `Neq -> None (* Always true *)
      | `Eq -> Some (`Lt, 1) (* Always false *)
      | (`Geq | `Gt | `Leq | `Lt) as op ->
        let sign, result_op =  match op with
          | `Geq | `Gt -> (fun x -> x), `Geq
          | `Leq | `Lt -> (fun x -> -x), `Leq in
        let all_versions =
          OpamPackage.Map.filter (fun nv _ -> OpamPackage.name nv = name)
            version_map in
        let rev_version_map =
          OpamPackage.Map.fold (fun nv cv acc ->
              OpamMisc.IntMap.add (sign cv) (OpamPackage.version nv) acc)
            all_versions OpamMisc.IntMap.empty in
        let map =
          OpamMisc.IntMap.filter
            (fun _ v1 -> sign (OpamPackage.Version.compare v v1) < 0)
            rev_version_map in
        if OpamMisc.IntMap.is_empty map then Some (`Lt, 1)
        else Some (result_op, sign (fst (OpamMisc.IntMap.min_binding map)))

let opam2cudf universe depopts version_map package =
  let package = real_version universe package in
  let depends =
    try OpamPackage.Map.find package universe.u_depends
    with Not_found -> Empty in
  let depends =
    let opts = depopts_of_package universe package in
    if depopts then
      let opts = List.rev_map OpamFormula.of_conjunction opts in
      And (depends, Or(depends, OpamFormula.ors opts))
    else if universe.u_action = Remove then depends
    else
    let mem_installed conj = List.exists (is_installed universe) conj in
    let opts = List.filter mem_installed opts in
    let opts = List.rev_map OpamFormula.of_conjunction opts in
    And (depends, OpamFormula.ands opts) in
  let conflicts =
    try OpamPackage.Map.find package universe.u_conflicts
    with Not_found -> Empty in
  let conflicts = (* prevents install of multiple versions of the same pkg *)
    (OpamPackage.name package, None)::OpamFormula.to_conjunction conflicts in
  let installed =
    OpamPackage.Set.exists (fun pkg -> real_version universe pkg = package)
      universe.u_installed in
  let reinstall = match universe.u_action with
    | Upgrade reinstall ->
      OpamPackage.Set.exists (fun pkg -> package = real_version universe pkg)
        reinstall
    | _                 -> false in
  let installed_root = OpamPackage.Set.mem package universe.u_installed_roots in
  let extras =
    let e = [
      OpamCudf.s_source,
      `String (OpamPackage.Name.to_string (OpamPackage.name package));
      OpamCudf.s_source_number,
      `String (OpamPackage.Version.to_string (OpamPackage.version package));
    ] in
    let e = if OpamPackage.Set.mem package universe.u_builddeps
      then (OpamCudf.s_builddep, `Bool true)::e else e in
    let e = if installed && reinstall
      then (OpamCudf.s_reinstall, `Bool true)::e else e in
    let e = if installed_root
      then (OpamCudf.s_installed_root, `Bool true)::e else e in
    e
  in
  { Cudf.default_package with
    Cudf.
    package = name_to_cudf (OpamPackage.name package);
    version = OpamPackage.Map.find package version_map;
    (* keep = `Keep_none; -- XXX use `Keep_version to handle pinned packages ? *)
    depends = List.rev_map (List.rev_map (atom2cudf version_map))
        (OpamFormula.to_cnf depends);
    conflicts = List.rev_map (atom2cudf version_map) conflicts;
    installed;
    (* was_installed: ? ;
       provides: unused *)
    pkg_extra = extras;
  }

let cudf2opam cpkg =
  let sname = Cudf.lookup_package_property cpkg OpamCudf.s_source in
  let name = OpamPackage.Name.of_string sname in
  let sver = Cudf.lookup_package_property cpkg OpamCudf.s_source_number in
  let version = OpamPackage.Version.of_string sver in
  OpamPackage.create name version

let atom2cudf opam2cudf (n, v) : Cudf_types.vpkg =
  Common.CudfAdd.encode (OpamPackage.Name.to_string n),
  match v with
  | None       -> None
  | Some (r,v) ->
    let pkg =
      try opam2cudf (OpamPackage.create n v)
      with Not_found ->
        OpamGlobals.error_and_exit "Package %s does not have a version %s"
          (OpamPackage.Name.to_string n)
          (OpamPackage.Version.to_string v) in
    Some (r, pkg.Cudf.version)

(* load a cudf universe from an opam one *)
let load_cudf_universe ?(depopts=false) universe =
  (* The package numbering can be different in the universe if we
     consider optional dependencies or not. To avoid that, we create a
     dumb package which depends on all the optional dependencies. This
     package should never appear to the user, so we make it
     non-installable by adding conflicting constraints. *)
  let opam2cudf =
    let version_map = cudf_versions_map universe in
    OpamPackage.Set.fold (fun pkg map ->
        OpamPackage.Map.add (real_version universe pkg)
          (opam2cudf universe depopts version_map pkg)
          map)
      universe.u_packages
      OpamPackage.Map.empty in
  let cudf2opam cpkg =
    let pkg = cudf2opam cpkg in
    if try Lazy.force (OpamPackage.Name.Map.find
                         (OpamPackage.name pkg) universe.u_pinned)
           = OpamPackage.version pkg
      with Not_found -> false
    then OpamPackage.pinned (OpamPackage.name pkg)
    else pkg in
  let opam_universe = universe in
  let universe =
    let universe =
      OpamPackage.Set.fold
        (fun nv list -> try
            let nv = OpamPackage.Map.find (real_version universe nv) opam2cudf in
            nv :: list
          with Not_found ->
            OpamGlobals.error
              "The package %s (real-version: %s) cannot be found by the solver, \
               skipping."
              (OpamPackage.to_string nv)
              (OpamPackage.to_string (real_version universe nv));
            list)
        universe.u_available [] in
    try Cudf.load_universe universe
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "Malformed CUDF universe (%s)" s in
  (* We can trim the universe here to get faster results, but we
     choose to keep it bigger to get more precise conflict messages. *)
  (* let universe = Algo.Depsolver.trim universe in *)
  (fun opam ->
    let opam = real_version opam_universe opam in
    try OpamPackage.Map.find opam opam2cudf
    with Not_found ->
      OpamGlobals.error_and_exit
        "opam2cudf: Cannot find %s" (OpamPackage.to_string opam)),
  cudf2opam,
  universe

let string_of_request r =
  let to_string = OpamFormula.string_of_conjunction OpamFormula.string_of_atom in
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (to_string r.wish_install)
    (to_string r.wish_remove)
    (to_string r.wish_upgrade)

let map_action f = function
  | To_change (Some x, y) -> To_change (Some (f x), f y)
  | To_change (None, y)   -> To_change (None, f y)
  | To_delete y           -> To_delete (f y)
  | To_recompile y        -> To_recompile (f y)

let map_cause f = function
  | Upstream_changes -> Upstream_changes
  | Use l            -> Use (List.rev_map f l)
  | Required_by l    -> Required_by (List.rev_map f l)
  | Conflicts_with l -> Conflicts_with (List.rev_map f l)
  | Requested        -> Requested
  | Unknown          -> Unknown

let graph cudf2opam cudf_graph =
  let size = OpamCudf.ActionGraph.nb_vertex cudf_graph in
  let opam_graph = ActionGraph.create ~size () in
  OpamCudf.ActionGraph.iter_vertex (fun package ->
    ActionGraph.add_vertex opam_graph (map_action cudf2opam package)

  ) cudf_graph;
  OpamCudf.ActionGraph.iter_edges (fun p1 p2 ->
    ActionGraph.add_edge opam_graph
      (map_action cudf2opam p1)
      (map_action cudf2opam p2)
  ) cudf_graph;
  opam_graph

let solution cudf2opam cudf_solution =
  let to_remove =
    List.rev (List.rev_map cudf2opam cudf_solution.to_remove) in
  let to_process = graph cudf2opam cudf_solution.to_process in
  let root_causes =
    List.rev_map
      (fun (p, c) -> cudf2opam p, map_cause cudf2opam c)
      cudf_solution.root_causes in
  { to_remove ; to_process; root_causes }

let map_request f r =
  let f = List.rev_map f in
  { wish_install = f r.wish_install;
    wish_remove  = f r.wish_remove ;
    wish_upgrade = f r.wish_upgrade }

(* Remove duplicate packages *)
(* Add upgrade constraints *)
let cleanup_request universe (req:atom request) =
  let wish_install =
    let upgrade_packages = List.rev_map (fun (n,_) -> n) req.wish_upgrade in
    List.filter (fun (n,_) -> not (List.mem n upgrade_packages)) req.wish_install in
  let wish_upgrade =
    List.rev_map (fun (n,c as pkg) ->
        if c = None
        && is_installed universe pkg
        && is_available universe req.wish_remove pkg then
          n, Some (`Geq, find_installed universe pkg)
        else
          pkg
      ) req.wish_upgrade in
  { req with wish_install; wish_upgrade }

let resolve ?(verbose=true) universe ~requested request =
  log "resolve request=%s" (string_of_request request);
  let opam2cudf, cudf2opam, simple_universe = load_cudf_universe universe in
  let request = cleanup_request universe request in
  let cudf_request = map_request (atom2cudf opam2cudf) request in
  let resolve u req =
    if OpamCudf.external_solver_available ()
    then
      try OpamCudf.resolve u req
      with Failure "opamSolver" ->
        OpamGlobals.msg "Falling back to the internal heuristic.\n";
        OpamHeuristic.resolve ~verbose u req
    else OpamHeuristic.resolve ~verbose u req in
  match resolve simple_universe cudf_request with
  | Conflicts c     ->
    Conflicts (fun () -> OpamCudf.string_of_reasons cudf2opam universe (c ()))
  | Success actions ->
    let _, _, complete_universe = load_cudf_universe ~depopts:true universe in
    let cudf_solution =
      OpamCudf.solution_of_actions
        ~simple_universe ~complete_universe ~requested actions in
    Success (solution cudf2opam cudf_solution)

let installable universe =
  log "trim";
  let _, cudf2opam, simple_universe = load_cudf_universe universe in
  let trimed_universe = Algo.Depsolver.trim simple_universe in
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimed_universe

let filter_dependencies f_direction ~depopts ~installed universe packages =
  let opam2cudf, cudf2opam, cudf_universe = load_cudf_universe ~depopts universe in
  let cudf_universe =
    if installed then
      Cudf.load_universe
        (List.filter (fun pkg -> pkg.Cudf.installed) (Cudf.get_packages cudf_universe))
    else
      cudf_universe in
  let cudf_packages = List.rev_map opam2cudf (OpamPackage.Set.elements packages) in
  let topo_packages = f_direction cudf_universe cudf_packages in
  let result = List.rev_map cudf2opam topo_packages in
  log "filter_dependencies packages=%s result=%s"
    (OpamPackage.Set.to_string packages)
    (OpamMisc.string_of_list OpamPackage.to_string result);
  result

let dependencies = filter_dependencies OpamCudf.dependencies

let reverse_dependencies = filter_dependencies OpamCudf.reverse_dependencies

let delete_or_update t =
  t.to_remove <> [] ||
  ActionGraph.fold_vertex
    (fun v acc ->
      acc || match v with To_change (Some _, _) -> true | _ -> false)
    t.to_process
    false

let new_packages sol =
  ActionGraph.fold_vertex (fun action packages ->
    OpamPackage.Set.add (action_contents action) packages
  ) sol.to_process OpamPackage.Set.empty

let stats sol =
  let s_install, s_reinstall, s_upgrade, s_downgrade =
    ActionGraph.fold_vertex (fun action (i,r,u,d) ->
      match action with
      | To_change (None, _)             -> i+1, r, u, d
      | To_change (Some x, y) when x<>y ->
        if OpamPackage.Version.compare
            (OpamPackage.version x) (OpamPackage.version y) < 0 then
          i, r, u+1, d
        else
          i, r, u, d+1
      | To_change (Some _, _)
      | To_recompile _                  -> i, r+1, u, d
      | To_delete _ -> assert false)
      sol.to_process
      (0, 0, 0, 0) in
  let s_remove = List.length sol.to_remove in
  { s_install; s_reinstall; s_upgrade; s_downgrade; s_remove }

let string_of_stats stats =
  Printf.sprintf
    "%d to install | %d to reinstall | %d to upgrade | %d to downgrade | %d to remove"
    stats.s_install
    stats.s_reinstall
    stats.s_upgrade
    stats.s_downgrade
    stats.s_remove

let solution_is_empty t =
  t.to_remove = []
  && ActionGraph.is_empty t.to_process

let print_solution ~messages ~rewrite t =
  if not (solution_is_empty t) then
    let print_action a =
      let cause = try List.assoc (action_contents a) t.root_causes
        with Not_found -> Unknown in
      match string_of_cause OpamPackage.name_to_string cause with
      | "" -> OpamGlobals.msg " - %s\n" (Action.to_string a)
      | c  -> OpamGlobals.msg " - %-47s [%s]\n" (Action.to_string a) c in
    List.iter (fun p -> print_action (To_delete (rewrite p))) t.to_remove;
    ActionGraph.Topological.iter (function action ->
        let action = map_action rewrite action in
        print_action action;
        match action with
        | To_change(_,p)
        | To_recompile p -> List.iter (OpamGlobals.msg "     %s.\n")  (messages p)
        | To_delete _    -> ()
      ) t.to_process

let sequential_solution l =
  let g = ActionGraph.create () in
  List.iter (ActionGraph.add_vertex g) l;
  let rec aux = function
    | [] | [_]       -> ()
    | x::(y::_ as t) ->
      ActionGraph.add_edge g x y;
      aux t in
  aux l;
  {
    to_remove = [];
    to_process = g;
    root_causes = []
  }
