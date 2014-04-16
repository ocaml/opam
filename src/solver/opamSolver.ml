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
open OpamMisc.OP
open OpamPackage.Set.Op

let log fmt = OpamGlobals.log "SOLVER" fmt
let slog = OpamGlobals.slog

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

let cudf_versions_map universe packages =
  let pmap = OpamPackage.to_map packages in
  OpamPackage.Name.Map.fold (fun name versions acc ->
      let versions =
        OpamPackage.Version.Set.map (fun v ->
            OpamPackage.version (real_version universe (OpamPackage.create name v)))
          versions in
      let _, map =
        OpamPackage.Version.Set.fold
          (fun version (i,acc) ->
             let nv = OpamPackage.create name version in
             i + 1, OpamPackage.Map.add nv i acc)
          versions (1,acc) in
      map)
    pmap OpamPackage.Map.empty

let name_to_cudf name =
  Common.CudfAdd.encode (OpamPackage.Name.to_string name)

let atom2cudf universe (version_map : int OpamPackage.Map.t) (name,cstr) =
  name_to_cudf name, match cstr with
  | None -> None
  | Some (op,v) ->
    let nv = OpamPackage.create name v in
    let nv = real_version universe nv in
    let v = OpamPackage.version nv in
    try
      let cv = OpamPackage.Map.find nv version_map in
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

let opam2cudf universe ?(depopts=false) version_map package =
  let package = real_version universe package in
  let name = OpamPackage.name package in
  let depends =
    try OpamPackage.Map.find package universe.u_depends
    with Not_found -> Empty in
  let depopts_packages =
    try OpamPackage.Map.find package universe.u_depopts
    with Not_found -> Empty in
  let conflicts =
    try OpamPackage.Map.find package universe.u_conflicts
    with Not_found -> Empty in
  let depends =
    if depopts then
      let opts = OpamFormula.to_dnf depopts_packages in
      let opts = List.rev_map OpamFormula.of_conjunction opts in
      OpamFormula.ands [depends; OpamFormula.ors (depends :: opts)]
    else depends
  in
  let conflicts = (* add complement of the depopts as conflicts *)
    let module NM = OpamPackage.Name.Map in
    let depopts = (* get back a map (name => version_constraint) *)
      (* XXX this takes _all_ the atoms not considering con/disjunctions *)
      OpamFormula.fold_left (fun acc (name,f) ->
          try NM.add name (Or (f, NM.find name acc)) acc
          with Not_found -> NM.add name f acc)
        OpamPackage.Name.Map.empty
        depopts_packages in
    let neg_depopts =
      NM.fold (fun name f acc ->
          if f = OpamFormula.Empty then acc else
          let f = OpamFormula.(neg (fun (op,v) -> neg_relop op, v) f) in
          match OpamFormula.to_dnf (Atom (name,f)) with
          | [] -> acc
          | [conj] -> conj @ acc
          | dnf ->
            (* Formula is not a conjunction, we are left with no choice
               but to enumerate *)
            log "enum-depopts of %a / depopt %a"
              (slog OpamPackage.to_string) package
              (slog OpamPackage.Name.to_string) name;
            let f =
              OpamFormula.to_atom_formula @@ OpamFormula.ors @@
              List.map OpamFormula.of_conjunction dnf in
            let conflict_packages =
              OpamPackage.Set.filter
                (fun pkg ->
                   OpamFormula.eval (fun atom -> OpamFormula.check atom pkg) f)
                (OpamPackage.packages_of_name universe.u_available name)
            in
            OpamPackage.Set.fold (fun nv acc ->
                (OpamPackage.name nv, Some (`Eq, OpamPackage.version nv))
                :: acc)
              conflict_packages acc)
        depopts [] in
    let self = name, None in (* prevent multiples of the same pkg *)
    self ::
    OpamFormula.to_conjunction conflicts
    @ neg_depopts
  in
  let installed =
    OpamPackage.Set.exists (fun pkg -> real_version universe pkg = package)
      universe.u_installed in
  let reinstall = match universe.u_action with
    | Upgrade reinstall ->
      OpamPackage.Set.exists (fun pkg -> package = real_version universe pkg)
        reinstall
    | _                 -> false in
  let installed_root = OpamPackage.Set.mem package universe.u_installed_roots in
  let pinned_to_current_version =
    try Lazy.force (OpamPackage.Name.Map.find name universe.u_pinned)
        = OpamPackage.version package
    with Not_found -> false in
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
    let e = if pinned_to_current_version
      then (OpamCudf.s_pinned, `Bool true)::e else e in
    e
  in
  { Cudf.default_package with
    Cudf.
    package = name_to_cudf (OpamPackage.name package);
    version = OpamPackage.Map.find package version_map;
    depends = List.rev_map (List.rev_map (atom2cudf universe version_map))
        (OpamFormula.to_cnf depends);
    conflicts = List.rev_map (atom2cudf universe version_map) conflicts;
    installed;
    (* was_installed: ? ;
       provides: unused *)
    pkg_extra = extras;
  }

let cudf2opam cpkg =
  let sname = Cudf.lookup_package_property cpkg OpamCudf.s_source in
  let name = OpamPackage.Name.of_string sname in
  let pinned =
    try Cudf.lookup_typed_package_property cpkg OpamCudf.s_pinned
    with Not_found -> `Bool false in
  if pinned = `Bool true then OpamPackage.pinned name else
  let sver = Cudf.lookup_package_property cpkg OpamCudf.s_source_number in
  let version = OpamPackage.Version.of_string sver in
  OpamPackage.create name version

(* load a cudf universe from an opam one *)
let load_cudf_universe ?depopts opam_universe ?version_map opam_packages =
  let version_map = match version_map with
    | Some vm -> vm
    | None -> cudf_versions_map opam_universe opam_packages in
  let opam_packages =
    OpamPackage.Set.map (real_version opam_universe) opam_packages in
  let cudf_universe =
    let cudf_packages =
      OpamPackage.Set.fold
        (fun nv list ->
           opam2cudf opam_universe ?depopts version_map nv :: list)
        opam_packages [] in
    try Cudf.load_universe cudf_packages
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "Malformed CUDF universe (%s)" s
  in
  (* We can trim the universe here to get faster results, but we
     choose to keep it bigger to get more precise conflict messages. *)
  (* let universe = Algo.Depsolver.trim universe in *)
  cudf_universe

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
    List.filter (fun (n,_) -> not (List.mem_assoc n req.wish_upgrade))
      req.wish_install in
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
  log "resolve request=%a" (slog string_of_request) request;
  let version_map =
    cudf_versions_map universe (universe.u_available ++ universe.u_installed) in
  let simple_universe =
    load_cudf_universe universe ~version_map universe.u_available in
  let request = cleanup_request universe request in
  let cudf_request = map_request (atom2cudf universe version_map) request in
  let orphan_packages =
    universe.u_installed -- universe.u_available in
  let orphan_packages =
    OpamPackage.Set.filter (fun n -> not (OpamPackage.is_pinned n))
      orphan_packages in
  let add_orphan_packages u =
    load_cudf_universe universe ~version_map
      (orphan_packages ++
         (OpamPackage.Set.of_list
            (List.map cudf2opam (Cudf.get_packages u)))) in
  let resolve u req =
    if OpamCudf.external_solver_available ()
    then
      try
        let resp = OpamCudf.resolve ~extern:true ~version_map u req in
        OpamCudf.to_actions add_orphan_packages u resp
      with Failure "opamSolver" ->
        OpamGlobals.error_and_exit
          "Please retry with option --use-internal-solver"
    else OpamHeuristic.resolve ~verbose ~version_map add_orphan_packages u req in
  match resolve simple_universe cudf_request with
  | Conflicts c     -> Conflicts (fun () ->
      OpamCudf.string_of_reasons cudf2opam simple_universe universe (c ()))
  | Success actions ->
    let all_packages =
      universe.u_available ++ orphan_packages in
    let simple_universe =
      load_cudf_universe universe ~version_map all_packages in
    let complete_universe =
      load_cudf_universe ~depopts:true universe ~version_map all_packages in
    let cudf_solution =
      OpamCudf.solution_of_actions
        ~simple_universe ~complete_universe ~requested actions in
    Success (solution cudf2opam cudf_solution)

let installable universe =
  log "trim";
  let simple_universe =
    load_cudf_universe universe universe.u_available in
  let trimed_universe = Algo.Depsolver.trim simple_universe in
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimed_universe

let filter_dependencies f_direction ~depopts ~installed universe packages =
  let u_packages =
    if installed then universe.u_installed else universe.u_available in
  let version_map = cudf_versions_map universe u_packages in
  let cudf_universe =
    load_cudf_universe ~depopts universe ~version_map u_packages in
  let cudf_packages =
    List.rev_map (opam2cudf universe ~depopts version_map)
      (OpamPackage.Set.elements packages) in
  let topo_packages = f_direction cudf_universe cudf_packages in
  let result = List.rev_map cudf2opam topo_packages in
  log "filter_dependencies packages=%a result=%a"
    (slog OpamPackage.Set.to_string) packages
    (slog (OpamMisc.string_of_list OpamPackage.to_string)) result;
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
  let col n =
    OpamGlobals.colorise `yellow (string_of_int n)
  in
  let utf = !(OpamGlobals.utf8_msgs) in
  let stats = [
    stats.s_install;
    stats.s_reinstall;
    stats.s_upgrade;
    stats.s_downgrade;
    stats.s_remove;
  ] in
  let titles =
    if utf then
      ["+";"\xe2\x86\xbb";"\xe2\x86\x91";"\xe2\x86\x93";"\xe2\x8a\x98"]
    else
      ["install";"reinstall";"upgrade";"downgrade";"remove"]
  in
  let msgs = List.filter (fun (a,_) -> a <> 0) (List.combine stats titles) in
  if utf then
    String.concat "   " @@
    List.map (fun (n,t) -> Printf.sprintf "%s %s" t (col n)) msgs
  else
    String.concat " | " @@
    List.map (fun (n,t) -> Printf.sprintf "%s to %s" (col n) t) msgs

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

let sequential_solution universe ~requested actions =
  let version_map =
    cudf_versions_map universe (universe.u_available ++ universe.u_installed) in
  let simple_universe =
    load_cudf_universe universe ~version_map universe.u_available in
  let complete_universe =
    load_cudf_universe universe ~version_map ~depopts:true universe.u_available in
  let actions =
    List.map
      (map_action (opam2cudf universe ~depopts:true version_map))
      actions in
  let cudf_solution =
    OpamCudf.solution_of_actions
      ~simple_universe ~complete_universe ~requested
      actions in
  solution cudf2opam cudf_solution
