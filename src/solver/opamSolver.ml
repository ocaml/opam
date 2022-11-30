(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase
open OpamPackage.Set.Op
[@@@ocaml.warning "-33"]
open OpamDoseCompat

let log ?level fmt = OpamConsole.log ?level "SOLVER" fmt
let slog = OpamConsole.slog

module Action = OpamActionGraph.MakeAction(OpamPackage)
module ActionGraph = OpamActionGraph.Make(Action)
type solution = OpamCudf.ActionGraph.t

let empty_universe =
  {
    u_packages = OpamPackage.Set.empty;
    u_installed = OpamPackage.Set.empty;
    u_available = OpamPackage.Set.empty;
    u_depends = OpamPackage.Map.empty;
    u_depopts = OpamPackage.Map.empty;
    u_conflicts = OpamPackage.Map.empty;
    u_action = Install;
    u_installed_roots = OpamPackage.Set.empty;
    u_pinned = OpamPackage.Set.empty;
    u_base = OpamPackage.Set.empty;
    u_invariant = OpamFormula.Empty;
    u_reinstall = OpamPackage.Set.empty;
    u_attrs = [];
  }

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

let solution_to_json solution =
  OpamCudf.ActionGraph.to_json solution
let solution_of_json json =
  OpamCudf.ActionGraph.of_json json

let cudf_versions_map universe packages =
  log ~level:3 "cudf_versions_map";
  let add_referred_to_packages filt acc refmap =
    OpamPackage.Map.fold (fun _ deps acc ->
        List.fold_left (fun acc -> function
            | n, Some (_, v) -> OpamPackage.Set.add (OpamPackage.create n v) acc
            | _, None -> acc)
          acc (OpamFormula.atoms (filt deps)))
      refmap acc
  in
  let filt f =
    OpamFilter.filter_deps ~build:true ~post:true ~default:false f
  in
  let id = fun x -> x in
  let packages = add_referred_to_packages filt packages universe.u_depends in
  let packages = add_referred_to_packages filt packages universe.u_depopts in
  let packages = add_referred_to_packages id packages universe.u_conflicts in
  let pmap = OpamPackage.to_map packages in
  OpamPackage.Name.Map.fold (fun name versions acc ->
      let _, map =
        OpamPackage.Version.Set.fold
          (fun version (i,acc) ->
             let nv = OpamPackage.create name version in
             i + 1, OpamPackage.Map.add nv i acc)
          versions (1,acc) in
      map)
    pmap OpamPackage.Map.empty

let name_to_cudf name =
  Dose_common.CudfAdd.encode (OpamPackage.Name.to_string name)

let constraint_to_cudf version_map name (op,v) =
  let nv = OpamPackage.create name v in
  try
    Some (op, OpamPackage.Map.find nv version_map)
  with Not_found ->
    (* The version for comparison doesn't exist: match to the closest
       existing version according to the direction of the comparison
       (this shouldn't happen for any constraint in the universe, now that we
       compute a full version map, but may still happen for user-provided
       constraints) *)
    log "Warn: fallback constraint for %s"
      (OpamFormula.string_of_atom (name, Some (op,v)));
    let all_versions =
      OpamPackage.Map.filter (fun nv _ -> nv.name = name)
        version_map in
    match op with
    | `Neq -> None (* Always true *)
    | `Eq -> (* Always false *)
      Some (`Gt, OpamPackage.Map.cardinal all_versions)
    | (`Geq | `Gt | `Leq | `Lt) as op ->
      let sign, result_op =  match op with
        | `Geq | `Gt -> (fun x -> x), `Geq
        | `Leq | `Lt -> (fun x -> -x), `Leq in
      let rev_version_map =
        OpamPackage.Map.fold (fun nv cv acc ->
            OpamStd.IntMap.add (sign cv) nv.version acc)
          all_versions OpamStd.IntMap.empty in
      let map =
        OpamStd.IntMap.filter
          (fun _ v1 -> sign (OpamPackage.Version.compare v v1) < 0)
          rev_version_map in
      if OpamStd.IntMap.is_empty map then
        match result_op with
        | `Geq -> Some (`Gt, max 1 (OpamPackage.Map.cardinal all_versions))
        | `Leq -> Some (`Lt, 1)
      else Some (result_op, sign (fst (OpamStd.IntMap.min_binding map)))

let atom2cudf _universe (version_map : int OpamPackage.Map.t) (name,cstr) =
  name_to_cudf name,
  OpamStd.Option.Op.(cstr >>= constraint_to_cudf version_map name)

let opam_invariant_package version_map invariant =
  let depends =
    OpamFormula.to_atom_formula invariant
    |> OpamFormula.map (fun at -> Atom (atom2cudf () version_map at))
    |> OpamFormula.cnf_of_formula
    |> OpamFormula.ands_to_list
    |> List.map (OpamFormula.fold_right (fun acc x -> x::acc) [])
  in {
    Cudf.
    package = OpamCudf.opam_invariant_package_name;
    version = snd OpamCudf.opam_invariant_package;
    depends;
    conflicts = [];
    provides = [];
    installed = true;
    was_installed = true;
    keep = `Keep_version;
    pkg_extra = [
      OpamCudf.s_source, `String "SWITCH_INVARIANT";
      OpamCudf.s_source_number, `String "NULL";
    ];
  }

let lag_function =
  let rec power n x = if n <= 0 then 1 else x * power (n-1) x in
  power OpamSolverConfig.(!r.version_lag_power)

let opam2cudf universe version_map packages =
  let set_to_bool_map set =
    OpamPackage.Set.fold (fun nv -> OpamPackage.Map.add nv true)
      (packages %% set) OpamPackage.Map.empty
  in
  let base_map =
    OpamPackage.Set.fold (fun nv ->
        OpamPackage.Map.add nv
          { Cudf.default_package with
            Cudf.package = name_to_cudf nv.name;
            pkg_extra = [
              OpamCudf.s_source, `String(OpamPackage.name_to_string nv);
              OpamCudf.s_source_number, `String(OpamPackage.version_to_string nv);
            ];
          })
      packages OpamPackage.Map.empty
  in
  let only_packages m =
    OpamPackage.Map.merge
      (fun _ -> function None -> fun _ -> None | Some _ -> fun x -> x)
      base_map m
  in
  let installed_map = set_to_bool_map universe.u_installed in
  let reinstall_map = set_to_bool_map universe.u_reinstall in
  let installed_root_map = set_to_bool_map universe.u_installed_roots in
  let pinned_to_current_version_map = set_to_bool_map universe.u_pinned in
  let avoid_versions =
    OpamStd.Option.default OpamPackage.Set.empty @@
    OpamStd.List.assoc_opt "avoid-version" universe.u_attrs
  in
  let version_lag_map =
    OpamPackage.Name.Map.fold (fun name version_set acc ->
        let nvers, vs =
          OpamPackage.Version.Set.fold (fun v (i,acc) ->
              if OpamPackage.Set.mem (OpamPackage.create name v) avoid_versions
              then i, acc
              else i+1, OpamPackage.Version.Map.add v i acc)
            version_set (0, OpamPackage.Version.Map.empty)
        in
        let nvers, vs =
          (* Place all avoid-versions after normal versions *)
          (* Not strictly necessary, but gives a better fallback in case the
             specific criteria for avoided versions are not set *)
          OpamPackage.Version.Set.fold (fun v (i,acc) ->
              if OpamPackage.Set.mem (OpamPackage.create name v) avoid_versions
              then i+1, OpamPackage.Version.Map.add v i acc
              else i, acc)
            version_set (nvers, vs)
        in
        OpamPackage.Version.Map.fold (fun v i ->
            let lag = lag_function (nvers - i - 1) in
            if lag > 0 then
              OpamPackage.Map.add (OpamPackage.create name v) lag
            else fun acc -> acc)
          vs acc)
      (OpamPackage.to_map packages)
      OpamPackage.Map.empty
  in
  let extras_maps =
    List.map (fun (label, set) ->
        OpamPackage.Set.fold (fun nv ->
            OpamPackage.Map.add nv (label, `Int 1))
          (packages %% set) OpamPackage.Map.empty)
      universe.u_attrs
  in
  let add elts f map =
    OpamPackage.Map.merge (fun nv a b ->
        match a, b with
        | Some cp, None -> Some cp
        | Some cp, Some x -> Some (f nv x cp)
        | None, _ -> None)
      map elts
  in
  let univ0 =
    base_map
    |> add version_map (fun _ version cp -> {cp with Cudf.version})
    |> add installed_map (fun _ installed cp -> {cp with Cudf.installed})
    |> add reinstall_map (fun _ x cp ->
        {cp with Cudf.pkg_extra =
                   (OpamCudf.s_reinstall, `Bool x) :: cp.Cudf.pkg_extra})
    |> add installed_root_map (fun _ x cp ->
        {cp with Cudf.pkg_extra =
                   (OpamCudf.s_installed_root, `Bool x) :: cp.Cudf.pkg_extra})
    |> add pinned_to_current_version_map (fun _ x cp ->
        {cp with Cudf.pkg_extra =
                   (OpamCudf.s_pinned, `Bool x) :: cp.Cudf.pkg_extra})
    |> add version_lag_map (fun _ x cp ->
        {cp with Cudf.pkg_extra =
                   (OpamCudf.s_version_lag, `Int x) :: cp.Cudf.pkg_extra})
    |> List.fold_right (fun m ->
        add m (fun _ x cp -> {cp with Cudf.pkg_extra = x :: cp.Cudf.pkg_extra}))
      extras_maps
  in
  let preresolve_deps f =
    OpamFilter.atomise_extended f |>
    OpamFormula.map
      (fun (name, (filter, cstr)) ->
         let cstr = match cstr with
           | None -> None
           | Some (op, FString v) ->
             let v = OpamPackage.Version.of_string v in
             constraint_to_cudf version_map name (op, v)
           | _ -> assert false
         in
         Atom (name_to_cudf name, (filter, cstr))) |>
    OpamFormula.cnf_of_formula
  in
  let depends_map =
    OpamPackage.Map.map preresolve_deps
      (only_packages universe.u_depends)
  in
  let depopts_map =
    OpamPackage.Map.map preresolve_deps
      (only_packages universe.u_depopts)
  in
  let conflicts_map =
    OpamPackage.Map.mapi
      (fun nv conflicts ->
         (nv.name, None) ::
         (* prevents install of multiple versions of the same pkg *)
         OpamFormula.set_to_disjunction universe.u_packages conflicts)
      (only_packages universe.u_conflicts)
  in
  let conflicts_map_resolved =
    OpamPackage.Map.map (List.rev_map (atom2cudf universe version_map))
      conflicts_map
  in
  fun ~depopts ~build ~post ->
    let all_depends_map =
      if depopts then
        OpamPackage.Map.union (fun d dopts -> OpamFormula.(ands [d; dopts]))
          depends_map depopts_map
      else depends_map
    in
    let depends_map_resolved =
      OpamPackage.Map.map (fun f ->
          f
          |> OpamFormula.map (fun (name, (filter, cstr)) ->
              if OpamFilter.eval_to_bool ~default:false
                  (OpamFilter.deps_var_env ~build ~post) filter
              then Atom (name, cstr)
              else Empty)
          |> OpamFormula.ands_to_list
          |> List.map (OpamFormula.fold_right (fun acc x -> x::acc) []))
        all_depends_map
    in
    univ0
    |> add depends_map_resolved (fun _ depends cp -> {cp with Cudf.depends})
    |> add conflicts_map_resolved (fun _ conflicts cp -> {cp with Cudf.conflicts})
    |> OpamPackage.Map.values

(* load a cudf universe from an opam one *)
let load_cudf_universe
    opam_universe ?version_map ?(add_invariant=false) opam_packages =
  let chrono = OpamConsole.timer () in
  let version_map = match version_map with
    | Some vm -> vm
    | None -> cudf_versions_map opam_universe opam_packages in
  log ~level:3 "Load cudf universe: opam2cudf";
  let univ_gen =
    opam2cudf opam_universe version_map opam_packages
  in
  log ~level:3 "Preload of cudf universe: done in %.3fs" (chrono ());
  fun ?(depopts=false) ~build ~post () ->
  log "Load cudf universe (depopts:%a, build:%b, post:%b)"
    (slog string_of_bool) depopts
    build
    post;
  let chrono = OpamConsole.timer () in
  let cudf_universe =
    let cudf_packages = univ_gen ~depopts ~build ~post in
    let cudf_packages =
      if add_invariant then
        opam_invariant_package version_map opam_universe.u_invariant
        :: cudf_packages
      else cudf_packages
    in
    log ~level:3 "opam2cudf: done in %.3fs" (chrono ());
    try Cudf.load_universe cudf_packages
    with Cudf.Constraint_violation s ->
      OpamConsole.error_and_exit `Solver_failure "Malformed CUDF universe (%s)" s
  in
  log ~level:3 "Secondary load of cudf universe: done in %.3fs" (chrono ());
  (* let universe = Dose_algo.Depsolver.trim universe in *)
  cudf_universe

let string_of_request r =
  let to_string = OpamFormula.string_of_conjunction OpamFormula.string_of_atom in
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (to_string r.wish_install)
    (to_string r.wish_remove)
    (to_string r.wish_upgrade)

(* Unused?
let map_cause f = function
  | Upstream_changes -> Upstream_changes
  | Use l            -> Use (List.rev_map f l)
  | Required_by l    -> Required_by (List.rev_map f l)
  | Conflicts_with l -> Conflicts_with (List.rev_map f l)
  | Requested        -> Requested
  | Unknown          -> Unknown
*)

let cudf_to_opam_graph cudf2opam cudf_graph =
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

let map_request f r =
  let f = List.rev_map f in
  { wish_install = f r.wish_install;
    wish_remove  = f r.wish_remove;
    wish_upgrade = f r.wish_upgrade;
    criteria = r.criteria;
    extra_attributes = r.extra_attributes; }

(* Remove duplicate packages *)
(* Add upgrade constraints *)
(* Remove constraints in best_effort mode *)
let cleanup_request universe (req:atom request) =
  if OpamSolverConfig.best_effort () then
    { req with wish_install = []; wish_upgrade = []; }
  else
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

let cycle_conflict ~version_map univ cycles =
  OpamCudf.cycle_conflict ~version_map univ
    (List.map
       (List.map
          (fun a ->
             Action.to_string (map_action OpamCudf.cudf2opam a)))
       cycles)

let resolve universe ~orphans request =
  log "resolve request=%a" (slog string_of_request) request;
  let all_packages = universe.u_available ++ universe.u_installed ++ orphans in
  let version_map = cudf_versions_map universe all_packages in
  let univ_gen = load_cudf_universe universe ~version_map all_packages in
  let simple_universe, cudf_orphans =
    let u = univ_gen ~build:true ~post:true () in
    let cudf_orphans =
      OpamPackage.Set.fold (fun nv acc ->
          let cnv = name_to_cudf nv.name, OpamPackage.Map.find nv version_map in
          let cp = Cudf.lookup_package u cnv in
          Cudf.remove_package u cnv;
          cp :: acc)
        orphans []
    in
    u, cudf_orphans
  in
  let add_orphan_packages u =
    Cudf.load_universe (List.rev_append cudf_orphans (Cudf.get_packages u))
  in
  let request =
    let extra_attributes =
      OpamStd.List.sort_nodup compare
        (List.map fst universe.u_attrs @ request.extra_attributes)
    in
    { request with extra_attributes }
  in
  let request = cleanup_request universe request in
  let cudf_request = map_request (atom2cudf universe version_map) request in
  let resolve u req =
    try
      let invariant_pkg =
        opam_invariant_package version_map universe.u_invariant
      in
      Cudf.add_package u invariant_pkg;
      let resp = OpamCudf.resolve ~extern:true ~version_map u req in
      Cudf.remove_package u
        (invariant_pkg.Cudf.package, invariant_pkg.Cudf.version);
      OpamCudf.to_actions add_orphan_packages u resp
    with OpamCudf.Solver_failure msg ->
      OpamConsole.error_and_exit `Solver_failure "%s" msg
  in
  match resolve simple_universe cudf_request with
  | Conflicts _ as c -> c
  | Success actions ->
    let simple_universe = univ_gen ~depopts:true ~build:false ~post:false () in
    let complete_universe = univ_gen ~depopts:true ~build:true ~post:false () in
    try
      let atomic_actions =
        OpamCudf.atomic_actions
          ~simple_universe ~complete_universe actions in
      Success atomic_actions
    with OpamCudf.Cyclic_actions cycles ->
      cycle_conflict ~version_map complete_universe cycles

let get_atomic_action_graph t =
  cudf_to_opam_graph OpamCudf.cudf2opam t

let installable universe =
  log "trim";
  let simple_universe =
    load_cudf_universe universe ~add_invariant:true
      universe.u_available ~build:true ~post:true ()
  in
  let trimmed_universe =
    (* Dose_algo.Depsolver.trim simple_universe => this can explode memory, we need
       to specify [~explain:false] *)
    let open Dose_algo in
    let open Depsolver in
    let trimmed_pkgs = ref [] in
    let callback d =
      if Dose_algo.Diagnostic.is_solution d then
        match d.Diagnostic.request with
        |[p] -> trimmed_pkgs := p::!trimmed_pkgs
        |_ -> assert false
    in
    ignore (univcheck ~callback ~explain:false simple_universe);
    Cudf.load_universe !trimmed_pkgs
  in
  Cudf.fold_packages
    (fun universe pkg ->
       if pkg.package = OpamCudf.opam_invariant_package_name then universe
       else OpamPackage.Set.add (OpamCudf.cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimmed_universe

let installable_subset universe packages =
  log "trim-subset";
  let version_map = cudf_versions_map universe universe.u_available in
  let simple_universe =
    load_cudf_universe ~build:true ~post:true universe
      ~version_map ~add_invariant:true
      universe.u_available ()
  in
  let cudf_packages =
    Cudf.get_packages
      ~filter:(fun p ->
          p.package <> OpamCudf.opam_invariant_package_name &&
          OpamPackage.Set.mem (OpamCudf.cudf2opam p) packages)
      simple_universe
  in
  let trimmed_universe =
    (* Dose_algo.Depsolver.trimlist simple_universe with [~explain:false] *)
    let open Dose_algo in
    let open Depsolver in
    let trimmed_pkgs = ref [] in
    let callback d =
      if Dose_algo.Diagnostic.is_solution d then
        match d.Diagnostic.request with
        |[p] -> trimmed_pkgs := p::!trimmed_pkgs
        |_ -> assert false
    in
    ignore (listcheck ~callback ~explain:false simple_universe cudf_packages);
    Cudf.load_universe !trimmed_pkgs
  in
  Cudf.remove_package trimmed_universe OpamCudf.opam_invariant_package;
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (OpamCudf.cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimmed_universe

let coinstallable_subset universe set packages =
  let u_invariant =
    OpamPackage.Set.fold (fun p acc ->
        OpamFormula.ands [acc; Atom (p.name, Atom (`Eq, p.version))])
      set OpamFormula.Empty
  in
  installable_subset {universe with u_invariant} packages

module PkgGraph = Graph.Imperative.Digraph.ConcreteBidirectional(OpamPackage)

let dependency_graph
    ~depopts ~build ~post ~installed ?(unavailable=false)
    universe =
  let u_packages =
    if installed then universe.u_installed else
    if unavailable then universe.u_packages else
      universe.u_available in
  let cudf_graph =
    load_cudf_universe ~depopts ~build ~post universe u_packages () |>
    OpamCudf.Graph.of_universe
  in
  let g = PkgGraph.create ~size:(OpamCudf.Graph.nb_vertex cudf_graph) () in
  OpamCudf.Graph.iter_vertex (fun v ->
      PkgGraph.add_vertex g (OpamCudf.cudf2opam v))
    cudf_graph;
  OpamCudf.Graph.iter_edges (fun v1 v2 ->
      PkgGraph.add_edge g (OpamCudf.cudf2opam v1) (OpamCudf.cudf2opam v2))
    cudf_graph;
  g

let filter_dependencies
    f_direction ~depopts ~build ~post ~installed
    ?(unavailable=false) universe packages =
  if OpamPackage.Set.is_empty packages then OpamPackage.Set.empty else
  let u_packages =
    packages ++
    if installed then universe.u_installed else
    if unavailable then universe.u_packages else
      universe.u_available in
  log ~level:3 "filter_dependencies packages=%a"
    (slog OpamPackage.Set.to_string) packages;
  let version_map = cudf_versions_map universe u_packages in
  let cudf_universe =
    load_cudf_universe ~depopts ~build ~post universe ~version_map
      u_packages () in
  let cudf_packages =
    OpamCudf.Set.of_list
      (opam2cudf universe ~depopts ~build ~post version_map packages)
  in
  log ~level:3 "filter_dependencies: dependency";
  let clos_packages = f_direction cudf_universe cudf_packages in
  let result =
    OpamCudf.Set.fold (fun cp -> OpamPackage.Set.add (OpamCudf.cudf2opam cp))
      clos_packages OpamPackage.Set.empty
  in
  log "filter_dependencies result=%a"
    (slog OpamPackage.Set.to_string) result;
  result

let dependencies = filter_dependencies OpamCudf.dependencies

let reverse_dependencies = filter_dependencies OpamCudf.reverse_dependencies

let dependency_sort ~depopts ~build ~post universe packages =
  let version_map = cudf_versions_map universe universe.u_packages in
  let cudf_universe =
    load_cudf_universe ~depopts ~build ~post universe ~version_map
      universe.u_packages () in
  let cudf_packages =
    OpamCudf.Set.of_list
      (opam2cudf universe ~depopts ~build ~post version_map packages)
  in
  List.map OpamCudf.cudf2opam
    (OpamCudf.dependency_sort cudf_universe cudf_packages)

let coinstallability_check universe packages =
  let version_map = cudf_versions_map universe universe.u_packages in
  let cudf_universe =
    load_cudf_universe ~build:true ~post:true ~version_map ~add_invariant:true
      universe universe.u_packages ()
  in
  let cudf_packages =
    opam2cudf universe ~depopts:false ~build:true ~post:true
      version_map packages
  in
  match Dose_algo.Depsolver.edos_coinstall cudf_universe cudf_packages with
  | { Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Success _; _ } ->
    None
  | { Dose_algo.Diagnostic.result = Dose_algo.Diagnostic.Failure _; _ } as c ->
    match OpamCudf.make_conflicts ~version_map cudf_universe c with
    | Conflicts cs -> Some cs
    | _ -> None

let check_for_conflicts universe =
  coinstallability_check universe universe.u_installed

let atom_coinstallability_check universe atoms =
  let version_map = cudf_versions_map universe universe.u_packages in
  let check_pkg = {
    Cudf.default_package with
    package = "=check_coinstallability";
    depends = List.map (fun at -> [atom2cudf () version_map at]) atoms;
  } in
  let cudf_universe =
    Cudf.load_universe
      (check_pkg ::
       opam_invariant_package version_map universe.u_invariant ::
       opam2cudf universe version_map universe.u_available
         ~depopts:false ~build:true ~post:true)
  in
  Dose_algo.Depsolver.edos_install cudf_universe check_pkg
  |> Dose_algo.Diagnostic.is_solution

let new_packages sol =
  OpamCudf.ActionGraph.fold_vertex (fun action packages ->
      match action with
      | `Install p | `Change (_,_,p) ->
        OpamPackage.Set.add (OpamCudf.cudf2opam p) packages
      | `Reinstall _ | `Remove _ | `Build _ | `Fetch _ -> packages
  ) sol OpamPackage.Set.empty

let all_packages sol =
  OpamCudf.ActionGraph.fold_vertex (fun action packages ->
      List.fold_left
        (fun packages p -> OpamPackage.Set.add (OpamCudf.cudf2opam p) packages)
        packages (full_action_contents action))
    sol OpamPackage.Set.empty

let stats sol =
  OpamCudf.ActionGraph.fold_vertex (fun action stats ->
      match action with
      | `Install _ -> {stats with s_install = stats.s_install+1}
      | `Change (`Up,_,_) -> {stats with s_upgrade = stats.s_upgrade+1}
      | `Change (`Down,_,_) -> {stats with s_downgrade = stats.s_downgrade+1}
      | `Reinstall _ -> {stats with s_reinstall = stats.s_reinstall+1}
      | `Remove _ -> {stats with s_remove = stats.s_remove+1}
      | `Build _ | `Fetch _ -> stats)
    (OpamCudf.ActionGraph.reduce sol)
    { s_install=0; s_reinstall=0; s_upgrade=0; s_downgrade=0; s_remove=0 }

let string_of_stats stats =
  let utf = (OpamConsole.utf8 ()) in
  let stats = [
    stats.s_install;
    stats.s_reinstall;
    stats.s_upgrade;
    stats.s_downgrade;
    stats.s_remove;
  ] in
  let titles =
    List.map
      (fun a ->
         let s = OpamActionGraph.action_strings a in
         if utf then OpamActionGraph.action_color a s else s)
      [`Install ();
       `Reinstall ();
       `Change (`Up,(),());
       `Change (`Down,(),());
       `Remove ()]
  in
  let msgs = List.filter (fun (a,_) -> a <> 0) (List.combine stats titles) in
  if utf then
    OpamStd.List.concat_map "   "
      (fun (n,t) -> Printf.sprintf "%s %s" t (string_of_int n))
      msgs
  else
    OpamStd.List.concat_map " | "
      (fun (n,t) ->
        Printf.sprintf "%s to %s"
          (OpamConsole.colorise `yellow (string_of_int n)) t)
      msgs

let solution_is_empty t =
  OpamCudf.ActionGraph.is_empty t

let print_solution ~messages ~append ~requested ~reinstall t =
  let dump_cudf sfx t = match OpamSolverConfig.(!r.cudf_file) with
    | None -> ()
    | Some f ->
      let filename = Printf.sprintf "%s-actions%s.dot" f sfx in
      let oc = open_out filename in
      ActionGraph.Dot.output_graph oc (cudf_to_opam_graph OpamCudf.cudf2opam t);
      close_out oc
  in
  dump_cudf "-full" t;
  let t = OpamCudf.ActionGraph.reduce t in
  dump_cudf "" t;
  let causes =
    OpamCudf.compute_root_causes t requested reinstall
  in
  let actions, details =
    OpamCudf.ActionGraph.Topological.fold (fun a (actions,details) ->
        let cause =
          try OpamCudf.Map.find (action_contents a) causes
          with Not_found -> Unknown in
        let action = map_action OpamCudf.cudf2opam a in
        let cudf_name p = OpamPackage.name_to_string (OpamCudf.cudf2opam p) in
        let cause = string_of_cause cudf_name cause in
        let messages =
          match a with
          | `Install p | `Change (_,_,p) | `Reinstall p ->
            messages (OpamCudf.cudf2opam p)
          | `Remove _ | `Build _ | `Fetch _ -> []
        in
        action :: actions, (cause, messages) :: details
      ) t ([],[])
  in
  let actions, details = List.rev actions, List.rev details in
  Action.to_aligned_strings ~append actions |>
  List.map2 (fun (cause, messages) line ->
      " " :: line @
      [if cause = "" then "" else Printf.sprintf "[%s]" cause] @
      if messages = [] then []
      else [String.concat "\n" messages]
    ) details |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table ~sep:" " stdout

let dump_universe universe oc =
  let version_map = cudf_versions_map universe universe.u_packages in
  let cudf_univ =
    load_cudf_universe ~depopts:false ~build:true ~post:true ~version_map
      universe universe.u_available () in
  OpamCudf.dump_universe oc cudf_univ;
  (* Add explicit bindings to retrieve original versions of non-available and
     non-existing (but referred to) packages *)
  OpamPackage.Map.iter (fun nv i ->
      if not (OpamPackage.Set.mem nv universe.u_available) then
        Printf.fprintf oc "#v2v:%s:%d=%s\n"
          (OpamPackage.name_to_string nv) i (OpamPackage.version_to_string nv)
    ) version_map

let filter_solution filter t =
  let t = OpamCudf.ActionGraph.copy t in
  let rec rm iter_deps v =
    if OpamCudf.ActionGraph.mem_vertex t v then (
      iter_deps (rm iter_deps) t v;
      OpamCudf.ActionGraph.remove_vertex t v
    ) in
  OpamCudf.ActionGraph.iter_vertex
    (function
      | `Remove nv as a when not (filter (OpamCudf.cudf2opam nv)) ->
        rm OpamCudf.ActionGraph.iter_pred a
      | (`Install nv | `Change (_,_,nv)) as a
        when not (filter (OpamCudf.cudf2opam nv)) ->
        rm OpamCudf.ActionGraph.iter_succ a
      | _ -> ())
    t;
  t

let request ?(criteria=`Default) ?(install=[]) ?(upgrade=[]) ?(remove=[]) () =
  { wish_install = install; wish_upgrade = upgrade; wish_remove = remove;
    criteria; extra_attributes = []; }
