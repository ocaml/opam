(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
    u_reinstall = OpamPackage.Set.empty;
    u_attrs = [];
  }

(* Get the optional dependencies of a package *)
let depopts_of_package universe ~build package =
  let opts =
    try
      OpamFilter.filter_deps ~build ~post:false ~default:false
        (OpamPackage.Map.find package universe.u_depopts)
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
  Common.CudfAdd.encode (OpamPackage.Name.to_string name)

let atom2cudf _universe (version_map : int OpamPackage.Map.t) (name,cstr) =
  name_to_cudf name, match cstr with
  | None -> None
  | Some (op,v) ->
    let nv = OpamPackage.create name v in
    try
      let cv = OpamPackage.Map.find nv version_map in
      Some (op, cv)
    with Not_found ->
      (* The version for comparison doesn't exist: match to the closest
         existing version according to the direction of the comparison
         (this shouldn't happen for any constraint in the universe, now that we
         compute a full version map, but may still happen for user-provided
         constraints) *)
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

let opam2cudf universe ?(depopts=false) ~build ~post version_map package =
  let name = OpamPackage.name package in
  let version = OpamPackage.version package in
  let depends =
    try
      OpamFilter.filter_deps ~build ~post ~default:false
        (OpamPackage.Map.find package universe.u_depends)
    with Not_found -> Empty in
(*
  let base_depends =
    if OpamPackage.has_name universe.u_base name then Empty else
      OpamFormula.ands
        (OpamPackage.Name.Set.fold (fun name acc -> Atom (name, Empty) :: acc)
           (OpamPackage.names_of_packages universe.u_base) [])
  in
  let depends = OpamFormula.ands [base_depends; depends] in
*)
  let depends =
    let opts = depopts_of_package ~build universe package in
    if depopts then
      let opts = List.rev_map OpamFormula.of_conjunction opts in
      And (depends, Or(depends, OpamFormula.ors opts))
    else depends
  in
  let conflicts =
    try OpamPackage.Map.find package universe.u_conflicts
    with Not_found -> Empty in
  let conflicts =
    (name, None) :: (* prevents install of multiple versions of the same pkg *)
    OpamFormula.set_to_disjunction universe.u_packages conflicts in
  let installed = OpamPackage.Set.mem package universe.u_installed in
  let keep =
    if OpamPackage.Set.mem package universe.u_base then
      if OpamPackage.Set.mem package universe.u_available
      then `Keep_version
      else if OpamPackage.has_name universe.u_available package.name
      then `Keep_package
      else `Keep_none
    else `Keep_none
  in
  let reinstall = OpamPackage.Set.mem package universe.u_reinstall in
  let installed_root = OpamPackage.Set.mem package universe.u_installed_roots in
  let pinned_to_current_version =
    OpamPackage.Set.mem package universe.u_pinned in
  let version_lag =
    let all_versions = OpamPackage.versions_of_name universe.u_available name in
    let count,i =
      OpamPackage.Version.Set.fold
        (fun v (i,r) -> if v = version then i,i else i+1, r)
        all_versions (0,0)
    in
    count - i
  in
  let extras =
    let e = [
      OpamCudf.s_source, `String (OpamPackage.Name.to_string name);
      OpamCudf.s_source_number, `String (OpamPackage.Version.to_string version);
    ] in
    let e = if installed && reinstall
      then (OpamCudf.s_reinstall, `Bool true)::e else e in
    let e = if installed_root
      then (OpamCudf.s_installed_root, `Bool true)::e else e in
    let e =
      if pinned_to_current_version then (OpamCudf.s_pinned, `Bool true)::e
      else if version_lag = 0 then e
      else (OpamCudf.s_version_lag, `Int version_lag)::e
    in
    e
  in
  let extras =
    List.fold_left (fun extras (label,set) ->
        if OpamPackage.Set.mem package set then (label, `Int 1)::extras
        else extras)
      extras universe.u_attrs
  in
  { Cudf.default_package with
    Cudf.
    package = name_to_cudf (OpamPackage.name package);
    version = OpamPackage.Map.find package version_map;
    depends = List.rev_map (List.rev_map (atom2cudf universe version_map))
        (OpamFormula.to_cnf depends);
    conflicts = List.rev_map (atom2cudf universe version_map) conflicts;
    installed;
    keep;
    (* was_installed: reserved for the solver; *)
    (* provides: unused atm *)
    pkg_extra = extras;
  }

(* load a cudf universe from an opam one *)
let load_cudf_universe ?depopts ~build ~post
    opam_universe ?version_map opam_packages =
  log "Load cudf universe (depopts:%a, build:%b, post:%b)"
    (slog @@ OpamStd.Option.to_string ~none:"false" string_of_bool) depopts
    build
    post;
  let chrono = OpamConsole.timer () in
  let version_map = match version_map with
    | Some vm -> vm
    | None -> cudf_versions_map opam_universe opam_packages in
  log ~level:3 "Load cudf universe: opam2cudf";
  let opam_packages =
    if OpamPackage.Set.is_empty
        (opam_universe.u_base -- opam_universe.u_available)
    then
      (* Filter out extra compiler versions, they add too much cost to the
         solver and are not needed *)
      opam_packages --
      (OpamPackage.packages_of_names opam_packages
         OpamPackage.Name.Set.Op.(
           OpamPackage.names_of_packages opam_universe.u_base
           -- OpamPackage.names_of_packages opam_universe.u_pinned)
       -- opam_universe.u_base)
    else opam_packages
  in
  let cudf_universe =
    let cudf_packages =
      (* TODO:
         Doing opam2cudf for every package is inefficient (lots of Set.mem to
         check if it is installed, etc. Optimise by gathering all info first *)
      OpamPackage.Set.fold
        (fun nv list ->
           opam2cudf opam_universe ?depopts ~build ~post version_map nv
           :: list)
        opam_packages [] in
    try Cudf.load_universe cudf_packages
    with Cudf.Constraint_violation s ->
      OpamConsole.error_and_exit `Solver_failure "Malformed CUDF universe (%s)" s
  in
  log ~level:3 "Load cudf universe: done in %.3fs" (chrono ());
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

(* Unused ?
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
  let version_map =
    cudf_versions_map universe
      (universe.u_available ++ universe.u_installed ++ orphans) in
  let simple_universe =
    load_cudf_universe universe ~version_map ~build:true ~post:true
      (universe.u_available ++ universe.u_installed -- orphans) in
  let request =
    let extra_attributes =
      OpamStd.List.sort_nodup compare
        (List.map fst universe.u_attrs @ request.extra_attributes)
    in
    { request with extra_attributes }
  in
  let request = cleanup_request universe request in
  let cudf_request = map_request (atom2cudf universe version_map) request in
  let add_orphan_packages u =
    load_cudf_universe universe ~version_map ~build:true ~post:true
      (orphans ++
         (OpamPackage.Set.of_list
            (List.map OpamCudf.cudf2opam (Cudf.get_packages u)))) in
  let resolve u req =
    try
      let resp = OpamCudf.resolve ~extern:true ~version_map u req in
      OpamCudf.to_actions add_orphan_packages u resp
    with OpamCudf.Solver_failure msg ->
      OpamConsole.error_and_exit `Solver_failure "%s" msg
  in
  match resolve simple_universe cudf_request with
  | Conflicts _ as c -> c
  | Success actions ->
    let all_packages =
      universe.u_available ++ orphans in
    let simple_universe =
      load_cudf_universe universe ~depopts:true ~build:false ~post:true
        ~version_map all_packages in
    let complete_universe =
      load_cudf_universe universe ~depopts:true ~build:true ~post:false
        ~version_map all_packages in
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
    load_cudf_universe universe universe.u_available ~build:true ~post:true
  in
  let trimmed_universe =
    (* Algo.Depsolver.trim simple_universe => this can explode memory, we need
       to specify [~explain:false] *)
    let open Algo in
    let open Depsolver in
    let trimmed_pkgs = ref [] in
    let callback d =
      if Algo.Diagnostic.is_solution d then
        match d.Diagnostic.request with
        |[p] -> trimmed_pkgs := p::!trimmed_pkgs
        |_ -> assert false
    in
    ignore (univcheck ~callback ~explain:false simple_universe);
    Cudf.load_universe !trimmed_pkgs
  in
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (OpamCudf.cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimmed_universe

let installable_subset universe packages =
  log "trim-subset";
  let version_map = cudf_versions_map universe universe.u_available in
  let simple_universe =
    load_cudf_universe ~build:true ~post:true universe ~version_map
      universe.u_available
  in
  let cudf_packages =
    List.map (opam2cudf universe ~build:true ~post:true version_map)
      (OpamPackage.Set.elements packages)
  in
  let trimmed_universe =
    (* Algo.Depsolver.trimlist simple_universe with [~explain:false] *)
    let open Algo in
    let open Depsolver in
    let trimmed_pkgs = ref [] in
    let callback d =
      if Algo.Diagnostic.is_solution d then
        match d.Diagnostic.request with
        |[p] -> trimmed_pkgs := p::!trimmed_pkgs
        |_ -> assert false
    in
    ignore (listcheck ~callback ~explain:false simple_universe cudf_packages);
    Cudf.load_universe !trimmed_pkgs
  in
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (OpamCudf.cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimmed_universe

let filter_dependencies
    f_direction ~depopts ~build ~post ~installed
    ?(unavailable=false) universe packages =
  if OpamPackage.Set.is_empty packages then [] else
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
      u_packages in
  let cudf_packages =
    List.rev_map (opam2cudf universe ~depopts ~build ~post version_map)
      (OpamPackage.Set.elements packages) in
  log ~level:3 "filter_dependencies: dependency";
  let topo_packages = f_direction cudf_universe cudf_packages in
  let result = List.rev_map OpamCudf.cudf2opam topo_packages in
  log "filter_dependencies result=%a"
    (slog (OpamStd.List.to_string OpamPackage.to_string)) result;
  result

let dependencies = filter_dependencies OpamCudf.dependencies

let reverse_dependencies = filter_dependencies OpamCudf.reverse_dependencies

let coinstallability_check universe packages =
  let version_map = cudf_versions_map universe universe.u_packages in
  let cudf_universe =
    load_cudf_universe ~build:true ~post:true ~version_map
      universe universe.u_packages
  in
  let cudf_packages =
    List.map (opam2cudf universe ~build:true ~post:true version_map)
      (OpamPackage.Set.elements packages)
  in
  match Algo.Depsolver.edos_coinstall cudf_universe cudf_packages with
  | { Algo.Diagnostic.result = Algo.Diagnostic.Success _; _ } ->
    None
  | { Algo.Diagnostic.result = Algo.Diagnostic.Failure _; _ } as c ->
    match OpamCudf.make_conflicts ~version_map cudf_universe c with
    | Conflicts cs -> Some cs
    | _ -> None

let check_for_conflicts universe =
  coinstallability_check universe universe.u_installed

let atom_coinstallability_check universe atoms =
  let packages = OpamFormula.packages_of_atoms universe.u_available atoms in
  let map = OpamPackage.to_map packages in
  List.for_all (fun (n, _) -> OpamPackage.Name.Map.mem n map) atoms &&
  let ll =
    List.map (fun (n, versions) ->
        List.map (fun v -> OpamPackage.create n v)
          (OpamPackage.Version.Set.elements versions))
      (OpamPackage.Name.Map.bindings map)
  in
  let version_map = cudf_versions_map universe universe.u_packages in
  let cudf_universe =
    load_cudf_universe ~build:true ~post:true ~version_map
      universe universe.u_packages
  in
  let cudf_ll =
    List.map
      (List.map (opam2cudf universe ~build:true ~post:true version_map))
      ll
  in
  let result = Algo.Depsolver.edos_coinstall_prod cudf_universe cudf_ll in
  List.exists Algo.Diagnostic.is_solution result

let new_packages sol =
  OpamCudf.ActionGraph.fold_vertex (fun action packages ->
      match action with
      | `Install p | `Change (_,_,p) ->
        OpamPackage.Set.add (OpamCudf.cudf2opam p) packages
      | `Reinstall _ | `Remove _ | `Build _ -> packages
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
      | `Build _ -> stats)
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
          | `Remove _ | `Build _ -> []
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
  OpamStd.Format.print_table ~sep:" " stdout

let dump_universe universe oc =
  let version_map = cudf_versions_map universe universe.u_packages in
  let cudf_univ =
    load_cudf_universe ~depopts:false ~build:true ~post:true ~version_map
      universe universe.u_available in
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
