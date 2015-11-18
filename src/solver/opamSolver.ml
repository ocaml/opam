(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "SOLVER" fmt
let slog = OpamConsole.slog

module Action = OpamActionGraph.MakeAction(OpamPackage)
module ActionGraph = OpamActionGraph.Make(Action)
type solution = OpamCudf.ActionGraph.t

let empty_universe =
  {
    u_pefuniv = Hashtbl.create 0;
    u_options = ("",[],[]);
    u_tables = Pef.Pefcudf.create 0;
    u_packages = OpamPackage.Set.empty;
    u_installed = OpamPackage.Set.empty;
    u_available = OpamPackage.Set.empty;
    u_action = Install (OpamPackage.Name.Set.empty,OpamSwitch.Set.empty);
    u_installed_roots = OpamPackage.Set.empty;
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

let name_to_cudf switch name =
  Common.CudfAdd.encode ((OpamPackage.Name.to_string name)^":"^switch)

(* The version for comparison doesn't exist: match to the closest
   existing version according to the direction of the comparison
   (this shouldn't happen for any constraint in the universe, now that we
   compute a full version map, but may still happen for user-provided
   constraints) *)
let atom2cudf universe version_map (name,cstr) =
  let tables = universe.u_tables in
  let (switch,_,_) = universe.u_options in
  let cudfname = name_to_cudf switch name in
  let cudfconstr =
    match cstr with
    | None -> None
    | Some (op,v) ->
      try
        let nv = (OpamPackage.Name.to_string name,OpamPackage.Version.to_string v) in
        let cv = Pef.Pefcudf.get_cudf_version tables nv in
        Some (op, cv)
      with Not_found ->
        let all_versions =
          try
            let l = Hashtbl.find version_map.Pef.Pefcudf.versions_table (OpamPackage.Name.to_string name) in
            List.fold_left (fun acc v ->
              let nv = OpamPackage.create name (OpamPackage.Version.of_string v) in
              OpamPackage.Map.add nv (Pef.Pefcudf.get_cudf_version tables ("",v)) acc
            ) OpamPackage.Map.empty l
          with Not_found -> OpamPackage.Map.empty
        in
        match op with
        | `Neq -> None (* Always true *)
        | `Eq -> (* Always false *)
          Some (`Eq, OpamPackage.Map.cardinal all_versions + 1)
        | (`Geq | `Gt | `Leq | `Lt) as op ->
          let sign, result_op =  match op with
            | `Geq | `Gt -> (fun x -> x), `Geq
            | `Leq | `Lt -> (fun x -> -x), `Leq in
          let rev_version_map =
            OpamPackage.Map.fold (fun nv cv acc ->
                OpamStd.IntMap.add (sign cv) (OpamPackage.version nv) acc)
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
  in
  (cudfname,cudfconstr)

let pefcudf_aux (switch,switches,profiles,depopts,installed) tables pefpkglist =
  let options = { Opam.Opamcudf.switch = switch; switches; profiles; depopts} in
  let extras = [ ("reinstall",("reinstall", `Bool (Some false))); ] in
  let cudfpkglist =
    List.map (fun pkg ->
      if (installed && (List.mem switch pkg#installedlist)) || not installed then
        let pl = Opam.Opamcudf.tocudf tables ~options ~extras pkg in
        List.fold_left (fun acc1 p -> p :: acc1) [] pl
      else []
    ) pefpkglist
  in
  List.flatten cudfpkglist

let pefcudflist universe ?(depopts=false) ~build opam_packages =
  let options = 
    let (switch,switches,profiles) = universe.u_options in
    log "Pef -> Cudflist (depopts:%b, build:%b, switch:%s, switches:%s)" 
    depopts build switch (String.concat "," switches);
    let l = ref profiles in
    if build then l := "build"::!l;
    (switch,switches,!l,depopts,false)
  in
  let pefuniv = universe.u_pefuniv in
  let tables = universe.u_tables in
  let reinstall = match universe.u_action with
    | Upgrade (reinstall , _ )
    | Reinstall (reinstall , _ ) -> reinstall
    | _ -> OpamPackage.Set.empty
  in
  OpamPackage.Set.iter (fun p ->
    let name = OpamPackage.name p in
    let version = OpamPackage.version p in
    let n = OpamPackage.Name.to_string name in
    let v = OpamPackage.Version.to_string version in
    try
      let pkg = Hashtbl.find pefuniv (n,v) in
      Hashtbl.replace pefuniv (n,v) (pkg#add_extra "reinstall" "true")
    with Not_found -> ()
  ) reinstall;
  let pefpkglist =
    OpamPackage.Set.fold (fun p acc -> 
      let name = OpamPackage.name p in
      let version = OpamPackage.version p in
      let n = OpamPackage.Name.to_string name in
      let v = OpamPackage.Version.to_string version in
      try (Hashtbl.find pefuniv (n,v))::acc
      with Not_found -> acc
    ) opam_packages []
  in
  pefcudf_aux options tables pefpkglist

let load_cudf_universe ?depopts ~build universe opam_packages =
  Cudf.load_universe (pefcudflist universe ?depopts ~build opam_packages)

let string_of_request r =
  let to_string = OpamFormula.string_of_conjunction OpamFormula.string_of_atom in
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (to_string r.wish_install)
    (to_string r.wish_remove)
    (to_string r.wish_upgrade)

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

let cycle_conflict ~version_map univ cycles =
  OpamCudf.cycle_conflict ~version_map univ
    (List.map
       (List.map
          (fun a ->
             Action.to_string (map_action OpamCudf.cudf2opam a)))
       cycles)

let resolve ?(verbose=true) universe ~orphans request =
  log "resolve request=%a" (slog string_of_request) request;
  let version_map = universe.u_tables in
  let simple_universe =
    load_cudf_universe universe ~build:true
      (universe.u_packages ++ universe.u_installed -- orphans) in
  let request = cleanup_request universe request in
  let cudf_request = map_request (atom2cudf universe version_map) request in
  let add_orphan_packages u =
    load_cudf_universe universe ~build:true
      (orphans ++
         (OpamPackage.Set.of_list
            (List.map OpamCudf.cudf2opam (Cudf.get_packages u)))) in
  let version_map = universe.u_tables in
  let resolve u req =
    if OpamCudf.external_solver_available ()
    then
      try
        let resp = OpamCudf.resolve ~extern:true ~version_map u req in
        OpamCudf.to_actions add_orphan_packages u resp
      with Failure "opamSolver" ->
        OpamConsole.error_and_exit
          "External solver failure, please fix your installation and check \
           $OPAMROOT/config and variable $OPAMEXTERNALSOLVER.\n\
           You may also retry with option --use-internal-solver"
    else OpamHeuristic.resolve ~verbose ~version_map add_orphan_packages u req in
  match resolve simple_universe cudf_request with
  | Conflicts _ as c -> c
  | Success actions ->
    let all_packages = universe.u_packages ++ orphans in
    let simple_universe =
      load_cudf_universe universe ~depopts:true ~build:false all_packages in
    let complete_universe =
      load_cudf_universe universe ~depopts:true ~build:true all_packages in
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
    load_cudf_universe universe universe.u_packages ~build:true in
  let trimed_universe = Algo.Depsolver.trim simple_universe in
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (OpamCudf.cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimed_universe

let filter_dependencies
    f_direction ~depopts ~build ~installed
    ?(unavailable=false) universe packages =
  if OpamPackage.Set.is_empty packages then [] else
    let version_map = universe.u_tables in
    let u_packages =
      packages ++
      if installed then universe.u_installed else
      if unavailable then universe.u_packages else
        universe.u_packages in
    let cudf_universe =
      load_cudf_universe ~depopts ~build universe u_packages in
    let cudf_packages =
      Cudf.get_packages (load_cudf_universe ~depopts ~build universe packages) in
    let topo_packages = f_direction cudf_universe cudf_packages in
    let result = List.rev_map OpamCudf.cudf2opam topo_packages in
    log "filter_dependencies packages=%a result=%a"
      (slog OpamPackage.Set.to_string) packages
      (slog (OpamStd.List.to_string OpamPackage.to_string)) result;
    result
;;

let dependencies = filter_dependencies OpamCudf.dependencies

let reverse_dependencies = filter_dependencies OpamCudf.reverse_dependencies

let check_for_conflicts universe =
  let version_map = universe.u_tables in
  let cudf_universe =
    load_cudf_universe ~depopts:false ~build:true universe universe.u_packages
  in
  let installed =
    Cudf.get_packages (
      load_cudf_universe ~depopts:false ~build:true universe universe.u_installed
    )
  in
  match Algo.Depsolver.edos_coinstall cudf_universe installed with
  | { Algo.Diagnostic.result = Algo.Diagnostic.Success _; _ } ->
    None
  | { Algo.Diagnostic.result = Algo.Diagnostic.Failure _; _ } as c ->
    match OpamCudf.make_conflicts ~version_map cudf_universe c with
    | Conflicts cs -> Some cs
    | _ -> None

let new_packages sol =
  OpamCudf.ActionGraph.fold_vertex (fun action packages ->
      match action with
      | `Install p | `Change (_,_,p) ->
        OpamPackage.Set.add (OpamCudf.cudf2opam p) packages
      | `Reinstall _ | `Remove _ | `Build _ -> packages
  ) sol OpamPackage.Set.empty

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

let print_solution ~messages ~rewrite ~requested t =
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
  let causes = OpamCudf.compute_root_causes t requested in
  let actions, details =
    OpamCudf.ActionGraph.Topological.fold (fun a (actions,details) ->
        let cause =
          try OpamCudf.Map.find (action_contents a) causes
          with Not_found -> Unknown in
        let action =
          map_action (fun p -> rewrite (OpamCudf.cudf2opam p)) a
        in
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
  let actions_str = Action.to_aligned_strings actions in
  List.iter2 (fun act (cause,messages) ->
      if cause <> "" then OpamConsole.msg "  %-60s  [%s]\n" act cause
      else OpamConsole.msg "  %s\n" act;
      List.iter (OpamConsole.msg "       %s\n") messages
    ) actions_str details

let dump_universe universe oc =
  let cudf_univ =
    load_cudf_universe ~depopts:false ~build:true universe universe.u_packages in
  OpamCudf.dump_universe oc cudf_univ

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

let request ?(criteria=`Default) ?(extra_attributes=[])
    ?(install=[]) ?(upgrade=[]) ?(remove=[]) () =
  { wish_install = install; wish_upgrade = upgrade; wish_remove = remove;
    criteria; extra_attributes; }
