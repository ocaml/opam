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

    u_packages = OpamPackage.Set.empty;
    u_installed = OpamPackage.Set.empty;
    u_available = OpamPackage.Set.empty;
    u_action = Install OpamPackage.Name.Set.empty;

    u_orphans = OpamPackage.Set.empty;
    u_versionmap = Pef.Pefcudf.create 0;
  }

let name_to_cudf name =
  Common.CudfAdd.encode (OpamPackage.Name.to_string name)

let get_cudf_constr tables (name,constr) = 
  match constr with
  |None -> (name,None)
  |Some(op,v) ->
      let cv = Pef.Pefcudf.get_cudf_version tables (name,v) in
      (name,Some (op, cv))

(* we assume the package exists in the universe *)
let atom2cudf universe (name,constr) =
  let tables = universe.u_versionmap in
  let name = OpamPackage.Name.to_string name in
  let constr =
    match constr with
    | None -> None
    | Some(op,v) -> Some(op,OpamPackage.Version.to_string v)
  in
  try get_cudf_constr tables (name,constr)
  with Not_found ->
    (* The version for comparison doesn't exist: match to the closest
       existing version according to the direction of the comparison
       (this shouldn't happen for any constraint in the universe, now that we
       compute a full version map, but may still happen for user-provided
       constraints) *)
    let cudf_name = Common.CudfAdd.decode name in
    let all_versions = Hashtbl.find tables.Pef.Pefcudf.versions_table cudf_name in
    let cudf_constr =
      match constr with
      | None -> assert false
      | Some(`Neq,_) -> None (* Always true *)
      | Some(`Eq,_) -> Some (`Eq, (List.length all_versions) + 1) (* Always false *)
      | Some((`Geq | `Gt | `Leq | `Lt) as op,v) ->
        let sign, result_op =  match op with
          | `Geq | `Gt -> (fun x -> x), `Geq
          | `Leq | `Lt -> (fun x -> -x), `Leq 
        in
        let rev_version_map =
            List.fold_left (fun acc v ->
              let cv = Pef.Pefcudf.get_cudf_version tables (name,v) in
              OpamStd.IntMap.add (sign cv) v acc
            ) OpamStd.IntMap.empty all_versions 
        in
        let map =
          OpamStd.IntMap.filter (fun _ v1 ->
            sign (OpamVersionCompare.compare v v1) < 0
          ) rev_version_map 
        in
        if OpamStd.IntMap.is_empty map then
          match result_op with
          | `Geq -> Some (`Gt, max 1 (List.length all_versions))
          | `Leq -> Some (`Lt, 1)
        else
          Some (result_op, sign (fst (OpamStd.IntMap.min_binding map)))
    in (cudf_name,cudf_constr)
(** cudf transformation *)

let pefcudf_aux (switch,switches,profiles,depopts,installed) tables pefpkglist =
  let options = { Opam.Opamcudf.switch = switch; switches; profiles; depopts} in
  let cudfpkglist =
    List.map (fun pkg ->
      let reinstallist =
        try OpamStd.String.split (pkg#get_extra "reinstall") ','
        with Not_found -> []
      in
      if (installed && (List.mem switch pkg#installedlist)) || not installed then (
        let pl = Opam.Opamcudf.tocudf tables ~options pkg in
        List.fold_left (fun acc1 p ->
          let sw =
            try Cudf.lookup_package_property p "switch"
            with Not_found -> failwith "mandatory field switch not found"
          in
          let p =
            if List.mem sw reinstallist then
              { p with Cudf.pkg_extra = ("reinstall",`Bool true) :: p.Cudf.pkg_extra }
            else
              p
          in
          p :: acc1
        ) [] pl
      )
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
  let tables = universe.u_versionmap in
  (*
  let reinstall = match universe.u_action with
    | Upgrade (reinstall , _ )
    | Reinstall (reinstall , _ ) -> reinstall
    | _ -> OpamPackage.Set.empty
  in
  *)
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


(* load a cudf universe from an opam one *)
let load_cudf_universe ?depopts ~build universe opam_packages =
  Cudf.load_universe (pefcudflist universe ?depopts ~build opam_packages)

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

let resolve ?(verbose=true) universe request =
  log "resolve request=%a" (slog string_of_request) request;
  let simple_universe =
    load_cudf_universe universe ~build:true
      (universe.u_available ++ universe.u_installed -- universe.u_orphans) in
  let cudf_request = map_request (atom2cudf universe) request in
  let add_orphan_packages u =
    load_cudf_universe universe ~build:true
      (universe.u_orphans ++
         (OpamPackage.Set.of_list
            (List.map OpamCudf.cudf2opam (Cudf.get_packages u)))) in
  let resolve u req =
    if OpamCudf.external_solver_available ()
    then
      try
        let resp = OpamCudf.resolve ~extern:true u req in
        OpamCudf.to_actions add_orphan_packages u resp
      with Failure "opamSolver" ->
        OpamConsole.error_and_exit
          "External solver failure, please fix your installation and check \
           $OPAMROOT/config and variable $OPAMEXTERNALSOLVER.\n\
           You may also retry with option --use-internal-solver"
    else OpamHeuristic.resolve ~verbose add_orphan_packages u req in
  match resolve simple_universe cudf_request with
  | Conflicts _ as cs -> cs
  | Success actions ->
    let all_packages = universe.u_available ++ universe.u_orphans in
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
	OpamCudf.cycle_conflict complete_universe
	  (List.map
	     (List.map
		  (fun a ->
		     Action.to_string (map_action OpamCudf.cudf2opam a)))
	     cycles)

let get_atomic_action_graph t =
  cudf_to_opam_graph OpamCudf.cudf2opam t

let installable universe =
  log "trim";
  let simple_universe =
    load_cudf_universe universe universe.u_available ~build:true in
  let trimed_universe = Algo.Depsolver.trim simple_universe in
  Cudf.fold_packages
    (fun universe pkg -> OpamPackage.Set.add (OpamCudf.cudf2opam pkg) universe)
    OpamPackage.Set.empty
    trimed_universe

let filter_dependencies
    f_direction ~depopts ~build ~installed
    ?(unavailable=false) universe packages =
  if OpamPackage.Set.is_empty packages then [] else
  let u_packages =
    packages ++
    if installed then universe.u_installed else
    if unavailable then universe.u_packages else
      universe.u_available in
  let cudf_universe = load_cudf_universe ~depopts ~build universe u_packages in
  let cudf_packages = pefcudflist universe ~depopts ~build packages in
  let topo_packages = f_direction cudf_universe cudf_packages in
  let result = List.rev_map OpamCudf.cudf2opam topo_packages in
  log "filter_dependencies packages=%a result=%a"
    (slog OpamPackage.Set.to_string) packages
    (slog (OpamStd.List.to_string OpamPackage.to_string)) result;
  result

let dependencies = filter_dependencies OpamCudf.dependencies

let reverse_dependencies = filter_dependencies OpamCudf.reverse_dependencies

(* This function is part of dose . Temporarely copied here *)
let is_consistent univ =
  match Cudf_checker.is_consistent univ with
  |true, None ->
      Algo.Diagnostic.Success (fun ?(all=false) () ->
        if all then
          Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) univ
        else []
      )
  |false, Some `Unsat_dep (nv,vpkgformula) ->
      Algo.Diagnostic.Failure (fun () ->
        let pkg = Cudf.lookup_package univ nv in
        List.map (fun vpkglist ->
          Algo.Diagnostic.Missing(pkg,vpkglist)
        ) vpkgformula
      )
  |false, Some `Conflict (nv,vpkglist) ->
      Algo.Diagnostic.Failure (fun () ->
        let pkg1 = Cudf.lookup_package univ nv in
        List.flatten (
          List.map (fun vpkg ->
            List.map (fun pkg2 ->
              Algo.Diagnostic.Conflict (pkg1,pkg2,vpkg)
            ) (Common.CudfAdd.who_provides univ vpkg)
          ) vpkglist
        )
      )
  |(true|false),_ -> failwith "Bug in Cudf_checker.is_consistent"

let check_for_conflicts universe =
  let cudf_universe =
    load_cudf_universe ~depopts:false ~build:true universe  universe.u_packages
  in
  match is_consistent cudf_universe with
  |Algo.Diagnostic.Success _ -> None
  |Algo.Diagnostic.Failure f -> 
      let Conflicts cs = OpamCudf.dep_conflict cudf_universe f in
      Some cs

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
    load_cudf_universe ~depopts:false ~build:true universe
      universe.u_available
  in
  OpamCudf.dump_universe oc cudf_univ

let filter_solution filter t =
  let t = OpamCudf.ActionGraph.copy t in
  let rec rm iter_deps v =
    if OpamCudf.ActionGraph.mem_vertex t v then (
      iter_deps (rm iter_deps) t v;
      OpamCudf.ActionGraph.remove_vertex t v
    ) 
  in
  OpamCudf.ActionGraph.iter_vertex (function
      | `Remove nv as a when not (filter (OpamCudf.cudf2opam nv)) ->
        rm OpamCudf.ActionGraph.iter_pred a
      | (`Install nv | `Change (_,_,nv)) as a
        when not (filter (OpamCudf.cudf2opam nv)) ->
        rm OpamCudf.ActionGraph.iter_succ a
      | _ -> ()
  ) t;
  t

let request ?(criteria=`Default) ?(extra_attributes=[])
    ?(install=[]) ?(upgrade=[]) ?(remove=[]) () =
  { wish_install = install; wish_upgrade = upgrade; wish_remove = remove;
    criteria; extra_attributes; }
