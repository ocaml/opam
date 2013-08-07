(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

let log fmt = OpamGlobals.log "SOLVER" fmt

(* see [Debcudf.add_inst] for more details about the format *)
let s_status = "status"
let s_installed   = "  installed"

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

(* Convert an OPAM package to a debian package *)
let opam2debian universe depopts package =
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
      let mem_installed conj =
        let is_installed (name,_) =
          OpamPackage.Set.exists
            (fun pkg -> OpamPackage.name pkg = name) universe.u_installed in
        List.exists is_installed conj in
      let opts = List.filter mem_installed opts in
      let opts = List.rev_map OpamFormula.of_conjunction opts in
      And (depends, OpamFormula.ands opts) in

  let conflicts =
    try OpamPackage.Map.find package universe.u_conflicts
    with Not_found -> Empty in
  let installed = OpamPackage.Set.mem package universe.u_installed in
  let reinstall = match universe.u_action with
    | Upgrade reinstall -> OpamPackage.Set.mem package reinstall
    | _                 -> false in
  let installed_root = OpamPackage.Set.mem package universe.u_installed_roots in
  let open Debian.Packages in
  { Debian.Packages.default_package with
    name      = OpamPackage.Name.to_string (OpamPackage.name package) ;
    version   = OpamPackage.Version.to_string (OpamPackage.version package);
    depends   = List.rev_map (List.rev_map atom2debian) (OpamFormula.to_cnf depends);
    conflicts = List.rev_map atom2debian (OpamFormula.to_conjunction conflicts);
    extras    =
      (if installed && reinstall
       then [OpamCudf.s_reinstall, "true"]
       else []) @
      (if installed
       then [s_status, s_installed]
       else []) @
      (if installed_root
       then [OpamCudf.s_installed_root, "true"]
       else []) @
      Debian.Packages.default_package.extras }

(* Convert an debian package to a CUDF package *)
let debian2cudf tables package =
  let options = {
    Debian.Debcudf.default_options with
    Debian.Debcudf.extras_opt = [
      OpamCudf.s_reinstall, (OpamCudf.s_reinstall, `Bool (Some false));
      OpamCudf.s_installed_root, (OpamCudf.s_installed_root, `Bool (Some false));
    ]
  } in
  Debian.Debcudf.tocudf ~options tables package

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
  let universe =
    let dummy_pkg = OpamPackage.create
        (OpamPackage.Name.of_string "--depopts--")
        (OpamPackage.Version.of_string "--none--") in
    let dummy_atom =
      Atom (OpamPackage.Name.of_string "--depopts--",
            Atom (`Eq, OpamPackage.Version.of_string "--")) in
    if not depopts then (
      let depopts =
        let all = OpamPackage.Set.fold (fun pkg acc ->
            depopts_of_package universe pkg @ acc
          ) universe.u_packages [] in
        let all = List.rev_map OpamFormula.of_conjunction all in
        And(dummy_atom, OpamFormula.ands all) in
      { universe with
        u_packages = OpamPackage.Set.add dummy_pkg universe.u_packages;
        u_depends  = OpamPackage.Map.add dummy_pkg depopts universe.u_depends; }
    ) else
      { universe with
        u_packages = OpamPackage.Set.add dummy_pkg universe.u_packages;
        u_depends  = OpamPackage.Map.add dummy_pkg dummy_atom universe.u_depends; } in

  let opam2cudf =
    let opam2debian =
      OpamPackage.Set.fold
        (fun pkg map -> OpamPackage.Map.add pkg (opam2debian universe depopts pkg) map)
        universe.u_packages
        OpamPackage.Map.empty in
    let tables = Debian.Debcudf.init_tables (OpamPackage.Map.values opam2debian) in
    OpamPackage.Map.map (debian2cudf tables) opam2debian in
  let cudf2opam =
    let h = Hashtbl.create 1024 in
    OpamPackage.Map.iter (fun opam cudf ->
      Hashtbl.add h (cudf.Cudf.package,cudf.Cudf.version) opam
    ) opam2cudf;
    h in
  let universe =
    let available = OpamPackage.Set.elements universe.u_available in
    let universe = List.rev_map (fun nv -> OpamPackage.Map.find nv opam2cudf) available in
    try Cudf.load_universe universe
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "Malformed CUDF universe (%s)" s in
  (* We can trim the universe here to get faster results, but we
     choose to keep it bigger to get more precise conflict messages. *)
  (* let universe = Algo.Depsolver.trim universe in *)
  (fun opam ->
    try OpamPackage.Map.find opam opam2cudf
    with Not_found ->
      OpamGlobals.error_and_exit
        "opam2cudf: Cannot find %s" (OpamPackage.to_string opam)),
  (fun cudf ->
    try Hashtbl.find cudf2opam (cudf.Cudf.package,cudf.Cudf.version)
    with Not_found ->
      (* This can happen if a dependency is not available *)
      try
        let lookup n = Cudf.lookup_package_property cudf n in
        let name = OpamPackage.Name.of_string (lookup "source") in
        let version = OpamPackage.Version.of_string (lookup "sourcenumber") in
        OpamPackage.unknown name (Some version)
      with Not_found ->
        OpamSystem.internal_error "cud2opam(%s,%d)"
          cudf.Cudf.package cudf.Cudf.version),
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
  | Unknown          -> Unknown

let graph cudf2opam cudf_graph =
  let size = OpamCudf.ActionGraph.nb_vertex cudf_graph in
  let opam_graph = PackageActionGraph.create ~size () in
  OpamCudf.ActionGraph.iter_vertex (fun package ->
    PackageActionGraph.add_vertex opam_graph (map_action cudf2opam package)

  ) cudf_graph;
  OpamCudf.ActionGraph.iter_edges (fun p1 p2 ->
    PackageActionGraph.add_edge opam_graph
      (map_action cudf2opam p1)
      (map_action cudf2opam p2)
  ) cudf_graph;
  opam_graph

let solution cudf2opam cudf_solution =
  let to_remove =
    List.rev (List.rev_map cudf2opam cudf_solution.OpamCudf.ActionGraph.to_remove) in
  let to_process = graph cudf2opam cudf_solution.OpamCudf.ActionGraph.to_process in
  let root_causes =
    List.rev_map
      (fun (p, c) -> cudf2opam p, map_cause cudf2opam c)
      cudf_solution.OpamCudf.ActionGraph.root_causes in
  { PackageActionGraph.to_remove ; to_process; root_causes }

let map_request f r =
  let f = List.rev_map f in
  { wish_install = f r.wish_install;
    wish_remove  = f r.wish_remove ;
    wish_upgrade = f r.wish_upgrade }

(* Remove duplicate packages *)
let cleanup_request req =
  let update_packages = List.rev_map (fun (n,_) -> n) req.wish_upgrade in
  let wish_install =
    List.filter (fun (n,_) -> not (List.mem n update_packages)) req.wish_install in
  { req with wish_install }

let resolve ?(verbose=true) universe request =
  log "resolve request=%s" (string_of_request request);
  let opam2cudf, cudf2opam, simple_universe = load_cudf_universe universe in
  let request = cleanup_request request in
  let cudf_request = map_request (atom2cudf opam2cudf) request in
  let resolve =
    if OpamCudf.external_solver_available ()
    then OpamCudf.resolve
    else OpamHeuristic.resolve ~verbose in
  match resolve simple_universe cudf_request with
  | Conflicts c     -> Conflicts (fun () -> OpamCudf.string_of_reasons cudf2opam (c ()))
  | Success actions ->
    let _, _, complete_universe = load_cudf_universe ~depopts:true universe in
    let cudf_solution =
      OpamCudf.solution_of_actions ~simple_universe ~complete_universe actions in
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
  t.PackageActionGraph.to_remove <> [] ||
  PackageActionGraph.fold_vertex
    (fun v acc ->
      acc || match v with To_change (Some _, _) -> true | _ -> false)
    t.PackageActionGraph.to_process
    false

let new_packages sol =
  PackageActionGraph.fold_vertex (fun action packages ->
    OpamPackage.Set.add (action_contents action) packages
  ) sol.PackageActionGraph.to_process OpamPackage.Set.empty

let stats sol =
  let s_install, s_reinstall, s_upgrade, s_downgrade =
    PackageActionGraph.fold_vertex (fun action (i,r,u,d) ->
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
      sol.PackageActionGraph.to_process
      (0, 0, 0, 0) in
  let s_remove = List.length sol.PackageActionGraph.to_remove in
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
  t.PackageActionGraph.to_remove = []
  && PackageActionGraph.is_empty t.PackageActionGraph.to_process

let print_solution ~messages ~rewrite t =
  if not (solution_is_empty t) then
    let causes pkg =
      try List.assoc pkg t.PackageActionGraph.root_causes
      with Not_found -> Unknown in
    List.iter (fun p -> OpamGlobals.msg "%s\n"
        (PackageAction.string_of_action ~causes (To_delete (rewrite p)))
    ) t.PackageActionGraph.to_remove;
    PackageActionGraph.Topological.iter (function action ->
        let action = map_action rewrite action in
        OpamGlobals.msg "%s\n" (PackageAction.string_of_action ~causes action);
        match action with
        | To_change(_,p)
        | To_recompile p -> List.iter (OpamGlobals.msg "     %s.\n")  (messages p)
        | To_delete _    -> ()
      ) t.PackageActionGraph.to_process

let sequential_solution l =
  let g = PackageActionGraph.create () in
  List.iter (PackageActionGraph.add_vertex g) l;
  let rec aux = function
    | [] | [_]       -> ()
    | x::(y::_ as t) ->
      PackageActionGraph.add_edge g x y;
      aux t in
  aux l;
  {
    PackageActionGraph.to_remove = [];
    to_process = g;
    root_causes = []
  }
