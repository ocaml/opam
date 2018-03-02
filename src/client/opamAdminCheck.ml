(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamPackage.Set.Op

let env ~with_test ~with_doc ~dev nv v =
  match OpamVariable.Full.scope v,
        OpamVariable.(to_string (Full.variable v))
  with
  | (OpamVariable.Full.Global | OpamVariable.Full.Self), "name" ->
    Some (S (OpamPackage.Name.to_string nv.name))
  | (OpamVariable.Full.Global | OpamVariable.Full.Self), "version" ->
    Some (S (OpamPackage.Version.to_string nv.version))
  | OpamVariable.Full.Global, "opam-version" ->
    Some (S OpamVersion.(to_string current))
  | OpamVariable.Full.Global, "with-test" ->
    Some (B with_test)
  | OpamVariable.Full.Global, "dev" ->
    Some (B dev)
  | OpamVariable.Full.Global, "with-doc" ->
    Some (B with_doc)
  | _ -> None

let get_universe ~with_test ~with_doc ~dev opams =
  let env = env ~with_test ~with_doc ~dev in
  let packages = OpamPackage.keys opams in
  {
    u_packages = packages;
    u_action = Query;
    u_installed = OpamPackage.Set.empty;
    u_available = packages;
    u_depends =
      OpamPackage.Map.mapi
        (fun nv o ->
           OpamFile.OPAM.depends o |>
           OpamFilter.partial_filter_formula (env nv))
        opams;
    u_depopts =
      OpamPackage.Map.mapi
        (fun nv o ->
           OpamFile.OPAM.depopts o |>
           OpamFilter.partial_filter_formula (env nv))
        opams;
    u_conflicts =
      OpamPackage.Map.mapi
        (fun nv o ->
           OpamFile.OPAM.conflicts o |>
           OpamFilter.filter_formula ~default:false (env nv))
        opams;
    u_installed_roots = OpamPackage.Set.empty;
    u_pinned = OpamPackage.Set.empty;
    u_base = OpamPackage.Set.empty;
    u_attrs = [];
    u_reinstall = OpamPackage.Set.empty;
  }

let installability_check univ =
  let packages = univ.u_packages in
  let graph =
    OpamCudf.Graph.of_universe @@
    OpamSolver.load_cudf_universe
      ~depopts:false ~build:true ~post:true univ packages
  in
  let filter_roots g packages =
    let has_pkg p = OpamPackage.Set.mem (OpamCudf.cudf2opam p) packages in
    OpamCudf.Graph.fold_vertex (fun p acc ->
        if has_pkg p &&
           not (List.exists has_pkg (OpamCudf.Graph.succ g p))
        then OpamPackage.Set.add (OpamCudf.cudf2opam p) acc
        else acc)
      g OpamPackage.Set.empty
  in
  let installable = OpamSolver.installable univ in
  let uninstallable = packages -- installable in
  let unav_roots = filter_roots graph uninstallable in
  unav_roots, uninstallable

let formula_of_pkglist packages = function
  | [] -> OpamFormula.Empty
  | [p] ->
    let nv = OpamCudf.cudf2opam p in
    Atom (nv.name, Atom (`Eq, nv.version))
  | p::ps ->
    let name = (OpamCudf.cudf2opam p).name in
    let nvs = List.map OpamCudf.cudf2opam (p::ps) in
    Atom
      (name,
       OpamFormula.formula_of_version_set
         (OpamPackage.versions_of_name packages name)
         (OpamPackage.versions_of_packages
            (OpamPackage.Set.of_list nvs)))

let cycle_check univ =
  let cudf_univ =
    OpamSolver.load_cudf_universe
      ~depopts:true ~build:true ~post:false univ univ.u_packages
  in
  let graph =
    OpamCudf.Graph.of_universe cudf_univ |>
    OpamCudf.Graph.mirror
  in
  (* conflicts break cycles *)
  let conflicts =
    Algo.Defaultgraphs.PackageGraph.conflict_graph cudf_univ
  in
  let module CGraph = Algo.Defaultgraphs.PackageGraph.UG in
  CGraph.iter_edges (fun nv1 nv2 ->
      OpamCudf.Graph.remove_edge graph nv1 nv2;
      OpamCudf.Graph.remove_edge graph nv2 nv1)
    conflicts;
  let scc =
    let module Comp = Graph.Components.Make(OpamCudf.Graph) in
    Comp.scc_list graph |>
    List.filter (function [] | [_] -> false | _ -> true)
  in
  let node_map, cy =
    List.fold_left (fun (node_map, acc) pkgs ->
        let univ = Cudf.load_universe pkgs in
        let g = OpamCudf.Graph.of_universe univ in
        let conflicts =
          Algo.Defaultgraphs.PackageGraph.conflict_graph univ
        in
        (* Simplify the graph by merging all equivalent versions of each
           package *)
        (* (note: this is not completely accurate, as dependencies might be
           conjunctions or disjunctions, information which is lost in the
           dependency graph) *)
        (* let count = OpamCudf.Graph.nb_vertex g in *)
        let node_map =
          Cudf.fold_packages_by_name (fun node_map _ pkgs ->
              let id p =
                let f pl =
                  List.sort compare @@
                  List.map (Cudf.uid_by_package univ) pl
                in
                f (OpamCudf.Graph.pred g p),
                f (OpamCudf.Graph.succ g p),
                f (CGraph.succ conflicts p)
              in
              let ids =
                List.fold_left (fun acc p ->
                    OpamCudf.Map.add p (id p) acc)
                  OpamCudf.Map.empty pkgs
              in
              let rec gather node_map = function
                | [] -> node_map
                | p::pkgs ->
                  let pid = OpamCudf.Map.find p ids in
                  let ps, pkgs =
                    List.partition
                      (fun p1 -> OpamCudf.Map.find p1 ids = pid)
                      pkgs
                  in
                  List.iter (OpamCudf.Graph.remove_vertex g) ps;
                  let node_map = OpamCudf.Map.add p (p::ps) node_map in
                  gather node_map pkgs
              in
              gather node_map pkgs)
            node_map univ
        in
        (* OpamConsole.msg
         *   "Number of vertices: before merge %d, after merge %d\n"
         *   count (OpamCudf.Graph.nb_vertex g); *)
        let it = ref 0 in
        let rec extract_cycles acc rpath v g =
          incr it;
          let rec find_pref acc v = function
            | [] -> None
            | v1::r ->
              if Cudf.(=%) v v1 then Some (v1::acc)
              else if CGraph.mem_edge conflicts v v1 then None
              else find_pref (v1::acc) v r
          in
          match find_pref [] v rpath with
          | Some cy -> cy :: acc
          | None ->
            let rpath = v::rpath in
            (* split into sub-graphs for each successor *)
            List.fold_left
              (fun acc s -> extract_cycles acc rpath s g)
              acc (OpamCudf.Graph.succ g v)
        in
        let p0 = List.find (OpamCudf.Graph.mem_vertex g) pkgs in
        let r = extract_cycles acc [] p0 g in
        (* OpamConsole.msg "Iterations: %d\n" !it; *)
        node_map, r
      )
      (OpamCudf.Map.empty, []) scc
  in
  (* OpamConsole.msg "all cycles: %d\n" (List.length cy); *)
  let rec has_conflict = function
    | [] | [_] -> false
    | p::r ->
      List.exists
        (CGraph.mem_edge conflicts p)
        r
      || has_conflict r
  in
  let cy =
    List.rev cy |>
    List.filter (fun c -> not (has_conflict c))
  in
  let cycle_packages =
    List.fold_left
      (List.fold_left (fun acc p ->
           List.fold_left (fun acc p ->
               OpamPackage.Set.add (OpamCudf.cudf2opam p) acc)
             acc (OpamCudf.Map.find p node_map)))
      OpamPackage.Set.empty cy
  in
  let cycle_formulas =
    cy |>
    List.map @@ List.map @@ fun p ->
    formula_of_pkglist univ.u_packages (OpamCudf.Map.find p node_map)
  in
  cycle_packages, cycle_formulas

let print_cycles cy =
  let arrow =
    OpamConsole.colorise `yellow @@
    if OpamConsole.utf8 () then " \xe2\x86\x92 " (* U+2192 *)
    else " -> "
  in
  OpamStd.Format.itemize
    ~bullet:(OpamConsole.colorise `bold "  * ")
    (OpamStd.List.concat_map arrow OpamFormula.to_string)
    cy

let check ~quiet ~installability ~cycles ~ignore_test repo_root =
  let repo = OpamRepositoryBackend.local repo_root in
  let pkg_prefixes = OpamRepository.packages_with_prefixes repo in
  let opams =
    OpamPackage.Map.fold (fun nv prefix acc ->
        let opam_file = OpamRepositoryPath.opam repo_root prefix nv in
        match OpamFile.OPAM.read_opt opam_file with
        | Some o -> OpamPackage.Map.add nv o acc
        | None ->
          OpamConsole.warning "Error while reading %s"
            (OpamFile.to_string opam_file);
          acc)
      pkg_prefixes
      OpamPackage.Map.empty
  in
  let univ =
    get_universe
      ~with_test:(not ignore_test) ~with_doc:(not ignore_test) ~dev:false
      opams
  in

  (* Installability check *)
  let unav_roots, uninstallable =
    if not installability then
      OpamPackage.Set.empty, OpamPackage.Set.empty
    else (
      if not quiet then
        OpamConsole.msg "Checking installability of every package. This may \
                         take a few minutes...\n";
      installability_check univ
    )
  in
  if not quiet then
    if not (OpamPackage.Set.is_empty uninstallable) then
      OpamConsole.error "These packages are not installable (%d):\n%s%s"
        (OpamPackage.Set.cardinal unav_roots)
        (OpamStd.List.concat_map " " OpamPackage.to_string
           (OpamPackage.Set.elements unav_roots))
        (let unav_others = uninstallable -- unav_roots in
         if OpamPackage.Set.is_empty unav_others then "" else
           "\n(the following depend on them and are also unavailable:\n"^
           (OpamStd.List.concat_map " " OpamPackage.to_string
              (OpamPackage.Set.elements unav_others))^")");
  (* Cyclic dependency checks *)
  let cycle_packages, cycle_formulas =
    if not cycles then OpamPackage.Set.empty, []
    else cycle_check univ
  in
  if not quiet && cycle_formulas <> [] then
    (OpamConsole.error "Dependency cycles detected:";
     OpamConsole.errmsg "%s" @@ print_cycles cycle_formulas);
  unav_roots, uninstallable, cycle_packages
