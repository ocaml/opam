(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
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
    u_invariant = OpamFormula.Empty;
    u_attrs = [];
    u_reinstall = OpamPackage.Set.empty;
  }

let installability_check univ =
  let packages = univ.u_packages in
  let graph =
    OpamCudf.Graph.of_universe @@
    OpamSolver.load_cudf_universe
      ~depopts:false ~build:true ~post:true univ packages ()
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
      ~depopts:true ~build:true ~post:false univ univ.u_packages ()
  in
  let graph =
    OpamCudf.Graph.of_universe cudf_univ |>
    OpamCudf.Graph.mirror
  in
  (* conflicts break cycles *)
  let conflicts =
    Dose_algo.Defaultgraphs.PackageGraph.conflict_graph cudf_univ
  in
  let module CGraph = Dose_algo.Defaultgraphs.PackageGraph.UG in
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
          Dose_algo.Defaultgraphs.PackageGraph.conflict_graph univ
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
        let rec extract_cycles acc seen rpath v g =
          incr it;
          let rec find_pref acc v = function
            | [] -> None
            | v1::r ->
              if Cudf.(=%) v v1 then Some (v1::acc)
              else if CGraph.mem_edge conflicts v v1 then None
              else find_pref (v1::acc) v r
          in
          match find_pref [] v rpath with
          | Some cy -> cy :: acc, seen
          | None ->
            if OpamCudf.Set.mem v seen then acc, seen else
            let seen = OpamCudf.Set.add v seen in
            let rpath = v::rpath in
            (* split into sub-graphs for each successor *)
            List.fold_left
              (fun (acc, seen) s -> extract_cycles acc seen rpath s g)
              (acc, seen) (OpamCudf.Graph.succ g v)
        in
        let p0 = List.find (OpamCudf.Graph.mem_vertex g) pkgs in
        (* OpamConsole.msg "Iterations: %d\n" !it; *)
        let r, _seen = extract_cycles acc OpamCudf.Set.empty [] p0 g in
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

(* Obsolete packages check *)

module PkgSet = OpamPackage.Set
module PkgMap = OpamPackage.Map
module PkgSetSet = OpamStd.Set.Make(PkgSet)
(* module PkgSetMap = OpamStd.Map.Make(PkgSet) *)

let pkg_deps univ package =
  let deps =
    try OpamFilter.filter_deps ~build:true ~post:true ~default:true
          (OpamPackage.Map.find package univ.u_depends)
    with Not_found -> Empty
  in
  let sets_formula =
    OpamFormula.map (fun (name, vconstr) ->
        OpamPackage.Version.Set.filter
          (OpamFormula.check_version_formula vconstr)
          (OpamPackage.versions_of_name univ.u_packages name)
        |> OpamPackage.Name.Map.singleton name
        |> OpamPackage.of_map
        |> fun s -> Atom (PkgSetSet.singleton s))
      deps
  in
  let product ss1 ss2 =
    PkgSetSet.fold (fun s1 ->
        PkgSetSet.union (PkgSetSet.map (PkgSet.union s1) ss2))
      ss1 PkgSetSet.empty
  in
  let depsets = (* PkgSetSet-encoded CNF *)
    match
      OpamFormula.map_up_formula (function
          | Atom s -> Atom s
          | And (Atom s1, Atom s2) -> Atom (PkgSetSet.union s1 s2)
          | Or (Atom s1, Atom s2) -> Atom (product s1 s2)
          | And _ | Or _ -> assert false
          | Block x -> x
          | Empty -> Atom (PkgSetSet.empty))
        sets_formula
    with
    | And _ | Or _ | Block _ | Empty -> assert false
    | Atom depsets ->
      depsets
  in
  let inferred_conflicts =
    (* Versions that may be present in some disjunctions but will always be
       rejected. We filter them out to get more accurate reverse deps *)
    PkgSetSet.fold (fun dset acc ->
        try
          let n = (PkgSet.choose dset).name in
          if PkgSet.for_all (fun p -> p.name = n) dset then
            acc ++ (OpamPackage.packages_of_name univ.u_packages n -- dset)
          else acc
        with Not_found -> acc)
      depsets PkgSet.empty
  in
  PkgSetSet.map (fun s -> s -- inferred_conflicts) depsets

let more_restrictive_deps_than deps1 deps2 =
  PkgSetSet.for_all (fun disj2 ->
      PkgSetSet.exists (fun disj1 -> PkgSet.subset disj1 disj2) deps1)
    deps2

(* Aggregates all versionned packages with an exclusive version relationship
   (when b.vb1 can only be installed with a.va1, and the only version of b that
   can be installed with a.va1 is vb1). An aggregate should not contain more
   than one version per package name. *)
let aggregate packages deps revdeps =
  if OpamClientConfig.E.noaggregate () = Some true then
    PkgSet.fold (fun nv -> PkgSetSet.add (PkgSet.singleton nv))
      packages PkgSetSet.empty
  else
  let friends p (deps, revdeps) =
    (* dependencies which have a 1-1 version relationship *)
    try
      PkgMap.find p deps |>
      OpamPackage.to_map |>
      OpamPackage.Name.Map.filter
        (fun _ vs -> OpamPackage.Version.Set.is_singleton vs) |>
      OpamPackage.of_map |>
      PkgSet.filter (fun d ->
          OpamPackage.packages_of_name (PkgMap.find d revdeps) p.name =
          PkgSet.singleton p)
    with Not_found -> PkgSet.empty
  in
  let rec all_friends acc p =
    let acc = PkgSet.add p acc in
    PkgSet.fold (fun p acc -> all_friends acc p)
      (friends p (deps, revdeps) ++
       friends p (revdeps, deps) --
       acc)
      acc
  in
  let rec aux acc packages =
    if PkgSet.is_empty packages then acc else
    let p = PkgSet.choose packages in
    let fr = all_friends PkgSet.empty p in
    aux (PkgSetSet.add fr acc) (packages -- fr)
  in
  aux PkgSetSet.empty packages

(* we work on aggregates of packages (expected to be a.g. different names with
   the same version), encode their dependencies as CNF mapped to sets, i.e. sets
   of sets from each of which one package must be present.

   Then, we detect aggregates with an inferior version, and equivalent or less
   restrictive dependencies: their members are obsolete *)
let get_obsolete univ opams =
  let deps_map = (* pkg -> setset-encoded CNF *)
    PkgSet.fold (fun p -> PkgMap.add p (pkg_deps univ p))
      univ.u_packages PkgMap.empty
  in
  let simple_deps = (* pkg -> set *)
    PkgMap.map (fun deps -> PkgSetSet.fold PkgSet.union deps PkgSet.empty)
      deps_map
  in
  let revdeps_map = (* pkg -> set *)
    PkgMap.fold (fun pkg ->
        PkgSet.fold (fun d ->
            PkgMap.update d (PkgSet.add pkg) PkgSet.empty))
      simple_deps PkgMap.empty
  in
  let aggregates =
    aggregate univ.u_packages simple_deps revdeps_map
  in
  let aggregate_deps pkgs =
    PkgSet.fold (fun pkg -> PkgSetSet.union (PkgMap.find pkg deps_map))
      pkgs PkgSetSet.empty
    |> PkgSetSet.map (fun ps -> ps -- pkgs)
  in
  let aggregate_revdeps pkgs =
    PkgSet.fold (fun pkg acc ->
        try PkgSet.union (PkgMap.find pkg revdeps_map) acc
        with Not_found -> acc)
      pkgs PkgSet.empty
    -- pkgs
  in
  let aggregate_nextv pkgs =
    let ps =
      OpamPackage.packages_of_names univ.u_packages
        (OpamPackage.names_of_packages pkgs)
    in
    PkgSet.map (fun p -> match PkgSet.split p ps with
        | (_, true, s1) ->
          let next = PkgSet.min_elt s1 in
          if next.name = p.name then next
          else raise Not_found
        | _ -> raise Not_found)
      pkgs
  in
  PkgSetSet.fold (fun pkgs acc ->
      let is_obsolete =
        not @@ PkgSet.exists (fun p ->
            OpamFile.OPAM.has_flag Pkgflag_Compiler
              (OpamPackage.Map.find p opams)) pkgs &&
        try
          let next = aggregate_nextv pkgs in
          more_restrictive_deps_than
            (aggregate_deps pkgs)
            (aggregate_deps next) &&
          let next_rd = aggregate_revdeps next in
          not (OpamPackage.Set.is_empty next_rd) &&
          PkgSet.subset (aggregate_revdeps pkgs) next_rd
        with Not_found -> false
      in
      if is_obsolete then acc ++ pkgs else acc)
    aggregates PkgSet.empty

let check ~quiet ~installability ~cycles ~obsolete ~ignore_test repo_root =
  let pkg_prefixes = OpamRepository.packages_with_prefixes repo_root in
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
      PkgSet.empty, PkgSet.empty
    else (
      if not quiet then
        OpamConsole.msg "Checking installability of every package. This may \
                         take a few minutes...\n";
      installability_check univ
    )
  in
  if not quiet then
    if not (PkgSet.is_empty uninstallable) then
      OpamConsole.error "These packages are not installable (%d):\n%s%s"
        (PkgSet.cardinal unav_roots)
        (OpamStd.List.concat_map " " OpamPackage.to_string
           (PkgSet.elements unav_roots))
        (let unav_others = uninstallable -- unav_roots in
         if PkgSet.is_empty unav_others then "" else
           "\n(the following depend on them and are also unavailable:\n"^
           (OpamStd.List.concat_map " " OpamPackage.to_string
              (PkgSet.elements unav_others))^")");

  (* Cyclic dependency checks *)
  let cycle_packages, cycle_formulas =
    if not cycles then PkgSet.empty, []
    else cycle_check univ
  in
  if not quiet && cycle_formulas <> [] then
    (OpamConsole.error "Dependency cycles detected:";
     OpamConsole.errmsg "%s" @@ print_cycles cycle_formulas);


  (* Obsolescence checks *)
  let obsolete_packages =
    if not obsolete then PkgSet.empty
    else get_obsolete univ opams
  in
  if not quiet && not( PkgSet.is_empty obsolete_packages) then
    (OpamConsole.error "Obsolete packages detected:";
     OpamConsole.errmsg "%s"
       (OpamStd.Format.itemize
          (fun (n, vs) ->
             Printf.sprintf "%s %s"
               (OpamConsole.colorise `bold (OpamPackage.Name.to_string n))
               (OpamStd.List.concat_map ", "
                  (fun v -> OpamConsole.colorise `magenta
                      (OpamPackage.Version.to_string v))
                  (OpamPackage.Version.Set.elements vs)))
          (OpamPackage.Name.Map.bindings
             (OpamPackage.to_map obsolete_packages))));

  univ.u_packages, unav_roots, uninstallable, cycle_packages, obsolete_packages
