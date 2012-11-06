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

(* TODO:
   1/ reinstall
   2/ heuristics *)

open OpamTypes

let log fmt = OpamGlobals.log "SOLVER" fmt

let map_action f = function
  | To_change (Some x, y) -> To_change (Some (f x), f y)
  | To_change (None, y)   -> To_change (None, f y)
  | To_delete y           -> To_delete (f y)
  | To_recompile y        -> To_recompile (f y)

let string_of_action action =
  let aux pkg = Printf.sprintf "%s.%d" pkg.Cudf.package pkg.Cudf.version in
  match action with
  | To_change (None, p)   -> Printf.sprintf " - install %s" (aux p)
  | To_change (Some o, p) ->
    let f action =
      Printf.sprintf " - %s %s to %d" action (aux o) p.Cudf.version in
    if compare o.Cudf.version p.Cudf.version < 0 then
      f "upgrade"
    else
      f "downgrade"
  | To_recompile p        -> Printf.sprintf " - recompile %s" (aux p)
  | To_delete p           -> Printf.sprintf " - delete %s" (aux p)

let string_of_actions l =
  OpamMisc.string_of_list string_of_action l

let string_of_package p =
  let installed = if p.Cudf.installed then "installed" else "not-installed" in
  Printf.sprintf "%s.%d(%s)"
    p.Cudf.package
    p.Cudf.version installed

let string_of_packages l =
  OpamMisc.string_of_list string_of_package l

(* Graph of cudf packages *)
module CudfPkg = struct
  type t = Cudf.package
  include Common.CudfAdd
  let to_string = string_of_package
  let string_of_action = string_of_action
end

module CudfActionGraph = MakeActionGraph(CudfPkg)
module CudfMap = OpamMisc.Map.Make(CudfPkg)
module CudfSet = OpamMisc.Set.Make(CudfPkg)
module CudfGraph = struct

  module PG = struct
    include Algo.Defaultgraphs.PackageGraph.G
    let union g1 g2 =
      let g1 = copy g1 in
      let () =
        begin
          iter_vertex (add_vertex g1) g2;
          iter_edges (add_edge g1) g2;
        end in
      g1
    let succ g v =
      try succ g v
      with _ -> []
  end

  module PO = Algo.Defaultgraphs.GraphOper (PG)

  module type FS = sig
    type iterator
    val start : PG.t -> iterator
    val step : iterator -> iterator
    val get : iterator -> PG.V.t
  end

  module Make_fs (F : FS) = struct
    let fold f acc g =
      let rec aux acc iter =
        match try Some (F.get iter, F.step iter) with Exit -> None with
        | None -> acc
        | Some (x, iter) -> aux (f acc x) iter in
      aux acc (F.start g)
  end

  module Topo = Graph.Topological.Make (PG)

  let dep_reduction u =
    let g = Algo.Defaultgraphs.PackageGraph.dependency_graph u in
    PO.transitive_reduction g;
    g

  let output g filename =
    if !OpamGlobals.debug then (
      let fd = open_out (filename ^ ".dot") in
      Algo.Defaultgraphs.PackageGraph.DotPrinter.output_graph fd g;
      close_out fd
    )

  (* Return a topoligal sort of the closures of pkgs in g *)
  let topo_closure g pkgs =
    let _, l =
      Topo.fold
        (fun pkg (closure, topo) ->
          if CudfSet.mem pkg closure then
            CudfSet.union closure (CudfSet.of_list (PG.succ g pkg)),
            pkg :: topo
          else
            closure, topo)
        g
        (pkgs, []) in
    l

  include PG
end

let string_of_atom (p, c) =
  let const = function
    | None       -> ""
    | Some (r,v) -> Printf.sprintf " (%s %d)" (OpamFormula.string_of_relop r) v in
  Printf.sprintf "%s%s" p (const c)

let string_of_request r =
  let to_string = OpamFormula.string_of_conjunction string_of_atom in
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (to_string r.wish_install)
    (to_string r.wish_remove)
    (to_string r.wish_upgrade)

let map_request f r =
  let f = List.map f in
  { wish_install = f r.wish_install;
    wish_remove  = f r.wish_remove ;
    wish_upgrade = f r.wish_upgrade }

let string_of_cudf_answer l =
  OpamMisc.string_of_list string_of_action  l

let string_of_universe u =
  string_of_packages (Cudf.get_packages u)

let string_of_reason cudf2opam r =
  let open Algo.Diagnostic in
  match r with
  | Conflict (i,j,_) ->
    let nvi = cudf2opam i in
    let nvj = cudf2opam j in
    Printf.sprintf "Conflict between %s and %s."
      (OpamPackage.to_string nvi) (OpamPackage.to_string nvj)
  | Missing (i,_) ->
    let nv = cudf2opam i in
    Printf.sprintf "Missing %s." (OpamPackage.to_string nv)
  | Dependency _ -> ""

let make_chains depends =
  let open Algo.Diagnostic in
  let g = CudfGraph.create () in
  let init = function
    | Dependency (i,_,jl) ->
      CudfGraph.add_vertex g i;
      List.iter (CudfGraph.add_vertex g) jl;
      List.iter (CudfGraph.add_edge g i) jl
    | _ -> () in
  List.iter init depends;
  CudfGraph.iter_vertex (fun v ->
    if v.Cudf.package = "dose-dummy-request" then
      CudfGraph.remove_vertex g v
  ) g;
  let roots =
    CudfGraph.fold_vertex (fun v accu ->
      if CudfGraph.in_degree g v = 0
      then v :: accu
      else accu
    ) g [] in
  let rec unroll root =
    match CudfGraph.succ g root with
    | []       -> [[root]]
    | children ->
      let chains = List.flatten (List.map unroll children) in
      List.map (fun cs -> root :: cs) chains in
  let chains = List.flatten (List.map unroll roots) in
  List.filter (function [x] -> false | _ -> true) chains

exception Found of Cudf.package

let string_of_reasons cudf2opam reasons =
  let open Algo.Diagnostic in
  let depends, reasons = List.partition (function Dependency _ -> true | _ -> false) reasons in
  let chains = make_chains depends in
  let rec string_of_chain = function
    | []   -> ""
    | [p]  -> OpamPackage.to_string (cudf2opam p)
    | p::t -> Printf.sprintf "%s <- %s" (OpamPackage.to_string (cudf2opam p)) (string_of_chain t) in
  let string_of_chain c = string_of_chain (List.rev c) in
  let b = Buffer.create 1024 in
  List.iter (fun r ->
    Printf.bprintf b " - %s\n" (string_of_reason cudf2opam r)
  ) reasons;
  List.iter (fun c ->
    Printf.bprintf b " + %s\n" (string_of_chain c)
  ) chains;
  Buffer.contents b

(* Convert an OPAM formula into a debian formula *)
let atom2debian (n, v) =
  (OpamPackage.Name.to_string n, None),
  match v with
  | None       -> None
  | Some (r,v) -> Some (OpamFormula.string_of_relop r, OpamPackage.Version.to_string v)

(* to convert to cudf *)
(* see [Debcudf.add_inst] for more details about the format *)
let s_status = "status"
let s_installed   = "  installed"
let s_reinstall = "reinstall"

(* Convert an OPAM package to a debian package *)
let opam2debian universe depopts package =
  let depends = OpamPackage.Map.find package universe.u_depends in
  let depends =
    let opts = OpamFormula.to_dnf (OpamPackage.Map.find package universe.u_depopts) in
    if depopts then
      let opts = List.map OpamFormula.of_conjunction opts in
      And (depends, Or(depends,OpamFormula.ors opts))
    else
      let mem_installed conj =
        let is_installed (name,_) =
          OpamPackage.Set.exists (fun pkg -> OpamPackage.name pkg = name) universe.u_installed in
        List.exists is_installed conj in
      let opts = List.filter mem_installed opts in
      let opts = List.map OpamFormula.of_conjunction opts in
      And (depends, OpamFormula.ands opts) in

  let conflicts = OpamPackage.Map.find package universe.u_conflicts in
  let installed = OpamPackage.Set.mem package universe.u_installed in
  let reinstall = match universe.u_action with
    | Upgrade reinstall -> OpamPackage.Set.mem package reinstall
    | _                 -> false in
  let open Debian.Packages in
  { Debian.Packages.default_package with
    name      = OpamPackage.Name.to_string (OpamPackage.name package) ;
    version   = OpamPackage.Version.to_string (OpamPackage.version package);
    depends   = List.map (List.map atom2debian) (OpamFormula.to_cnf depends);
    conflicts = List.map atom2debian (OpamFormula.to_conjunction conflicts);
    extras    =
      (if installed && reinstall
       then [s_reinstall, "true"]
       else []) @
      (if installed
       then [s_status, s_installed]
       else []) @
        Debian.Packages.default_package.extras }

let reinstall p =
  try Cudf.lookup_package_property p s_reinstall = "true"
  with Not_found -> false

(* Convert an debian package to a CUDF package *)
let debian2cudf tables package =
    let options = {
      Debian.Debcudf.default_options with
        Debian.Debcudf.extras_opt = [ s_reinstall, (s_reinstall, `Bool None) ]
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

let output_universe name universe =
  if !OpamGlobals.debug then (
    let oc = open_out (name ^ ".cudf") in
    Cudf_printer.pp_universe oc universe;
    close_out oc;
    let g = CudfGraph.dep_reduction universe in
    CudfGraph.output g name;
  )

(* load a cudf universe from an opam one *)
let load_cudf_universe ?(depopts=false) universe =
  let opam2debian =
    OpamPackage.Set.fold
      (fun pkg map -> OpamPackage.Map.add pkg (opam2debian universe depopts pkg) map)
      universe.u_available
      OpamPackage.Map.empty in
  let tables = Debian.Debcudf.init_tables (OpamPackage.Map.values opam2debian) in
  let opam2cudf = OpamPackage.Map.map (debian2cudf tables) opam2debian in
  let cudf2opam = Hashtbl.create 1024 in
  OpamPackage.Map.iter (fun opam cudf -> Hashtbl.add cudf2opam (cudf.Cudf.package,cudf.Cudf.version) opam) opam2cudf;
  let universe =
    try Cudf.load_universe (OpamPackage.Map.values opam2cudf)
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "Malformed CUDF universe (%s)" s in
  output_universe "opam-universe" universe;
  (fun opam ->
    try OpamPackage.Map.find opam opam2cudf
    with Not_found ->
      OpamGlobals.error_and_exit "opam2cudf: Cannot find %s" (OpamPackage.to_string opam)),
  (fun cudf ->
    try Hashtbl.find cudf2opam (cudf.Cudf.package,cudf.Cudf.version)
    with Not_found ->
      OpamGlobals.error "cudf2opam: Cannot find %s.%d" cudf.Cudf.package cudf.Cudf.version;
      OpamPackage.create (OpamPackage.Name.of_string "xxx") (OpamPackage.Version.of_string "yyy")),
  universe

let to_cudf univ req = (
  { Cudf.default_preamble with Cudf.property = [s_reinstall,`Bool None] },
  univ,
  { Cudf.request_id = "opam";
    install         = req.wish_install;
    remove          = req.wish_remove;
    upgrade         = req.wish_upgrade;
    req_extra       = [] }
)

let uninstall name universe =
  let packages = Cudf.get_packages universe in
  let packages = List.filter (fun p -> p.Cudf.package <> name) packages in
  Cudf.load_universe packages

(* Return the universe in which the system has to go *)
let get_final_universe univ req =
  let open Algo.Depsolver in
  match Algo.Depsolver.check_request ~explain:true (to_cudf univ req) with
  | Sat (_,u) -> Success (uninstall "dose-dummy-request" u)
  | Error str -> OpamGlobals.error_and_exit "solver error: str"
  | Unsat r   ->
    let open Algo.Diagnostic in
    match r with
    | Some {result=Failure f} -> Conflicts f
    | _                       -> failwith "opamSolver"

(* A modified version of CudfDiff to handle reinstallations *)
module MyCudfDiff = struct

  type solution = {
    installed : CudfSet.t;
    removed : CudfSet.t;
    reinstalled: CudfSet.t;
  }

  (* for each pkgname I've the list of all versions that were installed or removed *)
  let diff univ sol =
    let pkgnames =
      OpamMisc.StringSet.of_list (List.map (fun p -> p.Cudf.package) (Cudf.get_packages univ)) in
    let h = Hashtbl.create (OpamMisc.StringSet.cardinal pkgnames) in
    let needed_reinstall = CudfSet.of_list (Cudf.get_packages ~filter:reinstall univ) in
    OpamMisc.StringSet.iter (fun pkgname ->
      let were_installed = CudfSet.of_list (Cudf.get_installed univ pkgname) in
      let are_installed = CudfSet.of_list (Cudf.get_installed sol pkgname) in
      let removed = CudfSet.diff were_installed are_installed in
      let installed = CudfSet.diff are_installed were_installed in
      let reinstalled = CudfSet.inter are_installed needed_reinstall in
      let s = { removed; installed; reinstalled } in
      Hashtbl.add h pkgname s
    ) pkgnames ;
    h

end

(* Transform a diff from current to final state into a list of
   actions *)
let actions_of_diff diff =
  Hashtbl.fold (fun pkgname s acc ->
    let add x = x :: acc in
    let removed =
      try Some (CudfSet.choose_one s.MyCudfDiff.removed)
      with Not_found -> None in
    let installed =
      try Some (CudfSet.choose_one s.MyCudfDiff.installed)
      with Not_found -> None in
    let reinstalled =
      try Some (CudfSet.choose_one s.MyCudfDiff.reinstalled)
      with Not_found -> None in
    match removed, installed, reinstalled with
    | None      , Some p     , _      -> add (To_change (None, p))
    | Some p    , None       , _      -> add (To_delete p)
    | Some p_old, Some p_new , _      -> add (To_change (Some p_old, p_new))
    | None      , None       , Some p -> add (To_recompile p)
    | None      , None       , None   -> acc
  ) diff []

let cudf_resolve universe request =
  log "cudf_resolve request=%s" (string_of_request request);
  match get_final_universe universe request with
  | Conflicts e -> Conflicts e
  | Success u   ->
    log "cudf_resolve success=%s" (string_of_universe u);
    try
      let diff = MyCudfDiff.diff universe u in
      Success (actions_of_diff diff)
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "constraint violations: %s" s

let rec minimize minimizable universe =
  log "minimize minimizable=%s" (OpamMisc.StringSet.to_string minimizable);
  if OpamMisc.StringSet.is_empty minimizable then
    universe
  else
    let is_removable universe name =
      let b, r = Cudf_checker.is_consistent (uninstall name universe) in
      (match r with
      | None   -> log "%s is not necessary" name
      | Some r ->
        log "cannot remove %s: %s" name
          (Cudf_checker.explain_reason (r:>Cudf_checker.bad_solution_reason)));
      b in
    let to_remove = OpamMisc.StringSet.filter (is_removable universe) minimizable in
    let minimizable = OpamMisc.StringSet.diff minimizable to_remove in
    if OpamMisc.StringSet.is_empty to_remove then
      universe
    else
      let universe = OpamMisc.StringSet.fold uninstall to_remove universe in
      minimize minimizable universe

(* Try to play all the possible upgrade scenarios ... *)
let cudf_resolve_opt universe request =
  log "cudf_resolve_opt request=%s" (string_of_request request);
  match get_final_universe universe request with

  | Conflicts e ->
    log "cudf_resolve_opt conflict!";
    Conflicts e

  | Success u   ->
    log "cudf_resolve_opt success!";

    (* All the packages in the request *)
    let all = Hashtbl.create 1024 in

    (* Package which are maybe not so useful *)
    let minimizable = ref OpamMisc.StringSet.empty in

    (* The packages to upgrade *)
    let upgrade = Hashtbl.create 1024 in
    let add_upgrade name =
      let packages = Cudf.get_packages ~filter:(fun p -> p.Cudf.package = name) universe in
      let packages = List.sort (fun p1 p2 -> compare p2.Cudf.version p1.Cudf.version) packages in
      let packages =
        (* only keep the version greater or equal to the currently installed package *)
        match List.filter (fun p -> p.Cudf.installed) packages with
        | []   -> packages
        | i::_ -> List.filter (fun p -> p.Cudf.version >= i.Cudf.version) packages in
      let atoms = List.map (fun p -> p.Cudf.package, Some (`Eq, p.Cudf.version)) packages in
      Hashtbl.add upgrade name (Array.of_list atoms) in

    (* Register the packages in the request *)
    List.iter (fun (n,_) -> Hashtbl.add all n false) request.wish_install;
    List.iter (fun (n,_) -> Hashtbl.add all n true) request.wish_upgrade;

    (* Register the upgraded packages *)
    List.iter (fun (n,v as x) ->
      match v with
      | Some _ -> Hashtbl.add upgrade n [| x |]
      | None   -> add_upgrade n
    ) request.wish_upgrade;

    (* Register the new packages *)
    let diff = Common.CudfDiff.diff universe u in
    Hashtbl.iter (fun name s ->
      if not (Common.CudfAdd.Cudf_set.is_empty s.Common.CudfDiff.installed) then (
        if not (Hashtbl.mem all name) then
          minimizable := OpamMisc.StringSet.add name !minimizable;
        if not (Hashtbl.mem upgrade name) then
          add_upgrade name)
    ) diff;

    (* sort the requests by the distance to the optimal state *)
    let score_atom (n,_ as a) =
      let t = Hashtbl.find upgrade n in
      let s = ref 0 in
      while t.(!s) <> a do incr s done;
      !s in
    let score_wish request =
      List.fold_left (fun accu a -> score_atom a + accu) 0 request in
    let compare_wish r1 r2 =
      let s1 = score_wish r1 and s2 = score_wish r2 in
    if s1 = s2 then
      compare r1 r2
    else
      s1 - s2 in

  (* enumerate all the upgrade strategies: can be very costly when too many packages *)
  let size =
    Hashtbl.fold (fun _ a accu -> accu * Array.length a) upgrade 1 in
  log "Number of upgrade scenarios: %d" size;
  if size > 10000 then (
    log "the universe is too big, do not try to be too clever";
    cudf_resolve universe request
  ) else (
    let wish_upgrades =
      let aux _ array state = match state with
        | [] -> List.map (fun elt -> [elt]) (Array.to_list array)
        | l  ->
          List.fold_left (fun accu elt ->
            List.fold_left (fun accu l -> (elt::l)::accu) accu state
          ) [] (Array.to_list array) in
      Hashtbl.fold aux upgrade [] in
    let wish_upgrades = List.sort compare_wish wish_upgrades in
    let requests = List.map (fun wish_upgrade -> { request with wish_upgrade }) wish_upgrades in

    let solution =
      List.fold_left (fun accu request ->
        match accu with
        | Success _ -> accu
        | _         -> get_final_universe universe request
      ) (Conflicts (fun _ -> assert false)) requests in

    match solution with
    | Conflicts _ -> cudf_resolve universe request
    | Success u   ->
      try
        let diff = MyCudfDiff.diff universe (minimize !minimizable u) in
        Success (actions_of_diff diff)
      with Cudf.Constraint_violation s ->
        OpamGlobals.error_and_exit "constraint violations: %s" s
  )

let create_graph filter universe =
  let pkgs = Cudf.get_packages ~filter universe in
  let u = Cudf.load_universe pkgs in
  CudfGraph.dep_reduction u

(* Build the graph of actions.
   - [simple_universe] is the graph with 'depends' only
   - [complex_universe] is the graph with 'depends' + 'depopts' *)
let solution_of_actions ~simple_universe ~complete_universe actions =
  log "graph_of_actions actions=%s" (string_of_actions actions);

  (* The packages to remove or upgrade *)
  let to_remove_or_upgrade =
    OpamMisc.filter_map (function
      | To_change (Some pkg, _)
      | To_delete pkg -> Some pkg
      | _ -> None
    ) actions in

  (* the packages to remove *)
  let to_remove =
    CudfSet.of_list (OpamMisc.filter_map (function
      | To_delete pkg -> Some pkg
      | _ -> None
    ) actions) in

  (* the packages to recompile *)
  let to_recompile =
    CudfSet.of_list (OpamMisc.filter_map (function
      | To_recompile pkg -> Some pkg
      | _ -> None
    ) actions) in

  (* compute initial packages to install *)
  let to_process_init =
    CudfMap.of_list (OpamMisc.filter_map (function
      | To_recompile pkg
      | To_change (_, pkg) as act -> Some (pkg, act)
      | To_delete _ -> None
    ) actions) in

  let complete_graph =
    let g =
      CudfGraph.PO.O.mirror
        (create_graph (fun p -> p.Cudf.installed || CudfMap.mem p to_process_init) complete_universe) in
    List.iter (CudfGraph.remove_vertex g) to_remove_or_upgrade;
    g in

  (* compute packages to recompile due to the REMOVAL of packages *)
  let to_recompile =
    CudfSet.fold (fun pkg to_recompile ->
      let succ = CudfGraph.succ complete_graph pkg in
      CudfSet.union to_recompile (CudfSet.of_list succ)
    ) to_remove to_recompile in

  let to_remove =
    CudfGraph.topo_closure (create_graph (fun p -> CudfSet.mem p to_remove) simple_universe) to_remove in

  (* compute packages to recompile and to process due to NEW packages *)
  let to_recompile, to_process_map =
    CudfGraph.Topo.fold
      (fun pkg (to_recompile, to_process_map) ->
        let add_succ pkg action =
          (CudfSet.union to_recompile (CudfSet.of_list (CudfGraph.succ complete_graph pkg)),
           CudfMap.add pkg action (CudfMap.remove pkg to_process_map)) in
        if CudfMap.mem pkg to_process_init then
          add_succ pkg (CudfMap.find pkg to_process_init)
        else if CudfSet.mem pkg to_recompile then
          add_succ pkg (To_recompile pkg)
        else
          to_recompile, to_process_map)
      complete_graph
      (to_recompile, CudfMap.empty) in

  (* construct the answer [graph] to add.
     Then, it suffices to fold it topologically
     by following the action given at each node (install or recompile). *)
  let to_process = CudfActionGraph.create () in
  CudfMap.iter (fun _ act -> CudfActionGraph.add_vertex to_process act) to_process_map;
  CudfGraph.iter_edges
    (fun v1 v2 ->
      try
        let v1 = CudfMap.find v1 to_process_map in
        let v2 = CudfMap.find v2 to_process_map in
        CudfActionGraph.add_edge to_process v1 v2
      with Not_found ->
        ())
    complete_graph;
  { CudfActionGraph.to_remove; to_process }


(******************************************************************************)

let string_of_request r =
  let to_string = OpamFormula.string_of_conjunction OpamFormula.string_of_atom in
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (to_string r.wish_install)
    (to_string r.wish_remove)
    (to_string r.wish_upgrade)

let opam_graph cudf2opam cudf_graph =
  let size = CudfActionGraph.nb_vertex cudf_graph in
  let opam_graph = PackageActionGraph.create ~size () in
  CudfActionGraph.iter_vertex (fun package ->
    PackageActionGraph.add_vertex opam_graph (map_action cudf2opam package)

  ) cudf_graph;
  CudfActionGraph.iter_edges (fun p1 p2 ->
    PackageActionGraph.add_edge opam_graph
      (map_action cudf2opam p1)
      (map_action cudf2opam p2)
  ) cudf_graph;
  opam_graph

let opam_solution cudf2opam cudf_solution =
  let to_remove = List.map cudf2opam cudf_solution.CudfActionGraph.to_remove in
  let to_process = opam_graph cudf2opam cudf_solution.CudfActionGraph.to_process in
  { PackageActionGraph.to_remove ; to_process }

let resolve universe request =
  log "resolve universe=%s" (OpamPackage.Set.to_string universe.u_available);
  log "resolve request=%s" (string_of_request request);
  let opam2cudf, cudf2opam, simple_universe = load_cudf_universe universe in
  let cudf_request = map_request (atom2cudf opam2cudf) request in
  match cudf_resolve_opt simple_universe cudf_request with
  | Conflicts c     -> Conflicts (fun () -> string_of_reasons cudf2opam (c ()))
  | Success actions ->
    let _, _, complete_universe = load_cudf_universe ~depopts:true universe in
    let solution = solution_of_actions ~simple_universe ~complete_universe actions in
    Success (opam_solution cudf2opam solution)

let filter_dependencies f_direction ~depopts ~installed universe packages =
  let opam2cudf, cudf2opam, cudf_universe = load_cudf_universe ~depopts universe in
  let cudf_universe =
    if installed then
      Cudf.load_universe (List.filter (fun pkg -> pkg.Cudf.installed) (Cudf.get_packages cudf_universe))
    else
      cudf_universe in
  let cudf_packages = List.map opam2cudf (OpamPackage.Set.elements packages) in
  let graph = f_direction (CudfGraph.dep_reduction cudf_universe) in
  let packages_topo = CudfGraph.topo_closure graph (CudfSet.of_list cudf_packages) in
  let result = List.map cudf2opam packages_topo in
  log "filter_dependencies packages=%s result=%s"
    (OpamPackage.Set.to_string packages)
    (OpamMisc.string_of_list OpamPackage.to_string result);
  result

let get_backward_dependencies = filter_dependencies (fun x -> x)

let get_forward_dependencies = filter_dependencies CudfGraph.PO.O.mirror

let delete_or_update t =
  t.PackageActionGraph.to_remove <> [] ||
  PackageActionGraph.fold_vertex
    (fun v acc ->
      acc || match v with To_change (Some _, _) -> true | _ -> false)
    t.PackageActionGraph.to_process
    false

let stats sol =
  let s_install, s_reinstall, s_upgrade, s_downgrade =
    PackageActionGraph.fold_vertex (fun action (i,r,u,d) ->
      match action with
      | To_change (None, _)             -> i+1, r, u, d
      | To_change (Some x, y) when x<>y ->
        if OpamPackage.Version.compare (OpamPackage.version x) (OpamPackage.version y) < 0 then
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
  Printf.sprintf "%d to install | %d to reinstall | %d to upgrade | %d to downgrade | %d to remove"
    stats.s_install
    stats.s_reinstall
    stats.s_upgrade
    stats.s_downgrade
    stats.s_remove

let solution_is_empty t =
  t.PackageActionGraph.to_remove = [] && PackageActionGraph.is_empty t.PackageActionGraph.to_process

let print_solution t =
  if solution_is_empty t then
    ()
  (*Globals.msg
    "No actions will be performed, the current state satisfies the request.\n"*)
  else
    let f = OpamPackage.to_string in
    List.iter (fun p -> OpamGlobals.msg " - remove %s\n" (f p)) t.PackageActionGraph.to_remove;
    PackageActionGraph.Topological.iter
      (function action -> OpamGlobals.msg "%s\n" (PackageAction.string_of_action action))
      t.PackageActionGraph.to_process
