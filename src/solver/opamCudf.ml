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

open OpamTypes

let log fmt = OpamGlobals.log "SOLVER" fmt

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
module Pkg = struct
  type t = Cudf.package
  include Common.CudfAdd
  let to_string = string_of_package
  let string_of_action = string_of_action
end

module ActionGraph = MakeActionGraph(Pkg)
module Map = OpamMisc.Map.Make(Pkg)
module Set = OpamMisc.Set.Make(Pkg)
module Graph = struct

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

  let of_universe u =
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
  let closure g pkgs =
    let _, l =
      Topo.fold
        (fun pkg (closure, topo) ->
          if Set.mem pkg closure then
            Set.union closure (Set.of_list (PG.succ g pkg)),
            pkg :: topo
          else
            closure, topo)
        g
        (pkgs, []) in
    l

  let mirror = PO.O.mirror

  include PG
end

let filter_dependencies f_direction universe packages =
  let graph = f_direction (Graph.of_universe universe) in
  let packages = Set.of_list packages in
  Graph.closure graph packages

let backward_dependencies = filter_dependencies (fun x -> x)

let forward_dependencies = filter_dependencies Graph.mirror

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

let string_of_answer l =
  OpamMisc.string_of_list string_of_action  l

let string_of_universe u =
  string_of_packages (List.sort compare (Cudf.get_packages u))

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
  let g = Graph.create () in
  let init = function
    | Dependency (i,_,jl) ->
      Graph.add_vertex g i;
      List.iter (Graph.add_vertex g) jl;
      List.iter (Graph.add_edge g i) jl
    | _ -> () in
  List.iter init depends;
  Graph.iter_vertex (fun v ->
    if v.Cudf.package = "dose-dummy-request" then
      Graph.remove_vertex g v
  ) g;
  let roots =
    Graph.fold_vertex (fun v accu ->
      if Graph.in_degree g v = 0
      then v :: accu
      else accu
    ) g [] in
  let rec unroll root =
    match Graph.succ g root with
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

let s_reinstall = "reinstall"

let need_reinstall p =
  try Cudf.lookup_package_property p s_reinstall = "true"
  with Not_found -> false

let output_universe name universe =
  if !OpamGlobals.debug then (
    let oc = open_out (name ^ ".cudf") in
    Cudf_printer.pp_universe oc universe;
    close_out oc;
    let g = Graph.of_universe universe in
    Graph.output g name;
  )

let default_preamble =
  let l = [
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None));
    ("source",(`String (Some ""))) ;
    ("sourcenumber",(`String (Some "")));
    ("sourceversion",(`Int (Some 1))) ;
    ("essential",(`Bool (Some false))) ;
    ("buildessential",(`Bool (Some false))) ;
    ("depopts",(`Enum (["None"],Some "None")));
    (s_reinstall,`Bool None);
  ] in
  Common.CudfAdd.add_properties Cudf.default_preamble l

let to_cudf univ req = (
  default_preamble,
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

let aspcud_path = lazy (
  try
    match OpamSystem.read_command_output ~verbose:false [ "which"; "aspcud" ] with
    | []   -> None
    | h::t -> Some (OpamMisc.strip h)
  with _ ->
    None
)

let external_solver_available () =
  match Lazy.force aspcud_path with
  | None   -> false
  | Some _ -> true

let solver_call = ref 0
let call_external_solver univ req =
  match Lazy.force aspcud_path with
  | None ->
    (* No external solver is available, use the default one *)
    Algo.Depsolver.check_request ~explain:true (to_cudf univ req)
  | Some path ->
  if Cudf.universe_size univ > 0 then begin
    let cmd = Printf.sprintf "%s $in $out $pref" path in
    let criteria = "-removed,-new" in
    let cudf = to_cudf univ req in
    if !OpamGlobals.debug then (
      Common.Util.Debug.all_enabled ();
      let oc = open_out (Printf.sprintf "/tmp/univ%d.cudf" !solver_call) in
      Cudf_printer.pp_cudf oc cudf;
      incr solver_call;
      close_out oc;
    );
    Algo.Depsolver.check_request ~cmd ~criteria ~explain:true cudf
  end else
    Algo.Depsolver.Sat(None,Cudf.load_universe [])

(* Return the universe in which the system has to go *)
let get_final_universe univ req =
  log "get_final_universe req=%s" (string_of_request req);
  let open Algo.Depsolver in
  match call_external_solver univ req with
  | Sat (_,u) -> Success (uninstall "dose-dummy-request" u)
  | Error str -> OpamGlobals.error_and_exit "solver error: str"
  | Unsat r   ->
    let open Algo.Diagnostic in
    match r with
    | Some {result=Failure f} -> Conflicts f
    | _                       -> failwith "opamSolver"

(* A modified version of CudfDiff to handle reinstallations *)
module Diff = struct

  type package = {
    installed  : Set.t;
    removed    : Set.t;
    reinstalled: Set.t;
  }

  type universe = (string, package) Hashtbl.t

  (* for each pkgname I've the list of all versions that were installed or removed *)
  let diff univ sol =
    let pkgnames =
      OpamMisc.StringSet.of_list (List.map (fun p -> p.Cudf.package) (Cudf.get_packages univ)) in
    let h = Hashtbl.create (OpamMisc.StringSet.cardinal pkgnames) in
    let needed_reinstall = Set.of_list (Cudf.get_packages ~filter:need_reinstall univ) in
    OpamMisc.StringSet.iter (fun pkgname ->
      let were_installed = Set.of_list (Cudf.get_installed univ pkgname) in
      let are_installed = Set.of_list (Cudf.get_installed sol pkgname) in
      let removed = Set.diff were_installed are_installed in
      let installed = Set.diff are_installed were_installed in
      let reinstalled = Set.inter are_installed needed_reinstall in
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
      try Some (Set.choose_one s.Diff.removed)
      with Not_found -> None in
    let installed =
      try Some (Set.choose_one s.Diff.installed)
      with Not_found -> None in
    let reinstalled =
      try Some (Set.choose_one s.Diff.reinstalled)
      with Not_found -> None in
    match removed, installed, reinstalled with
    | None      , Some p     , _      -> add (To_change (None, p))
    | Some p    , None       , _      -> add (To_delete p)
    | Some p_old, Some p_new , _      -> add (To_change (Some p_old, p_new))
    | None      , None       , Some p -> add (To_recompile p)
    | None      , None       , None   -> acc
  ) diff []

let resolve universe request =
  log "resolve request=%s" (string_of_request request);
  match get_final_universe universe request with
  | Conflicts e -> Conflicts e
  | Success u   ->
    log "resolve success=%s" (string_of_universe u);
    try
      let diff = Diff.diff universe u in
      Success (actions_of_diff diff)
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "constraint violations: %s" s

let create_graph filter universe =
  let pkgs = Cudf.get_packages ~filter universe in
  let u = Cudf.load_universe pkgs in
  Graph.of_universe u

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
    Set.of_list (OpamMisc.filter_map (function
      | To_delete pkg -> Some pkg
      | _ -> None
    ) actions) in

  (* the packages to recompile *)
  let to_recompile =
    Set.of_list (OpamMisc.filter_map (function
      | To_recompile pkg -> Some pkg
      | _ -> None
    ) actions) in

  (* compute initial packages to install *)
  let to_process_init =
    Map.of_list (OpamMisc.filter_map (function
      | To_recompile pkg
      | To_change (_, pkg) as act -> Some (pkg, act)
      | To_delete _ -> None
    ) actions) in

  let complete_graph =
    let g =
      Graph.mirror
        (create_graph (fun p -> p.Cudf.installed || Map.mem p to_process_init) complete_universe) in
    List.iter (Graph.remove_vertex g) to_remove_or_upgrade;
    g in

  (* compute packages to recompile due to the REMOVAL of packages *)
  let to_recompile =
    Set.fold (fun pkg to_recompile ->
      let succ = Graph.succ complete_graph pkg in
      Set.union to_recompile (Set.of_list succ)
    ) to_remove to_recompile in

  let to_remove =
    Graph.closure (create_graph (fun p -> Set.mem p to_remove) simple_universe) to_remove in

  (* compute packages to recompile and to process due to NEW packages *)
  let to_recompile, to_process_map =
    Graph.Topo.fold
      (fun pkg (to_recompile, to_process_map) ->
        let add_succ pkg action =
          (Set.union to_recompile (Set.of_list (Graph.succ complete_graph pkg)),
           Map.add pkg action (Map.remove pkg to_process_map)) in
        if Map.mem pkg to_process_init then
          add_succ pkg (Map.find pkg to_process_init)
        else if Set.mem pkg to_recompile then
          add_succ pkg (To_recompile pkg)
        else
          to_recompile, to_process_map)
      complete_graph
      (to_recompile, Map.empty) in

  (* construct the answer [graph] to add.
     Then, it suffices to fold it topologically
     by following the action given at each node (install or recompile). *)
  let to_process = ActionGraph.create () in
  Map.iter (fun _ act -> ActionGraph.add_vertex to_process act) to_process_map;
  Graph.iter_edges
    (fun v1 v2 ->
      try
        let v1 = Map.find v1 to_process_map in
        let v2 = Map.find v2 to_process_map in
        ActionGraph.add_edge to_process v1 v2
      with Not_found ->
        ())
    complete_graph;
  { ActionGraph.to_remove; to_process }
