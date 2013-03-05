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

let string_of_action a =
  let aux pkg = Printf.sprintf "%s.%d" pkg.Cudf.package pkg.Cudf.version in
  match a with
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
  let string_of_action ?causes:_ = string_of_action
end

module ActionGraph = MakeActionGraph(Pkg)
module Map = OpamMisc.Map.Make(Pkg)
module Set = OpamMisc.Set.Make(Pkg)
module Graph = struct

  module PG = struct
    include Algo.Defaultgraphs.PackageGraph.G
    let succ g v =
      try succ g v
      with _ -> []
  end

  module PO = Algo.Defaultgraphs.GraphOper (PG)

  module Topo = Graph.Topological.Make (PG)

  let of_universe u =
    let g = Algo.Defaultgraphs.PackageGraph.dependency_graph u in
    PO.transitive_reduction g;
    g

  let output g filename =
    let fd = open_out (filename ^ ".dot") in
    Algo.Defaultgraphs.PackageGraph.DotPrinter.output_graph fd g;
    close_out fd

  (* Return the transitive closure of [pkgs] in [g], sorted in topological order *)
  let closure g pkgs =
    let g = PO.O.add_transitive_closure g in
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

let dependencies = filter_dependencies (fun x -> x)

let reverse_dependencies = filter_dependencies Graph.mirror

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

let string_of_universe u =
  string_of_packages (List.sort compare (Cudf.get_packages u))

let vpkg2opamstr cudf2opam (name, constr) =
  match constr with
  | None         -> Common.CudfAdd.decode name
  | Some (r,v) ->
    let c = Cudf.({
        package       = name;
        version       = v;
        depends       = [];
        conflicts     = [];
        provides      = [];
        installed     = false;
        was_installed = false;
        keep          = `Keep_none;
        pkg_extra     = [];
      }) in
    try
      let nv = cudf2opam c in
      let str r =
        let name = OpamPackage.name nv in
        let version = OpamPackage.version nv in
        Printf.sprintf "%s%s%s"
          (OpamPackage.Name.to_string name) r
          (OpamPackage.Version.to_string version) in
      match r with
      | `Eq  -> OpamPackage.to_string nv
      | `Neq -> str "!="
      | `Geq -> str ">="
      | `Gt  -> str ">"
      | `Leq -> str "<="
      | `Lt  -> str "<"
    with _ ->
      Common.CudfAdd.decode name


let string_of_reason cudf2opam r =
  let open Algo.Diagnostic in
  match r with
  | Conflict (i,j,_) ->
    let nvi = cudf2opam i in
    let nvj = cudf2opam j in
    let str = Printf.sprintf
        "The package %s is in conflict with %s."
        (OpamPackage.to_string nvi)
        (OpamPackage.to_string nvj) in
    Some str
  | Missing (p,m) ->
    let of_package =
      if p.Cudf.package = "dose-dummy-request" then ""
      else
        let nv = cudf2opam p in
        Printf.sprintf " of package %s" (OpamPackage.to_string nv) in
    let dependencies =
      if List.length m > 1 then "dependencies" else "dependency" in
    let deps = List.rev_map (vpkg2opamstr cudf2opam) m in
    let str = Printf.sprintf
        "The %s %s%s is not available for your compiler or your OS."
        dependencies
        (String.concat ", " deps)
        of_package in
    Some str
  | Dependency _  -> None

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
  List.filter (function [_] -> false | _ -> true) chains

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
  let reasons = OpamMisc.filter_map (string_of_reason cudf2opam) reasons in
  let reasons = OpamMisc.StringSet.(elements (of_list reasons)) in
  begin match reasons with
    | []  -> ()
    | [r] -> Buffer.add_string b r
    | _   ->
      let reasons = String.concat "\n  - " reasons in
      Printf.bprintf b "Your request cannot be satisfied:\n  - %s" reasons;
      match chains with
      | [] -> ()
      | _  ->
        let chains = List.map string_of_chain chains in
        let chains = String.concat "\n  -" chains in
        Printf.bprintf b "This is due to the following dependency chain(s):\n  - %s" chains
  end;
  Buffer.contents b

let s_reinstall = "reinstall"
let s_installed_root = "installed-root"

let check flag p =
  try Cudf.lookup_package_property p flag = "true"
  with Not_found -> false

let need_reinstall = check s_reinstall

let is_installed_root = check s_installed_root

let solver_calls = ref 0

let aspcud_path = lazy (
  try
    match OpamSystem.read_command_output ~verbose:false [ "which"; "aspcud" ] with
    | []   -> None
    | h::_ -> Some (OpamMisc.strip h)
  with _ ->
    None
)

let aspcud_command path =
  Printf.sprintf "%s $in $out $pref" path

let external_solver_available () =
  match Lazy.force aspcud_path with
  | None   -> false
  | Some _ -> true

let dump_cudf_request (_, univ,_ as cudf) =
  match !OpamGlobals.cudf_file with
  | None   -> ()
  | Some f ->
    incr solver_calls;
    let oc = open_out (Printf.sprintf "%s-%d.cudf" f !solver_calls) in
    begin match Lazy.force aspcud_path with
    | None      -> Printf.fprintf oc "#internal OPAM solver\n"
    | Some path -> Printf.fprintf oc "#!%s %s\n" (aspcud_command path) OpamGlobals.aspcud_criteria
    end;
    Cudf_printer.pp_cudf oc cudf;
    close_out oc;
    Graph.output (Graph.of_universe univ) f

let default_preamble =
  let l = [
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None));
    ("source",(`String (Some ""))) ;
    ("sourcenumber",(`String (Some "")));
    ("sourceversion",(`Int (Some 1))) ;
    ("essential",(`Bool (Some false))) ;
    ("buildessential",(`Bool (Some false))) ;
    (s_reinstall,`Bool (Some false));
    (s_installed_root, `Bool (Some false));
  ] in
  Common.CudfAdd.add_properties Cudf.default_preamble l

let uninstall name universe =
  let packages = Cudf.get_packages universe in
  let packages = List.filter (fun p -> p.Cudf.package <> name) packages in
  Cudf.load_universe packages

let to_cudf univ req = (
  default_preamble,
  univ,
  { Cudf.request_id = "opam";
    install         = req.wish_install;
    remove          = req.wish_remove;
    upgrade         = req.wish_upgrade;
    req_extra       = [] }
)

let call_external_solver ~explain univ req =
  let cudf_request = to_cudf univ req in
  dump_cudf_request cudf_request;
  match Lazy.force aspcud_path with
  | None ->
    (* No external solver is available, use the default one *)
    Algo.Depsolver.check_request ~explain cudf_request
  | Some path ->
  if Cudf.universe_size univ > 0 then begin
    let cmd = aspcud_command path in
    let criteria = OpamGlobals.aspcud_criteria in
    Algo.Depsolver.check_request ~cmd ~criteria ~explain:true cudf_request
  end else
    Algo.Depsolver.Sat(None,Cudf.load_universe [])

(* Return the universe in which the system has to go *)
let get_final_universe univ req =
  let open Algo.Depsolver in
  match call_external_solver ~explain:true univ req with
  | Sat (_,u) -> Success (uninstall "dose-dummy-request" u)
  | Error str -> OpamGlobals.error_and_exit "solver error: %s" str
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
      OpamMisc.StringSet.of_list
        (List.rev_map (fun p -> p.Cudf.package) (Cudf.get_packages univ)) in
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
   actions. At this point, we don't know about the root causes of the
   actions, they will be computed later. *)
let actions_of_diff diff =
  Hashtbl.fold (fun _ s acc ->
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
    try
      let diff = Diff.diff universe u in
      Success (actions_of_diff diff)
    with Cudf.Constraint_violation s ->
      OpamGlobals.error_and_exit "constraint violations: %s" s

let create_graph filter universe =
  let pkgs = Cudf.get_packages ~filter universe in
  let u = Cudf.load_universe pkgs in
  Graph.of_universe u

(*
  Compute a full solution from a set of root actions. This means:
  1/ computing the right sequence of removal.
  2/ computing the transitive closure of reinstallations.
  3/ computing the root causes of actions

  Parameters:
  - [simple _universe] is the graph with 'depends' only
  - [complex_universe] is the graph with 'depends' + 'depopts'
*)
let solution_of_actions ~simple_universe ~complete_universe root_actions =
  log "graph_of_actions root_actions=%s" (string_of_actions root_actions);

  (* The packages to remove or upgrade *)
  let to_remove_or_upgrade =
    OpamMisc.filter_map (function
      | To_change (Some pkg, _)
      | To_delete pkg -> Some pkg
      | _ -> None
    ) root_actions in

  (* Initial actions to process *)
  let actions =
    Map.of_list (OpamMisc.filter_map (function
      | To_recompile pkg
      | To_change (_, pkg) as act -> Some (pkg, act)
      | To_delete _ -> None
    ) root_actions) in

  (* the graph of interesting packages, which might be impacted by the
     current actions *)
  let interesting_packages =
    let graph =
      (* we consider the complete universe here (eg. including optional dependencies) *)
      create_graph
        (fun p -> p.Cudf.installed || Map.mem p actions)
        complete_universe in
    List.iter (Graph.remove_vertex graph) to_remove_or_upgrade;
    Graph.mirror graph in

  (* the packages to remove, and the associated root causes *)
  let to_remove, root_causes =
    let remove_roots =
      Set.of_list (OpamMisc.filter_map (function
        | To_delete pkg -> Some pkg
        | _ -> None
      ) root_actions) in
    (* we consider here only the simple universe (eg. hard
       dependencies only): we don't want to uninstall a package if
       some of its optional dependencies disapear, however we must
       recompile it (see below). *)
    let graph = create_graph (fun p -> Set.mem p remove_roots) simple_universe in
    let to_remove = List.rev (Graph.closure graph remove_roots) in
    let root_causes =
      let graph = Graph.PO.O.add_transitive_closure graph in
      let cause pkg =
        let roots = List.filter (fun v -> Graph.in_degree graph v = 0) (Graph.pred graph pkg) in
        let roots = List.filter is_installed_root roots in
        let sinks = List.filter (fun v -> Graph.out_degree graph v = 0) (Graph.succ graph pkg) in
        let sinks = List.filter is_installed_root sinks in
        match roots, sinks with
        | [], [] -> Unknown
        | [], _  -> Use sinks
        | _      -> Required_by roots in
      List.rev_map (fun pkg -> pkg, cause pkg) to_remove in
    to_remove, root_causes in

  (* the packages to recompile *)
  let to_recompile =
    let recompile_roots =
      Set.of_list (OpamMisc.filter_map (function
        | To_recompile pkg -> Some pkg
        | _ -> None
      ) root_actions) in
    (* add the packages to recompile due to the REMOVAL of packages
       (ie. when an optional dependency has been removed). *)
    List.fold_left (fun to_recompile pkg ->
      let succ = Graph.succ interesting_packages pkg in
      Set.union to_recompile (Set.of_list succ)
    ) recompile_roots to_remove in

  (* Compute the transitive closure of packages to recompile *)
  let _, actions =
    Graph.Topo.fold
      (fun pkg (to_recompile, actions) ->
        let add_succ pkg action =
          let succ = Graph.succ interesting_packages pkg in
          let to_recompile = Set.union to_recompile (Set.of_list succ) in
          let actions = Map.add pkg action (Map.remove pkg actions) in
          to_recompile, actions in
        if Map.mem pkg actions then
          add_succ pkg (Map.find pkg actions)
        else if Set.mem pkg to_recompile then
          add_succ pkg (To_recompile pkg)
        else
          to_recompile, actions)
      interesting_packages
      (to_recompile, actions) in

  (* Construct the full graph of actions to proceed to reach the
     new state given by the solver.  *)
  let to_process = ActionGraph.create () in
  Map.iter (fun _ act -> ActionGraph.add_vertex to_process act) actions;
  Graph.iter_edges
    (fun v1 v2 ->
      try
        let v1 = Map.find v1 actions in
        let v2 = Map.find v2 actions in
        ActionGraph.add_edge to_process v1 v2
      with Not_found ->
        ())
    interesting_packages;

  (* Now we can compute the root causes. Install & Upgrades are either
     the original cause of the action, or they are here because of some
     dependency constrains: so we need to look forward in the graph. At
     the opposite, Reinstall are there because of some install/upgrade,
     so need to look backward in the graph. *)
  let root_causes =
    let to_process_complete = ActionGraph.add_transitive_closure (ActionGraph.copy to_process) in
    ActionGraph.Topological.fold (fun action root_causes ->
      match ActionGraph.out_degree to_process action, action with
      | 0, To_change _        -> root_causes
      | _, To_change (_, pkg) ->
        let succ = ActionGraph.succ to_process_complete action in
        let causes = List.filter (fun a -> ActionGraph.out_degree to_process a = 0) succ in
        let causes = List.filter (function To_change _ -> true | _ -> false) causes in
        let causes = List.rev_map action_contents causes in
        let cause = match causes with
          | []  -> Unknown
          | _   -> Required_by causes in
        (pkg, cause) :: root_causes
      | _, To_recompile pkg ->
        let pred = ActionGraph.pred to_process_complete action in
        let causes = List.filter (fun a -> ActionGraph.in_degree to_process a = 0) pred in
        let causes = List.rev_map action_contents causes in
        let cause = match causes with
          | [] -> Upstream_changes
          | _  -> Use causes in
        (pkg, cause) :: root_causes
      | _, To_delete _ ->
          (* the to_process graph should not contain remove actions. *)
          assert false
    ) to_process root_causes in

  { ActionGraph.to_remove; to_process; root_causes }
