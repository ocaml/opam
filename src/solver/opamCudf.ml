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
open OpamTypesBase

let log fmt = OpamGlobals.log "CUDF" fmt
let slog = OpamGlobals.slog

let string_of_action a =
  let aux pkg = Printf.sprintf "%s.%d" pkg.Cudf.package pkg.Cudf.version in
  match a with
  | To_change (None, p)   -> Printf.sprintf "install %s" (aux p)
  | To_change (Some o, p) ->
    let f action =
      Printf.sprintf "%s %s to %d" action (aux o) p.Cudf.version in
    if compare o.Cudf.version p.Cudf.version < 0 then
      f "upgrade"
    else
      f "downgrade"
  | To_recompile p        -> Printf.sprintf "recompile %s" (aux p)
  | To_delete p           -> Printf.sprintf "delete %s" (aux p)

let string_of_actions l =
  OpamMisc.string_of_list (fun a -> " - " ^ string_of_action a) l

let string_of_package p =
  let installed = if p.Cudf.installed then "installed" else "not-installed" in
  Printf.sprintf "%s.%d(%s)"
    p.Cudf.package
    p.Cudf.version installed

let string_of_packages l =
  OpamMisc.string_of_list string_of_package l

let to_json p =
  `O [ ("name", `String p.Cudf.package);
       ("version", `String (string_of_int p.Cudf.version));
       ("installed", `String (string_of_bool p.Cudf.installed));
     ]

(* Graph of cudf packages *)
module Pkg = struct
  type t = Cudf.package
  include Common.CudfAdd
  let to_string = string_of_package
  let name_to_string t = t.Cudf.package
  let version_to_string t = string_of_int t.Cudf.version
  let to_json = to_json
end

module Action = OpamActionGraph.MakeAction(Pkg)
module ActionGraph = OpamActionGraph.Make(Action)
type solution = (Cudf.package, ActionGraph.t) gen_solution

module Map = OpamMisc.Map.Make(Pkg)
module Set = OpamMisc.Set.Make(Pkg)
module Graph = struct

  module PG = struct
    include Algo.Defaultgraphs.PackageGraph.G
    let succ g v =
      try succ g v
      with e -> OpamMisc.fatal e; []
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

  let transitive_closure g =
    PO.O.add_transitive_closure g

  let close_and_linearize g pkgs =
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

let dose_dummy_request = "dose-dummy-request"
let is_dose_request cpkg = cpkg.Cudf.package = dose_dummy_request

let filter_dependencies f_direction universe packages =
  let graph = f_direction (Graph.of_universe universe) in
  let packages = Set.of_list packages in
  Graph.close_and_linearize graph packages

let dependencies = filter_dependencies (fun x -> x)

let reverse_dependencies = filter_dependencies Graph.mirror

let string_of_atom (p, c) =
  let const = function
    | None       -> ""
    | Some (r,v) -> Printf.sprintf " (%s %d)" (OpamFormula.string_of_relop r) v in
  Printf.sprintf "%s%s" p (const c)

let string_of_vpkgs constr =
  let constr = List.sort (fun (a,_) (b,_) -> String.compare a b) constr in
  OpamFormula.string_of_conjunction string_of_atom constr

let string_of_request r =
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (string_of_vpkgs r.wish_install)
    (string_of_vpkgs r.wish_remove)
    (string_of_vpkgs r.wish_upgrade)

let string_of_universe u =
  string_of_packages (List.sort compare (Cudf.get_packages u))

let vpkg2opam cudf2opam cudf_universe (name,constr) =
  let name2opam name =
    OpamPackage.Name.of_string (Common.CudfAdd.decode name) in
  let cstr2opam name = function
    | None -> Empty
    | Some (relop,v) ->
      try
        let cpkg = Cudf.lookup_package cudf_universe (name,v) in
        Atom (relop, OpamPackage.version (cudf2opam cpkg))
      with Not_found -> Atom (relop, OpamPackage.Version.of_string "??") in
  name2opam name, cstr2opam name constr

let vpkg2opamstr cudf2opam cudf_universe vpkg =
  OpamFormula.to_string (Atom (vpkg2opam cudf2opam cudf_universe vpkg))

let strings_of_reason cudf2opam cudf_universe opam_universe r =
  let open Algo.Diagnostic in
  match r with
  | Conflict (i,j,_) ->
    if is_dose_request i || is_dose_request j then
      let a = if is_dose_request i then j else i in
      if is_dose_request a then [] else
        let str =
          Printf.sprintf "Conflicting query for package %s"
            (OpamPackage.to_string (cudf2opam a)) in
        [str]
    else
    let nva, nvb =
      let nvi = cudf2opam i in
      let nvj = cudf2opam j in
      min nvi nvj, max nvi nvj in
    if i.Cudf.package = j.Cudf.package then
      let str = Printf.sprintf "Conflicting version constraints for %s"
          (OpamPackage.name_to_string nva) in
      [str]
    else
    let str = Printf.sprintf "%s is in conflict with %s"
        (OpamPackage.to_string nva)
        (OpamPackage.to_string nvb) in
    [str]
  | Missing (p,m) ->
    let of_package =
      if is_dose_request p then "" else
      let nv = cudf2opam p in
      Printf.sprintf " of package %s" (OpamPackage.to_string nv) in
    let pinned_deps, deps =
      List.partition
        (fun (p,_) ->
           let name = OpamPackage.Name.of_string (Common.CudfAdd.decode p) in
           OpamPackage.Name.Map.mem name opam_universe.u_pinned)
        m in
    let pinned_deps =
      List.rev_map (vpkg2opamstr cudf2opam cudf_universe) pinned_deps in
    let deps =
      List.rev_map (vpkg2opamstr cudf2opam cudf_universe) deps in
    (* XXX This duplicates some work which is better done in
       OpamState.unavailable_reason. Factor it and pass the function as argument
       to this function ? *)
    let str = [] in
    let str =
      if pinned_deps <> [] then
        let dependencies, are, s, have =
          if List.length pinned_deps > 1 then "dependencies", "are", "s", "have"
          else "dependency", "is", "", "has" in
        Printf.sprintf
          "The %s %s%s %s not available because the package%s %s been pinned"
          dependencies
          (OpamMisc.pretty_list pinned_deps)
          of_package
          are s have
        :: str
      else str in
    let str =
      if deps <> [] then
        let dependencies, are =
          if List.length deps > 1 then "dependencies", "are"
          else "dependency", "is" in
        Printf.sprintf
          "The %s %s%s %s not available for your compiler or OS"
          dependencies
          (OpamMisc.pretty_list deps)
          of_package
          are
        :: str
      else str in
    List.rev str
  | Dependency _  -> []

let make_chains cudf_universe cudf2opam depends =
  let open Algo.Diagnostic in
  let map_addlist k v map =
    try Map.add k (v @ Map.find k map) map
    with Not_found -> Map.add k v map in
  let roots,notroots,deps,vpkgs =
    List.fold_left (fun (roots,notroots,deps,vpkgs) -> function
        | Dependency (i, vpkgl, jl) when not (is_dose_request i) ->
          Set.add i roots,
          List.fold_left (fun notroots j -> Set.add j notroots) notroots jl,
          map_addlist i jl deps,
          map_addlist i vpkgl vpkgs
        | _ -> roots, notroots, deps, vpkgs)
      (Set.empty,Set.empty,Map.empty,Map.empty)
      depends
  in
  let roots = Set.diff roots notroots in
  if Set.is_empty roots then [] else
  let children cpkgs =
    List.fold_left (fun acc c ->
        List.fold_left (fun m a -> a :: m) acc
          (try Map.find c deps with Not_found -> []))
      [] cpkgs
  in
  let rec aux constrs direct_deps =
    if direct_deps = [] then [[]] else
    let depnames =
      List.fold_left (fun set p -> OpamMisc.StringSet.add p.Cudf.package set)
        OpamMisc.StringSet.empty direct_deps in
    OpamMisc.StringSet.fold (fun name acc ->
        let name_deps = (* Gather all deps with the given name *)
          List.filter (fun p -> p.Cudf.package = name) direct_deps in
        let name_constrs =
          List.map (List.filter (fun (n,_) -> n = name)) constrs in
        let name_constrs = List.filter ((<>) []) name_constrs in
        let name_constrs =
          OpamMisc.remove_duplicates (List.sort compare name_constrs) in
        let to_opam_and_formula constrs =
          let atoms =
            (List.map (fun p -> Atom (vpkg2opam cudf2opam cudf_universe p))
               (OpamMisc.remove_duplicates (List.sort compare constrs))) in
          match atoms with _::_::_ -> Block (OpamFormula.ands atoms)
                         | _ -> OpamFormula.ands atoms in
        let formula =
          match name_constrs with
          | [f] -> to_opam_and_formula f
          | fs -> OpamFormula.ors (List.map to_opam_and_formula fs) in
        let children_constrs =
          List.map (fun p -> try Map.find p vpkgs with Not_found -> []) name_deps in
        let chains = aux children_constrs (children name_deps) in
        List.fold_left
          (fun acc chain -> (formula :: chain) :: acc)
          acc chains
      )
      depnames []
  in
  let roots_list = Set.elements roots in
  let start_constrs =
    List.map (fun cpkg -> [cpkg.Cudf.package,None]) roots_list in
  aux start_constrs roots_list

let string_of_reasons cudf2opam cudf_universe opam_universe reasons =
  let open Algo.Diagnostic in
  let depends, reasons =
    List.partition (function Dependency _ -> true | _ -> false) reasons in
  let b = Buffer.create 1024 in
  let reasons =
    List.flatten
      (List.map
         (strings_of_reason cudf2opam cudf_universe opam_universe)
         reasons) in
  let reasons = OpamMisc.StringSet.(elements (of_list reasons)) in
  if reasons <> [] then
    Printf.bprintf b
      "Sorry, your request cannot be satisfied:\n%a\n"
      (fun b -> List.iter (Printf.bprintf b " - %s\n"))
      reasons;
  let chains = make_chains cudf_universe cudf2opam depends in
  let string_of_chain c =
    String.concat (OpamGlobals.colorise `yellow " -> ")
      (List.map OpamFormula.to_string c) in
  if chains <> [] then
    Printf.bprintf b
      "The following dependencies couldn't be met:\n%a\n"
      (fun b ->
         List.iter
           (fun c -> Printf.bprintf b " - %s\n" (string_of_chain c)))
      chains;
  if reasons = [] && chains = [] then (* No explanation found :( *)
    Printf.bprintf b
      "Sorry, no solution found: \
       there seems to be a problem with your request.\n";
  Buffer.contents b

(* custom cudf field labels *)
let s_source = "opam-name"
let s_source_number = "opam-version"
let s_reinstall = "reinstall"
let s_installed_root = "installed-root"
let s_builddep = "build-dep"
let s_pinned = "pinned"

let check flag p =
  try Cudf.lookup_typed_package_property p flag = `Bool true
  with Not_found -> false

let need_reinstall = check s_reinstall

let is_installed_root = check s_installed_root

let is_builddep = check s_builddep

let is_pinned = check s_pinned

let aspcud_exists = lazy (OpamSystem.command_exists "aspcud")

let aspcud_command =
  Printf.sprintf "aspcud $in $out $pref"

let default_preamble =
  let l = [
    (s_source,         `String None) ;
    (s_source_number,  `String None);
    (s_reinstall,      `Bool (Some false));
    (s_installed_root, `Bool (Some false));
    (s_builddep,       `Bool (Some false));
    (s_pinned,         `Bool (Some false));
  ] in
  Common.CudfAdd.add_properties Cudf.default_preamble l

let remove universe name constr =
  let filter p =
    p.Cudf.package <> name
    || not (Cudf.version_matches p.Cudf.version constr) in
  let packages = Cudf.get_packages ~filter universe in
  Cudf.load_universe packages

let uninstall_all universe =
  let packages = Cudf.get_packages universe in
  let packages = List.rev_map (fun p -> { p with Cudf.installed = false }) packages in
  Cudf.load_universe packages

let install universe package =
  let p = Cudf.lookup_package universe (package.Cudf.package, package.Cudf.version) in
  let p = { p with Cudf.installed = true } in
  let packages =
    let filter p =
      p.Cudf.package <> package.Cudf.package
      || p.Cudf.version <> package.Cudf.version in
    Cudf.get_packages ~filter universe in
  Cudf.load_universe (p :: packages)

let remove_all_uninstalled_versions_but universe name constr =
  let filter p =
    p.Cudf.installed
    || p.Cudf.package <> name
    || Cudf.version_matches p.Cudf.version constr in
  let packages = Cudf.get_packages ~filter universe in
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

let external_solver_available () =
  !OpamGlobals.use_external_solver && Lazy.force aspcud_exists

let solver_calls = ref 0

let dump_cudf_request ~extern ~version_map (_, univ,_ as cudf) = function
  | None   -> None
  | Some f ->
    ignore ( version_map: int OpamPackage.Map.t );
    incr solver_calls;
    let filename = Printf.sprintf "%s-%d.cudf" f !solver_calls in
    let oc = open_out filename in
    if extern then
      Printf.fprintf oc "#%s %s\n" aspcud_command !OpamGlobals.solver_preferences
    else
      Printf.fprintf oc "#internal OPAM solver\n";
    Cudf_printer.pp_cudf oc cudf;
    OpamPackage.Map.iter (fun (pkg:OpamPackage.t) (vnum: int) ->
      let name = OpamPackage.name_to_string pkg in
      let version = OpamPackage.version_to_string pkg in
      Printf.fprintf oc "#v2v:%s:%d=%s\n" name vnum version;
    ) version_map;
    close_out oc;
    Graph.output (Graph.of_universe univ) f;
    Some filename

let dump_cudf_error ~extern ~version_map univ req =
  let cudf_file = match !OpamGlobals.cudf_file with
    | Some f -> f
    | None ->
      let (/) = Filename.concat in
      !OpamGlobals.root_dir / "log" /
      ("solver-error-"^string_of_int (Unix.getpid())) in
  match dump_cudf_request ~extern (to_cudf univ req) ~version_map (Some cudf_file) with
  | Some f -> f
  | None -> assert false

let call_external_solver ~version_map univ req =
  let cudf_request = to_cudf univ req in
  ignore (dump_cudf_request ~extern:true ~version_map cudf_request !OpamGlobals.cudf_file);
  if Cudf.universe_size univ > 0 then begin
    let cmd = aspcud_command in
    let criteria = !OpamGlobals.solver_preferences in
    try Algo.Depsolver.check_request ~cmd ~criteria ~explain:true cudf_request
    with e ->
      OpamMisc.fatal e;
      OpamGlobals.warning "'%s' failed with %s" cmd (Printexc.to_string e);
      failwith "opamSolver"
  end else
    Algo.Depsolver.Sat(None,Cudf.load_universe [])

let check_request ?(explain=true) ~version_map univ req =
  match Algo.Depsolver.check_request ~explain (to_cudf univ req) with
  | Algo.Depsolver.Unsat
      (Some {Algo.Diagnostic.result = Algo.Diagnostic.Failure f}) ->
    Conflicts f
  | Algo.Depsolver.Sat (_,u) -> Success (remove u "dose-dummy-request" None)
  | Algo.Depsolver.Error msg ->
    let f = dump_cudf_error ~extern:false ~version_map univ req in
    OpamGlobals.error "Internal solver failed with %s Request saved to %S"
      msg f;
    failwith "opamSolver"
  | Algo.Depsolver.Unsat _ -> (* normally when [explain] = false *)
    Conflicts (fun () -> [])

(* Return the universe in which the system has to go *)
let get_final_universe ~version_map univ req =
  let fail msg =
    let f = dump_cudf_error ~extern:true ~version_map univ req in
    OpamGlobals.warning "External solver failed with %s Request saved to %S"
      msg f;
    failwith "opamSolver" in
  match call_external_solver ~version_map univ req with
  | Algo.Depsolver.Sat (_,u) -> Success (remove u "dose-dummy-request" None)
  | Algo.Depsolver.Error "(CRASH) Solution file is empty" ->
    (* XXX Is this still needed with latest dose ? *)
    Success (Cudf.load_universe [])
  | Algo.Depsolver.Error str -> fail str
  | Algo.Depsolver.Unsat r   ->
    let open Algo.Diagnostic in
    match r with
    | Some {result=Failure f} -> Conflicts f
    | Some {result=Success _} -> fail "inconsistent return value."
    | None ->
      (* External solver did not provide explanations, hopefully this will *)
      check_request ~version_map univ req

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

let resolve ~extern ~version_map universe request =
  log "resolve request=%a" (slog string_of_request) request;
  if extern then get_final_universe ~version_map universe request
  else check_request ~version_map universe request

let to_actions f universe result =
  let aux u1 u2 =
    let diff = Diff.diff (f u1) u2 in
    actions_of_diff diff
  in
  map_success (aux universe) result

let create_graph filter universe =
  let pkgs = Cudf.get_packages ~filter universe in
  let u = Cudf.load_universe pkgs in
  Graph.of_universe u

let action_graph_of_packages actions packages =
  let g = ActionGraph.create () in
  Map.iter (fun _ act -> ActionGraph.add_vertex g act) actions;
  Graph.iter_edges (fun v1 v2 ->
      try
        let v1 = Map.find v1 actions in
        let v2 = Map.find v2 actions in
        ActionGraph.add_edge g v1 v2
      with Not_found -> ())
    packages;
  g

(* Compute the original causes of the actions, from the original set of
   packages in the user request. In the restricted dependency graph, for each
   action we find the closest package belonging to the user request and print
   out the closest neighbour that gets there. This way, if a -> b -> c and the
   user requests a to be installed, we can print:
   - install a - install b [required by a] - intall c [required by b] *)
let compute_root_causes universe actions requested =
  let module StringSet = OpamMisc.StringSet in
  let module StringMap = OpamMisc.StringMap in
  let act_packages =
    Map.fold (fun pkg _ acc -> StringSet.add pkg.Cudf.package acc)
      actions StringSet.empty in
  (* g is the graph of actions:
     prerequisite -> action -> postrequisite (eg. recompile) *)
  let g =
    let cudf_univ =
      let filter p = StringSet.mem p.Cudf.package act_packages in
      Cudf.load_universe (Cudf.get_packages ~filter universe) in
    let packages =
      Algo.Defaultgraphs.PackageGraph.dependency_graph cudf_univ in
    let g =
      ActionGraph.mirror (action_graph_of_packages actions packages) in
    let conflicts_graph =
      Algo.Defaultgraphs.PackageGraph.conflict_graph cudf_univ in
    (* add conflicts to the graph to get all causality relations:
       cause (removed required pkg or conflicting pkg) -> removed pkg *)
    Map.iter (fun _ -> function
        | To_delete pkg as act when
            Algo.Defaultgraphs.PackageGraph.UG.mem_vertex conflicts_graph pkg ->
          Algo.Defaultgraphs.PackageGraph.UG.iter_succ (fun v1 ->
              if v1.Cudf.package <> pkg.Cudf.package then
                try ActionGraph.add_edge g (Map.find v1 actions) act
                with Not_found -> ())
            conflicts_graph pkg
        | _ -> ()
      ) actions;
    g in
  let () = match !OpamGlobals.cudf_file with None -> () | Some f ->
    let filename = Printf.sprintf "%s-actions.dot" f in
    let oc = open_out filename in
    ActionGraph.Dot.output_graph oc g;
    close_out oc in
  let requested_pkgnames =
    OpamPackage.Name.Set.fold (fun n s ->
        StringSet.add (Common.CudfAdd.encode (OpamPackage.Name.to_string n)) s)
      requested StringSet.empty in
  let requested_actions =
    Map.filter (fun pkg _ ->
        StringSet.mem pkg.Cudf.package requested_pkgnames)
      actions in
  let merge_causes (c1,depth1) (c2,depth2) =
      if c2 = Unknown || depth1 < depth2 then c1, depth1 else
      if c1 = Unknown || depth2 < depth1 then c2, depth2 else
        let (@) =
          List.fold_left (fun l a -> if List.mem a l then l else a::l) in
        match c1, c2 with
        | Required_by a, Required_by b -> Required_by (a @ b), depth1
        | Use a, Use b -> Use (a @ b), depth1
        | Conflicts_with a, Conflicts_with b -> Conflicts_with (a @ b), depth1
        | Requested, a | a, Requested
        | Unknown, a | a, Unknown
        | Upstream_changes , a | a, Upstream_changes -> a, depth1
        | _, c -> c, depth1 in
  let direct_cause cause consequence dep =
    match (cause, consequence, dep) with
    | To_change(_,p), To_change(_,_),      `Provides -> Required_by [p]
    | To_change(_,p), To_change(Some _,_), `Depends  -> Use [p]
    | a,              To_recompile _,      `Depends       -> Use [action_contents a]
    | _,              To_recompile _,      `Provides -> Unknown
    | To_delete p,    To_delete _,         `Depends  -> Use [p]
    | To_delete p,    To_delete _,         `Provides -> Required_by [p]
    | To_delete p,    To_change _,         `Depends  -> Use [p]
    | _,              To_change(None,_),   `Depends  -> Unknown
    | _,              To_change _,         _         -> Upstream_changes
    | To_change _,    To_delete _,         `Depends  -> Conflicts_with
                                                          [action_contents cause]
    | _,              _,                   _         -> Unknown
  in
  let get_causes acc roots =
    let rec aux seen depth pkgname causes =
      if depth > 100 then
        (OpamGlobals.error
           "Internal error computing action causes: sorry, please report.";
         causes)
      else
      let action = Map.find pkgname actions in
      let seen = Set.add pkgname seen in
      let propagate causes actions direction =
        List.fold_left (fun causes act ->
            let p = action_contents act in
            if Set.mem p seen then causes else
            let cause = direct_cause action act direction in
            if cause = Unknown then causes else
            try
              Map.add p (merge_causes (cause,depth) (Map.find p causes)) causes
            with Not_found ->
              aux seen (depth + 1) p (Map.add p (cause,depth) causes)
          ) causes actions in
      let causes = propagate causes (ActionGraph.pred g action) `Provides in
      let causes = propagate causes (ActionGraph.succ g action) `Depends in
      causes
    in
    let start = Map.fold (fun k _ acc -> Set.add k acc) roots Set.empty in
    let acc = Map.union (fun a _ -> a) acc roots in
    Set.fold (aux start 1) start acc
  in
  (* Compute the roots of the action given a condition *)
  let make_roots causes base_cause f =
    ActionGraph.fold_vertex (fun act acc ->
        if Map.mem (action_contents act) causes then acc else
        if f act then Map.add (action_contents act) (base_cause,0) acc else
          acc)
      g Map.empty in
  let causes = Map.empty in
  let causes =
      let roots =
        if Map.is_empty requested_actions then (* Assume a global update *)
          make_roots causes Requested (function
              | To_change (Some p1,p2) when p1.Cudf.version < p2.Cudf.version ->
                true
              | _ -> false)
        else (Map.map (fun _ -> Requested, 0) requested_actions) in
      get_causes causes roots in
    let causes =
      (* Compute causes for remaining upgrades
         (maybe these could be removed from the actions altogether since they are
         unrelated to the request ?) *)
      let roots = make_roots causes Unknown (function
          | To_change (Some p1,p2) as act
            when p1.Cudf.version < p2.Cudf.version &&
                 List.for_all (function To_change _ -> false | _ -> true)
                   (ActionGraph.pred g act) -> true
          | _ -> false) in
      get_causes causes roots in
    let causes =
      (* Compute causes for remaining changes (assume upstream changes) *)
      let roots =
        make_roots causes Upstream_changes
          (fun a -> ActionGraph.in_degree g a = 0) in
      get_causes causes roots in
  Map.fold (fun p (cause,_depth) acc -> (p,cause)::acc) causes []

(*
  Compute a full solution from a set of root actions. This means:
  1/ computing the right sequence of removal.
  2/ computing the transitive closure of reinstallations.
  3/ computing the root causes of actions

  Parameters:
  - [simple _universe] is the graph with 'depends' only
  - [complex_universe] is the graph with 'depends' + 'depopts'
  - [requested] the set of the package names that were part of the original request
*)
let solution_of_actions ~simple_universe ~complete_universe ~requested root_actions =
  log "graph_of_actions root_actions=%a"
    (slog string_of_actions) root_actions;

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

  let all_packages =
    (* we consider the complete universe here (eg. including optional dependencies) *)
    let graph =
      create_graph
        (fun p -> p.Cudf.installed || Map.mem p actions)
        complete_universe in
    Graph.mirror graph in

  (* the graph of interesting packages, which might be impacted by the
     current actions *)
  let interesting_packages =
    let graph = Graph.copy all_packages in
    List.iter (Graph.remove_vertex graph) to_remove_or_upgrade;
    graph in

  (* the packages to remove *)
  let to_remove =
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
    List.rev (Graph.close_and_linearize graph remove_roots) in

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
        if is_builddep pkg then to_recompile else
        let succ = Graph.succ all_packages pkg in
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
        if is_builddep pkg then
          to_recompile, actions
        else if Map.mem pkg actions then
          add_succ pkg (Map.find pkg actions)
        else if Set.mem pkg to_recompile then
          add_succ pkg (To_recompile pkg)
        else
          to_recompile, actions)
      interesting_packages
      (to_recompile, actions) in

  (* Construct the full graph of actions to proceed to reach the
     new state given by the solver.  *)
  let to_process = action_graph_of_packages actions interesting_packages in

  let all_actions =
    List.fold_left (fun acc a ->
        List.fold_left (fun acc pkg -> Map.add pkg a acc)
          acc (full_action_contents a))
      actions root_actions in

  let root_causes =
    compute_root_causes complete_universe all_actions requested in

  { to_remove; to_process; root_causes }
