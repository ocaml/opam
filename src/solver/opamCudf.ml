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

let log ?level fmt = OpamConsole.log ?level "CUDF" fmt
let slog = OpamConsole.slog

(* custom cudf field labels *)
let s_source = "opam-name"
let s_source_number = "opam-version"
let s_reinstall = "reinstall"
let s_installed_root = "installed-root"
let s_pinned = "pinned"
let s_version_lag = "version-lag"

let cudf2opam cpkg =
  let sname = Cudf.lookup_package_property cpkg s_source in
  let name = OpamPackage.Name.of_string sname in
  let sver = Cudf.lookup_package_property cpkg s_source_number in
  let version = OpamPackage.Version.of_string sver in
  OpamPackage.create name version

let cudfnv2opam ?version_map ?cudf_universe (name,v) =
  let nv = match cudf_universe with
    | None -> None
    | Some u ->
      try Some (cudf2opam (Cudf.lookup_package u (name,v)))
      with Not_found -> None
  in
  match nv with
  | Some nv -> nv
  | None ->
    let name = OpamPackage.Name.of_string (Common.CudfAdd.decode name) in
    match version_map with
    | Some vmap ->
      let nvset =
        OpamPackage.Map.filter
          (fun nv cv -> nv.name = name && cv = v)
          vmap
      in
      fst (OpamPackage.Map.choose nvset)
    | None -> raise Not_found

let string_of_package p =
  let installed = if p.Cudf.installed then "installed" else "not-installed" in
  Printf.sprintf "%s.%d(%s)"
    p.Cudf.package
    p.Cudf.version installed

let string_of_packages l =
  OpamStd.List.to_string string_of_package l

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

let string_of_action = Action.to_string

let string_of_actions l =
  OpamStd.List.to_string (fun a -> " - " ^ string_of_action a) l

exception Solver_failure
exception Cyclic_actions of Action.t list list

type conflict_case =
  | Conflict_dep of (unit -> Algo.Diagnostic.reason list)
  | Conflict_cycle of string list list
type conflict =
  Cudf.universe * int package_map * conflict_case

module Map = OpamStd.Map.Make(Pkg)
module Set = OpamStd.Set.Make(Pkg)
module Graph = struct

  module PG = struct
    include Algo.Defaultgraphs.PackageGraph.G
    let succ g v =
      try succ g v
      with e -> OpamStd.Exn.fatal e; []
  end

  module PO = Algo.Defaultgraphs.GraphOper (PG)

  module Topo = Graph.Topological.Make (PG)

  let of_universe u =
    Algo.Defaultgraphs.PackageGraph.dependency_graph u

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

(** Special package used by Dose internally, should generally be filtered out *)
let dose_dummy_request = "dose-dummy-request"
let is_dose_request cpkg = cpkg.Cudf.package = dose_dummy_request

let filter_dependencies f_direction universe packages =
  log ~level:3 "filter deps: build graph";
  let graph = f_direction (Graph.of_universe universe) in
  let packages = Set.of_list packages in
  log ~level:3 "filter deps: close_and_linearize";
  let r = Graph.close_and_linearize graph packages in
  log ~level:3 "filter deps: done";
  r

let dependencies = filter_dependencies (fun x -> x)

let reverse_dependencies = filter_dependencies Graph.mirror

let string_of_atom (p, c) =
  let const = function
    | None       -> ""
    | Some (r,v) -> Printf.sprintf " (%s %d)" (OpamPrinter.relop r) v in
  Printf.sprintf "%s%s" p (const c)

let string_of_vpkgs constr =
  let constr = List.sort (fun (a,_) (b,_) -> String.compare a b) constr in
  OpamFormula.string_of_conjunction string_of_atom constr

let string_of_universe u =
  string_of_packages (List.sort Common.CudfAdd.compare (Cudf.get_packages u))

let vpkg2atom cudfnv2opam (name,cstr) =
  match cstr with
  | None ->
    OpamPackage.Name.of_string (Common.CudfAdd.decode name), None
  | Some (relop,v) ->
    try
      let nv = cudfnv2opam (name,v) in
      nv.name, Some (relop, nv.version)
    with Not_found ->
      OpamConsole.error "Translation error for package %s"
        (string_of_atom (name, cstr));
      assert false
(* Should be unneeded now that we pass a full version_map along
   [{
      log "Could not find corresponding version in cudf universe: %a"
        (slog string_of_atom) (name,cstr);
      let candidates =
        Cudf.lookup_packages cudf_universe name in
      let solutions =
        Cudf.lookup_packages ~filter:cstr cudf_universe name in
      let module OVS = OpamPackage.Version.Set in
      let to_version_set l =
        OVS.of_list
          (List.map (fun p -> OpamPackage.version (cudf2opam p)) l) in
      let solutions = to_version_set solutions in
      let others = OVS.Op.(to_version_set candidates -- solutions) in
      OpamPackage.Name.of_string (Common.CudfAdd.decode name),
      match relop, OVS.is_empty solutions, OVS.is_empty others with
      | _, true, true -> None
      | `Leq, false, _ | `Lt, false, true -> Some (`Leq, OVS.max_elt solutions)
      | `Lt, _, false | `Leq, true, false -> Some (`Lt, OVS.min_elt others)
      | `Geq, false, _ | `Gt, false, true -> Some (`Geq, OVS.min_elt solutions)
      | `Gt, _, false | `Geq, true, false -> Some (`Gt, OVS.max_elt others)
      | `Eq, false, _ -> Some (`Eq, OVS.choose solutions)
      | `Eq, true, _ ->
        Some (`Eq, OpamPackage.Version.of_string "<unavailable version>")
      | `Neq, false, true -> None
      | `Neq, _, false -> Some (`Neq, OVS.choose others)
   }]
*)

let vpkg2opam cudfnv2opam vpkg =
  match vpkg2atom cudfnv2opam vpkg with
  | p, None -> p, Empty
  | p, Some (relop,v) -> p, Atom (relop, v)

let conflict_empty ~version_map univ =
  Conflicts (univ, version_map, Conflict_dep (fun () -> []))
let make_conflicts ~version_map univ = function
  | {Algo.Diagnostic.result = Algo.Diagnostic.Failure f; _} ->
    Conflicts (univ, version_map, Conflict_dep f)
  | {Algo.Diagnostic.result = Algo.Diagnostic.Success _; _} ->
    raise (Invalid_argument "make_conflicts")
let cycle_conflict ~version_map univ cycle =
  Conflicts (univ, version_map, Conflict_cycle cycle)

let arrow_concat sl =
  let arrow =
    if OpamConsole.utf8 () then " \xe2\x86\x92 " (* U+2192 *)
    else " -> "
  in
  String.concat (OpamConsole.colorise `yellow arrow) sl

let strings_of_reasons packages cudfnv2opam unav_reasons rs =
  let open Algo.Diagnostic in
  let is_base cpkg = cpkg.Cudf.keep = `Keep_version in
  let rec aux = function
    | [] -> []
    | Conflict (i,j,jc)::rs ->
      if is_dose_request i || is_dose_request j then
        let a = if is_dose_request i then j else i in
        if is_dose_request a then aux rs else
        if is_base a then
          let str =
            Printf.sprintf "Package %s is part of the base for this compiler \
                            and can't be changed"
              (OpamPackage.name_to_string (cudf2opam a)) in
          str :: aux rs
        else
        let str =
          Printf.sprintf "Conflicting query for package %s"
            (OpamPackage.to_string (cudf2opam a)) in
        str :: aux rs
      else
      if i.Cudf.package = j.Cudf.package then
        if is_base i || is_base j then
          let str =
            Printf.sprintf "Package %s is part of the base for this compiler \
                            and can't be changed"
              (OpamPackage.name_to_string (cudf2opam i)) in
          str :: aux rs
        else
        let str = Printf.sprintf "No available version of %s satisfies the \
                                  constraints"
            (OpamPackage.name_to_string (cudf2opam i)) in
        str :: aux rs
      else
      let nva = cudf2opam i in
      let versions, rs =
        List.fold_left (fun (versions, rs) -> function
            | Conflict (i1, _, jc1) when
                (cudf2opam i1).name = nva.name && jc1 = jc ->
              (cudf2opam i1).version :: versions, rs
            | r -> versions, r::rs)
          ([nva.version], []) rs
      in
      let rs = List.rev rs in
      let formula =
        OpamFormula.ors (List.map (function v -> Atom (`Eq, v)) versions)
      in
      let all_versions = OpamPackage.versions_of_name packages nva.name in
      let formula = OpamFormula.simplify_version_set all_versions formula in
      let str = Printf.sprintf "%s is in conflict with %s"
          (OpamFormula.to_string (Atom (nva.name, formula)))
          (OpamFormula.short_string_of_atom (vpkg2atom cudfnv2opam jc))
      in
      str :: aux rs
    | Missing (p,missing) :: rs when is_dose_request p ->
      (* Requested pkg missing *)
      let atoms = List.map (vpkg2atom cudfnv2opam) missing in
      let names = OpamStd.List.sort_nodup compare (List.map fst atoms) in
      List.map (fun name ->
          let formula =
            OpamFormula.ors (List.map (function
                | n, Some atom when n = name -> Atom atom
                | _ -> Empty)
                atoms)
          in
          let all_versions = OpamPackage.versions_of_name packages name in
          let formula = OpamFormula.simplify_version_set all_versions formula in
          unav_reasons (name, formula))
        names @
      aux rs
    | Missing _ :: rs (* dependency missing, treated in strings_of_chains *)
    | Dependency _ :: rs -> aux rs
  in
  aux rs


let make_chains packages cudfnv2opam depends =
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
        | Missing (i, vpkgl) when not (is_dose_request i) ->
          let jl =
            List.map (fun (package,_) ->
                {Cudf.default_package with Cudf.package})
              vpkgl in
          Set.add i roots,
          notroots,
          map_addlist i jl deps,
          map_addlist i vpkgl vpkgs
        | _ -> roots, notroots, deps, vpkgs)
      (Set.empty,Set.empty,Map.empty,Map.empty)
      depends
  in
  let roots = Set.diff roots notroots in
  if Set.is_empty roots then [] else
  let children cpkgs =
    Set.fold (fun c acc ->
        List.fold_left (fun m a -> Set.add a m) acc
          (try Map.find c deps with Not_found -> []))
      cpkgs Set.empty
  in
  let rec aux constrs direct_deps =
    if Set.is_empty direct_deps then [[]] else
    let depnames =
      Set.fold (fun p set -> OpamStd.String.Set.add p.Cudf.package set)
        direct_deps OpamStd.String.Set.empty in
    OpamStd.String.Set.fold (fun name acc ->
        let name_deps = (* Gather all deps with the given name *)
          Set.filter (fun p -> p.Cudf.package = name) direct_deps in
        let name_constrs =
          List.map (List.filter (fun (n,_) -> n = name)) constrs in
        let to_opam_constr p =
          snd (vpkg2opam cudfnv2opam p)
        in
        let formula =
          OpamFormula.ors
            (List.map (fun conj ->
                 OpamFormula.ands (List.map to_opam_constr conj))
                name_constrs)
        in
        let opam_name =
          OpamPackage.Name.of_string (Common.CudfAdd.decode name)
        in
        let all_versions = OpamPackage.versions_of_name packages opam_name in
        let formula = OpamFormula.simplify_version_set all_versions formula in
        let formula = opam_name, formula in
        let children_constrs =
          List.map (fun p -> try Map.find p vpkgs with Not_found -> [])
            (Set.elements name_deps) in
        let chains = aux children_constrs (children name_deps) in
        List.fold_left
          (fun acc chain -> (formula :: chain) :: acc)
          acc chains
      )
      depnames []
  in
  let start_constrs =
    let set =
      Set.fold (fun p acc -> OpamStd.String.Set.add p.Cudf.package acc)
        roots OpamStd.String.Set.empty in
    List.map (fun name -> [name,None]) (OpamStd.String.Set.elements set) in
  aux start_constrs roots

let strings_of_final_reasons packages cudfnv2opam unav_reasons reasons =
  let reasons =
    strings_of_reasons packages cudfnv2opam unav_reasons reasons
  in
  OpamStd.List.sort_nodup compare reasons

let strings_of_chains packages cudfnv2opam unav_reasons reasons =
  let chains = make_chains packages cudfnv2opam reasons in
  let string_of_chain c =
    match List.rev c with
    | (name, vform) :: _ ->
      let all_versions = OpamPackage.versions_of_name packages name in
      let formula = OpamFormula.simplify_version_set all_versions vform in
      arrow_concat (List.map (fun c -> OpamFormula.to_string (Atom c)) c)
      ^ (match unav_reasons (name, formula) with "" -> "" | s -> "\n" ^ s)
    | [] -> ""
  in
  List.map string_of_chain chains

let strings_of_cycles cycles =
  List.map arrow_concat cycles

let strings_of_conflict packages unav_reasons = function
  | univ, version_map, Conflict_dep reasons ->
    let r = reasons () in
    let cudfnv2opam = cudfnv2opam ~cudf_universe:univ ~version_map in
    strings_of_final_reasons packages cudfnv2opam unav_reasons r,
    strings_of_chains packages cudfnv2opam unav_reasons r,
    []
  | _univ, _version_map, Conflict_cycle cycles ->
    [], [], strings_of_cycles cycles

let conflict_chains packages = function
  | cudf_universe, version_map, Conflict_dep r ->
    make_chains packages (cudfnv2opam ~cudf_universe ~version_map) (r ())
  | _ -> []

let string_of_conflict packages unav_reasons conflict =
  let final, chains, cycles =
    strings_of_conflict packages unav_reasons conflict
  in
  let b = Buffer.create 1024 in
  let pr_items b l =
    Buffer.add_string b (OpamStd.Format.itemize (fun s -> s) l)
  in
  if cycles <> [] then
    Printf.bprintf b
      "The actions to process have cyclic dependencies:\n%a"
      pr_items cycles;
  if chains <> [] then
    Printf.bprintf b
      "The following dependencies couldn't be met:\n%a"
      pr_items chains;
  if final <> [] then
    Printf.bprintf b
      "Your request can't be satisfied:\n%a"
      pr_items final;
  if final = [] && chains = [] && cycles = [] then (* No explanation found *)
    Printf.bprintf b
      "Sorry, no solution found: \
       there seems to be a problem with your request.\n";
  Buffer.add_string b "\n";
  Buffer.contents b

let check flag p =
  try Cudf.lookup_typed_package_property p flag = `Bool true
  with Not_found -> false

let need_reinstall = check s_reinstall

(*
let is_installed_root = check s_installed_root

let is_pinned = check s_pinned
*)

let default_preamble =
  let l = [
    (s_source,         `String None) ;
    (s_source_number,  `String None);
    (s_reinstall,      `Bool (Some false));
    (s_installed_root, `Bool (Some false));
    (s_pinned,         `Bool (Some false));
    (s_version_lag,    `Nat (Some 0));
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
  Common.CudfAdd.add_properties default_preamble
    (List.map (fun s -> s, `Int (Some 0)) req.extra_attributes),
  univ,
  { Cudf.request_id = "opam";
    install         = req.wish_install;
    remove          = req.wish_remove;
    upgrade         = req.wish_upgrade;
    req_extra       = [] }
)



let string_of_request r =
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (string_of_vpkgs r.wish_install)
    (string_of_vpkgs r.wish_remove)
    (string_of_vpkgs r.wish_upgrade)

let solver_calls = ref 0

let dump_universe oc univ =
  Cudf_printer.pp_cudf oc
    (default_preamble, univ, Cudf.default_request)

let dump_cudf_request ~version_map (_, univ,_ as cudf) criteria =
  function
  | None   -> None
  | Some f ->
    ignore ( version_map: int OpamPackage.Map.t );
    incr solver_calls;
    let filename = Printf.sprintf "%s-%d.cudf" f !solver_calls in
    let oc = open_out filename in
    let module Solver = (val OpamSolverConfig.(Lazy.force !r.solver)) in
    Printf.fprintf oc "# Solver: %s\n" Solver.name;
    Printf.fprintf oc "# Criteria: %s\n" criteria;
    Cudf_printer.pp_cudf oc cudf;
    OpamPackage.Map.iter (fun (pkg:OpamPackage.t) (vnum: int) ->
      let name = OpamPackage.name_to_string pkg in
      let version = OpamPackage.version_to_string pkg in
      Printf.fprintf oc "#v2v:%s:%d=%s\n" name vnum version;
    ) version_map;
    close_out oc;
    Graph.output (Graph.of_universe univ) f;
    Some filename

let dump_cudf_error ~version_map univ req =
  let cudf_file = match OpamSolverConfig.(!r.cudf_file) with
    | Some f -> f
    | None ->
      let (/) = Filename.concat in
      OpamCoreConfig.(!r.log_dir) /
      ("solver-error-"^string_of_int (Unix.getpid())) in
  match
    dump_cudf_request (to_cudf univ req) ~version_map
      (OpamSolverConfig.criteria req.criteria)
      (Some cudf_file)
  with
  | Some f -> f
  | None -> assert false

let call_external_solver ~version_map univ req =
  let cudf_request = to_cudf univ req in
  if Cudf.universe_size univ > 0 then
    let criteria = OpamSolverConfig.criteria req.criteria in
    let chrono = OpamConsole.timer () in
    ignore (dump_cudf_request ~version_map cudf_request
              criteria OpamSolverConfig.(!r.cudf_file));
    try
      let r =
        Algo.Depsolver.check_request_using
          ~call_solver:(OpamSolverConfig.call_solver ~criteria)
          ~criteria ~explain:true cudf_request
      in
      log "Solver call done in %.3f" (chrono ());
      r
    with e ->
      OpamStd.Exn.fatal e;
      OpamConsole.warning "Solver failed:";
      OpamConsole.errmsg "%s\n" (Printexc.to_string e);
      raise Solver_failure
  else
    Algo.Depsolver.Sat(None,Cudf.load_universe [])

let check_request ?(explain=true) ~version_map univ req =
  match Algo.Depsolver.check_request ~explain (to_cudf univ req) with
  | Algo.Depsolver.Unsat
      (Some ({Algo.Diagnostic.result = Algo.Diagnostic.Failure _; _} as r)) ->
    make_conflicts ~version_map univ r
  | Algo.Depsolver.Sat (_,u) -> Success (remove u "dose-dummy-request" None)
  | Algo.Depsolver.Error msg ->
    let f = dump_cudf_error ~version_map univ req in
    OpamConsole.error "Internal solver failed with %s Request saved to %S"
      msg f;
    raise Solver_failure
  | Algo.Depsolver.Unsat _ -> (* normally when [explain] = false *)
    conflict_empty ~version_map univ

(* Return the universe in which the system has to go *)
let get_final_universe ~version_map univ req =
  let fail msg =
    let f = dump_cudf_error ~version_map univ req in
    OpamConsole.warning "External solver failed with %s Request saved to %S"
      msg f;
    raise Solver_failure in
  match call_external_solver ~version_map univ req with
  | Algo.Depsolver.Sat (_,u) -> Success (remove u "dose-dummy-request" None)
  | Algo.Depsolver.Error "(CRASH) Solution file is empty" ->
    (* XXX Is this still needed with latest dose ? *)
    Success (Cudf.load_universe [])
  | Algo.Depsolver.Error str -> fail str
  | Algo.Depsolver.Unsat r   ->
    match r with
    | Some ({Algo.Diagnostic.result = Algo.Diagnostic.Failure _; _} as r) ->
      make_conflicts ~version_map univ r
    | Some {Algo.Diagnostic.result = Algo.Diagnostic.Success _; _}(*  -> *)
      (* fail "inconsistent return value." *)
    | None ->
      (* External solver did not provide explanations, hopefully this will *)
      check_request ~version_map univ req

let diff univ sol =
  let before =
    Set.of_list
      (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) univ)
  in
  let after =
    Set.of_list
      (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) sol)
  in
  let open Set.Op in
  let reinstall = Set.filter need_reinstall after in
  let install = after -- before ++ reinstall in
  let remove = before -- after ++ reinstall in
  install, remove

(* Transform a diff from current to final state into a list of
   actions. At this point, we don't know about the root causes of the
   actions, they will be computed later. *)
let actions_of_diff (install, remove) =
  let actions = [] in
  let actions = Set.fold (fun p acc -> `Install p :: acc) install actions in
  let actions = Set.fold (fun p acc -> `Remove p :: acc) remove actions in
  actions

let resolve ~extern ~version_map universe request =
  log "resolve request=%a" (slog string_of_request) request;
  if extern then get_final_universe ~version_map universe request
  else check_request ~version_map universe request

let to_actions f universe result =
  let aux u1 u2 =
    let diff = diff (f u1) u2 in
    actions_of_diff diff
  in
  map_success (aux universe) result

let create_graph filter universe =
  let pkgs = Cudf.get_packages ~filter universe in
  let u = Cudf.load_universe pkgs in
  Graph.of_universe u

let find_cycles g =
  let open ActionGraph in
  let roots =
    fold_vertex (fun v acc -> if in_degree g v = 0 then v::acc else acc) g [] in
  let roots =
    if roots = [] then fold_vertex (fun v acc -> v::acc) g []
    else roots in
  let rec prefix_find acc v = function
    | x::_ when x = v -> Some (x::acc)
    | x::r -> prefix_find (x::acc) v r
    | [] -> None in
  let seen = Hashtbl.create 17 in
  let rec follow v path =
    match prefix_find [] v path with
    | Some cycle ->
      Hashtbl.add seen v ();
      [cycle@[v]]
    | None ->
      if Hashtbl.mem seen v then [] else
        let path = v::path in
        Hashtbl.add seen v ();
        List.fold_left (fun acc s -> follow s path @ acc) []
          (succ g v) in
  List.fold_left (fun cycles root ->
    follow root [] @ cycles
  ) [] roots

(* Compute the original causes of the actions, from the original set of
   packages in the user request. In the restricted dependency graph, for each
   action we find the closest package belonging to the user request and print
   out the closest neighbour that gets there. This way, if a -> b -> c and the
   user requests a to be installed, we can print:
   - install a - install b [required by a] - intall c [required by b] *)
let compute_root_causes g requested reinstall =
  let module StringSet = OpamStd.String.Set in
  let requested_pkgnames =
    OpamPackage.Name.Set.fold (fun n s ->
        StringSet.add (Common.CudfAdd.encode (OpamPackage.Name.to_string n)) s)
      requested StringSet.empty in
  let reinstall_pkgnames =
    OpamPackage.Set.fold (fun nv s ->
        StringSet.add (Common.CudfAdd.encode (OpamPackage.name_to_string nv)) s)
      reinstall StringSet.empty in
  let actions =
    ActionGraph.fold_vertex (fun a acc -> Map.add (action_contents a) a acc)
      g Map.empty in
  let requested_actions =
    Map.filter (fun pkg _ ->
        StringSet.mem pkg.Cudf.package requested_pkgnames)
      actions in
  let merge_causes (c1,depth1) (c2,depth2) =
    (* When we found several causes explaining the same action, only keep the
       most likely one *)
    if c2 = Unknown || depth1 < depth2 then c1, depth1 else
    if c1 = Unknown || depth2 < depth1 then c2, depth2 else
    let (@) =
      List.fold_left (fun l a -> if List.mem a l then l else a::l)
    in
    match c1, c2 with
    | Required_by a, Required_by b -> Required_by (a @ b), depth1
    | Use a, Use b -> Use (a @ b), depth1
    | Conflicts_with a, Conflicts_with b -> Conflicts_with (a @ b), depth1
    | Requested, a | a, Requested
    | Unknown, a | a, Unknown
    | Upstream_changes , a | a, Upstream_changes -> a, depth1
    | _, c -> c, depth1
  in
  let direct_cause consequence order cause =
    (* Investigate the reason of action [consequence], that was possibly
       triggered by [cause], where the actions are ordered as [consequence]
       [order] [cause]. *)
    match consequence, order, cause with
    | (`Install _ | `Change _), `Before, (`Install p | `Change (_,_,p)) ->
      (* Prerequisite *)
      Required_by [p]
    | `Change _, `After, (`Install p | `Change (_,_,p)) ->
      (* Change caused by change in dependencies *)
      Use [p]
    | `Reinstall _, `After, a ->
      (* Reinstall caused by action on deps *)
      Use [action_contents a]
    | (`Remove _ | `Change _ ), `Before, `Remove p ->
      (* Removal or change caused by the removal of a dependency *)
      Use [p]
    | `Remove _, `Before, (`Install p | `Change (_,_,p) | `Reinstall p) ->
      (* Removal caused by conflict *)
      Conflicts_with [p]
    | (`Install _ | `Change _), `Before, `Reinstall p ->
      (* New dependency of p ? *)
      Required_by [p]
    | `Change _, _, _ ->
      (* The only remaining cause for changes is upstream *)
      Upstream_changes
    | (`Install _ | `Remove _), `After, _  ->
      (* Nothing can cause these actions after itself *)
      Unknown
    | (`Install _ | `Reinstall _), `Before, _ ->
      (* An install or reinstall doesn't cause any oter actions on its
         dependendants *)
      Unknown
    | `Build _, _, _ | _, _, `Build _ -> assert false
  in
  let get_causes acc roots =
    let rec aux seen depth pkgname causes =
      if depth > 100 then
        (OpamConsole.error
           "Internal error computing action causes: sorry, please report.";
         causes)
      else
      let action = Map.find pkgname actions in
      let seen = Set.add pkgname seen in
      let propagate causes actions direction =
        List.fold_left (fun causes act ->
            let p = action_contents act in
            if Set.mem p seen then causes else
            let cause = direct_cause act direction action in
            if cause = Unknown then causes else
            try
              Map.add p (merge_causes (cause,depth) (Map.find p causes)) causes
            with Not_found ->
              aux seen (depth + 1) p (Map.add p (cause,depth) causes)
          ) causes actions in
      let causes = propagate causes (ActionGraph.pred g action) `Before in
      let causes = propagate causes (ActionGraph.succ g action) `After in
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
      if Map.is_empty requested_actions then (* Assume a global upgrade *)
        make_roots causes Requested (function
            | `Change (`Up,_,_) -> true
            | _ -> false)
      else (Map.map (fun _ -> Requested, 0) requested_actions) in
    get_causes causes roots in
  let causes =
    (* Compute causes for remaining upgrades
       (maybe these could be removed from the actions altogether since they are
       unrelated to the request ?) *)
    let roots = make_roots causes Unknown (function
        | `Change _ as act ->
          List.for_all
            (function `Change _ -> false | _ -> true)
            (ActionGraph.pred g act)
        | _ -> false) in
    get_causes causes roots in
  let causes =
    (* Compute causes for marked reinstalls *)
    let roots =
      make_roots causes Upstream_changes (function
          | `Reinstall p ->
            (* need_reinstall p is not available here *)
            StringSet.mem p.Cudf.package reinstall_pkgnames
          | _ -> false)
    in
    get_causes causes roots in
  Map.map fst causes

(* Compute a full solution from a set of root actions. This means adding all
   required reinstallations and computing the graph of dependency of required
   actions *)
let atomic_actions ~simple_universe ~complete_universe root_actions =
  log "graph_of_actions root_actions=%a"
    (slog string_of_actions) root_actions;

  let to_remove, to_install =
    List.fold_left (fun (rm,inst) a -> match a with
        | `Change (_,p1,p2) ->
          Set.add p1 rm, Set.add p2 inst
        | `Install p -> rm, Set.add p inst
        | `Reinstall p -> Set.add p rm, Set.add p inst
        | `Remove p -> Set.add p rm, inst)
      (Set.empty, Set.empty) root_actions in

  (* transitively add recompilations *)
  let to_remove, to_install =
    let packages = Set.union to_remove to_install in
    let package_graph =
      let filter p = p.Cudf.installed || Set.mem p packages in
      Graph.mirror (create_graph filter simple_universe)
    in
    Graph.Topo.fold (fun p (rm,inst) ->
        let actionned p = Set.mem p rm || Set.mem p inst in
        if not (actionned p) &&
           List.exists actionned (Graph.pred package_graph p)
        then Set.add p rm, Set.add p inst
        else rm, inst)
      package_graph (to_remove, to_install)
  in
  let pkggraph set = create_graph (fun p -> Set.mem p set) complete_universe in

  (* Build the graph of atomic actions: Removals or installs *)
  let g = ActionGraph.create () in
  Set.iter (fun p -> ActionGraph.add_vertex g (`Remove p)) to_remove;
  Set.iter (fun p -> ActionGraph.add_vertex g (`Install (p))) to_install;
  (* reinstalls and upgrades: remove first *)
  Set.iter
    (fun p1 ->
       try
         let p2 =
           Set.find (fun p2 -> p1.Cudf.package = p2.Cudf.package) to_install
         in
         ActionGraph.add_edge g (`Remove p1) (`Install (p2))
       with Not_found -> ())
    to_remove;
  (* uninstall order *)
  Graph.iter_edges (fun p1 p2 ->
      ActionGraph.add_edge g (`Remove p1) (`Remove p2)
    ) (pkggraph to_remove);
  (* install order *)
  Graph.iter_edges (fun p1 p2 ->
      if Set.mem p1 to_install then
        let cause =
          if Set.mem p2 to_install then `Install ( p2) else `Remove p2
        in
        ActionGraph.add_edge g cause (`Install ( p1))
    ) (pkggraph (Set.union to_install to_remove));
  (* conflicts *)
  let conflicts_graph =
    let filter p = Set.mem p to_remove || Set.mem p to_install in
    Algo.Defaultgraphs.PackageGraph.conflict_graph
      (Cudf.load_universe (Cudf.get_packages ~filter complete_universe))
  in
  Algo.Defaultgraphs.PackageGraph.UG.iter_edges (fun p1 p2 ->
      if Set.mem p1 to_remove && Set.mem p2 to_install then
        ActionGraph.add_edge g (`Remove p1) (`Install ( p2))
      else if Set.mem p2 to_remove && Set.mem p1 to_install then
        ActionGraph.add_edge g (`Remove p2) (`Install ( p1)))
    conflicts_graph;
  (* check for cycles *)
  match find_cycles g with
  | [] -> g
  | cycles -> raise (Cyclic_actions cycles)

let packages u = Cudf.get_packages u
