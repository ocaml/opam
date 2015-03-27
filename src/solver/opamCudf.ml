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

(* custom cudf field labels *)
let s_source = "opam-name"
let s_source_number = "opam-version"
let s_reinstall = "reinstall"
let s_installed_root = "installed-root"
let s_pinned = "pinned"

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
          (fun nv cv -> OpamPackage.name nv = name && cv = v)
          vmap
      in
      fst (OpamPackage.Map.choose nvset)
    | None -> raise Not_found

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

exception Cyclic_actions of Action.t list list

type conflict_case =
  | Conflict_dep of (unit -> Algo.Diagnostic.reason list)
  | Conflict_cycle of string list list
type conflict =
  Cudf.universe * int package_map * conflict_case

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
  Printf.sprintf "install:%s remove:%s upgrade:%s criteria:%S"
    (string_of_vpkgs r.wish_install)
    (string_of_vpkgs r.wish_remove)
    (string_of_vpkgs r.wish_upgrade)
    (OpamGlobals.get_solver_criteria r.criteria)

let string_of_universe u =
  string_of_packages (List.sort Common.CudfAdd.compare (Cudf.get_packages u))

let vpkg2atom cudfnv2opam (name,cstr) =
  match cstr with
  | None ->
    OpamPackage.Name.of_string (Common.CudfAdd.decode name), None
  | Some (relop,v) ->
    try
      let nv = cudfnv2opam (name,v) in
      OpamPackage.name nv, Some (relop, OpamPackage.version nv)
    with Not_found -> assert false
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
  | {Algo.Diagnostic.result = Algo.Diagnostic.Failure f} ->
    Conflicts (univ, version_map, Conflict_dep f)
  | {Algo.Diagnostic.result = Algo.Diagnostic.Success _} ->
    raise (Invalid_argument "make_conflicts")
let cycle_conflict ~version_map univ cycle =
  Conflicts (univ, version_map, Conflict_cycle cycle)

let arrow_concat =
  String.concat (OpamGlobals.colorise `yellow " -> ")

let print_cycles cycles =
  Printf.sprintf
    "The actions to process have cyclic dependencies:\n%s"
    (OpamMisc.itemize arrow_concat cycles)

let strings_of_reason cudfnv2opam (unav_reasons: atom -> string) r =
  let open Algo.Diagnostic in
  let is_base cpkg = cpkg.Cudf.keep = `Keep_version in
  match r with
  | Conflict (i,j,_) ->
    if is_dose_request i || is_dose_request j then
      let a = if is_dose_request i then j else i in
      if is_dose_request a then [] else
      if is_base a then
        let str =
          Printf.sprintf "Package %s is part of the base for this compiler \
                          and can't be changed"
            (OpamPackage.name_to_string (cudf2opam a)) in
        [str]
      else
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
      if is_base j then
        let str =
          Printf.sprintf "Package %s is part of the base for this compiler \
                          and can't be changed"
            (OpamPackage.name_to_string nvb) in
        [str]
      else
      let str = Printf.sprintf "Conflicting version constraints for %s"
          (OpamPackage.name_to_string nva) in
      [str]
    else
    let str = Printf.sprintf "%s is in conflict with %s"
        (OpamPackage.to_string nva)
        (OpamPackage.to_string nvb) in
    [str]
  | Missing (p,missing) when is_dose_request p -> (* Requested pkg missing *)
    List.map (fun p ->
        unav_reasons (vpkg2atom cudfnv2opam p)
      ) missing
  | Missing (_,missing) -> (* Dependencies missing *)
    List.map (fun m -> unav_reasons (vpkg2atom cudfnv2opam m))
      missing
  | Dependency _  -> []


let make_chains cudfnv2opam depends =
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
      Set.fold (fun p set -> OpamMisc.StringSet.add p.Cudf.package set)
        direct_deps OpamMisc.StringSet.empty in
    OpamMisc.StringSet.fold (fun name acc ->
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
        let formula = OpamFormula.simplify_version_formula formula in
        let opam_name =
          OpamPackage.Name.of_string (Common.CudfAdd.decode name)
        in
        let formula = Atom (opam_name, formula) in
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
      Set.fold (fun p acc -> OpamMisc.StringSet.add p.Cudf.package acc)
        roots OpamMisc.StringSet.empty in
    List.map (fun name -> [name,None]) (OpamMisc.StringSet.elements set) in
  aux start_constrs roots

let strings_of_final_reasons cudfnv2opam unav_reasons reasons =
  let reasons =
    List.flatten
      (List.map
         (strings_of_reason cudfnv2opam unav_reasons)
         reasons) in
  OpamMisc.StringSet.(elements (of_list reasons))

let strings_of_chains cudfnv2opam reasons =
  let chains = make_chains cudfnv2opam reasons in
  let string_of_chain c =
    arrow_concat (List.map OpamFormula.to_string c) in
  List.map string_of_chain chains

let strings_of_cycles cycles =
  List.map arrow_concat cycles

let strings_of_conflict unav_reasons = function
  | univ, version_map, Conflict_dep reasons ->
    let r = reasons () in
    let cudfnv2opam = cudfnv2opam ~cudf_universe:univ ~version_map in
    strings_of_final_reasons cudfnv2opam unav_reasons r,
    strings_of_chains cudfnv2opam r,
    []
  | _univ, _version_map, Conflict_cycle cycles ->
    [], [], strings_of_cycles cycles

let string_of_conflict unav_reasons conflict =
  let final, chains, cycles = strings_of_conflict unav_reasons conflict in
  let b = Buffer.create 1024 in
  let pr_items b l = List.iter (Printf.bprintf b "  - %s\n") l in
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

let is_installed_root = check s_installed_root

let is_pinned = check s_pinned

let default_preamble =
  let l = [
    (s_source,         `String None) ;
    (s_source_number,  `String None);
    (s_reinstall,      `Bool (Some false));
    (s_installed_root, `Bool (Some false));
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

let external_solver_name () =
  match OpamGlobals.external_solver ~input:"" ~output:"" ~criteria:"" with
  | cmd::_ -> cmd
  | [] -> raise Not_found

let external_solver_exists =
  lazy (
    try
      let name = external_solver_name () in
      let ex = OpamSystem.command_exists name in
      if not ex && !OpamGlobals.use_external_solver &&
         !OpamGlobals.env_external_solver <> None
      then
        OpamGlobals.error_and_exit
          "Your configuration or environment specifies external solver %s, but \
           it cannot be found. Fix your installation, your configuration or \
           use '--use-internal-solver'."
          name
      else ex
    with Not_found -> false
  )

let external_solver_available () =
  !OpamGlobals.use_external_solver && (Lazy.force external_solver_exists)

let solver_calls = ref 0

let dump_universe oc univ =
  Cudf_printer.pp_cudf oc
    (default_preamble, univ, Cudf.default_request)

let dump_cudf_request ~extern ~version_map (_, univ,_ as cudf) criteria =
  function
  | None   -> None
  | Some f ->
    ignore ( version_map: int OpamPackage.Map.t );
    incr solver_calls;
    let filename = Printf.sprintf "%s-%d.cudf" f !solver_calls in
    let oc = open_out filename in
    if extern then
      Printf.fprintf oc "# %s\n"
        (String.concat " "
           (OpamGlobals.external_solver ~input:"$in" ~output:"$out" ~criteria))
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
  match
    dump_cudf_request ~extern (to_cudf univ req) ~version_map
      (OpamGlobals.get_solver_criteria req.criteria)
      (Some cudf_file)
  with
  | Some f -> f
  | None -> assert false

let dose_solver_callback ~criteria (_,universe,_ as cudf) =
  let solver_in =
    OpamFilename.of_string (OpamSystem.temp_file "solver-in") in
  let solver_out =
    OpamFilename.of_string (OpamSystem.temp_file "solver-out") in
  try
    let _ =
      let oc = OpamFilename.open_out solver_in in
      Cudf_printer.pp_cudf oc cudf;
      close_out oc
    in
    OpamSystem.command
      ~verbose:(!OpamGlobals.debug_level >= 2)
      (OpamGlobals.external_solver
         ~input:(OpamFilename.to_string solver_in)
         ~output:(OpamFilename.to_string solver_out)
         ~criteria);
    OpamFilename.remove solver_in;
    if not (OpamFilename.exists solver_out) then
      raise (Common.CudfSolver.Error "no output")
    else if
      (let ic = OpamFilename.open_in solver_out in
       try
         let i = input_line ic in close_in ic;
         i = "FAIL"
       with End_of_file -> close_in ic; false)
    then
      raise Common.CudfSolver.Unsat
    else
    let r =
      Cudf_parser.load_solution_from_file
        (OpamFilename.to_string solver_out) universe in
    OpamFilename.remove solver_out;
    if Cudf.universe_size (snd r) = 0 &&
       not !OpamGlobals.no_base_packages &&
       Cudf.installed_size universe <> 0
    then
      raise (Common.CudfSolver.Error "empty solution");
    r
  with e ->
    OpamFilename.remove solver_in;
    OpamFilename.remove solver_out;
    raise e

let check_cudf_version =
  let r = lazy (
    if external_solver_available () then
      try
        log "Checking version of criteria accepted by the external solver";
        (* Run with closed stdin to workaround bug in some solver scripts *)
        match
          OpamSystem.read_command_output ~verbose:false ~allow_stdin:false
            [external_solver_name (); "-v"]
        with
        | [] ->
          log "No response from 'solver -v', using compat criteria";
          `Compat
        | s::_ ->
          match OpamMisc.split s ' ' with
          | "aspcud"::_::v::_ when Debian.Version.compare v "1.9" >= 0 ->
            log "Solver is aspcud > 1.9: using latest version criteria";
            `Latest
          | _ ->
            log "Solver isn't aspcud > 1.9, using compat criteria";
            `Compat
      with OpamSystem.Process_error _ ->
        log "Solver doesn't know about '-v': using compat criteria";
        `Compat
    else
      `Compat (* don't care, internal solver doesn't handle them *)
  )
  in
  fun () -> Lazy.force r

let call_external_solver ~version_map univ req =
  let cudf_request = to_cudf univ req in
  if Cudf.universe_size univ > 0 then
    let criteria = OpamGlobals.get_solver_criteria req.criteria in
    ignore (dump_cudf_request ~extern:true ~version_map cudf_request
              criteria !OpamGlobals.cudf_file);
    try
      Algo.Depsolver.check_request_using
        ~call_solver:(dose_solver_callback ~criteria)
        ~criteria ~explain:true cudf_request
    with e ->
      OpamMisc.fatal e;
      OpamGlobals.warning "External solver failed:";
      OpamGlobals.errmsg "%s\n" (Printexc.to_string e);
      failwith "opamSolver"
  else
    Algo.Depsolver.Sat(None,Cudf.load_universe [])

let check_request ?(explain=true) ~version_map univ req =
  match Algo.Depsolver.check_request ~explain (to_cudf univ req) with
  | Algo.Depsolver.Unsat
      (Some ({Algo.Diagnostic.result = Algo.Diagnostic.Failure _} as r)) ->
    make_conflicts ~version_map univ r
  | Algo.Depsolver.Sat (_,u) -> Success (remove u "dose-dummy-request" None)
  | Algo.Depsolver.Error msg ->
    let f = dump_cudf_error ~extern:false ~version_map univ req in
    OpamGlobals.error "Internal solver failed with %s Request saved to %S"
      msg f;
    failwith "opamSolver"
  | Algo.Depsolver.Unsat _ -> (* normally when [explain] = false *)
    conflict_empty ~version_map univ

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
    | Some ({result=Failure _} as r) -> make_conflicts ~version_map univ r
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
let compute_root_causes g requested =
  let module StringSet = OpamMisc.StringSet in
  let module StringMap = OpamMisc.StringMap in
  let requested_pkgnames =
    OpamPackage.Name.Set.fold (fun n s ->
        StringSet.add (Common.CudfAdd.encode (OpamPackage.Name.to_string n)) s)
      requested StringSet.empty in
  let actions =
    ActionGraph.fold_vertex (fun a acc -> Map.add (action_contents a) a acc)
      g Map.empty in
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
    | a,              To_recompile _,      `Depends  -> Use [action_contents a]
    | _,              To_recompile _,      `Provides -> Unknown
    | To_delete p,    To_delete _,         `Provides -> Use [p]
    | To_delete p,    To_change _,         `Provides -> Use [p]
    | To_recompile p, To_change _,         `Provides -> Required_by [p]
    | _,              To_change(None,_),   `Depends  -> Unknown
    | _,              To_change _,         _         -> Upstream_changes
    | To_change _,    To_delete _,         `Provides -> Conflicts_with
                                                          [action_contents cause]
    | To_recompile p, To_delete _,         `Provides -> Conflicts_with [p]
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
        if Map.is_empty requested_actions then (* Assume a global upgrade *)
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
      (* Compute causes for packages marked to reinstall *)
      let roots =
        make_roots causes Upstream_changes
          (function To_recompile p -> need_reinstall p
                  | _ -> false) in
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
        | To_change (Some p1,p2) -> Set.add p1 rm, Set.add p2 inst
        | To_change (None,p) -> rm, Set.add p inst
        | To_recompile p -> Set.add p rm, Set.add p inst
        | To_delete p -> Set.add p rm, inst)
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
  Set.iter (fun p -> ActionGraph.add_vertex g (To_delete p)) to_remove;
  Set.iter (fun p -> ActionGraph.add_vertex g (To_change (None,p))) to_install;
  (* reinstalls and upgrades: remove first *)
  Set.iter
    (fun p1 ->
       try
         let p2 =
           Set.find (fun p2 -> p1.Cudf.package = p2.Cudf.package) to_install
         in
         ActionGraph.add_edge g (To_delete p1) (To_change (None,p2))
       with Not_found -> ())
    to_remove;
  (* uninstall order *)
  Graph.iter_edges (fun p1 p2 ->
      ActionGraph.add_edge g (To_delete p1) (To_delete p2)
    ) (pkggraph to_remove);
  (* install order *)
  Graph.iter_edges (fun p1 p2 ->
      if Set.mem p1 to_install then
        let cause =
          if Set.mem p2 to_install then To_change (None, p2) else To_delete p2
        in
        ActionGraph.add_edge g cause (To_change (None, p1))
    ) (pkggraph (Set.union to_install to_remove));
  (* conflicts *)
  let conflicts_graph =
    let filter p = Set.mem p to_remove || Set.mem p to_install in
    Algo.Defaultgraphs.PackageGraph.conflict_graph
      (Cudf.load_universe (Cudf.get_packages ~filter complete_universe))
  in
  Algo.Defaultgraphs.PackageGraph.UG.iter_edges (fun p1 p2 ->
      if Set.mem p1 to_remove && Set.mem p2 to_install then
        ActionGraph.add_edge g (To_delete p1) (To_change (None, p2))
      else if Set.mem p2 to_remove && Set.mem p1 to_install then
        ActionGraph.add_edge g (To_delete p2) (To_change (None, p1)))
    conflicts_graph;
  (* check for cycles *)
  match find_cycles g with
  | [] -> g
  | cycles -> raise (Cyclic_actions cycles)

let packages u = Cudf.get_packages u
