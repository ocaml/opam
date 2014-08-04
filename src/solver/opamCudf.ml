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

exception Cyclic_actions of Action.t list list

type conflict_case =
  | Conflict_dep of (unit -> Algo.Diagnostic.reason list)
  | Conflict_cycle of string list list
type conflict =
  Cudf.universe * conflict_case

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
  string_of_packages (List.sort compare (Cudf.get_packages u))

let vpkg2atom cudf2opam cudf_universe (name,cstr) =
  match cstr with
  | None ->
    OpamPackage.Name.of_string (Common.CudfAdd.decode name), None
  | Some (relop,v) ->
    try
      let nv = cudf2opam (Cudf.lookup_package cudf_universe (name,v)) in
      OpamPackage.name nv, Some (relop, OpamPackage.version nv)
    with Not_found ->
      log "Could not find corresponding version in cudf universe: %a"
        (slog string_of_atom) (name,cstr);
      let solutions =
        Cudf.lookup_packages ~filter:cstr cudf_universe name in
      if solutions = [] then
        OpamPackage.Name.of_string (Common.CudfAdd.decode name), None
      else
        let opam_sol = OpamPackage.Set.of_list (List.map cudf2opam solutions) in
        OpamPackage.name (OpamPackage.Set.choose opam_sol),
        match relop with
        | `Leq | `Lt ->
          Some (`Lt, OpamPackage.version (OpamPackage.Set.min_elt opam_sol))
        | `Geq | `Gt ->
          Some (`Gt, OpamPackage.version (OpamPackage.Set.max_elt opam_sol))
        | `Neq | `Eq -> None

let vpkg2opam cudf2opam cudf_universe vpkg =
  match vpkg2atom cudf2opam cudf_universe vpkg with
  | p, None -> p, Empty
  | p, Some (relop,v) -> p, Atom (relop, v)

let conflict_empty univ = Conflicts (univ, Conflict_dep (fun () -> []))
let make_conflicts univ = function
  | {Algo.Diagnostic.result = Algo.Diagnostic.Failure f} ->
    Conflicts (univ, Conflict_dep f)
  | {Algo.Diagnostic.result = Algo.Diagnostic.Success _} ->
    raise (Invalid_argument "make_conflicts")
let cycle_conflict univ cycle =
  Conflicts (univ, Conflict_cycle cycle)

let print_cycles cycles =
  Printf.sprintf
    "The actions to process have cyclic dependencies:\n  - %s\n"
    (String.concat "\n  - "
       (List.map
          (String.concat (OpamGlobals.colorise `yellow " -> "))
          cycles))

let strings_of_reason cudf2opam (unav_reasons: atom -> string) cudf_universe r =
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
  | Missing (p,missing) when is_dose_request p -> (* Requested pkg missing *)
    List.map (fun p ->
        unav_reasons (vpkg2atom cudf2opam cudf_universe p)
      ) missing
  | Missing (_,missing) -> (* Dependencies missing *)
    List.map (fun m -> unav_reasons (vpkg2atom cudf2opam cudf_universe m))
      missing
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

let strings_of_final_reasons cudf2opam unav_reasons cudf_universe reasons =
  let reasons =
    List.flatten
      (List.map
         (strings_of_reason cudf2opam unav_reasons cudf_universe)
         reasons) in
  OpamMisc.StringSet.(elements (of_list reasons))

let arrow_concat =
  String.concat (OpamGlobals.colorise `yellow " -> ")

let strings_of_chains cudf2opam cudf_universe reasons =
  let chains = make_chains cudf_universe cudf2opam reasons in
  let string_of_chain c =
    arrow_concat (List.map OpamFormula.to_string c) in
  List.map string_of_chain chains

let strings_of_cycles cycles =
  List.map arrow_concat cycles

let strings_of_conflict unav_reasons = function
  | univ, Conflict_dep reasons ->
    let r = reasons () in
    strings_of_final_reasons cudf2opam unav_reasons univ r,
    strings_of_chains cudf2opam univ r,
    []
  | _univ, Conflict_cycle cycles ->
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

let external_solver_available =
  let exists = lazy (OpamSystem.command_exists !OpamGlobals.external_solver) in
  fun () ->
  !OpamGlobals.use_external_solver && Lazy.force exists

let external_solver_command =
  let run = lazy (
    let cmd = !OpamGlobals.external_solver in
    let is_old_aspcud =
      (* Older aspcud exe is a shell script. Run it through 'bash -e' to be sure
         we catch all errors *)
      match OpamSystem.find_in_path cmd with
      | None -> false (* ?? *)
      | Some dir ->
        let f = Filename.concat dir cmd in
        try
          let ic = open_in_bin f in
          let bash_shebang = "#!/bin/bash" in
          let len = String.length bash_shebang in
          let buf = String.create len in
          really_input ic buf 0 len;
          close_in ic;
          buf = bash_shebang
        with e -> OpamMisc.fatal e; false
    in
    if is_old_aspcud then ["/bin/bash"; "-e"; cmd]
    else [cmd]
  ) in
  fun ~input ~output ~criteria ->
    Lazy.force run @ [
      OpamFilename.to_string input;
      OpamFilename.to_string output;
      criteria
    ]

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
        (String.concat " " (
            external_solver_command
              ~input:(OpamFilename.of_string "$in")
              ~output:(OpamFilename.of_string "$out") ~criteria))
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
      (external_solver_command ~input:solver_in ~output:solver_out ~criteria);
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
       Cudf.universe_size universe <> 0
    then
      raise (Common.CudfSolver.Error "empty solution");
    r
  with e ->
    OpamFilename.remove solver_in;
    OpamFilename.remove solver_out;
    raise e

let call_external_solver ~version_map univ req =
  let cudf_request = to_cudf univ req in
  if Cudf.universe_size univ > 0 then begin
    let rec attempt () =
      let criteria = OpamGlobals.get_solver_criteria req.criteria in
      ignore (dump_cudf_request ~extern:true ~version_map cudf_request
                criteria !OpamGlobals.cudf_file);
      try
        match
          Algo.Depsolver.check_request_using
            ~call_solver:(dose_solver_callback ~criteria)
            ~criteria ~explain:true cudf_request
        with
        | Algo.Depsolver.Unsat
            (Some {Algo.Diagnostic.result=Algo.Diagnostic.Success _})
        | Algo.Depsolver.Error _
          when OpamGlobals.set_compat_preferences req.criteria ->
          log "Solver failed. Retrying with compat criteria.";
          attempt ()
        | r -> r
      with e ->
        OpamMisc.fatal e;
        if OpamGlobals.set_compat_preferences req.criteria then
          (log "Solver failed with %s. Retrying with compat criteria."
             (Printexc.to_string e);
           attempt ())
        else
          (OpamGlobals.warning "'%s' failed with %s"
             !OpamGlobals.external_solver (Printexc.to_string e);
           failwith "opamSolver")
    in
    attempt ()
  end else
    Algo.Depsolver.Sat(None,Cudf.load_universe [])

let check_request ?(explain=true) ~version_map univ req =
  match Algo.Depsolver.check_request ~explain (to_cudf univ req) with
  | Algo.Depsolver.Unsat
      (Some ({Algo.Diagnostic.result = Algo.Diagnostic.Failure _} as r)) ->
    make_conflicts univ r
  | Algo.Depsolver.Sat (_,u) -> Success (remove u "dose-dummy-request" None)
  | Algo.Depsolver.Error msg ->
    let f = dump_cudf_error ~extern:false ~version_map univ req in
    OpamGlobals.error "Internal solver failed with %s Request saved to %S"
      msg f;
    failwith "opamSolver"
  | Algo.Depsolver.Unsat _ -> (* normally when [explain] = false *)
    conflict_empty univ

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
    | Some ({result=Failure _} as r) -> make_conflicts univ r
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
  (match find_cycles g with
  | [] -> ()
  | cycles -> raise (Cyclic_actions cycles)
  );
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
    | To_recompile p, To_change _,         `Provides -> Required_by [p]
    | _,              To_change(None,_),   `Depends  -> Unknown
    | _,              To_change _,         _         -> Upstream_changes
    | To_change _,    To_delete _,         `Depends  -> Conflicts_with
                                                          [action_contents cause]
    | To_recompile p, To_delete _,         `Depends  -> Conflicts_with [p]
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
  1/ computing the right sequence of removals.
  2/ set all the installed packages which depend on a changed package and have
     no action planned to 'recompile'
  3/ compute the DAG of actions to process in order
  4/ computing the root causes of actions
*)
let solution_of_actions ~simple_universe ~complete_universe ~requested root_actions =
  log "graph_of_actions root_actions=%a"
    (slog string_of_actions) root_actions;

  let actions_map =
    List.fold_left (fun map a ->
        List.fold_left (fun map p -> Map.add p a map)
          map (full_action_contents a))
      Map.empty root_actions in
  let package_graph =
    (* only installed or mentionned packages may be acted upon *)
    let graph =
      create_graph
        (fun p -> p.Cudf.installed || Map.mem p actions_map)
        simple_universe in
    Graph.mirror graph in

  (* complete graph includes build-depopts, which may have an inpact on build
     order *)
  let full_graph =
    let graph =
      create_graph
        (fun p -> p.Cudf.installed || Map.mem p actions_map)
        complete_universe in
    Graph.mirror graph in

  (* the packages to remove, in order *)
  let to_remove =
    Graph.Topo.fold (fun p acc ->
        try match Map.find p actions_map with
          | To_delete _ -> p::acc
          | _ -> acc
        with Not_found -> acc)
      full_graph [] in

  (* transitively add recompilations *)
  let actions_map =
    Graph.Topo.fold (fun p actions_map ->
        if not (Map.mem p actions_map) &&
           List.exists
             (fun p -> try match Map.find p actions_map with
                | To_change (_,p1) when p = p1 -> false
                | _ -> true
                with Not_found -> false)
             (Graph.pred package_graph p)
        then Map.add p (To_recompile p) actions_map
        else actions_map)
      package_graph actions_map
  in

  (* Construct the full graph of actions to proceed to reach the
     new state given by the solver.  *)
  let nonremove_actions_map =
    Map.filter (fun _ -> function To_delete _ -> false | _ -> true)
      actions_map in

  let to_process =
    action_graph_of_packages nonremove_actions_map full_graph in

  let root_causes =
    compute_root_causes complete_universe actions_map requested in

  { to_remove; to_process; root_causes }

let packages u = Cudf.get_packages u
