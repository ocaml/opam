(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes
open OpamStd.Op

type deps
type revdeps
type 'a node =
  | Root of package
  | Dependency : {
      package: package;
      satisfies: condition option;
      is_dup: bool;
    } -> deps node
  | Requirement : {
      package: package;
      demands: condition option;
      is_dup: bool;
    } -> revdeps node

module Tree = OpamConsole.Tree
type 'a forest = 'a Tree.t list

type tree_filter =
  | Roots_from
  | Leads_to

type mode =
  | Deps
  | ReverseDeps

type resulting_forest =
  | DepsForest of deps node forest
  | RevdepsForest of revdeps node forest

(** Forest building *)

let build_condition_map tog st =
  let OpamListCommand.{ recursive = _; depopts = _;
                        build; post; test; dev_setup; doc; dev; } = tog in
  let partial_env =
    let vars = [
      "build", build;
      "post", post;
      "dev", dev;
      "with-doc", doc;
      "with-test", test;
      "with-dev-setup", dev_setup;
    ] |> List.map (fun (v, f) -> v, if f then None else Some (B false))
    in
    fun var ->
      OpamStd.List.find_map_opt (fun (v,c) ->
          if String.equal v (OpamVariable.Full.to_string var) then
            c else None) vars
  in
  OpamPackage.Set.fold (fun package cmap ->
      let map =
        OpamSwitchState.opam st package
        |> OpamFile.OPAM.depends
        (* remove any irrelevant variables to simplify the output *)
        |> OpamFilter.partial_filter_formula partial_env
        |> OpamFormula.formula_to_dnf
        |> OpamStd.List.find_map_opt (fun cnj ->
            let is_valid, result =
              cnj
              (* filter out non-installed dependencies *)
              |> List.filter (fun (name, _) ->
                  OpamSwitchState.is_name_installed st name)
              |> OpamStd.List.fold_left_map (fun is_valid orig ->
                  if not is_valid then
                    is_valid, orig
                  else
                  let filtered =
                    OpamFilter.filter_deps ~build ~post ~doc ~test ~dev_setup
                    ~dev ~default:true (Atom orig)
                  in
                  match filtered with
                  | Atom (name, _) ->
                    let package =
                      OpamSwitchState.find_installed_package_by_name st name
                    in
                    let is_valid =
                      OpamFormula.eval (fun atom ->
                          OpamFormula.check atom package)
                        (OpamFormula.to_atom_formula filtered)
                    in
                    is_valid, orig
                  | _ -> false, orig (* should be impossible *)
                ) true
            in
            if is_valid then Some result else None
          )
        |> OpamStd.Option.default []
        |> OpamPackage.Name.Map.of_list
      in
      cmap |> OpamPackage.Map.add package map
    ) st.installed OpamPackage.Map.empty

let is_root graph p =
  OpamSolver.PkgGraph.in_degree graph p = 0

let is_leaf graph p =
  OpamSolver.PkgGraph.out_degree graph p = 0

let cut_leaves (mode: [ `succ | `pred]) ~names ~root st graph =
  let fold, is_final =
    match mode with
    | `succ -> OpamSolver.PkgGraph.fold_succ, is_leaf graph
    | `pred -> OpamSolver.PkgGraph.fold_pred, is_root graph
  in
  (* compute the packages which are connected to one of the `names` *)
  let rec go package set =
    fold (fun p ps ->
        if OpamPackage.Set.mem p ps then ps
        else
        let ps = OpamPackage.Set.add p ps in
        if not (is_final p) then go p ps
        else ps
      ) graph package set
  in
  let packages = names |> OpamPackage.Set.fold go names in
  (* cut leaves not belonging to the packages *)
  OpamPackage.Set.diff st.installed packages
  |> OpamPackage.Set.iter (OpamSolver.PkgGraph.remove_vertex graph);
  (* return the new roots and the new graph *)
  OpamPackage.Set.inter root packages, graph

let build_deps_forest st universe tog filter names =
  let OpamListCommand.{ build; post; _ } = tog in
  let root, graph =
    let graph =
      OpamSolver.dependency_graph
        ~depopts:false ~build ~post ~installed:true ~unavailable:false
        universe
    in
    let root =
      st.installed |> OpamPackage.Set.filter (is_root graph)
    in
    match OpamPackage.Set.is_empty names, filter with
    | false, Roots_from  -> names, graph
    | false, Leads_to -> cut_leaves `pred ~names ~root st graph
    | true, _ -> root, graph
  in
  let condition_map = build_condition_map tog st in
  let rec build visited package node =
    if visited |> OpamPackage.Set.mem package then
      let node =
        match node with
        | Root p -> Root p (* but impossible *)
        | Dependency x -> Dependency { x with is_dup = true }
      in
      visited, Tree.create node
    else
    let visited = visited |> OpamPackage.Set.add package in
    let conditions = condition_map |> OpamPackage.Map.find package in
    let succ = OpamSolver.PkgGraph.succ graph package in
    let visited, children =
      OpamStd.List.fold_left_map (fun visited package ->
          let satisfies =
            OpamPackage.(Name.Map.find_opt package.name) conditions
          in
          let child_node = Dependency { package; satisfies; is_dup = false } in
          build visited package child_node
        ) visited succ
    in
    visited, Tree.create ~children node
  in
  let build_root visited package =
    build visited package (Root package)
  in
  root
  |> OpamPackage.Set.elements
  |> OpamStd.List.fold_left_map build_root OpamPackage.Set.empty
  |> snd

let build_revdeps_forest st universe tog filter names =
  let OpamListCommand.{ build; post; _ } = tog in
  let root, graph =
    let graph =
      OpamSolver.dependency_graph
        ~depopts:false ~build ~post ~installed:true ~unavailable:false
        universe
    in
    let root =
      st.installed |> OpamPackage.Set.filter (is_leaf graph)
    in
    match OpamPackage.Set.is_empty names, filter with
    | false, Roots_from  -> names, graph
    | false, Leads_to -> cut_leaves `succ ~names ~root st graph
    | true, _ -> root, graph
  in
  let condition_map = build_condition_map tog st in
  let rec build visited package node =
    if visited |> OpamPackage.Set.mem package then
      let node =
        match node with
        | Root p -> Root p (* but impossible *)
        | Requirement x -> Requirement { x with is_dup = true }
      in
      visited, Tree.create node
    else
    let visited = visited |> OpamPackage.Set.add package in
    let pred = OpamSolver.PkgGraph.pred graph package in
    let visited, children =
      OpamStd.List.fold_left_map (fun visited child ->
          let demands =
            condition_map
            |> OpamPackage.Map.find child
            |> OpamPackage.Name.Map.find_opt package.name
          in
          let child_node =
            Requirement { package = child; demands; is_dup = false }
          in
          build visited child child_node
        ) visited pred
    in
    visited, Tree.create ~children node
  in
  let build_root visited package =
    let visited = OpamPackage.Set.(remove package (union visited root)) in
    build visited package (Root package)
  in
  root
  |> OpamPackage.Set.elements
  |> OpamStd.List.fold_left_map build_root OpamPackage.Set.empty
  |> snd

let build st universe tog mode filter names =
  match mode with
  | Deps ->
    DepsForest (build_deps_forest st universe tog filter names)
  | ReverseDeps ->
    RevdepsForest (build_revdeps_forest st universe tog filter names)


(* Forest printing *)

let string_of_condition cond =
  let custom ~context:_ ~paren:_ = function
    | FString s -> Some s
    | _ -> None
  in
  let string_of_atom fc =
    match fc with
    | Filter f -> OpamFilter.to_string ~custom f
    | Constraint (relop, f) ->
      Printf.sprintf "%s %s"
        (OpamPrinter.FullPos.relop_kind relop)
        (OpamFilter.to_string ~custom f)
  in
  "(" ^ OpamFormula.string_of_formula string_of_atom cond ^ ")"

let duplicate_symbol = "[*]"

let print_deps ?(no_constraint=false) = function
  | Root p -> OpamPackage.to_string p
  | Dependency { package; satisfies; is_dup } ->
    let p = OpamPackage.to_string package in
    let dup = if is_dup then " "^duplicate_symbol else "" in
    match satisfies with
    | _ when no_constraint -> Printf.sprintf "%s%s" p dup
    | None | Some Empty -> Printf.sprintf "%s%s" p dup
    | Some c -> Printf.sprintf "%s %s%s" p (string_of_condition c) dup

let print_revdeps ?(no_constraint=false) = function
  | Root p -> OpamPackage.to_string p
  | Requirement { package; demands; is_dup } ->
    let p = OpamPackage.to_string package in
    let dup = if is_dup then " "^duplicate_symbol else "" in
    match demands with
    | _ when no_constraint -> Printf.sprintf "%s%s" p dup
    | None | Some Empty -> Printf.sprintf "%s%s" p dup
    | Some c -> Printf.sprintf "%s %s%s" (string_of_condition c) p dup

let print ?no_constraint = function
  | DepsForest (tree :: trees) ->
    let printer = print_deps ?no_constraint in
    Tree.print ~printer tree;
    trees |> List.iter (fun tree -> print_newline (); Tree.print ~printer tree)
  | RevdepsForest (tree :: trees) ->
    let printer = print_revdeps ?no_constraint in
    Tree.print ~printer tree;
    trees |> List.iter (fun tree -> print_newline (); Tree.print ~printer tree)
  | DepsForest [] | RevdepsForest [] -> ()

let name_version_json nv =
  [ "name", OpamPackage.Name.to_json (OpamPackage.name nv);
    "version", OpamPackage.Version.to_json (OpamPackage.version nv) ]

let deps_to_json_fields ?(no_constraint=false) =
  function
  | Root p -> name_version_json p
  | Dependency { package; satisfies; is_dup } ->
    let p = name_version_json package in
    let s =
      if no_constraint then [] else
      match satisfies with
      | None | Some Empty -> ["satisfies", `Null]
      | Some c -> ["satisfies", `String (string_of_condition c)]
    in
    let d = ["is_duplicate", `Bool is_dup] in
    p @ s @ d

let revdeps_to_json_fields ?(no_constraint=false) = function
  | Root p -> name_version_json p
  | Requirement { package; demands; is_dup } ->
    let p = name_version_json package in
    let s =
      if no_constraint then [] else
      match demands with
      | None | Some Empty -> ["demands", `Null]
      | Some c -> ["demands", `String (string_of_condition c)]
    in
    let d = ["is_duplicate", `Bool is_dup] in
    p @ s @ d

let rec tree_to_json children value_to_fields t =
  let cs =
    List.map (tree_to_json children value_to_fields)
      (Tree.children t)
  in
  `O (value_to_fields (Tree.value t) @ [children, `A cs])

let forest_to_json ?no_constraint forest =
  match forest with
  | DepsForest xs ->
    `A (List.map
          (tree_to_json "dependencies" (deps_to_json_fields ?no_constraint))
          xs)
  | RevdepsForest xs ->
    `A (List.map
          (tree_to_json "requirements" (revdeps_to_json_fields ?no_constraint))
          xs)

let print_solution st new_st missing solution =
  OpamConsole.msg "The following actions are simulated:\n";
  let messages p =
    let opam = OpamSwitchState.opam new_st p in
    let messages = OpamFile.OPAM.messages opam in
    OpamStd.List.filter_map (fun (s,f) ->
        if OpamFilter.opt_eval_to_bool
            (OpamPackageVar.resolve ~opam new_st) f
        then Some s
        else None
      )  messages in
  let append nv =
    let pinned =
      if OpamPackage.Set.mem nv st.pinned then " (pinned)"
      else ""
    and deprecated =
      let opam = OpamSwitchState.opam new_st nv in
      if OpamFile.OPAM.has_flag Pkgflag_Deprecated opam then " (deprecated)"
      else ""
    in
    pinned ^ deprecated
  in
  let skip =
    OpamPackage.Set.fold
      (fun p m -> OpamPackage.Map.add p p m)
      (Lazy.force new_st.reinstall)
      OpamPackage.Map.empty
  in
  OpamSolver.print_solution ~messages ~append
    ~requested:missing ~reinstall:(Lazy.force st.reinstall)
    ~available:(Lazy.force st.available_packages)
    ~skip (* hide recompiled packages because they don't make sense here *)
    solution;
  OpamConsole.msg "\n"


(** Setting states for building *)

let get_universe tog requested st =
  let OpamListCommand.{doc; test; dev_setup; dev; _} = tog in
  OpamSwitchState.universe st ~doc ~test ~dev_setup ~force_dev_deps:dev
    ~requested
    Query

let dry_install tog st universe install =
  match OpamSolver.resolve universe
          (OpamSolver.request ~install ()) with
  | Success solution ->
    let new_st = OpamSolution.dry_run st solution in
    print_solution st new_st
      (OpamPackage.Name.Set.of_list (List.map fst install))
      solution;
    let requested = OpamFormula.packages_of_atoms new_st.installed install in
    new_st, get_universe tog requested new_st
  | Conflicts cs ->
    OpamConsole.error
      "Could not simulate installing the specified package(s) to this switch:";
    OpamConsole.errmsg "%s%s"
      (OpamCudf.string_of_conflicts st.packages
         (OpamSwitchState.unavailable_reason st) cs)
      (OpamSwitchState.did_you_mean st install);
    OpamStd.Sys.exit_because `No_solution

let run st tog ?no_constraint mode filter atoms =
  let open OpamPackage.Set.Op in
  let select, missing =
    List.fold_left (fun (select, missing) atom ->
        let installed =
          OpamPackage.Set.filter (OpamFormula.check atom) st.installed
        in
        if OpamPackage.Set.is_empty installed then
          (select, atom :: missing)
        else
          (installed ++ select, missing))
      (OpamPackage.Set.empty, []) atoms
  in
  let st, universe =
    let universe =
      let requested =
        OpamFormula.packages_of_atoms
          (if missing = [] then st.installed else st.packages) atoms
      in
      get_universe tog requested st
    in
    match mode, filter, missing with
    | Deps, _, [] -> st, universe
    | Deps, Roots_from, _::_ ->
        dry_install tog st universe missing
    | Deps, Leads_to, _::_
    | ReverseDeps, _, _ ->
      (* non-installed names don't make sense in rev-deps *)
      if missing <> [] then
        (OpamConsole.warning "Not installed package%s %s, skipping%s"
           (match missing with | [_] -> "" | _ -> "s")
           (OpamStd.Format.pretty_list
              (List.map OpamFormula.string_of_atom missing))
           (OpamSwitchState.did_you_mean ~installed_only:true st missing));
      if OpamPackage.Set.is_empty select && atoms <> [] then
        OpamConsole.error_and_exit `Not_found "No package to display"
      else
        st, universe
  in
  if OpamPackage.Set.is_empty st.installed then
    OpamConsole.error_and_exit `Not_found "No package is installed"
  else
  let simulated = OpamFormula.packages_of_atoms st.installed missing in
  let forest =
    build st universe tog mode filter (select ++ simulated)
  in
  print ?no_constraint forest;
  if OpamClientConfig.(!r.json_out) <> None then
    (if OpamSwitch.compare st.switch OpamSwitch.unset <> 0 then
       OpamJson.append "switch" (OpamSwitch.to_json st.switch);
     OpamJson.append "tree" (forest_to_json ?no_constraint forest))
