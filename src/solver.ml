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

open Types
open Path

let log fmt = Globals.log "SOLVER" fmt

type action = (* NV.t internal_action *)
  | To_change of NV.t option * NV.t
  | To_delete of NV.t
  | To_recompile of NV.t

let map_reinstall ~installed a =
  match a with
  | To_change (None, nv) ->
      if NV.Set.mem nv installed then
        To_recompile nv
      else
        let name = NV.name nv in
        if NV.Set.exists (fun nv -> NV.name nv = name) installed then
          let old_nv = NV.Set.find (fun nv -> NV.name nv = name) installed in
          To_change (Some old_nv, nv)
        else
          a
  | _ -> a

let string_of_action = function
  | To_change (None, p)   -> Printf.sprintf " - install %s" (NV.to_string p)
  | To_change (Some o, p) ->
      Printf.sprintf " - upgrade %s to %s"
        (NV.to_string o) (V.to_string (NV.version p))
  | To_recompile p        -> Printf.sprintf " - recompile %s" (NV.to_string p)
  | To_delete p           -> Printf.sprintf " - delete %s" (NV.to_string p)

type package_action = {
  cudf: Cudf.package;
  mutable action: action;
}

let action t = t.action

module PA_graph = struct

  module PkgV =  struct

    type t = package_action

    let compare t1 t2 =
      Algo.Defaultgraphs.PkgV.compare t1.cudf t2.cudf

    let hash t =
      Algo.Defaultgraphs.PkgV.hash t.cudf

    let equal t1 t2 =
      Algo.Defaultgraphs.PkgV.equal t1.cudf t2.cudf

  end

  module PG = Graph.Imperative.Digraph.ConcreteBidirectional (PkgV)
  module Topological = Graph.Topological.Make (PG)
  module Parallel = Parallel.Make(struct
    include PG
    include Topological
    let string_of_vertex v = string_of_action v.action
  end)
  include PG

  let iter_update_reinstall ~installed g =
    PG.iter_vertex (fun v ->
      v.action <- map_reinstall ~installed v.action
    ) g

end

type request = {
  wish_install:  and_formula;
  wish_remove :  and_formula;
  wish_upgrade:  and_formula;
}

let string_of_vpkg =
  string_of_atom_formula

let string_of_list f l =
  Printf.sprintf "{%s}"
    (String.concat ", " (List.map f l))

let string_of_vpkgs =
  string_of_list string_of_vpkg

let string_of_request r =
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (string_of_vpkgs r.wish_install)
    (string_of_vpkgs r.wish_remove)
    (string_of_vpkgs r.wish_upgrade)

type solution = {
  to_remove: NV.t list; (* order : first element needs to be removed before the others *)
  to_add   : PA_graph.t;
}

let solution_is_empty s =
  s.to_remove = [] && PA_graph.is_empty s.to_add

let print_solution t =
  if t.to_remove = [] && PA_graph.is_empty t.to_add then
    ()
  (*Globals.msg
    "No actions will be performed, the current state satisfies the request.\n"*)
  else
    let f = NV.to_string in
    List.iter (fun p -> Globals.msg " - remove %s\n" (f p)) t.to_remove;
    PA_graph.Topological.iter
      (function { action ; _ } -> Globals.msg "%s\n" (string_of_action action))
      t.to_add

type 'a internal_action = 
  | I_to_change of 'a option * 'a
  | I_to_delete of 'a
  | I_to_recompile of 'a

let string_of_internal_action f = function
  | I_to_change (None, p)   -> Printf.sprintf "Install: %s" (f p)
  | I_to_change (Some o, p) ->
      Printf.sprintf "Update: %s (Remove) -> %s (Install)" (f o) (f p)
  | I_to_recompile p        -> Printf.sprintf "Recompile: %s" (f p)
  | I_to_delete p           -> Printf.sprintf "Delete: %s" (f p)

let action_map f = function
  | I_to_change (Some x, y) -> To_change (Some (f x), f y)
  | I_to_change (None, y)   -> To_change (None, f y)
  | I_to_delete y           -> To_delete (f y)
  | I_to_recompile y        -> To_recompile (f y)

type 'a internal_request = {
  i_wish_install:  'a list;
  i_wish_remove :  'a list;
  i_wish_upgrade:  'a list;
}

let string_of_internal_request f r =
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (string_of_list f r.i_wish_install)
    (string_of_list f r.i_wish_remove)
    (string_of_list f r.i_wish_upgrade)

let request_map f r = 
  let f = List.map f in
  { i_wish_install = f r.wish_install
  ; i_wish_remove  = f r.wish_remove
  ; i_wish_upgrade = f r.wish_upgrade }

type package = Debian.Packages.package

let string_of_package p =
  let installed =
    if List.mem_assoc "status" p.Debian.Packages.extras
      && List.assoc "status" p.Debian.Packages.extras = "  installed"
    then "installed"
    else "not-installed" in
  Printf.sprintf "%s::%s(%s)"
    p.Debian.Packages.name p.Debian.Packages.version installed

let string_of_packages l =
  Printf.sprintf "{%s}"
    (String.concat "," (List.map string_of_package l))

let string_of_cudf (p, c) =
  let relop = function
    | `Eq  -> "="
    | `Neq -> "!="
    | `Geq -> ">="
    | `Gt  -> ">"
    | `Leq -> "<="
    | `Lt  -> "<" in
  let const = function
    | None       -> ""
    | Some (r,v) -> Printf.sprintf " (%s %d)" (relop r) v in
  Printf.sprintf "%s%s" p (const c)

(* Universe of packages *)
type universe = U of package list

(* Subset of packages *)
type packages = P of package list

let string_of_cudf_package p =
  let installed = if p.Cudf.installed then "installed" else "not-installed" in
  Printf.sprintf "%s.%d(%s)"
    p.Cudf.package
    p.Cudf.version installed

let string_of_universe u =
  let l =
    Cudf.fold_packages
      (fun accu p -> string_of_cudf_package p :: accu)
      [] u in
  Printf.sprintf "{%s}" (String.concat ", " l)
    
module CudfDiff : sig

  type answer = Cudf.package internal_action list

  val resolve_diff :
    Cudf.universe ->
    Cudf_types.vpkg internal_request ->
    answer list option

end = struct
    
  type answer = Cudf.package internal_action list

  module Cudf_set = struct

    include Common.CudfAdd.Cudf_set

    let to_string s =
      Printf.sprintf "{%s}"
        (String.concat "," (List.map string_of_cudf_package (elements s)))

    let choose_one s =
      match elements s with
      | []  -> raise Not_found
      | [x] -> x
      | _   -> invalid_arg "choose_one"

  end

  let to_cudf_doc univ req = 
    None, 
    Cudf.fold_packages (fun l x -> x :: l) [] univ, 
    { Cudf.request_id = "";
      install   = req.i_wish_install;
      remove    = req.i_wish_remove;
      upgrade   = req.i_wish_upgrade;
      req_extra = [] }

  let cudf_resolve univ req = 
    log "INTERNAL(cudf_resolve) universe=%s request=<%s>"
      (string_of_universe univ)
      (string_of_internal_request string_of_cudf req);
    let open Algo in
    let r = Depsolver.check_request (to_cudf_doc univ req) in
(*    Diagnostic.fprintf ~explain:true ~failure:true ~success:true Format.err_formatter r;
      Format.pp_print_flush Format.err_formatter (); *)
    if Diagnostic.is_solution r then
      match r with
      | { Diagnostic.result = Diagnostic.Success f } -> Some (f ~all:true ())
      | _ -> assert false
    else
      None

  let resolve f_diff univ_init req =
    match cudf_resolve univ_init req with
    | None   -> None
    | Some l ->
      log "INTERNAL(resolve) %s" (String.concat " " (List.map string_of_cudf_package l));
        try 
          let diff = Common.CudfDiff.diff univ_init (Cudf.load_universe l) in
          Some (f_diff diff)
        with
          Cudf.Constraint_violation _ -> 
            log "INTERNAL(resolve) constraint violation";
            None

  let resolve_with_optimization = 
    let nb_iter_max = 20 in 
    (** maximum number of request we can send to the solver 
        before the algorithm stops and returns the current solution *)

    fun f_diff univ req ->
    let string_of_c l = 
      String.concat " " (List.map (string_of_internal_action string_of_cudf_package) l) in

    match req, resolve f_diff univ req with
      | { i_wish_remove = [] ; i_wish_upgrade = [] ; _ }, Some ans -> 

        let rec aux nb_iter univ l_ans req ans =
          match
            (** partition [ans] so that we have one group of packages with version increase-able *)
            List.partition
              (function
                | I_to_change (None, p) -> 
                    (match 
                        (** determine if [p] belongs to [req] *)
                        try Some (List.find (fun (wish_name, _) -> p.Cudf.package = wish_name) req) with Not_found -> None
                     with
                       | None -> false
                       | Some (_, wish_version) -> 
                           (** determine if the user has put a constraint on the package associated to [p] in [req] *) 
                           wish_version <> None)
                | I_to_change (Some _, _)
                | I_to_delete _
                | I_to_recompile _ -> true) 
              ans
          with
          | _, [] -> l_ans
          | l_p_ans, l_new_pkg ->
              log "INTERNAL(optimization) requested=%s, new_computed=%s" (string_of_c l_p_ans) (string_of_c l_new_pkg);

              (** we preserve versions of packages that were explicitely requested *)
              let l_p_ans = 
                Utils.filter_map
                  (function 
                    | I_to_change (_, p)
                    | I_to_recompile p -> Some (p.Cudf.package, Some (`Eq, p.Cudf.version))
                    | I_to_delete _ -> None)
                  l_p_ans in

              (** we compute the last version of every new package (not requested) *)
              let to_continue, l_new_pkg = 
                let univ_map = (** similar as [univ] but represented with map *)
                  List.fold_left (fun map p -> 
                    Utils.StringMap.add p.Cudf.package
                      (Utils.IntMap.add 
                         p.Cudf.version
                         p
                         (try Utils.StringMap.find p.Cudf.package map with Not_found -> Utils.IntMap.empty))
                      map) Utils.StringMap.empty (Cudf.get_packages univ) in
                
                let find_max p univ =
                  snd (Utils.IntMap.max_binding (Utils.StringMap.find p.Cudf.package univ)) in

                List.fold_left 
                  (fun (to_continue, l_new_pkg) -> function
                    | I_to_change (None, p) -> 
                        let v_max = (find_max p univ_map).Cudf.version in
                        to_continue || p.Cudf.version <> v_max, (p.Cudf.package, Some (`Eq, v_max)) :: l_new_pkg
                    | I_to_change (Some _, _)
                    | I_to_delete _
                    | I_to_recompile _ -> assert false (* already filtered before *))
                  (false, []) 
                  l_new_pkg in

              if to_continue then
                let i_wish_upgrade =
                  let l = l_p_ans @ l_new_pkg in
                  let s = List.filter (fun (p,_) -> not (List.mem_assoc p l)) req in
                  l @ s in
                match resolve f_diff univ { i_wish_install = [] ; i_wish_remove = [] ; i_wish_upgrade } with
                  | None -> 
                      let () = Globals.warning "INTERNAL(optimization) no solution" in
                      l_ans
                  | Some ans_new -> 
                      if ans_new = ans then (* NOTE the strict equality is a bit strong. 
                                               For instance, we can just require that [ans_new] is a permutation of [ans]. *)
                        l_ans
                      else if nb_iter > nb_iter_max then
                        let () = Globals.warning "number of iteration exceeded" in
                        (* We stop the loop. 
                           Even if it is correct to stop at any time, 
                           determine if the equality above is too strong
                           or if we have reached a cycle of answer (i1, i2, ..., iN, i1) so that (forall J, iJ <> iJ+1). *)
                        l_ans
                      else
                        aux (succ nb_iter) univ (ans_new :: l_ans) i_wish_upgrade ans_new
              else
                let _ = log "INTERNAL(optimization) the answer does not contain some package with a more recent version to try" in
                l_ans in
        
        Some (aux 0 univ [ans] req.i_wish_install ans)

      | _, Some ans -> Some [ans]
      | _, None -> None

  let resolve_diff =
    let f_diff diff =
      Hashtbl.fold (fun pkgname s acc ->
        let add x = x :: acc in
(*        log "%s removed=%s installed=%s"
          pkgname
          (Cudf_set.to_string s.Common.CudfDiff.removed)
          (Cudf_set.to_string s.Common.CudfDiff.installed); *)
        (* NOTE for the following [choose_one] : 
           As we have always at most one version of a package installed,
           the set is always either empty or a singleton *)
        let removed =
          try Some (Cudf_set.choose_one s.Common.CudfDiff.removed)
          with Not_found -> None in
        let installed =
          try Some (Cudf_set.choose_one s.Common.CudfDiff.installed)
          with Not_found -> None in
        match removed, installed with
        | None      , Some p     -> add (I_to_change (None, p))
        | Some p    , None       -> add (I_to_delete p)
        | Some p_old, Some p_new -> add (I_to_change (Some p_old, p_new))
        | None      , None       -> acc
      ) diff []
    in
    resolve_with_optimization f_diff

end

module Graph = 
struct
  open Algo

  module PG = struct
    module G = Defaultgraphs.PackageGraph.G
    let union g1 g2 =
      let g1 = G.copy g1 in
      let () = 
        begin
          G.iter_vertex (G.add_vertex g1) g2;
          G.iter_edges (G.add_edge g1) g2;
        end in
      g1
    include G
  end

  module PO = Defaultgraphs.GraphOper (PG)

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

  module PG_topo = Graph.Topological.Make (PG)
 (* (* example of instantiation *)
    module PG_bfs = Make_fs (Graph.Traverse.Bfs (PG))
    module PG_dfs = Make_fs (Graph.Traverse.Dfs (PG))
 *)

  module O_pkg = struct
    type t = Cudf.package
    let to_string = string_of_cudf_package
    let summary pkg = pkg.Cudf.package, pkg.Cudf.version
    let compare pkg1 pkg2 =
      compare (summary pkg1) (summary pkg2)
  end
  module PkgMap = Map.Make (O_pkg)
  module PkgSet = Set.Make (O_pkg)

  let dep_reduction v =
    let g = Defaultgraphs.PackageGraph.dependency_graph (Cudf.load_universe v) in
    let () = PO.transitive_reduction g in
    (* uncomment to view the dependency graph:
       XXX: cycles are not detected, which can lead to very weird situations
       Defaultgraphs.PackageGraph.D.output_graph stdout g; *)
    g

  let tocudf table pkg =
    let options = {
      Debian.Debcudf.default_options with
        Debian.Debcudf.extras_opt = [
          File.OPAM.s_depopts, (File.OPAM.s_depopts, `String None)
        ]
    } in
    Debian.Debcudf.tocudf ~options table pkg

  let cudfpkg_of_debpkg table = List.map (tocudf table)

  let get_table l_pkg_pb f = 
    let table = Debian.Debcudf.init_tables l_pkg_pb in
    let v = f table (cudfpkg_of_debpkg table l_pkg_pb) in
    let () = Debian.Debcudf.clear table in
    v

  let topo_fold g pkg_set = 
    let _, l =
      PG_topo.fold
        (fun p (set, l) ->
          let add_succ_rem pkg set act =
            (let set = PkgSet.remove pkg set in
             try
               List.fold_left (fun set x -> 
                 PkgSet.add x set) set (PG.succ g pkg)
             with _ -> set), 
            act :: l in
          
          if PkgSet.mem p set then 
            add_succ_rem p set p
          else
            set, l)
        g 
        (pkg_set, []) in
    l

  (* Add the optional dependencies to the list of dependencies *)
  (* The dependencies are encoded in the pkg_extra of cudf packages,
     as a raw string. So we need to parse the string and convert it
     to cudf list of package dependencies.
     NOTE: the cudf encoding (to replace '_' by '%5f' is done in
     file.ml when we create the debian package. It could make sense
     to do it here. *)
  let extended_dependencies table pkg =
    let opt = File.OPAM.s_depopts in
    if List.mem_assoc opt pkg.Cudf.pkg_extra then
      match List.assoc opt pkg.Cudf.pkg_extra with
      | `String s ->
          let deps = File_format.parse_cnf_formula
            (Parser.value Lexer.token (Lexing.from_string s)) in
          let deps = Debian.Debcudf.lltocudf table deps in
          { pkg with Cudf.depends = deps @ pkg.Cudf.depends }
      | _ -> assert false
    else
      pkg

  let filter_dependencies f_filter f_direction ?(depopts=false) (U l_pkg_pb) (P pkg_l) =
    let pkg_map = 
      List.fold_left
        (fun map pkg -> NV.Map.add (NV.of_dpkg pkg) pkg map)
        NV.Map.empty
        l_pkg_pb in
    get_table l_pkg_pb
      (fun table pkglist ->
        let pkglist = 
          if depopts then
            List.map (extended_dependencies table) pkglist
          else
            pkglist in
        let pkg_set = List.fold_left
          (fun accu pkg -> PkgSet.add (tocudf table pkg) accu)
          PkgSet.empty
          pkg_l in
        let g = f_direction (dep_reduction pkglist) in
        let l = topo_fold g pkg_set in
        List.map (fun pkg -> NV.Map.find (NV.of_cudf table pkg) pkg_map)
          (f_filter pkg_set l))

  let filter_dep = filter_dependencies (fun _ x -> x)

  let filter_backward_dependencies = filter_dep (fun x -> x)
  let filter_forward_dependencies = filter_dep PO.O.mirror

  let sort_by_backward_dependencies = 
    filter_dependencies
      (fun pkg_set -> List.filter (fun p -> PkgSet.mem p pkg_set))
      (fun x -> x)

  let resolve (U l_pkg_pb) req installed =
    (* filter-out the default package from the universe *)
    let l_pkg_pb =
      List.filter
        (fun pkg -> pkg.Debian.Packages.name <> Globals.default_package)
        l_pkg_pb in
    let filter ((n,_),_) = n <> Globals.default_package in
    let req = {
      wish_install = List.filter filter req.wish_install;
      wish_remove  = List.filter filter req.wish_remove;
      wish_upgrade = List.filter filter req.wish_upgrade;
    } in
    log "universe=%s request=<%s>"
      (string_of_packages (List.rev l_pkg_pb))
      (string_of_request req);
    get_table l_pkg_pb 
      (fun table pkglist ->
        let package_map pkg = NV.of_cudf table pkg in

        let i_req = 
          request_map
            (fun x -> 
              match Debian.Debcudf.ltocudf table [x] with
                | [n,c] -> Common.CudfAdd.encode n, c
                | _   -> failwith "TODO"
            ) req in
        let resolve_diff universe = 
          CudfDiff.resolve_diff universe i_req in
        
        let req_only_remove = 
          (** determine if the request is a remove case *)
          match req with
            | { wish_remove = _ :: _; _ } -> true
            | _ -> false in

        (** [graph_simple] contains the graph of packages 
            where the dependency relation is without optional dependencies  *)
        let graph_simple, (universe, sol_o) = 
          let universe0 = Cudf.load_universe pkglist in
          dep_reduction (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) universe0),
          let universe = Cudf.load_universe (List.map (extended_dependencies table) pkglist) in
          universe, 
          resolve_diff
            (if req_only_remove then
                (* Universe with all the optional dependencies *)
                universe
             else
                (* Universe without optional dependencies *)
                universe0) in

        log "full-universe: (*%B*) %s" req_only_remove (string_of_universe universe);
        let create_graph filter = dep_reduction (Cudf.get_packages ~filter universe) in

        let action_of_answer l =
            let l_s =
              String.concat " "
                (List.map (string_of_internal_action string_of_cudf_package)  l) in
            log "SOLUTION: %s" l_s;

            (** compute all packages to remove *)
            let l_del_p, set_del = 
              Utils.filter_map (function
                | I_to_change (Some pkg, _) 
                | I_to_delete pkg -> Some pkg
                | _ -> None) l,
              PkgSet.of_list
                (Utils.filter_map (function
                  | I_to_delete pkg -> Some pkg
                  | _ -> None) l) in

            (** compute initial packages to add *)
            let map_add = 
              PkgMap.of_list (Utils.filter_map (function 
                | I_to_change (_, pkg) as act -> Some (pkg, act)
                | I_to_delete _ -> None
                | I_to_recompile _ -> assert false) l) in

            (** [graph_toinstall] is similar to [graph_simple] except that 
                the dependency relation is complete *)
            let graph_toinstall = 
              PO.O.mirror 
                (create_graph (fun p -> p.Cudf.installed || PkgMap.mem p map_add)) in
            let graph_toinstall =
              let graph_toinstall = PG.copy graph_toinstall in
              List.iter (PG.remove_vertex graph_toinstall) l_del_p;
              graph_toinstall in

            (** compute packages to recompile (and perform the merge with packages to add) *)
            let _, map_act = 
              PG_topo.fold
                (fun pkg (set_recompile, l_act) ->
                  let add_succ_rem pkg set act =
                    (let set = PkgSet.remove pkg set in
                     try
                       List.fold_left
                         (fun set x -> PkgSet.add x set)
                         set (PG.succ graph_toinstall pkg)
                     with _ -> set), 
                    Utils.IntMap.add
                      (PG.V.hash pkg)
                      { cudf = pkg ; action = action_map package_map act } l_act in
                  try
                    let act = PkgMap.find pkg map_add in
                    add_succ_rem pkg set_recompile act
                  with Not_found ->
                    if PkgSet.mem pkg set_recompile then
                      add_succ_rem pkg set_recompile (I_to_recompile pkg)
                    else
                      set_recompile, l_act
                )
                graph_toinstall
                (PkgSet.empty, Utils.IntMap.empty) in

            (** compute packages to recompile and remove *)
            let map_act, to_remove = 
              let l_remove = topo_fold (create_graph (fun p -> PkgSet.mem p set_del)) set_del in
              let () = 
                match l_remove, req_only_remove with
                  | _ :: _, false -> 
                    Globals.warning "the removing optimization will be applied but the solver has only taken a universe with partial dependencies"
                  (* check below if there are some packages, that depend optionally on packages to be removed, and that will not be recompiled *)
                  | _ -> () in

              (** partition the [l_remove] to decide for each element if we recompile them or delete. *)
              List.fold_left
                (fun (map_act, l_folded) pkg -> 
                  if 
                    (** check if the user has set some packages that will explicitely be removed *)
                    List.exists
                      (fun (p, _) -> p = pkg.Cudf.package) 
                      i_req.i_wish_remove
                    ||
                    (** check if [pkg] contains an optional package which has already been visited in [l_folded] *)
                    List.exists
                      (fun p -> List.exists (fun p0 -> O_pkg.compare p0 p = 0) l_folded)
                      (try PG.succ graph_simple pkg with _ -> [])
                  then
                    (** [pkg] will be deleted *)
                    map_act, (*package_map*) pkg :: l_folded
                  else
                    (** [pkg] will be recompiled *)
                    Utils.IntMap.add
                      (PG.V.hash pkg)
                      { cudf = pkg ; action = action_map package_map (I_to_recompile pkg) }
                      map_act,
                    l_folded
                ) 
                (map_act, [])
                l_remove in

            (** construct the answer [graph] to add. 
                Then, it suffices to fold it topologically 
                by following the action given at each node (install or recompile). *)
            let graph = PA_graph.create () in
            Utils.IntMap.iter (fun _ -> PA_graph.add_vertex graph) map_act;
            PG.iter_edges
              (fun v1 v2 ->
                try
                  let v1 = Utils.IntMap.find (PG.V.hash v1) map_act in
                  let v2 = Utils.IntMap.find (PG.V.hash v2) map_act in
                  PA_graph.add_edge graph v1 v2
                with Not_found ->
                  ())
              graph_toinstall;
            PA_graph.iter_update_reinstall ~installed graph;
            { to_remove = List.map package_map to_remove
            ; to_add = graph } in

        match sol_o with
        | None   -> []
        | Some l -> (* [l] is not empty *) List.map action_of_answer l)

end

let filter_backward_dependencies = Graph.filter_backward_dependencies
let filter_forward_dependencies = Graph.filter_forward_dependencies
let sort_by_backward_dependencies = Graph.sort_by_backward_dependencies

let resolve = Graph.resolve

let delete_or_update t =
  t.to_remove <> [] || 
  PA_graph.fold_vertex
    (fun v acc ->
      acc || match v.action with To_change (Some _, _) -> true | _ -> false)
    t.to_add
    false
