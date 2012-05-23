(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
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

let string_of_action = function
  | To_change (None, p)   -> Printf.sprintf "Install: %s" (NV.to_string p)
  | To_change (Some o, p) ->
      Printf.sprintf "Update: %s (Remove) -> %s (Install)"
        (NV.to_string o) (NV.to_string p)
  | To_recompile p        -> Printf.sprintf "Recompile: %s" (NV.to_string p)
  | To_delete p           -> Printf.sprintf "Delete: %s" (NV.to_string p)

type package_action = {
  cudf   : Cudf.package;
  action : action;
}

let action t = t.action

module PA_graph = struct

  module PkgV =  struct

    type t = package_action

    let compare t1 t2 =
      Algo.Defaultgraphs.PackageGraph.PkgV.compare t1.cudf t2.cudf

    let hash t =
      Algo.Defaultgraphs.PackageGraph.PkgV.hash t.cudf

    let equal t1 t2 =
      Algo.Defaultgraphs.PackageGraph.PkgV.equal t1.cudf t2.cudf

  end

  module PG = Graph.Imperative.Digraph.ConcreteBidirectional (PkgV)
  module Topological = Graph.Topological.Make (PG)
  module Parallel = Parallel.Make(struct
    include PG
    include Topological
    let string_of_vertex v = string_of_action v.action
  end)
  include PG

end

type request = {
  wish_install:  Debian.Format822.vpkg list;
  wish_remove :  Debian.Format822.vpkg list;
  wish_upgrade:  Debian.Format822.vpkg list;
}

let string_of_vpkg = function
  | ((n,_), None)       -> n
  | ((n,_), Some (r,c)) -> Printf.sprintf "%s %s %s" n r c

let string_of_list f l =
  Printf.sprintf "{%s}"
    (String.concat "," (List.map f l))

let string_of_vpkgs = string_of_list string_of_vpkg

let string_of_request r =
  Printf.sprintf "install:%s remove:%s upgrade:%s"
    (string_of_vpkgs r.wish_install)
    (string_of_vpkgs r.wish_remove)
    (string_of_vpkgs r.wish_upgrade)

type solution = {
  to_remove: NV.t list;
  to_add   : PA_graph.t;
}

let print_solution t =
  if t.to_remove = [] && PA_graph.is_empty t.to_add then
    Globals.msg "No actions will be performed, the current state satisfies the request.\n"
  else
    let f = NV.to_string in
    List.iter (fun p -> Globals.msg "Remove: %s\n" (f p)) t.to_remove;
    PA_graph.Topological.iter
      (function { action ; _ } -> Globals.msg "%s\n" (string_of_action action))
      t.to_add

type 'a internal_action = 
  | I_to_change of 'a option * 'a
  | I_to_delete of 'a
  | I_to_recompile of 'a

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
  Printf.sprintf "%s.%s(%s)"
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
    | Some (r,v) -> Printf.sprintf "%s %d" (relop r) v in
  Printf.sprintf "%s %s" p (const c)

(* Universe of packages *)
type universe = U of package list

(* Subset of packages *)
type packages = P of package list

let string_of_package p =
  let installed = if p.Cudf.installed then "installed" else "not-installed" in
  Printf.sprintf "%s.%d(%s)"
    (Common.CudfAdd.decode p.Cudf.package)
    p.Cudf.version installed

let string_of_universe u =
  let l =
    Cudf.fold_packages
      (fun accu p -> string_of_package p :: accu)
      [] u in
  Printf.sprintf "{%s}" (String.concat ", " l)
    
module CudfDiff : sig

  val resolve_diff :
    Cudf.universe ->
    Cudf_types.vpkg internal_request ->
    Cudf.package internal_action list option

end = struct
    
  module Cudf_set = struct
    module S = Common.CudfAdd.Cudf_set

    let choose_one s = match S.cardinal s with
      | 0 -> raise Not_found
      | 1 -> S.choose s
      | _ -> assert false
          (* As we have always at most one version of a package installed,
             the set is always either empty or a singleton *)

    let to_string s =
      Printf.sprintf "{%s}"
        (String.concat "," (List.map string_of_package (S.elements s)))

    include S
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
    log "(INTERNAL) universe=%s request=<%s>"
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
        try 
          let diff = Common.CudfDiff.diff univ_init (Cudf.load_universe l) in
          Some (f_diff diff)
        with
          Cudf.Constraint_violation _ -> None

  let resolve_diff =
    let f_diff diff =
      Hashtbl.fold (fun pkgname s acc ->
        let add x = x :: acc in
(*        log "%s removed=%s installed=%s"
          pkgname
          (Cudf_set.to_string s.Common.CudfDiff.removed)
          (Cudf_set.to_string s.Common.CudfDiff.installed); *)
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
    resolve f_diff

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

  module O_pkg = struct type t = Cudf.package let compare = compare end
  module PkgMap = Map.Make (O_pkg)
  module PkgSet = Set.Make (O_pkg)

  let dep_reduction v =
    let g = Defaultgraphs.PackageGraph.dependency_graph (Cudf.load_universe v) in
    let () = PO.transitive_reduction g in
    (* uncomment to view the dependency graph:
       XXX: cycles are not detected, which can lead to very weird situations
       Defaultgraphs.PackageGraph.D.output_graph stdout g; *)
    g

  let cudfpkg_of_debpkg table = List.map (Debian.Debcudf.tocudf table)
      
  let get_table l_pkg_pb f = 
    let table = Debian.Debcudf.init_tables l_pkg_pb in
    let v = f table (cudfpkg_of_debpkg table l_pkg_pb) in
    let () = Debian.Debcudf.clear table in
    v

  let filter_dependencies f_direction (U l_pkg_pb) (P pkg_l) =
    let pkg_map = 
      List.fold_left
        (fun map pkg -> NV.Map.add (NV.of_dpkg pkg) pkg map)
        NV.Map.empty
        l_pkg_pb in
    get_table l_pkg_pb
      (fun table pkglist ->
        let pkg_set = List.fold_left
          (fun accu pkg -> PkgSet.add (Debian.Debcudf.tocudf table pkg) accu)
          PkgSet.empty
          pkg_l in
        let g = f_direction (dep_reduction pkglist) in
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
            g (pkg_set, []) in
        List.map (fun pkg -> NV.Map.find (NV.of_cudf table pkg) pkg_map) l)

  let filter_backward_dependencies = filter_dependencies (fun x -> x)
  let filter_forward_dependencies = filter_dependencies PO.O.mirror

  let resolve (U l_pkg_pb) req =
    log "universe=%s request=<%s>"
      (string_of_packages l_pkg_pb)
      (string_of_request req);
    get_table l_pkg_pb 
      (fun table pkglist ->
        let package_map pkg = NV.of_cudf table pkg in
        let universe = Cudf.load_universe pkglist in 
        let sol_o =
          CudfDiff.resolve_diff universe 
            (request_map
               (fun x -> 
                 match Debian.Debcudf.ltocudf table [x] with
                 | [x] -> x
                 | _   -> failwith "TODO"
               ) req) in

        match sol_o with
        | None   -> None
        | Some l ->

            let l_del_p, l_del = 
              Utils.filter_map (function
                | I_to_change (Some pkg, _) 
                | I_to_delete pkg -> Some pkg
                | _ -> None) l,
              Utils.filter_map (function
                | I_to_delete pkg -> Some pkg
                | _ -> None) l in

            let map_add = 
              Utils.map_of_list PkgMap.empty PkgMap.add (Utils.filter_map (function 
                | I_to_change (_, pkg) as act -> Some (pkg, act)
                | I_to_delete _ -> None
                | I_to_recompile _ -> assert false) l) in

            let graph_installed = 
              PO.O.mirror 
                (dep_reduction 
                   (Cudf.get_packages 
                      ~filter:(fun p -> p.Cudf.installed || PkgMap.mem p map_add) 
                      universe)) in

            let graph_installed =
              let graph_installed = PG.copy graph_installed in
              List.iter (PG.remove_vertex graph_installed) l_del_p;
              graph_installed in

            let _, map_act = 
              PG_topo.fold
                (fun pkg (set_recompile, l_act) ->
                  let add_succ_rem pkg set act =
                    (let set = PkgSet.remove pkg set in
                     try
                       List.fold_left
                         (fun set x -> PkgSet.add x set) set (PG.succ graph_installed pkg)
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
                graph_installed
                (PkgSet.empty, Utils.IntMap.empty) in

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
              graph_installed;
            Some { to_remove = List.rev_map package_map l_del ; to_add = graph })

end

let filter_backward_dependencies = Graph.filter_backward_dependencies
let filter_forward_dependencies = Graph.filter_forward_dependencies
let resolve = Graph.resolve

let delete_or_update t =
  t.to_remove <> [] || 
  PA_graph.fold_vertex
    (fun v acc ->
      acc || match v.action with To_change (Some _, _) -> true | _ -> false)
    t.to_add
    false
