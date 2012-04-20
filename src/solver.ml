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

open ExtList
open Namespace
open Path
open Server
open Protocol

let log fmt = Globals.log "SOLVER" fmt

type 'a installed_status =
  | Was_installed of 'a
  | Was_not_installed

module Parallel_fold =
struct
  module type G =
  sig
    include Graph.Topological.G
    val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  end

  module type TOPOLOGICAL =
  sig
    (* This module considers the graph to fold as a forest where the children of each node are topologically ordered after their parent. *)

    module G : G

    type 'a plist = 'a list (* nodes with minimal in-degree are proposed simultaneously *)
    type t

    val root : G.t -> t * G.V.t plist (** several proposed choices that can be considered in parallel *)

    (** [Invalid_argument _] in case the children of [G.V.t] has already been requested before. *)
    val children : t -> G.V.t (** answers that we have consumed *) -> t * G.V.t plist (* new choices resulting from the consumption that we can append with [root] or with any previous calls to [children] *)
  end

  module Make (G : G) : TOPOLOGICAL with module G = G = struct

    module H = Hashtbl.Make (G.V)
    module G = G

    module IntSet = Set.Make (struct type t = int let compare = compare end)

    type 'a plist = 'a list (* nodes with minimal in-degree are proposed simultaneously *)
    type t = 
        { graph : G.t
        ; visited_node : IntSet.t (* [int] represents the hash of [G.V.t] *)
        ; queue_size : int
        ; degree : int H.t }

    let root graph = 
      let degree = H.create 997 in
      let l, queue_size = 
        G.fold_vertex
          (fun v (todo, queue_size) ->
            let d = G.in_degree graph v in
            if d = 0 then v :: todo, succ queue_size
            else let () = H.add degree v d in todo, queue_size)
          graph
          ([], 0) in
      { graph ; degree ; queue_size ; visited_node = IntSet.empty }, l
      
    let children t x =
      let t = 
        if IntSet.mem (G.V.hash x) t.visited_node then
          invalid_arg "This node has already been visited."
        else
          { t with visited_node = IntSet.add (G.V.hash x) t.visited_node } in
      let t, l = 
        (* simulate the removing of [x] and the adding of the children of [x] *)
        let l, queue_size = 
          G.fold_succ
            (fun x (l, queue_size) ->
              try
                let d = H.find t.degree x in
                if d = 1 then 
                  let () = H.remove t.degree x in 
                  x :: l, succ queue_size
                else
                  let () = H.replace t.degree x (d-1) in 
                  l, queue_size
              with Not_found ->
                (* [x] already visited *)
                l, queue_size)
            t.graph
            x
            ([], pred t.queue_size) in
        { t with queue_size }, l in

      match t.queue_size, l with
        | 0, [] -> 
          (* let's find any nodes of minimal degree *)
          let min =
            H.fold
              (fun v d acc ->
                 match acc with
                 | None -> Some ([v], d)
                 | Some (l, min) -> 
                     if d < min then
                       Some ([v], d) 
                     else if d = min then
                       Some (v :: l, min) 
                     else
                       acc)
              t.degree
              None
          in
          let l = 
            match min with
              | None -> []
              | Some (l, min) -> 
                  let () = List.iter (H.remove t.degree) l in 
                  l in
          { t with queue_size = List.length l }, l
        | _ -> t, l
  end
end

module Action = 
struct

  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type 'a action = 
    (* The package must be installed. The package could have been present or not, 
       but if present, it is another version than the proposed solution. *)
    | To_change of 'a installed_status * 'a 

    (* The package must be deleted. *)
    | To_delete of 'a

    (* The package is already installed, but it must be recompiled. *)
    | To_recompile of 'a 

  module NV_graph =
  struct
    module PkgV = 
    struct
      type t = 
          { cudf : Cudf.package 
          ; action : name_version action }
      (* NOTE the field [action] currently does not need to contain a boolean sum type
         (i.e. something isomorphic to [name_version action option] where [None] means an action not to perform) 
         because the graph containing all these nodes is composed of 2 parts :
         1. starting from the root, all the nodes that we ignore,
         2. when we encounter a node action, every children is an action (not a node to ignore). *)

      module PkgV = Algo.Defaultgraphs.PackageGraph.PkgV

      let compare t1 t2 = PkgV.compare t1.cudf t2.cudf
      let hash t = PkgV.hash t.cudf
      let equal t1 t2 = PkgV.equal t1.cudf t2.cudf
    end

    module PG = Graph.Imperative.Digraph.ConcreteBidirectional (PkgV)
    module PG_topo = Graph.Topological.Make (PG)
    module PG_topo_para = Parallel_fold.Make (PG)
  end

  type solution = 
      { to_remove : name_version list
      ; to_add : NV_graph.PG.t }

  let action_map f = function
    | To_change (Was_installed p1, p2) -> To_change (Was_installed (f p1), f p2)
    | To_change (Was_not_installed, p) -> To_change (Was_not_installed, f p)
    | To_delete p                      -> To_delete (f p)
    | To_recompile p                   -> To_recompile (f p)

  let solution_print f t =
    let pf = Globals.msg in
    if t.to_remove = [] && NV_graph.PG.is_empty t.to_add then
      pf "No actions will be performed, the current state satisfies the request.\n"
    else
      let pf f = Printf.kprintf (pf " %s") f in
      begin
        List.iter (fun p -> pf "Remove: %s\n" (f p)) t.to_remove;
        NV_graph.PG_topo.iter
          (let open NV_graph.PkgV in function { action ; _ } -> 
           match action with
             | To_recompile p                   -> pf "Recompile: %s\n" (f p)
             | To_delete p                      -> assert false (* items to delete are listed above *)
             | To_change (Was_not_installed, p) -> pf "Install: %s\n" (f p)
             | To_change (Was_installed o, p)   -> pf "Update: %s (Remove) -> %s (Install)\n" (f o) (f p))
          t.to_add;
      end
end

open Action

module type SOLVER =
sig
  val request_map : ('a -> 'b) -> 'a request -> 'b request

  (** Given a description of packages, return a solution preserving
      the consistency of the initial description.
      [None] : No solution found. *)
  val resolve :
    Debian.Packages.package list -> Debian.Format822.vpkg request
    -> solution option

  (** Same as [resolve], but each element of the list of solutions does not precise
      which request in the initial list was satisfied. *)
  val resolve_list : 
    Debian.Packages.package list -> Debian.Format822.vpkg request list
    -> solution list

  (** Return the recursive dependencies of a package
      Note : the given package exists in the list in input because this list describes the entire universe. 
      By convention, it also appears in output. *)
  val filter_backward_dependencies :
    Debian.Packages.package list (* few packages from the universe *)
    -> Debian.Packages.package list (* universe *)
    -> Debian.Packages.package list

  (** Same as [filter_backward_dependencies] but for forward dependencies *)
  val filter_forward_dependencies :
    Debian.Packages.package list (* few packages from the universe *)
    -> Debian.Packages.package list (* universe *)
    -> Debian.Packages.package list

  val delete_or_update : solution -> bool
end

module Solver : SOLVER = struct

  let request_map f r = 
    let f = List.map f in
    { wish_install = f r.wish_install
    ; wish_remove = f r.wish_remove
    ; wish_upgrade = f r.wish_upgrade }

  module type CUDFDIFF = 
  sig
    val resolve_diff :
      Cudf.universe -> Cudf_types.vpkg request -> Cudf.package action list option

    val resolve_summary : Cudf.universe -> Cudf_types.vpkg request ->
      ( Cudf.package list
        * (Cudf.package * Cudf.package) list
        * (Cudf.package * Cudf.package) list
        * Cudf.package list ) option
  end

  module CudfDiff : CUDFDIFF = struct

    let to_cudf_doc univ req = 
      None, 
      Cudf.fold_packages (fun l x -> x :: l) [] univ, 
      { Cudf.request_id = "" 
      ; install = req.wish_install
      ; remove = req.wish_remove
      ; upgrade = req.wish_upgrade
      ; req_extra = [] }

    let cudf_resolve univ req = 
      let open Algo in
      let r = Depsolver.check_request (to_cudf_doc univ req) in
      if Diagnostic.is_solution r then
        match r with
        | { Diagnostic.result = Diagnostic.Success f } -> Some (f ~all:true ())
        | _ -> assert false
      else
        None

    module Cudf_set = struct
      module S = Common.CudfAdd.Cudf_set

      let choose_one s = 
        match S.cardinal s with
          | 0 -> raise Not_found
          | 1 -> S.choose s
          | _ ->
            failwith "to complete ! Determine if it suffices to remove one arbitrary element from the \"removed\" class, or remove completely every element."

      include S
    end

    let resolve f_diff univ_init req = 
      BatOption.bind
        (fun l_pkg_sol -> 
          BatOption.bind 
            (f_diff univ_init)
            (try Some (Common.CudfDiff.diff univ_init (Cudf.load_universe l_pkg_sol))
             with Cudf.Constraint_violation _ -> None))
        (cudf_resolve univ_init req)

    let resolve_diff = 
      resolve
        (fun _ diff -> 
          match 
            Hashtbl.fold (fun pkgname s acc ->
              let add x = x :: acc in
              match 
                (try Some (Cudf_set.choose_one s.Common.CudfDiff.removed) with Not_found -> None), 
                try Some (Cudf_set.choose s.Common.CudfDiff.installed) with Not_found -> None
              with
                | None, Some p -> add (To_change (Was_not_installed, p))
                | Some p, None -> add (To_delete p)
                | Some p_old, Some p_new -> add (To_change (Was_installed p_old, p_new))
                | None, None -> acc) diff []
          with
            | [] -> None
            | l -> Some l)

    let resolve_summary = resolve (fun univ_init diff -> Some (Common.CudfDiff.summary univ_init diff))
  end

  module Graph = 
  struct
    open Algo

    module PG = 
    struct
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
    module PkgMap = BatMap.Make (O_pkg)
    module PkgSet = BatSet.Make (O_pkg)

    let dep_reduction v =
      let g = Defaultgraphs.PackageGraph.dependency_graph (Cudf.load_universe v) in
      let () = PO.transitive_reduction g in
      (* uncomment to view the dependency graph:
         XXX: cycles are not detected, which can lead to very weird situations
         Defaultgraphs.PackageGraph.D.output_graph stdout g; *)
      g

    let tocudf table pkg = 
      let p = Debian.Debcudf.tocudf table pkg in
      { p with Cudf.conflicts = List.tl p.Cudf.conflicts
              (* we cancel the 'self package conflict' notion introduced in [loadlc] in debcudf.ml *) }

    let cudfpkg_of_debpkg table = List.map (tocudf table)

    let get_table l_pkg_pb f = 
      let table = Debian.Debcudf.init_tables l_pkg_pb in
      let v = f table (cudfpkg_of_debpkg table l_pkg_pb) in
      let () = Debian.Debcudf.clear table in
      v

    let filter_dependencies f_direction pkg_l l_pkg_pb =
      let pkg_map = 
        List.fold_left
          (fun map pkg -> NV_map.add (Namespace.nv_of_dpkg pkg) pkg map)
          NV_map.empty
          l_pkg_pb in
      get_table l_pkg_pb
        (fun table pkglist ->
          let pkg_set = List.fold_left
            (fun accu pkg -> PkgSet.add (tocudf table pkg) accu)
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
          List.map (fun pkg -> 
            NV_map.find 
              (Namespace.name_of_string pkg.Cudf.package, 
               Namespace.version_of_string
                 (Debian.Debcudf.get_real_version
                    table
                    (pkg.Cudf.package, pkg.Cudf.version))
              ) pkg_map) l)

    let filter_backward_dependencies = filter_dependencies (fun x -> x)
    let filter_forward_dependencies = filter_dependencies PO.O.mirror

    open BatMap

    let resolve l_pkg_pb req = 
      get_table l_pkg_pb 
      (fun table pkglist ->
      let package_map pkg =
        Namespace.name_of_string pkg.Cudf.package,
        Namespace.version_of_string
          (Debian.Debcudf.get_real_version
             table
             (pkg.Cudf.package, pkg.Cudf.version)) in
      let universe = Cudf.load_universe pkglist in 

      BatOption.bind 
        (let cons pkg act = Some (pkg, act) in
         fun l -> 
           let l_del_p, l_del = 
             List.filter_map (function
               | To_change (Was_installed pkg, _) 
               | To_delete pkg -> Some pkg
               | _ -> None) l,
             List.filter_map (function
               | To_delete pkg -> Some pkg
               | _ -> None) l in

          let map_add = 
            PkgMap.of_list (List.filter_map (function 
              | To_change (_, pkg) as act -> cons pkg act
              | To_delete _ -> None
              | To_recompile _ -> assert false) l) in

          let graph_installed = 
            PO.O.mirror 
              (dep_reduction 
                 (Cudf.get_packages 
                    ~filter:(fun p -> p.Cudf.installed || PkgMap.mem p map_add) 
                    universe)) in

          let graph_installed =
            let graph_installed = PG.copy graph_installed in
            let () = List.iter (PG.remove_vertex graph_installed) l_del_p in
            graph_installed in
          let _, map_act = 
            let open NV_graph.PkgV in
            PG_topo.fold
              (fun pkg (set_recompile, l_act) ->
                let add_succ_rem pkg set act =
                  (let set = PkgSet.remove pkg set in
                   try
                     List.fold_left
                       (fun set x -> PkgSet.add x set) set (PG.succ graph_installed pkg)
                   with _ -> set), 
                  IntMap.add (PG.V.hash pkg) { cudf = pkg ; action = action_map package_map act } l_act in
                
                match PkgMap.Exceptionless.find pkg map_add with
                  | Some act -> 
                    add_succ_rem pkg set_recompile act
                  | None ->
                    if PkgSet.mem pkg set_recompile then
                      add_succ_rem pkg set_recompile (To_recompile pkg)
                    else
                      set_recompile, l_act)
              
              graph_installed
              (PkgSet.empty, IntMap.empty) in
          let graph = NV_graph.PG.create () in
          let () =
            begin
              IntMap.iter (fun _ -> NV_graph.PG.add_vertex graph) map_act;
              PG.iter_edges
                (fun v1 v2 -> 
                  match 
                    IntMap.Exceptionless.find (PG.V.hash v1) map_act, 
                    IntMap.Exceptionless.find (PG.V.hash v2) map_act 
                  with
                    | Some v1, Some v2 -> NV_graph.PG.add_edge graph v1 v2
                    | _ -> ())
                graph_installed;
            end in
          Some { to_remove = List.rev_map package_map l_del ; to_add = graph })

        (CudfDiff.resolve_diff universe 
           (request_map
              (fun x -> 
                match Debian.Debcudf.ltocudf table [x] with
                  | [x] -> x
                  | _ -> failwith "to complete !") req)))
  end

  let filter_backward_dependencies = Graph.filter_backward_dependencies
  let filter_forward_dependencies = Graph.filter_forward_dependencies
  let resolve = Graph.resolve
  let resolve_list pkg = List.filter_map (resolve pkg)

  let delete_or_update t =
    t.to_remove <> []
    || 
    NV_graph.PG.fold_vertex
      (let open NV_graph.PkgV in fun v acc -> 
        acc || match v.action with Action.To_change (Was_installed _, _) -> true | _ -> false)
      t.to_add
      false
end
