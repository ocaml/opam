open ExtList
open Namespace
open Path
open Server

type 'a installed_status =
  | Was_installed of 'a
  | Was_not_installed

module type SOLVER =
sig
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

    (* The package is already installed, but it must be recompiled it. *)
    | To_recompile of 'a 

  type 'a parallel = P of 'a list (* order irrelevant : elements are considered in parallel *)

  (** Sequence describing the action to perform.
      Natural order : the first element to execute is the first element of the list. *)
  type 'a solution = 'a action parallel list

  val solution_print : ('a -> string) -> 'a solution -> unit
  val solution_map : ('a -> 'b) -> 'a solution -> 'b solution
  val request_map : ('a -> 'b) -> 'a request -> 'b request

  (** Given a description of packages, return a solution preserving
      the consistency of the initial description.
      [None] : No solution found. *)
  val resolve :
    Debian.Packages.package list -> Debian.Format822.vpkg request
    -> name_version solution option

  (** Same as [resolve], but each element of the list of solutions does not precise
      which request in the initial list was satisfied. *)
  val resolve_list : 
    Debian.Packages.package list -> Debian.Format822.vpkg request list
    -> name_version solution list

  (** Return the recursive dependencies of a package
      Note : the given package exists in the list in input because this list describes the entire universe. 
      However, by convention, it does not appear in output. *)
  val filter_dependencies : Debian.Packages.package -> Debian.Packages.package list -> Debian.Packages.package list
end

module Solver : SOLVER = struct

  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type 'a action = 
    | To_change of 'a installed_status * 'a
    | To_delete of 'a
    | To_recompile of 'a

  type 'a parallel = P of 'a list

  type 'a solution = 'a action parallel list

  let solution_map f = 
    List.map (function P l -> P (List.map (function
      | To_change (Was_installed p1, p2) -> To_change (Was_installed (f p1), f p2)
      | To_change (Was_not_installed, p) -> To_change (Was_not_installed, f p)
      | To_delete p                      -> To_delete (f p)
      | To_recompile p                   -> To_recompile (f p)
    ) l))

  let solution_print f =
    let pf = Globals.msg in
    function 
      | [] -> pf "No actions will be performed, the current state satisfies the request.\n"
      | l -> 
        let l_total = List.fold_left (fun acc (P l) -> acc + List.length l) 0 l in
        List.iteri (fun i1 (P l) ->
          List.iteri (fun i2 -> 
            let pf f = Printf.kprintf (pf "[%d/%d] %s" (succ (i1 + i2)) l_total) f in
            function
            | To_recompile p                   -> pf "Recompile: %s\n" (f p)
            | To_delete p                      -> pf "Remove: %s\n" (f p)
            | To_change (Was_not_installed, p) -> pf "Install: %s\n" (f p)
            | To_change (Was_installed o, p)   -> pf "Update: %s (Remove) -> %s (Install)\n" (f o) (f p)
          ) l) l

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

    module PG_bfs = Make_fs (Graph.Traverse.Bfs (PG))
    module PG_dfs = Make_fs (Graph.Traverse.Dfs (PG))
    module PG_topo = Graph.Topological.Make (PG)

    module O_pkg = struct type t = Cudf.package let compare = compare end
    module PkgMap = BatMap.Make (O_pkg)
    module PkgSet = BatSet.Make (O_pkg)

    let dep_reduction v =
      let g = Defaultgraphs.PackageGraph.dependency_graph (Cudf.load_universe v) in
      let () = PO.transitive_reduction g in
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

    let filter_dependencies pkg l_pkg_pb = 
      let pkg_map = 
        List.fold_left
          (fun map pkg -> 
            NV_map.add
              (Namespace.Name pkg.Debian.Packages.name, { Namespace.deb = pkg.Debian.Packages.version }) 
              pkg
              map)
          NV_map.empty
          l_pkg_pb in
      get_table l_pkg_pb
        (fun table pkglist -> 
          let g = dep_reduction pkglist in
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
              (PkgSet.add (tocudf table pkg) PkgSet.empty, []) in
          List.map (fun pkg -> 
            NV_map.find 
              (Namespace.Name pkg.Cudf.package, 
               { Namespace.deb =
                   Debian.Debcudf.get_real_version
                     table
                     (pkg.Cudf.package, pkg.Cudf.version) }) pkg_map) l)
        
    let resolve l_pkg_pb req = 
      get_table l_pkg_pb 
      (fun table pkglist ->
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
               | To_delete _ as act -> Some act
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

          let _, l_act = 
            PG_bfs.fold
              (fun (set_recompile, l_act) pkg -> 
                let add_succ_rem pkg set act =
                  (let set = PkgSet.remove pkg set in
                   try
                     List.fold_left
                       (fun set x -> PkgSet.add x set) set (PG.succ graph_installed pkg)
                   with _ -> set), 
                  act :: l_act in
                
                match PkgMap.Exceptionless.find pkg map_add with
                  | Some act -> 
                    add_succ_rem pkg set_recompile act
                  | None ->
                    if PkgSet.mem pkg set_recompile then
                      add_succ_rem pkg set_recompile (To_recompile pkg)
                    else
                      set_recompile, l_act)
              (PkgSet.empty, List.rev l_del)
              (let graph_installed = PG.copy graph_installed in
               let () = List.iter (PG.remove_vertex graph_installed) l_del_p in
               graph_installed) in

          Some
            (solution_map
               (fun pkg ->
                 Namespace.Name pkg.Cudf.package,
                 { Namespace.deb =
                     Debian.Debcudf.get_real_version
                       table
                       (pkg.Cudf.package, pkg.Cudf.version) })
               (List.map (fun x -> P [ x ]) (List.rev l_act))))

        (CudfDiff.resolve_diff universe 
           (request_map
              (fun x -> 
                match Debian.Debcudf.ltocudf table [x] with
                  | [x] -> x
                  | _ -> failwith "to complete !") req)))
  end

  let filter_dependencies = Graph.filter_dependencies
  let resolve = Graph.resolve
  let resolve_list pkg = List.filter_map (resolve pkg)
end
