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

  (** Given a description of packages, return the list of solution preserving
      the consistency of the initial description. *)
  val resolve :
    Debian.Packages.package list -> Debian.Format822.vpkg request
    -> name_version solution list
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
    let pf = Printf.printf in
    List.iter (fun (P l) ->
      List.iter (function
        | To_recompile p                   -> pf "Recompile: %s\n" (f p)
        | To_delete p                      -> pf "Remove: %s\n" (f p)
        | To_change (Was_not_installed, p) -> pf "Install: %s\n" (f p)
        | To_change (Was_installed o, p)   -> pf "Update: %s -> %s\n" (f o) (f p)
      ) l)

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

    module PG_bfs = 
    struct
      include Graph.Traverse.Bfs (PG)
      let fold f acc g = 
        let rec aux acc iter = 
          match try Some (get iter, step iter) with Exit -> None with
            | None -> acc
            | Some (x, iter) -> aux (f acc x) iter in
        aux acc (start g)
    end

    module O_pkg = struct type t = Cudf.package let compare = compare end
    module PkgMap = BatMap.Make (O_pkg)
    module PkgSet = BatSet.Make (O_pkg)

    let dep_reduction v =
      let g = Defaultgraphs.PackageGraph.dependency_graph (Cudf.load_universe v) in
      let () = PO.transitive_reduction g in
      g

    let resolve l_pkg_pb req = 
      let table = Debian.Debcudf.init_tables l_pkg_pb in
      let pkglist = 
        List.map
          (fun pkg ->
            let p = Debian.Debcudf.tocudf table pkg in
            { p with Cudf.conflicts = List.tl p.Cudf.conflicts
              (* we cancel the 'self package conflict' notion introduced in [loadlc] in debcudf.ml *) } ) l_pkg_pb in
      let universe = Cudf.load_universe pkglist in 
      let l = 
      [ match
      BatOption.bind 
        (let cons pkg act = Some (pkg, act) in
         fun l -> 
          let graph_installed = dep_reduction (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) universe) in
          
          let l_del_p, l_del = 
            List.split
              (List.filter_map (function
                | To_delete pkg as act -> cons pkg act
                | _ -> None) l) in

          let map_add = 
            PkgMap.of_list (List.filter_map (function 
              | To_change (_, pkg) as act -> cons pkg act
              | To_delete _ -> None
              | To_recompile _ -> assert false) l) in

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
                      set_recompile, l_act) (PkgSet.empty, List.rev l_del) 
              (let graph_installed = PG.copy graph_installed in
               let () = List.iter (PG.remove_vertex graph_installed) l_del_p in              
               PG.union graph_installed (dep_reduction (PkgMap.keys map_add))) in
          Some (List.rev l_act))
        (CudfDiff.resolve_diff universe 
           (request_map
              (fun x -> 
                match Debian.Debcudf.ltocudf table [x] with
                  | [x] -> x
                  | _ -> failwith "to complete !") req))
      with
        | None -> []
        | Some l -> 
          solution_map
            (fun pkg ->
              Namespace.Name pkg.Cudf.package,
              { Namespace.deb =
                  Debian.Debcudf.get_real_version
                    table
                    (pkg.Cudf.package, pkg.Cudf.version) })
            (List.map (fun x -> P [ x ]) l) ] in
      let () = Debian.Debcudf.clear table in
      l
  end

  let resolve = Graph.resolve
end
