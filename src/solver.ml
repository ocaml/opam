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

  type ('a, 'b) action = 
    | To_change of 'a 
        (* Version to install. The package could have been present or not, 
           but if present, it is another version than the proposed solution. *)
    | To_delete of 'b (* The package has been installed. *)
    | To_recompile of 'b (* The package is already installed, we just recompile it. *)

  type 'a parallel = P of 'a list (* order irrelevant : elements are considered in parallel *)

  type 'a solution = 
      ( 'a (* old *) installed_status * 'a (* new *)
      , 'a (* old *) )
        action parallel list
      (** Sequence describing the action to perform.
          Order natural : first element to execute is the first element of the list. *)

  val solution_print : ('a BatIO.output -> 'b -> unit) -> 'a BatIO.output -> 'b solution -> unit
  val solution_map : ('a -> 'b) -> 'a solution -> 'b solution

  val resolve : Cudf.package list -> Cudf_types.vpkg request -> Cudf.package solution list
    (** Given a description of packages, it returns a list of solution preserving the consistency of the initial description. *)
end

module Solver = struct

  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type ('a, 'b) action = 
    | To_change of 'a 
    | To_delete of 'b
    | To_recompile of 'b

  type 'a parallel = P of 'a list

  type 'a solution = 
      ( 'a (* old *) installed_status * 'a (* new *)
      , 'a (* old *) )
        action parallel list

  let solution_map f = 
    BatList.map (function P l -> P (BatList.map (function
      | To_change (o_p, p) -> To_change ((match o_p with
          |  Was_installed p -> Was_installed (f p)
          | Was_not_installed -> Was_not_installed), f p)
      | To_delete p -> To_delete (f p)
      | To_recompile p -> To_recompile (f p)) l))

  let solution_print f = 
    BatList.print ~first:"" ~last:"" ~sep:", " 
      (fun oc (P l) -> 
        BatList.print ~first:"" ~last:"" ~sep:", " 
          (fun oc act -> 
            let f_act s l_p = 
              begin
                BatString.print oc (Printf.sprintf "%s : " s);
                BatList.print f oc l_p;
              end in
            match act with
              | To_change (o_v_old, p_new) -> 
                f_act "change"
                  (match o_v_old with
                    | Was_not_installed -> [ p_new ]
                    | Was_installed p_old -> [ p_old ; p_new ])
              | To_recompile _ -> ()
              | To_delete v -> f_act "remove" [v]) oc l)

  module type CUDFDIFF = 
  sig
    val resolve_diff : Cudf.package list -> Cudf_types.vpkg request ->
      (Cudf.package installed_status * Cudf.package, Cudf.package) action list option

    val resolve_summary : Cudf.package list -> Cudf_types.vpkg request ->
      ( Cudf.package list
        * (Cudf.package * Cudf.package) list
        * (Cudf.package * Cudf.package) list
        * Cudf.package list ) option
  end

  module CudfDiff : CUDFDIFF = struct

    let to_cudf_doc l_pkg req = 
      None, l_pkg, { Cudf.request_id = "" 
                   ; install = req.wish_install
                   ; remove = req.wish_remove
                   ; upgrade = req.wish_upgrade
                   ; req_extra = [] }


    let cudf_resolve l_pkg req = 
      let open Algo in
      let r = Depsolver.check_request (to_cudf_doc l_pkg req) in
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

    let resolve f_diff l_pkg_pb req = 
      BatOption.bind
        (fun l_pkg_sol -> 
          let univ_init = Cudf.load_universe l_pkg_pb in
          BatOption.bind 
            (f_diff univ_init)
            (try Some (Common.CudfDiff.diff univ_init (Cudf.load_universe l_pkg_sol))
             with Cudf.Constraint_violation _ -> None))
        (cudf_resolve l_pkg_pb req)

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
      [ match
      BatOption.bind 
        (let cons pkg act = Some (pkg, act) in
         fun l -> 
          let graph_installed = dep_reduction (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed) (Cudf.load_universe l_pkg_pb)) in
          
          let l_del_p, l_del = 
            BatList.split
              (BatList.filter_map (function
                | To_delete pkg as act -> cons pkg act
                | _ -> None) l) in

          let map_add = 
            PkgMap.of_enum (BatList.enum (BatList.filter_map (function 
              | To_change (_, pkg) as act -> cons pkg act
              | To_delete _ -> None
              | To_recompile _ -> assert false) l)) in

          let _, l_act = 
            PG_bfs.fold
              (fun (set_recompile, l_act) pkg -> 
                let add_succ_rem pkg set act =
                  (try
                     List.fold_left (fun set x -> PkgSet.add x set) (PkgSet.remove pkg set) (PG.succ graph_installed pkg) 
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
               PG.union graph_installed (dep_reduction (BatList.of_enum (PkgMap.keys map_add)))) in
          Some (List.rev l_act))
        (CudfDiff.resolve_diff l_pkg_pb req)
      with
        | None -> []
        | Some l -> BatList.map (fun x -> P [ x ]) l ]
  end

  let resolve = Graph.resolve
end
