open OUnit2
open ExtLib

module MapString = Map.Make(String)
module SetString = Set.Make(String)

module StringOut =
  struct
    type t = string
    let compare = String.compare
    let pp_printer = Format.pp_print_string
    let pp_print_sep = OUnitDiff.pp_comma_separator
  end

module DiffSetString = OUnitDiff.SetMake (StringOut)
module DiffListString = OUnitDiff.ListSimpleMake (StringOut)

module CudfOut =
  struct
    type t = Cudf.package
    let compare p q =
      let c = Common.CudfAdd.compare p q in
      if c = 0 then
        let dc = Pervasives.compare p.Cudf.depends q.Cudf.depends in
        if dc = 0 then
          Pervasives.compare p.Cudf.conflicts q.Cudf.conflicts
        else
          dc
      else c

    let pp_printer fmt p = 
      let o = IO.output_string () in
      Cudf_printer.pp_io_package o p;
      Format.fprintf fmt "%s" (IO.close_out o)

    let pp_print_sep fmt () = Format.fprintf fmt "\n--\n"

    let mk_pkg ?(version=1) ?(depends=[]) ?(conflicts=[]) name =
      { Cudf.default_package with
        Cudf.package = name ;
        version;
        conflicts;
        depends
      }

  end

module DiffSetCudf = OUnitDiff.SetMake (CudfOut)
module DiffListCudf = OUnitDiff.ListSimpleMake (CudfOut)

module ActionGraphOut =
  struct
    open OpamSolver
    module EdgeSet = Set.Make(ActionGraph.E)
    module VertexSet = Set.Make(ActionGraph.V)
    type t = ActionGraph.t
    let compare g1 g2 =
      let g1_v = ActionGraph.fold_vertex VertexSet.add g1 VertexSet.empty in
      let g2_v = ActionGraph.fold_vertex VertexSet.add g2 VertexSet.empty in
      let c = VertexSet.compare g1_v g2_v in
      if c = 0 then
        let g1_e = ActionGraph.fold_edges_e EdgeSet.add g1 EdgeSet.empty in
        let g2_e = ActionGraph.fold_edges_e EdgeSet.add g2 EdgeSet.empty in
        EdgeSet.compare g1_e g2_e
      else c

    let pp_printer fmt g = 
      let g_v = ActionGraph.fold_vertex VertexSet.add g VertexSet.empty in
      VertexSet.iter (fun v -> Format.fprintf fmt "%s" (Action.to_string v)) g_v
    let printer g = Common.CudfAdd.string_of pp_printer g

    let pp_print_sep = OUnitDiff.pp_comma_separator

    let equal g1 g2 = (compare g1 g2) = 0
    let assert_equal = assert_equal ~cmp:equal ~printer
    let make_actiongraph vertexlist edgelist =
      let g = ActionGraph.create () in
      let opampkg (n,v) =
        OpamPackage.create
          (OpamPackage.Name.of_string n)
          (OpamPackage.Version.of_string v)
      in
      let vrtx = function 
        |`Remove (n,v) -> `Remove (opampkg (n,v))
        |`Install (n,v) -> `Install (opampkg (n,v))
        |`Reinstall (n,v) -> `Reinstall (opampkg (n,v))
        |`Build (n,v) -> `Build (opampkg (n,v))
        |`Change (t, (n1,v1), (n2,v2)) -> `Change (t,(opampkg (n1,v1)),(opampkg (n2,v2)))
      in
      List.iter (fun (a,b) -> ActionGraph.add_edge g (vrtx a) (vrtx b)) edgelist;
      List.iter (fun a -> ActionGraph.add_vertex g (vrtx a) ) vertexlist;
      g
  end

module SolverResultOut =
  struct
    open OpamTypes
    type t = OpamTypes.solver_result
    let compare = Pervasives.compare
    let pp_printer fmt = function
	| No_solution -> Format.fprintf fmt "No_solution"
	| Error (success, failed, _remaining) -> Format.fprintf fmt "Error"
	| OK actions -> Format.fprintf fmt "OK %s" (String.concat "," (List.map OpamSolver.Action.to_string actions))
	| Nothing_to_do -> Format.fprintf fmt "Nothing to do.\n"
	| Aborted     -> Format.fprintf fmt "Aborted"
    let printer g = Common.CudfAdd.string_of pp_printer g

    let pp_print_sep = OUnitDiff.pp_comma_separator
    let equal g1 g2 = (compare g1 g2) = 0
    let assert_equal = assert_equal ~cmp:equal ~printer

  end

(*

OpamCudf.conflict is pretty much uncomparable ...

module ConflictOut =
  struct
    type t = OpamCudf.conflict
    let compare c1 c2 =
      let ch1 = OpamCudf.conflict_chains c1 in
      let ch2 = OpamCudf.conflict_chains c2 in
      let cnf_ch1 = List.map (List.map OpamFormula.to_cnf) ch1 in
      let cnf_ch2 = List.map (List.map OpamFormula.to_cnf) ch2 in


  end
*)

let timer_start str =
  Unix.gettimeofday (), str

let timer_stop test_ctxt (time_start, str) =
  logf test_ctxt `Info "Time spent in '%s': %fs"
    str ((Unix.gettimeofday ()) -. time_start)

