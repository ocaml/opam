open OUnit2
open TestsCommon

open OpamTypes

let make_package (n,v) =
  let name = OpamPackage.Name.of_string n in
  let version = OpamPackage.Version.of_string v in
  OpamPackage.create name version

let make_vpkg name l =
  let fml = 
    List.fold_left (fun acc -> function
      |None -> Empty
      |Some(op,v) ->
          let version = OpamPackage.Version.of_string v in
          Atom(op,version)
    ) Empty l
  in
  Atom (name,fml)

let make_ext_vpkg name depfl l =
  match make_vpkg name l with
  |Atom (name,fml) -> Atom (name,(depfl,fml))
  |_ -> assert false

let mk_universe ~packages ~installed ~depends ~conflicts = {
  OpamSolver.empty_universe with
  u_packages =
    List.fold_left (fun acc p ->
      OpamPackage.Set.add (make_package p) acc
    ) OpamPackage.Set.empty packages;
  u_installed =
    List.fold_left (fun acc p ->
      OpamPackage.Set.add (make_package p) acc
    ) OpamPackage.Set.empty installed;
  u_available = 
    List.fold_left (fun acc p ->
      OpamPackage.Set.add (make_package p) acc
    ) OpamPackage.Set.empty packages;
  u_depends =
    List.fold_left (fun acc (nv,n,l) ->
      let pkg = make_package nv in
      let fml = make_ext_vpkg (OpamPackage.Name.of_string n) [] l in
      OpamPackage.Map.add pkg fml acc
    ) OpamPackage.Map.empty depends;
  u_conflicts =
    List.fold_left (fun acc (nv,n,l) ->
      let pkg = make_package nv in
      let fml = make_vpkg (OpamPackage.Name.of_string n) l in
      OpamPackage.Map.add pkg fml acc
    ) OpamPackage.Map.empty conflicts;
} 

let test_load_cudf_universe_simple_1 test_ctxt =
  let universe = mk_universe
    ~packages:[("a","1");("b","1")]
    ~installed:[]
    ~depends:[(("a","1"),"b",[None])]
    ~conflicts:[]
  in
  let version_map = OpamSolver.cudf_versions_map universe universe.u_packages in
  let cudfuniv = 
    OpamSolver.load_cudf_universe ~build:false universe ~version_map universe.u_packages
  in
  let expectcudf = 
    let pkg_a = CudfOut.mk_pkg "a" ~depends:[["b",None]] ~conflicts:[("a",None)] in
    let pkg_b = CudfOut.mk_pkg "b" ~conflicts:[("b",None)] in
    [pkg_a;pkg_b]
  in
  DiffListCudf.assert_equal expectcudf (Cudf.get_packages cudfuniv)

let test_load_cudf_universe_simple_2 test_ctxt =
  let universe = mk_universe
    ~packages:[("a","1");("b","1");("c","1")]
    ~installed:[]
    ~depends:[(("a","1"),"b",[None])]
    ~conflicts:[(("a","1"),"c",[None])]
  in
  let version_map = OpamSolver.cudf_versions_map universe universe.u_packages in
  let cudfuniv = 
    OpamSolver.load_cudf_universe ~build:false universe ~version_map universe.u_packages
  in
  let expectcudf = 
    let pkg_a = CudfOut.mk_pkg "a" ~depends:[["b",None]] ~conflicts:[("c",None);("a",None)] in
    let pkg_b = CudfOut.mk_pkg "b" ~conflicts:[("b",None)] in
    let pkg_c = CudfOut.mk_pkg "c" ~conflicts:[("c",None)] in
    [pkg_a;pkg_b;pkg_c]
  in
  DiffListCudf.assert_equal expectcudf (Cudf.get_packages cudfuniv)

(** List of test cases as (name, function) tuples *)
let test_load_cudf_universe = [
  "load_cudf_universe simple", test_load_cudf_universe_simple_1;
  "load_cudf_universe simple", test_load_cudf_universe_simple_2;
]

let test_resolve_simple test_ctxt =
  OpamSolverConfig.init ();
  let universe = mk_universe
    ~packages:[("a","1");("b","1");("c","1")]
    ~depends:[(("a","1"),"b",[None])]
    ~conflicts:[(("a","1"),"c",[None])]
    ~installed:[("a","1")];
  in
  let request = OpamSolver.request ~criteria:`Fixup () in
  match OpamSolver.resolve universe ~orphans:OpamPackage.Set.empty request with
  |Success solution ->
      let expected = ActionGraphOut.make_actiongraph [`Remove ("a","1")] [] in
      let actiongraph = OpamSolver.get_atomic_action_graph solution in
      ActionGraphOut.assert_equal actiongraph expected
  |Conflicts cs -> assert_equal true true (* no way to test conflicts *)

(** List of test cases as (name, function) tuples *)
let test_resolve = [
  "resolve", test_resolve_simple;
]

(** Test suite for all functions *)
let test_suite =
  "load_cudf_universe" >::: (ListLabels.map test_load_cudf_universe ~f:(fun (name, test_fn) ->
    name >:: (fun test_ctxt ->
      bracket TestsScenarios.set_up TestsScenarios.tear_down test_ctxt;
      test_fn test_ctxt
    )
  )) @
   (ListLabels.map test_resolve ~f:(fun (name, test_fn) ->
      name >:: (fun test_ctxt ->
        bracket TestsScenarios.set_up TestsScenarios.tear_down test_ctxt;
        test_fn test_ctxt
      )
    ))

let () = run_test_tt_main test_suite
