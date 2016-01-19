open OUnit2
open TestsCommon

open OpamTypes

let tear_down () test_ctxt = ()

let make_package (n,v) =
  let name = OpamPackage.Name.of_string n in
  let version = OpamPackage.Version.of_string v in
  OpamPackage.create name version

let make_state l =
  let open OpamState in
  let opams =
    List.fold_left (fun acc (n,v) ->
      let pkg = make_package (n,v) in
      let opam = OpamFile.OPAM.create pkg in
      OpamPackage.Map.add pkg opam acc
    ) OpamPackage.Map.empty l
  in
  let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys opams) in
  {
    Types.partial = true;
    root = OpamFilename.Dir.of_string "";
    switch = OpamSwitch.system;
    compiler = OpamCompiler.system;
    compiler_version = lazy (OpamCompiler.Version.of_string "none");
    compiler_packages = OpamPackage.Set.empty;
    switch_config = OpamFile.Dot_config.empty;
    opams;
    repositories = OpamRepositoryName.Map.empty;
    packages;
    available_packages = lazy packages;
    aliases = OpamFile.Aliases.empty;
    compilers = OpamCompiler.Set.singleton OpamCompiler.system;
    pinned = OpamPackage.Name.Map.empty;
    installed = OpamPackage.Set.empty;
    installed_roots = OpamPackage.Set.empty;
    reinstall = OpamPackage.Set.empty;
    config = OpamFile.Config.empty;
    package_index = OpamPackage.Map.empty;
    compiler_index = OpamCompiler.Map.empty;
  }


let test_resolve_and_apply test_ctxt =
  OpamSolverConfig.init ();
  let t = make_state ["a","1"] in
  let action = Install (OpamPackage.Name.Set.singleton (OpamPackage.Name.of_string "a")) in
  let requested = OpamPackage.Name.Set.empty in
  let orphans = OpamPackage.Set.empty in
  let request = OpamSolver.request ~criteria:`Default () in
  let result = OpamSolution.resolve_and_apply t action requested orphans request in
  let expected = OK [] in
  SolverResultOut.assert_equal result expected

(** List of test cases as (name, function) tuples *)
let test_resolve = [
  "resolve", test_resolve_and_apply;
]

(** Test suite for all functions *)
let test_suite =
  "test resolve" >::: (ListLabels.map 
    test_resolve ~f:(fun (name, test_fn) ->
    name >:: (fun test_ctxt ->
      bracket ignore tear_down test_ctxt;
      test_fn test_ctxt
    )
  )) 

let () = run_test_tt_main test_suite
