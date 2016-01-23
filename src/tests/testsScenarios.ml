
(** create different repository scenarios with different packages sets *)

open TestsCommon

(* snippet from opamClient.ml *)

let init_repo repo root =
  let root = OpamFilename.Op.(OpamFilename.Dir.of_string root) in
  let compiler = OpamCompiler.system in
  let repo = OpamRepositoryBackend.default () in
  (* Create (possibly empty) configuration files *)
  let switch =
    if compiler = OpamCompiler.system then
	OpamSwitch.system
    else
	OpamSwitch.of_string (OpamCompiler.to_string compiler) in

  (* Create ~/.opam/compilers/system.comp *)
  OpamState.create_system_compiler_description root;

  (* Create ~/.opam/config *)
  let config =
    OpamFile.Config.create switch [repo.repo_name]
	OpamStateConfig.(Lazy.force default.jobs)
	OpamStateConfig.(default.dl_jobs)
  in
  OpamStateConfig.write root config;

  (* Create ~/.opam/aliases *)
  OpamFile.Aliases.write
    (OpamPath.aliases root)
    (OpamSwitch.Map.singleton switch compiler);

  (* Init repository *)
  OpamFile.Package_index.write (OpamPath.package_index root)
    OpamPackage.Map.empty;
  OpamFile.Compiler_index.write (OpamPath.compiler_index root)
    OpamCompiler.Map.empty;
  OpamFile.Repo_config.write (OpamRepositoryPath.config repo) repo;
  OpamProcess.Job.run (OpamRepository.init repo);
  ignore (OpamState.install_global_config root switch);

  (* Init global dirs *)
  OpamFilename.mkdir (OpamPath.packages_dir root);
  OpamFilename.mkdir (OpamPath.compilers_dir root);

  (* Load the partial state, and update the global state *)
  (* "updating repository state" *)
  let t = OpamState.load_state ~save_cache:false "init-1" switch in
  (* "Fetching repository information" *)
  (*
  let t = OpamProcess.Job.run (OpamRepositoryCommand.update t repo) t in
  OpamRepositoryCommand.fix_descriptions t ~save_cache:false ~verbose:false;
  *)

  (* Load the partial state, and install the new compiler if needed *)
  (* "updating package state" *)
  let quiet = (compiler = OpamCompiler.system) in
  OpamState.install_compiler t ~quiet switch compiler;

  (* Finally, load the complete state and install the compiler packages *)
  (* "installing compiler packages" *)
  OpamSwitchCommand.install_packages switch compiler
;;

let set_up test_ctxt =
  let root = OUnit2.bracket_tmpdir ~suffix:".opam" test_ctxt in
  init_repo "RepoTest" root ;
  OpamSystem.init ();
  OpamStd.Config.init ();
(*  OpamRepositoryConfig.init (); *)
  OpamSolverConfig.init ()

let tear_down () test_ctxt = ()
