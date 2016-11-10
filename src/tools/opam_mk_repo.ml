(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamFilename.Op
open OpamProcess.Job.Op

type args = {
  names: string list;
  dryrun: bool;
  recurse: bool;
  dev: bool;
  resolve: bool;
  debug: bool;
  (* option for resolve *)
  compiler_version: string option;
  switch: OpamSwitch.t option;
  os_string: string;
}

let args =
  let open Cmdliner in
  let dryrun =
    let doc = "Simply display the possible actions instead of executing them." in
    Arg.(value & flag & info ["d";"dryrun"] ~doc)
  in
  let recurse =
    let doc = "Recurse among the transitive dependencies of the packages and \
               download all available versions of the dependencies." in
    Arg.(value & flag & info ["r";"recursive"] ~doc)
  in
  let dev =
    let doc = "Include development dependencies while recursing among the \
               transitive dependencies of the packages." in
    Arg.(value & flag & info ["dev"] ~doc)
  in
  let resolve =
    let doc = "A more advanced version of `--recursive' that calculates \
               a single usable snapshot of the package universe, resulting \
               in a single version of each package.  If you need more flexibility \
               among package versions, you should continue to use `--recursive' \
               to download all the archives." in
    Arg.(value & flag & info ["resolve"] ~doc)
  in
  let compiler_version =
    let doc = "Specify the variable 'ocaml_version' to be used to filter \
               packages when using '--resolve'." in
    Arg.(value & opt (some string) None & info ~doc ["compiler"])
  in
  let switch =
    let doc = "Specify the variable switch' to be used to filter \
               packages when using '--resolve'. (Default: 'compiler_version')" in
    Arg.(value & opt (some string) None & info ~doc ["switch"])
  in
  let names =
    let doc = "Names of the packages to include in the repo." in
    Arg.(value & pos_all string [] & info [] ~docv:"PKG" ~doc)
  in
  let debug =
    let doc = "Display debug messages." in
    Arg.(value & flag & info ["debug"] ~doc)
  in
  Term.(
    pure
      (fun dryrun recurse dev names debug resolve
        compiler_version switch ->
        let switch =
          match switch with
          | None -> Option.map OpamSwitch.of_string compiler_version
          | Some switch -> Some (OpamSwitch.of_string switch) in
         {dryrun; recurse; dev; names; debug; resolve;
          compiler_version; switch; os_string = OpamStd.Sys.os_string ()})
    $dryrun $recurse $dev $names $debug $resolve
    $compiler_version $switch
  )

let string str = Some (S str)
let bool b = Some (B b)
let is_global_conf v =
  OpamVariable.Full.is_global v

let faked_var_resolve args v =
  let get_global_var v =
    if not (is_global_conf v) then None else
    match OpamVariable.to_string (OpamVariable.Full.variable v) with
    | "ocaml-version" -> begin
        match args.compiler_version with
        | None -> None
        | Some cv -> string cv
      end
    | "opam-version"  -> string (OpamVersion.to_string OpamVersion.current)
    | "preinstalled"  -> begin
        match args.compiler_version with
        | None -> None
        | Some cv -> bool (cv = "system")
      end
    | "switch"        -> begin
        match args.switch with
        | None -> None
        | Some s -> string (OpamSwitch.to_string s)
      end
    | "arch"          -> string (OpamStd.Sys.arch ())
    | _               -> None
  in
    try
      List.fold_left
        (function None -> (fun (f,v) -> f v) | r -> (fun _ -> r))
        None
        [
          OpamVariable.Full.read_from_env, v;
          get_global_var, v;
        ]
    with Exit -> None


let consistent_available_field args opam =
  OpamFilter.eval_to_bool ~default:false (faked_var_resolve args)
    (OpamFile.OPAM.available opam)

let resolve_deps args dir names =
  let atoms =
    List.map (fun str ->
        match OpamPackage.of_string_opt str with
        | Some nv ->
          OpamSolution.eq_atom nv.name nv.version
        | None -> OpamPackage.Name.of_string str, None)
      names in
  let opams =
    List.fold_left
      (fun opams f ->
         if OpamFilename.basename f = OpamFilename.Base.of_string "opam" then
           match OpamPackage.of_dirname (OpamFilename.dirname f) with
           | Some nv ->
             let opam = OpamFile.OPAM.read (OpamFile.make f) in
             if consistent_available_field args opam
             then OpamPackage.Map.add nv opam opams else opams
           | None -> opams
         else opams)
      OpamPackage.Map.empty
      (OpamFilename.rec_files dir) in
  let packages = OpamPackage.Set.of_list (OpamPackage.Map.keys opams) in
  let requested = OpamPackage.Name.Set.of_list (List.map fst atoms) in
  let universe = {
    OpamSolver.empty_universe with
    u_packages = packages;
    u_available = packages; (* XXX add a compiler/OS option ? *)
    u_depends = OpamPackage.Map.map OpamFile.OPAM.depends opams;
    u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts opams;
    u_action = Install requested;
  } in
  let request = { wish_install = atoms; wish_remove = []; wish_upgrade = [];
                  criteria = `Default; extra_attributes = []; } in
  match OpamSolver.resolve ~verbose:true universe
          ~orphans:OpamPackage.Set.empty request
  with
  | Success solution ->
    OpamSolver.ActionGraph.fold_vertex (fun act acc -> match act with
        | `Install p -> OpamPackage.Set.add p acc
        | `Remove _ -> acc
        | _ -> assert false)
      (OpamSolver.get_atomic_action_graph solution) OpamPackage.Set.empty
  | Conflicts cs ->
    OpamConsole.error_and_exit "%s"
      (OpamCudf.string_of_conflict
         (fun atom ->
            Printf.sprintf "%s is unavailable"
              (OpamFormula.string_of_atom atom))
         cs)

let rec process
    ({dryrun; recurse; names; debug; resolve; dev;_} as args) =
  OpamStd.Config.init
    ?debug_level:(if debug then Some 1 else None)
    ();

  let tmp_dirs = [ "tmp"; "log" ] in

  List.iter (fun dir ->
      if Sys.file_exists dir then (
        OpamConsole.error
          "The subdirectory '%s' already exists in the current directory. \n\
           Please remove or rename it or run %s in a different folder.\n"
          dir Sys.argv.(0);
        exit 1;
      )
    ) tmp_dirs;

  let () =
    let checkdir dir = Sys.file_exists dir && Sys.is_directory dir in
    if not (checkdir "packages" || checkdir "compilers") then
      OpamConsole.error_and_exit
        "No repository found in current directory.\n\
         Please make sure there is a \"packages\" or \"compilers\" directory" in

  let repo = OpamRepositoryBackend.local (OpamFilename.cwd ()) in

  let prefixes = OpamRepository.packages_with_prefixes repo in
  let packages = OpamRepository.packages repo in

  let mk_packages str =
    match OpamPackage.of_string_opt str with
    | Some nv -> OpamPackage.Set.singleton nv
    | None    ->
      let n = OpamPackage.Name.of_string str in
      match OpamPackage.Version.Set.elements
              (OpamPackage.versions_of_name packages n)
      with
      | []       ->
        OpamConsole.msg "Skipping unknown package %s.\n" str;
        OpamPackage.Set.empty
      | versions ->
        OpamPackage.Set.of_list (List.map (OpamPackage.create n) versions) in

  let new_packages =
    List.fold_left
      (fun accu str -> OpamPackage.Set.union accu (mk_packages str))
      OpamPackage.Set.empty names in

  if names <> [] && OpamPackage.Set.is_empty new_packages then (
    OpamConsole.msg "No package to process.\n";
    exit 0
  );

  (* Compute the transitive closure of packages *)
  let get_dependencies nv =
    let prefix = OpamPackage.Map.find nv prefixes in
    let opam_f = OpamRepositoryPath.opam repo.repo_root prefix nv in
    match OpamFile.OPAM.read_opt opam_f with
    | Some opam ->
      OpamFormula.ands OpamFile.OPAM.([depends opam; depopts opam]) |>
      OpamPackageVar.filter_depends_formula
        ~build:true ~dev ~default:true
        ~env:(fun _ -> None) |>
      OpamFormula.atoms |>
      List.fold_left (fun acc atom ->
          (* fixme: this is a vast super-approximation *)
          OpamPackage.Set.union acc @@
          OpamPackage.Set.filter (fun nv -> OpamFormula.check atom nv) packages)
        OpamPackage.Set.empty
    | None ->
      OpamPackage.Set.empty
  in
  let get_transitive_dependencies packages =
    let rec get_transitive_dependencies_aux visited to_visit = 
      match to_visit with 
        | [] -> visited
        | nv :: tl ->
            if OpamPackage.Set.mem nv visited then begin
              get_transitive_dependencies_aux visited tl
            end else begin
              let deps = OpamPackage.Set.elements (get_dependencies nv) in
                get_transitive_dependencies_aux
                  (* Mark the node as visited. *)
                  (OpamPackage.Set.add nv visited)
                  (* Plan to explore all deps. *)
                  (List.rev_append deps tl)
            end
    in
    get_transitive_dependencies_aux
      OpamPackage.Set.empty (OpamPackage.Set.elements packages) in
  let packages =
    if resolve then
      resolve_deps args repo.repo_root names
    else if recurse then
      get_transitive_dependencies new_packages
    else
      packages in

  let errors =
    OpamParallel.reduce ~jobs:8 ~dry_run:dryrun
      ~nil:OpamPackage.Map.empty
      ~merge:(OpamPackage.Map.union (fun a _ -> a))
      ~command:(fun nv ->
          let prefix = OpamPackage.Map.find nv prefixes in
          match
            OpamFileTools.read_opam
              (OpamRepositoryPath.packages repo.repo_root prefix nv)
          with
          | None -> Done (OpamPackage.Map.empty)
          | Some opam ->
            let add_to_cache ?name urlf errors =
              let label =
                OpamPackage.to_string nv ^
                OpamStd.Option.to_string ((^) "/") name
              in
              match OpamFile.URL.checksum urlf with
              | [] ->
                OpamConsole.warning "[%s] no checksum, not caching"
                  (OpamConsole.colorise `green label);
                Done errors
              | checksums ->
                OpamRepository.pull_file_to_cache label
                  ~cache_dir:(repo.repo_root / "cache")
                  checksums
                  (OpamFile.URL.url urlf :: OpamFile.URL.mirrors urlf)
                @@| function
                | Not_available m -> OpamPackage.Map.add nv m errors
                | _ -> errors
            in
            let urls =
              (match OpamFile.OPAM.url opam with
               | None -> []
               | Some urlf -> [add_to_cache urlf]) @
              (List.map (fun (name,urlf) ->
                   add_to_cache ~name:(OpamFilename.Base.to_string name) urlf)
                  (OpamFile.OPAM.extra_sources opam))
            in
            OpamProcess.Job.seq urls OpamPackage.Map.empty)
      (List.sort (fun nv1 nv2 ->
           (* Some pseudo-randomisation to avoid downloading all files from the
              same host simultaneously *)
         match compare (Hashtbl.hash nv1) (Hashtbl.hash nv2) with
         | 0 -> compare nv1 nv2
         | n -> n)
        (OpamPackage.Set.elements packages))
  in

  (* Create index.tar.gz *)
  OpamConsole.msg "Rebuilding index.tar.gz ...\n";
  if not dryrun then (
    OpamHTTP.make_index_tar_gz repo.repo_root;
  );

  if not dryrun then
    List.iter OpamSystem.remove tmp_dirs;

  if not (OpamPackage.Map.is_empty errors) then (
    OpamConsole.error "Got some errors while processing: %s"
      (OpamStd.List.concat_map ", " OpamPackage.to_string
         (OpamPackage.Map.keys errors));
    OpamConsole.errmsg "%s"
      (OpamStd.Format.itemize
         (fun (nv,e) -> Printf.sprintf "[%s] %s" (OpamPackage.to_string nv) e)
         (OpamPackage.Map.bindings errors))
  );
  let subdir = OpamFilename.Dir.of_string "2.0" in
  if OpamFilename.exists (subdir // "repo") then (
    OpamConsole.header_msg "Now processing within subdir 2.0/";
    let archives = OpamFilename.Dir.of_string "archives" in
    if OpamFilename.exists_dir archives then
      OpamFilename.copy_dir
        ~src:archives
        ~dst:(subdir / "archives");
    OpamFilename.in_dir subdir (fun () -> process args)
  )
