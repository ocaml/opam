(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let log fmt = OpamGlobals.log "SOLUTION" fmt

open OpamTypes
open OpamFilename.OP
open OpamState.Types

let proceed_to_install t nv =
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  if OpamFilename.exists_dir build_dir then OpamFilename.in_dir build_dir (fun () ->

    OpamGlobals.msg "Installing %s.\n" (OpamPackage.to_string nv);
    let t = OpamState.load_state () in
    let name = OpamPackage.name nv in
    let opam_f = OpamPath.opam t.root nv in
    let opam_ = OpamState.opam t nv in
    let config_f = OpamPath.Switch.build_config t.root t.switch nv in
    let config = OpamFile.Dot_config.safe_read config_f in
    let install_f = OpamPath.Switch.build_install t.root t.switch nv in
    let install = OpamFile.Dot_install.safe_read install_f in

    (* check that libraries and syntax extensions specified in .opam and
       .config are in sync *)
    let check kind config_sections opam_sections =
      List.iter (fun cs ->
        if not (List.mem cs opam_sections) then
          OpamGlobals.error_and_exit "The %s %s does not appear in %s"
            kind (OpamVariable.Section.to_string cs) (OpamFilename.to_string opam_f)
      ) config_sections;
      List.iter (fun os ->
        if not (List.mem os config_sections) then
          OpamGlobals.error_and_exit "The %s %s does not appear in %s"
            kind (OpamVariable.Section.to_string os) (OpamFilename.to_string config_f)
      ) opam_sections in
    if not (OpamFilename.exists config_f)
    && (OpamFile.OPAM.libraries opam_ <> [] || OpamFile.OPAM.syntax opam_ <> []) then
      OpamGlobals.error_and_exit
        "%s does not exist but %s defines some libraries and syntax extensions"
        (OpamFilename.to_string config_f)
        (OpamFilename.to_string opam_f);
    check "library"
      (OpamFile.Dot_config.Library.available config)
      (OpamFile.OPAM.libraries opam_);
    check "syntax"
      (OpamFile.Dot_config.Syntax.available config)
      (OpamFile.OPAM.syntax opam_);

    (* check that depends (in .opam) and requires (in .config) fields
       are in almost in sync *)
    (* NOTES: the check is partial as we don't know which clause is valid
       in depends (XXX there is surely a way to get it from the solver) *)
    let local_sections = OpamFile.Dot_config.Section.available config in
    let libraries_in_opam =
      OpamFormula.fold_left (fun accu (n,_) ->
        if OpamState.mem_installed_package_by_name t n then (
          let nv = OpamState.find_installed_package_by_name t n in
          let opam = OpamState.opam t nv in
          let libs = OpamFile.OPAM.libraries opam in
          let syntax = OpamFile.OPAM.syntax opam in
          List.fold_right OpamVariable.Section.Set.add (libs @ syntax) accu
        ) else
          accu
      ) OpamVariable.Section.Set.empty (OpamFile.OPAM.depends opam_) in
    let libraries_in_config =
      List.fold_left (fun accu s ->
        List.fold_left (fun accu r ->
          OpamVariable.Section.Set.add r accu
        ) accu (OpamFile.Dot_config.Section.requires config s)
      ) OpamVariable.Section.Set.empty local_sections in
    OpamVariable.Section.Set.iter (fun s ->
      if not (List.mem s local_sections)
      && not (OpamVariable.Section.Set.mem s libraries_in_opam) then
        let config_f = OpamFilename.to_string (OpamPath.Switch.build_config t.root t.switch nv) in
        let opam_f = OpamFilename.to_string (OpamPath.opam t.root nv) in
        let local_sections = List.map OpamVariable.Section.to_string local_sections in
        let opam_sections = List.map OpamVariable.Section.to_string (OpamVariable.Section.Set.elements libraries_in_opam) in
        OpamGlobals.error_and_exit
          "%s appears as a library dependency in %s, but:\n\
             - %s defines the libraries {%s}\n\
             - Packages in %s defines the libraries {%s}"
          (OpamVariable.Section.to_string s) config_f
          config_f (String.concat ", " local_sections)
          opam_f (String.concat ", " opam_sections)
    ) libraries_in_config;

    (* .install *)
    OpamFile.Dot_install.write (OpamPath.Switch.install t.root t.switch name) install;

    (* .config *)
    OpamFile.Dot_config.write (OpamPath.Switch.config t.root t.switch name) config;

    (* lib *)
    let warnings = ref [] in
    let check f dst =
      if not f.optional && not (OpamFilename.exists f.c) then (
        warnings := (f.c, dst) :: !warnings
      );
      OpamFilename.exists f.c in
    let lib = OpamPath.Switch.lib t.root t.switch name in
    List.iter (fun f ->
      if check f lib then
        OpamFilename.copy_in f.c lib
    ) (OpamFile.Dot_install.lib install);

    (* toplevel *)
    let toplevel = OpamPath.Switch.toplevel t.root t.switch in
    List.iter (fun f ->
      if check f toplevel then
        OpamFilename.copy_in f.c toplevel
    ) (OpamFile.Dot_install.toplevel install);

    (* bin *)
    List.iter (fun (src, dst) ->
      let dst = OpamPath.Switch.bin t.root t.switch // OpamFilename.Base.to_string dst in
      (* WARNING [dst] could be a symbolic link (in this case, it will be removed). *)
      if check src  (OpamPath.Switch.bin t.root t.switch) then
        OpamFilename.copy src.c dst;
    ) (OpamFile.Dot_install.bin install);

    (* misc *)
    List.iter
      (fun (src, dst) ->
        if OpamFilename.exists dst && OpamState.confirm "Overwriting %s ?" (OpamFilename.to_string dst) then
          OpamFilename.copy src.c dst
        else begin
          OpamGlobals.msg "Installing %s to %s.\n" (OpamFilename.to_string src.c) (OpamFilename.to_string dst);
          if OpamState.confirm "Continue ?" then
            OpamFilename.copy src.c dst
        end
      ) (OpamFile.Dot_install.misc install);

    (* Shared files *)
    List.iter (fun (src, dst) ->
      let share = OpamPath.Switch.share t.root t.switch name in
      let dst = share // OpamFilename.Base.to_string dst in
      (* WARNING [dst] could be a symbolic link (in this case, it will be removed). *)
      if not (OpamFilename.exists_dir share) then
        OpamFilename.mkdir share;
      OpamFilename.copy src.c dst;
    ) (OpamFile.Dot_install.share install);

    if !warnings <> [] then (
      let print (f, dst) = Printf.sprintf " - %s in %s" (OpamFilename.to_string f) (OpamFilename.Dir.to_string dst) in
      OpamGlobals.error
        "Error while installing the following files:\n%s"
        (String.concat "\n" (List.map print !warnings));
      OpamGlobals.exit 2;
    )
  );
  if not (!OpamGlobals.keep_build_dir || !OpamGlobals.debug) then
    OpamFilename.rmdir build_dir

let pinned_path t nv =
  let name = OpamPackage.name nv in
  if OpamPackage.Name.Map.mem name t.pinned then
    match OpamPackage.Name.Map.find name t.pinned with
    | Path _
    | Darcs _
    | Git _ as k -> Some k
    | _          -> None
  else
    None

let get_archive t nv =
  log "get_archive %s" (OpamPackage.to_string nv);
  let aux repo_p repo =
    OpamRepository.download repo nv;
    let src = OpamPath.Repository.archive repo_p nv in
    let dst = OpamPath.archive t.root nv in
    if OpamFilename.exists src then (
      OpamFilename.link src dst;
      Some dst
    ) else
      None in
  OpamState.with_repository t nv aux

(* For pinned packages, we keep the build cache in
   ~/.opam/<switch>/pinned.cache/<name> as:
   i) it's quite important to build and sync-up in different places
   ii) the build directory will be deleted afterwards anyway *)
let extract_package t nv =
  log "extract_package: %s" (OpamPackage.to_string nv);
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  OpamFilename.rmdir build_dir;
  match pinned_path t nv with
  | Some (Git p | Darcs p | Path p as pin) ->
    let pinned_dir = OpamPath.Switch.pinned_dir t.root t.switch (OpamPackage.name nv) in
    if not (OpamFilename.exists_dir pinned_dir) then (
      match OpamState.update_pinned_package t nv pin with
      | Not_available -> OpamGlobals.error "%s is not available" (OpamFilename.Dir.to_string p)
      | Result _
      | Up_to_date _  -> ()
    ) else
      OpamGlobals.msg "Synchronization: nothing to do as the pinned package has already been initialized.\n";
    let _files = OpamState.with_repository t nv (fun repo _ ->
      OpamFilename.in_dir pinned_dir (fun () -> OpamRepository.copy_files repo nv)
    ) in
    OpamFilename.link_dir pinned_dir build_dir
  | _ ->
    match get_archive t nv with
    | None         -> ()
    | Some archive ->
      OpamGlobals.msg "Extracting %s.\n" (OpamFilename.to_string archive);
      OpamFilename.extract archive build_dir

let string_of_commands commands =
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  "  " ^
  if commands_s <> [] then
    String.concat "\n  " commands_s
  else
    "Nothing to do."

let compilation_env t opam =
  let env0 = OpamState.get_env t in
  OpamState.update_env t env0 (OpamFile.OPAM.build_env opam)

let proceed_to_delete ~rm_build t nv =
  log "deleting %s" (OpamPackage.to_string nv);
  OpamGlobals.msg "Uninstalling %s:\n" (OpamPackage.to_string nv);
  let name = OpamPackage.name nv in

  (* Run the remove script *)
  let opam_f = OpamPath.opam t.root nv in
  if OpamFilename.exists opam_f then (
    let opam = OpamState.opam t nv in
    let env = compilation_env t opam in
    match OpamState.filter_commands t (OpamFile.OPAM.remove opam) with
    | []     -> ()
    | remove ->
      let p_build = OpamPath.Switch.build t.root t.switch nv in
      (* We try to run the remove scripts in the folder where it was extracted
         If it does not exist, we try to download and extract the archive again,
         if that fails, we don't really care. *)
      (* We also use a small hack: if the remove command is simply 'ocamlfind remove xxx'
         then, no need to extract the archive again. *)
      let use_ocamlfind = function
        | [] -> true
        | "ocamlfind" :: _ -> true
        | _ -> false in
      if not (OpamFilename.exists_dir p_build)
      && OpamState.mem_repository t nv
      && not (List.for_all use_ocamlfind remove) then (
        try extract_package t nv
        with _ -> OpamFilename.mkdir p_build;
      );
      try
        OpamGlobals.msg "%s\n" (string_of_commands remove);
        OpamFilename.exec
          ~add_to_env:env.add_to_env
          ~add_to_path:[OpamPath.Switch.bin t.root t.switch]
          p_build
          remove
      with _ ->
        ();
  );

  (* Remove the libraries *)
  OpamFilename.rmdir (OpamPath.Switch.lib t.root t.switch name);

  (* Remove the documentation *)
  OpamFilename.rmdir (OpamPath.Switch.doc t.root t.switch name);
  (* XXX: remove the man pages *)

  (* Remove build/<package> if requested *)
  if rm_build then
    OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);

  (* Clean-up the repositories *)
  log "Cleaning-up the repositories";
  let repos =
    try OpamPackage.Name.Map.find (OpamPackage.name nv) t.repo_index
    with _ -> [] in
  List.iter (fun r ->
    let repo_p = OpamPath.Repository.create t.root r in
    let tmp_dir = OpamPath.Repository.tmp_dir repo_p nv in
    OpamFilename.rmdir tmp_dir
  ) repos;

  (* Remove the binaries *)
  log "Removing the binaries";
  let install = OpamFile.Dot_install.safe_read (OpamPath.Switch.install t.root t.switch name) in
  List.iter (fun (_,dst) ->
    let dst = OpamPath.Switch.bin t.root t.switch // (OpamFilename.Base.to_string dst) in
    OpamFilename.remove dst
  ) (OpamFile.Dot_install.bin install);

  (* Remove the misc files *)
  log "Removing the misc files";
  List.iter (fun (_,dst) ->
    if OpamFilename.exists dst then begin
      OpamGlobals.msg "Removing %s." (OpamFilename.to_string dst);
      if OpamState.confirm "Continue ?" then
        OpamFilename.remove dst
    end
  ) (OpamFile.Dot_install.misc install);

  (* Remove the shared files *)
  log "Removing the shared files";
  let share = OpamPath.Switch.share t.root t.switch name in
  List.iter (fun (_,dst) ->
    let dst = share // (OpamFilename.Base.to_string dst) in
    if OpamFilename.exists dst then
      OpamGlobals.msg "Removing %s." (OpamFilename.to_string dst);
  ) (OpamFile.Dot_install.share install);
  (match OpamFilename.list_files share, OpamFilename.list_dirs share with
  | [], [] -> OpamFilename.rmdir share
  | _      ->
    OpamGlobals.msg
      "%s is not empty, I'm keeping its content for futur installations."
      (OpamFilename.Dir.to_string share));

  (* Remove .config and .install *)
  log "Removing config and install files";
  OpamFilename.remove (OpamPath.Switch.install t.root t.switch name);
  OpamFilename.remove (OpamPath.Switch.config t.root t.switch name)

(* In case of error, simply return the error traces, and let the
   repo in a state that the user can explore.
   Do not try to recover yet. *)
let proceed_to_change t nv_old nv =
  OpamGlobals.msg "\n=-=-= %s =-=-=\n" (OpamPackage.to_string nv);

  (* First, uninstall any previous version *)
  (match nv_old with
  | Some nv_old -> proceed_to_delete ~rm_build:true t nv_old
  | None        -> ());

  let opam = OpamState.opam t nv in

  (* Get the env variables set up in the compiler description file *)
  let env = compilation_env t opam in

  (* Prepare the package for the build. *)

  extract_package t nv;

  let p_build = OpamPath.Switch.build t.root t.switch nv in

  if not (OpamFilename.exists_dir p_build) then
    OpamFilename.mkdir p_build;

  (* Substitute the patched files.*)
  let patches = OpamFile.OPAM.patches opam in
  OpamFilename.in_dir p_build (fun () ->
    let all = OpamFile.OPAM.substs opam in
    let patches =
      OpamMisc.filter_map (fun (f,_) ->
        if List.mem f all then Some f else None
      ) patches in
    List.iter (OpamState.substitute_file t) patches
  );

  (* Apply the patches *)
  List.iter (fun (base, filter) ->
    let root = OpamPath.Switch.build t.root t.switch nv in
    let patch = root // OpamFilename.Base.to_string base in
    if OpamState.eval_filter t filter then (
      OpamGlobals.msg "Applying %s.\n" (OpamFilename.Base.to_string base);
      OpamFilename.patch patch p_build)
  ) patches;

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  OpamFilename.in_dir p_build (fun () ->
    List.iter (OpamState.substitute_file t) (OpamFile.OPAM.substs opam)
  );

  (* Generate an environnement file *)
  let env_f = OpamPath.Switch.build_env t.root t.switch nv in
  OpamFile.Env.write env_f env.new_env;

  (* Exec the given commands. *)
  let exec name f =
    let commands = OpamState.filter_commands t (f opam) in
    OpamGlobals.msg "%s:\n%s\n" name (string_of_commands commands);
    OpamFilename.exec
      ~add_to_env:env.add_to_env
      ~add_to_path:[env.add_to_path]
      p_build
      commands in
    try
      (* First, we build the package. *)
      exec ("Building " ^ OpamPackage.to_string nv) OpamFile.OPAM.build;

      (* If necessary, build and run the test. *)
      if !OpamGlobals.build_test then
        exec "Building and running the test" OpamFile.OPAM.build_test;

      (* If necessary, build the documentation. *)
      if !OpamGlobals.build_doc then
        exec "Generating the documentation" OpamFile.OPAM.build_doc;

      (* If everyting went fine, finally install the package. *)
      proceed_to_install t nv;
  with e ->
    (* We keep the build dir to help debugging *)
    proceed_to_delete ~rm_build:false t nv;
    begin match nv_old with
    | None        ->
      OpamGlobals.error
        "The compilation of %s failed in %s."
        (OpamPackage.to_string nv)
        (OpamFilename.Dir.to_string p_build)
    | Some nv_old ->
      OpamGlobals.error
        "The recompilation of %s failed in %s."
        (OpamPackage.to_string nv)
        (OpamFilename.Dir.to_string p_build)
    end;
    raise e

(* We need to clean-up things before recompiling. *)
let proceed_to_recompile t nv =
  proceed_to_change t (Some nv) nv

let error_if_no_solution = function
  | No_solution -> OpamGlobals.exit 3
  | _           -> ()

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let unknown_package name version =
  match version with
  | None   -> OpamGlobals.error_and_exit "%S is not a valid package.\n" (OpamPackage.Name.to_string name)
  | Some v -> OpamGlobals.error_and_exit "The package %S has no version %s." (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v)

let unavailable_package name version =
  match version with
  | None   ->
    OpamGlobals.error_and_exit
      "%S is not available for your compiler or your OS.\n"
      (OpamPackage.Name.to_string name)
  | Some v ->
    OpamGlobals.error_and_exit
      "Version %s of %S is incompatible with your compiler or your OS."
      (OpamPackage.Version.to_string v)
      (OpamPackage.Name.to_string name)

let eq_atom name version =
  name, Some (`Eq, version)

let eq_atoms_of_packages set =
  List.map (fun nv -> eq_atom (OpamPackage.name nv) (OpamPackage.version nv)) (OpamPackage.Set.elements set)

let atom_of_package nv =
  OpamPackage.name nv, None

let atoms_of_packages set =
  List.map atom_of_package (OpamPackage.Set.elements set)

let atom_of_name name =
  name, None

(* transform a name into:
   - <name, installed version> package
   - <$n,$v> package when name = $n.$v *)
let atoms_of_names t names =
  let available = OpamPackage.to_map (Lazy.force t.available_packages) in
  let installed = OpamState.installed_map t in
  let packages = OpamPackage.to_map t.packages in
  List.map
    (fun name ->
      if OpamPackage.Name.Map.mem name packages then (
        if OpamPackage.Name.Map.mem name installed
        || OpamPackage.Name.Map.mem name available then
          atom_of_name name
        else
        (* perhaps the package is unavailable for this compiler *)
          let versions = OpamPackage.Name.Map.find name packages in
          if not (OpamPackage.Version.Set.is_empty versions) then
            unavailable_package name None
          else
            unknown_package name None
      ) else (
        (* consider 'name' to be 'name.version' *)
        let nv =
          try OpamPackage.of_string (OpamPackage.Name.to_string name)
          with Not_found -> unknown_package name None in
        let sname = OpamPackage.name nv in
        let sversion = OpamPackage.version nv in
        log "The raw name %S not found, looking for package %s version %s"
          (OpamPackage.Name.to_string name)
          (OpamPackage.Name.to_string sname)
          (OpamPackage.Version.to_string sversion);
        if OpamPackage.Name.Map.mem sname available
        && OpamPackage.Version.Set.mem sversion (OpamPackage.Name.Map.find sname available) then
          eq_atom sname sversion
        else if OpamPackage.Name.Map.mem sname packages then
          unavailable_package sname (Some sversion)
        else
          unknown_package sname (Some sversion)
      ))
    (OpamPackage.Name.Set.elements names)

(* Apply a solution *)
let apply_solution ?(force = false) t sol =
  let open PackageActionGraph in
  if OpamSolver.solution_is_empty sol then
    (* The current state satisfies the request contraints *)
    Nothing_to_do
  else (
    let stats = OpamSolver.stats sol in
    OpamGlobals.msg "The following actions will be performed:\n";
    OpamSolver.print_solution sol;
    OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats);

    let continue =
      if force || sum stats <= 1 then
        true
      else
        OpamState.confirm "Do you want to continue ?" in

    if continue then (

      let installed = ref t.installed in
      (* This function should be called by the parent process only, as it modifies
         the global state of OPAM *)
      let write_installed () =
        OpamFile.Installed.write (OpamPath.Switch.installed t.root t.switch) !installed in

      (* Delete the requested packages in the parent process *)
      (* In case of errors, we try to keep the list of installed packages up-to-date *)
      List.iter
        (fun nv ->
          if OpamPackage.Set.mem nv !installed then begin
            try
              proceed_to_delete ~rm_build:true t nv;
              installed := OpamPackage.Set.remove nv !installed;
              write_installed ()
            with _ ->
              ()
          end)
        sol.to_remove;

      (* Installation and recompilation are done by child processes *)
      let child n =
        let t = OpamState.load_state () in
        match n with
        | To_change (o, nv) -> proceed_to_change t o nv
        | To_recompile nv   -> proceed_to_recompile t nv
        | To_delete _       -> assert false in

      let pre _ = () in

      (* Update the installed file in the parent process *)
      let post = function
        | To_delete _    -> assert false
        | To_recompile _ -> ()
        | To_change (None, nv) ->
          installed := OpamPackage.Set.add nv !installed;
          write_installed ()
        | To_change (Some o, nv)   ->
          installed := OpamPackage.Set.add nv (OpamPackage.Set.remove o !installed);
          write_installed () in

      (* Try to recover from errors.
         XXX: this is higly experimental. *)
      let can_try_to_recover_from_error l =
        List.exists (function (n,_) ->
          match n with
          | To_change(Some _,_)
          | To_recompile _ -> true
          | _ -> false
        ) l in

      let recover_from_error (n, _) = match n with
        | To_change (Some o, _) ->
          (try
            proceed_to_install t o;
            installed := OpamPackage.Set.add o !installed;
            write_installed ()
           with _ ->
             ())
        | To_change (None, _)   -> ()
        | To_recompile nv       ->
          (* this case is quite tricky. We have to remove all the packages
             depending in nv, as they will be broken if nv is uninstalled. *)
          let universe = OpamState.universe t Depends in
          let depends =
            let set = OpamPackage.Set.singleton nv in
            OpamPackage.Set.of_list
              (OpamSolver.forward_dependencies ~depopts:true ~installed:true universe set) in
          OpamPackage.Set.iter (proceed_to_delete ~rm_build:false t) depends;
          installed := OpamPackage.Set.diff !installed depends;
          write_installed ();
        | To_delete nv            -> assert false in

      let display_error (n, error) =
        let f action nv =
          OpamGlobals.error "[ERROR] while %s %s" action (OpamPackage.to_string nv);
          match error with
          | OpamParallel.Process_error r  -> OpamProcess.display_error_message r
          | OpamParallel.Internal_error s -> OpamGlobals.error "  %s" s in
        match n with
        | To_change (Some o, nv) ->
          if OpamPackage.Version.compare (OpamPackage.version o) (OpamPackage.version nv) < 0 then
            f "upgrading to" nv
          else
            f "downgrading to" nv
        | To_change (None, nv)   -> f "installing" nv
        | To_recompile nv        -> f "recompiling" nv
        | To_delete nv           -> f "removing" nv in

      let string_of_errors errors =
        let actions = List.map fst errors in
        let packages =
          List.map (function
          | To_change (_,nv)
          | To_recompile nv
          | To_delete nv -> nv
          ) actions in
        match packages with
        | []  -> assert false
        | [h] -> OpamPackage.to_string h
        | l   -> OpamPackage.Set.to_string (OpamPackage.Set.of_list l) in

      let cores = OpamFile.Config.cores t.config in
      try
        PackageActionGraph.Parallel.parallel_iter cores sol.to_process ~pre ~child ~post;
        OK
      with PackageActionGraph.Parallel.Errors (errors, remaining) ->
        OpamGlobals.msg "\n";
        if remaining <> [] then (
          OpamGlobals.error
            "Due to some errors while processing %s, the following actions will NOT be proceeded:"
            (string_of_errors errors);
          List.iter (fun n -> OpamGlobals.error "%s" (PackageAction.string_of_action n)) remaining;
        );
        if can_try_to_recover_from_error errors then (
          OpamGlobals.msg "\nRecovering from errors:\n";
          List.iter recover_from_error errors;
        );
        List.iter display_error errors;
        OpamGlobals.exit 2
    ) else
      Aborted
  )

let new_variables e =
  let open OpamMisc in
  let e = List.filter (fun (_,s,_) -> s="=") e in
  let e = List.map (fun (v,_,_) -> v) e in
  OpamMisc.StringSet.of_list e

let variable_warnings = ref false
let print_variable_warnings () =
  let variables = ref [] in
  if not !variable_warnings then (
    let t = OpamState.load_state () in
    let warn w =
      let is_defined s =
        try let _ = OpamMisc.getenv s in true
        with Not_found -> false in
      if is_defined w then
        variables := w :: !variables in

    (* 1. Warn about OCAMLFIND variables if it is installed *)
    let ocamlfind_vars = [
      "OCAMLFIND_DESTDIR";
      "OCAMLFIND_CONF";
      "OCAMLFIND_METADIR";
      "OCAMLFIND_COMMANDS";
      "OCAMLFIND_LDCONF";
    ] in
    if OpamPackage.Set.exists (fun nv -> OpamPackage.Name.to_string (OpamPackage.name nv) = "ocamlfind") t.installed then
      List.iter warn ocamlfind_vars;
    (* 2. Warn about variables possibly set by other compilers *)
    let new_variables comp =
      let comp_f = OpamPath.compiler t.root comp in
      let env = OpamFile.Comp.env (OpamFile.Comp.read comp_f) in
      new_variables env in
    let vars = ref OpamMisc.StringSet.empty in
    OpamSwitch.Map.iter (fun _ comp ->
      vars := OpamMisc.StringSet.union !vars (new_variables comp)
    ) t.aliases;
    vars := OpamMisc.StringSet.diff !vars (new_variables t.compiler);
    OpamMisc.StringSet.iter warn !vars;
    if !variables <> [] then (
      OpamGlobals.msg "The following variables are set in your environment, \
                     you should better unset it if you want OPAM to work \
                     correctly.\n";
      List.iter (OpamGlobals.msg " - %s\n") !variables;
      if not (OpamState.confirm "Do you want to continue ?") then
        OpamGlobals.exit 0;
    );
    variable_warnings := true;
  )

let resolve t action request =
  OpamSolver.resolve (OpamState.universe t action) request

let resolve_and_apply ?(force=false) t action request =
  match resolve t action request with
  | Conflicts cs ->
    OpamGlobals.msg "No solution has been found:\n%s\n" (cs ());
    No_solution
  | Success sol ->
    print_variable_warnings ();
    apply_solution ~force t sol
