(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log fmt = OpamConsole.log "SOLUTION" fmt

open OpamTypes
open OpamTypesBase
open OpamStateTypes
open OpamProcess.Job.Op

module PackageAction = OpamSolver.Action
module PackageActionGraph = OpamSolver.ActionGraph

let post_message ?(failed=false) st action =
  match action, failed with
  | `Remove _, _ | `Reinstall _, _ | `Build _, false | `Fetch _, _ -> ()
  | `Build pkg, true | `Install pkg, _ | `Change (_,_,pkg), _ ->
    let opam = OpamSwitchState.opam st pkg in
    let messages = OpamFile.OPAM.post_messages opam in
    let local_variables = OpamVariable.Map.empty in
    let local_variables =
      OpamVariable.Map.add (OpamVariable.of_string "success")
        (Some (B (not failed))) local_variables
    in
    let local_variables =
      OpamVariable.Map.add (OpamVariable.of_string "failure")
        (Some (B failed)) local_variables
    in
    let messages =
      let filter_env = OpamPackageVar.resolve ~opam ~local:local_variables st in
      OpamStd.List.filter_map (fun (message,filter) ->
          if OpamFilter.opt_eval_to_bool filter_env filter then
            Some (OpamFilter.expand_string ~default:(fun _ -> "")
                    filter_env message)
          else None)
        messages
    in
    let mark = OpamConsole.colorise (if failed then `red else `green) "=> " in
    if messages <> [] then (
      OpamConsole.header_msg "%s %s"
        (OpamPackage.to_string pkg)
        (if failed then "troubleshooting" else "installed successfully");
      List.iter (fun msg ->
          OpamConsole.formatted_msg
            ~indent:(OpamStd.Format.visual_length mark)
            "%s%s\n" mark msg)
        messages
    )

let print_depexts_helper st actions =
  if OpamFile.Config.depext st.switch_global.config then () else
  let depexts =
    List.fold_left (fun depexts -> function
        | `Build nv ->
          OpamSysPkg.Set.union depexts (OpamSwitchState.depexts st nv)
        | _ -> depexts)
      OpamSysPkg.Set.empty
      actions
  in
  if not (OpamSysPkg.Set.is_empty depexts) then (
    OpamConsole.formatted_msg
      "\nThe packages you requested declare the following system dependencies. \
       Please make sure they are installed before retrying:\n";
    OpamConsole.formatted_msg ~indent:4 "    %s\n\n"
      (OpamStd.List.concat_map " " (fun s ->
           OpamConsole.colorise `bold (OpamSysPkg.to_string s))
          (OpamSysPkg.Set.elements depexts))
  )

let check_solution ?(quiet=false) st = function
  | Conflicts _ ->
    OpamConsole.msg "No solution found, exiting\n";
    OpamStd.Sys.exit_because `No_solution
  | Success (Partial_error { actions_successes; actions_errors; _ }) ->
    List.iter (post_message st) actions_successes;
    List.iter (fun (a, _) -> post_message ~failed:true st a) actions_errors;
    print_depexts_helper st (List.map fst actions_errors);
    OpamEnv.check_and_print_env_warning st;
    OpamStd.Sys.exit_because `Package_operation_error
  | Success (OK actions) ->
    List.iter (post_message st) actions;
    OpamEnv.check_and_print_env_warning st
  | Success Nothing_to_do ->
    if not quiet then OpamConsole.msg "Nothing to do.\n";
    OpamEnv.check_and_print_env_warning st
  | Success Aborted ->
    if not OpamClientConfig.(!r.show) then
      OpamStd.Sys.exit_because `Aborted

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let eq_atom name version =
  name, Some (`Eq, version)

let eq_atom_of_package nv =
  eq_atom nv.name nv.version

let eq_atoms_of_packages set =
  List.rev_map eq_atom_of_package (OpamPackage.Set.elements set)

let atom_of_package nv =
  nv.name, None

let atoms_of_packages set =
  List.rev_map (fun n -> n, None)
    (OpamPackage.Name.Set.elements (OpamPackage.names_of_packages set))

(* unused?
let atom_of_name name =
  name, None
*)

let check_availability ?permissive t set atoms =
  let available = OpamPackage.to_map set in
  let check_depexts atom =
    let pkgs = OpamFormula.packages_of_atoms t.packages [atom] in
    try
      let depexts_missing =
        (OpamPackage.Map.find (OpamPackage.Set.max_elt pkgs)
           (Lazy.force t.sys_packages))
        .OpamSysPkg.s_not_found
      in
      if OpamSysPkg.Set.is_empty depexts_missing then None
      else
        Some
          (Printf.sprintf
             "Package %s depends on the unavailable system package '%s'. You \
              can use `--no-depexts' to attempt installation anyway."
             (OpamFormula.short_string_of_atom atom)
             (OpamStd.List.concat_map " " OpamSysPkg.to_string
                (OpamSysPkg.Set.elements depexts_missing)))
    with Not_found -> None
  in
  let check_atom (name, cstr as atom) =
    let exists =
      try
        OpamPackage.Version.Set.exists
          (fun v -> OpamFormula.check atom (OpamPackage.create name v))
          (OpamPackage.Name.Map.find name available)
      with Not_found -> false
    in
    if exists then None
    else match check_depexts atom with Some _ as some -> some | None ->
    if permissive = Some true
    then Some (OpamSwitchState.not_found_message t atom)
    else
    let f = name, match cstr with None -> Empty | Some c -> Atom c in
    Some (Printf.sprintf "%s %s"
            (OpamFormula.to_string (Atom f))
            (OpamSwitchState.unavailable_reason
               ~default:"unavailable for unknown reasons (this may be a bug in \
                         opam)"
               t f)) in
  let errors = OpamStd.List.filter_map check_atom atoms in
  if errors <> [] then
    (List.iter (OpamConsole.error "%s") errors;
     OpamStd.Sys.exit_because `Not_found)

let fuzzy_name t name =
  let lname = String.lowercase_ascii (OpamPackage.Name.to_string name) in
  let match_name nv =
    lname = String.lowercase_ascii (OpamPackage.name_to_string nv)
  in
  let matches =
    OpamPackage.Set.union
      (OpamPackage.Set.filter match_name t.installed)
      (OpamPackage.Set.filter match_name t.packages)
  in
  let names = OpamPackage.names_of_packages matches in
  match OpamPackage.Name.Set.elements names with
  | [name] -> name
  | _ -> name

let sanitize_atom_list ?(permissive=false) t atoms =
  let atoms = List.map (fun (name,cstr) -> fuzzy_name t name, cstr) atoms in
  if permissive then
    check_availability ~permissive t
      (OpamPackage.Set.union t.packages t.installed) atoms
  else
    check_availability t
      (OpamPackage.Set.union (Lazy.force t.available_packages) t.installed)
      atoms;
  atoms

(* Pretty-print errors *)
let display_error (n, error) =
  let f action nv =
    let disp =
      OpamConsole.header_error "while %s %s" action (OpamPackage.to_string nv) in
    match error with
    | Sys.Break | OpamParallel.Aborted -> ()
    | Failure s -> disp "%s" s
    | OpamSystem.Process_error e -> disp "%s" (OpamProcess.string_of_result e)
    | e ->
      disp "%s" (Printexc.to_string e);
      if OpamConsole.debug () then
        OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e)
  in
  match n with
  | `Change (`Up, _, nv)   -> f "upgrading to" nv
  | `Change (`Down, _, nv) -> f "downgrading to" nv
  | `Install nv        -> f "installing" nv
  | `Reinstall nv      -> f "recompiling" nv
  | `Remove nv         -> f "removing" nv
  | `Build nv          -> f "compiling" nv
  | `Fetch nv          -> f "fetching sources for" nv

module Json = struct
  let output_request request user_action =
    if OpamClientConfig.(!r.json_out = None) then () else
    let atoms =
      List.map (fun a -> `String (OpamFormula.short_string_of_atom a))
    in
    let j = `O [
        "action", `String (string_of_user_action user_action);
        "install", `A (atoms request.wish_install);
        "remove", `A (atoms request.wish_remove);
        "upgrade", `A (atoms request.wish_upgrade);
        "criteria", `String (OpamSolverConfig.criteria request.criteria);
      ]
    in
    OpamJson.append "request" j

  let output_solution t solution =
    if OpamClientConfig.(!r.json_out = None) then () else
    match solution with
    | Success solution ->
      let action_graph = OpamSolver.get_atomic_action_graph solution in
      let to_proceed =
        PackageActionGraph.Topological.fold (fun a acc ->
            PackageAction.to_json a :: acc
          ) action_graph []
      in
      OpamJson.append "solution" (`A (List.rev to_proceed))
    | Conflicts cs ->
      let causes,_,cycles =
        OpamCudf.strings_of_conflict
          t.packages (OpamSwitchState.unavailable_reason t) cs
      in
      let chains = OpamCudf.conflict_chains t.packages cs in
      let jchains =
        `A (List.map (fun c ->
            `A ((List.map (fun f ->
                `String (OpamFormula.to_string (Atom f))) c)))
            chains)
      in
      let toj l = `A (List.map (fun s -> `String s) l) in
      OpamJson.append "conflicts"
        (`O ((if cycles <> [] then ["cycles", toj cycles] else []) @
             (if causes <> [] then ["causes", toj causes] else []) @
             (if chains <> [] then ["broken-deps", jchains] else [])))

  let exc e =
    let lmap f l = List.rev (List.rev_map f l) in
    if OpamClientConfig.(!r.json_out = None) then `O [] else
    match e with
    | OpamSystem.Process_error
        {OpamProcess.r_code; r_duration; r_info; r_stdout; r_stderr; _} ->
      `O [ ("process-error",
            `O ([ ("code", `String (string_of_int r_code));
                  ("duration", `Float r_duration);
                  ("info", `O (lmap (fun (k,v) -> (k, `String v)) r_info)); ]
                @ if OpamCoreConfig.(!r.merged_output) then
                  [("output", `A (lmap (fun s -> `String s) r_stdout))]
                else
                  [("output", `A (lmap (fun s -> `String s) r_stdout));
                   ("stderr", `A (lmap (fun s -> `String s) r_stderr));
                  ]))]
    | OpamSystem.Internal_error s ->
      `O [ ("internal-error", `String s) ]
    | e -> `O [ ("exception", `String (Printexc.to_string e)) ]

end

(* Process the atomic actions in a graph in parallel, respecting graph order,
   and report to user. Takes a graph of atomic actions *)
let parallel_apply t ~requested ?add_roots ~assume_built ?(force_remove=false)
    action_graph =
  log "parallel_apply";

  let remove_action_packages =
    PackageActionGraph.fold_vertex
      (function `Remove nv -> OpamPackage.Set.add nv
              | _ -> fun acc -> acc)
      action_graph OpamPackage.Set.empty
  in

  let install_action_packages =
    PackageActionGraph.fold_vertex
      (function `Install nv -> OpamPackage.Set.add nv
              | _ -> fun acc -> acc)
      action_graph OpamPackage.Set.empty
  in

  (* the core set of installed packages that won't change *)
  let minimal_install =
    OpamPackage.Set.Op.(t.installed -- remove_action_packages)
  in

  let wished_removed =
    OpamPackage.Set.filter
      (fun nv -> not (OpamPackage.has_name install_action_packages nv.name))
      remove_action_packages
  in

  let root_installs =
    OpamPackage.Name.Set.union
      (OpamPackage.names_of_packages t.installed_roots) @@
    match add_roots with
    | Some r -> r
    | None ->
      OpamPackage.Name.Set.diff
        (OpamPackage.names_of_packages requested)
        (OpamPackage.names_of_packages remove_action_packages)
  in

  (* We keep an imperative state up-to-date and flush it to disk as soon
     as an operation terminates *)
  let t_ref = ref t in

  (* only needed when --update-invariant is set. Use the configured invariant,
     not the current one which will be empty. *)
  let original_invariant = t.switch_config.OpamFile.Switch_config.invariant in
  let invariant_ref = ref original_invariant in

  let bypass_ref = ref (t.switch_config.OpamFile.Switch_config.depext_bypass) in

  let add_to_install nv =
    let root = OpamPackage.Name.Set.mem nv.name root_installs in
    t_ref := OpamSwitchAction.add_to_installed !t_ref ~root nv;
    let missing_depexts =
      (* Turns out these depexts wheren't needed after all. Remember that and
         make the bypass permanent. *)
      try
        (OpamPackage.Map.find nv (Lazy.force !t_ref.sys_packages)).s_available
      with Not_found -> OpamSysPkg.Set.empty
    in
    let bypass = OpamSysPkg.Set.union missing_depexts !bypass_ref in
    let invariant =
      if OpamStateConfig.(!r.unlock_base) then
        OpamFormula.map (fun (n, cstr as at) ->
            if n <> nv.name || OpamFormula.check_version_formula cstr nv.version
            then Atom at else
            let cstr =
              OpamFormula.map (fun (relop, _ as vat) ->
                  if OpamFormula.check_version_formula (Atom vat) nv.version
                  then Atom vat
                  else match relop with
                    | `Neq | `Gt | `Lt -> OpamFormula.Empty
                    | `Eq | `Geq | `Leq -> Atom (relop, nv.version))
                cstr
            in
            Atom (n, cstr))
          !invariant_ref
      else !invariant_ref
    in
    if bypass <> !bypass_ref || invariant <> !invariant_ref then
      (bypass_ref := bypass;
       invariant_ref := invariant;
       let switch_config =
         {!t_ref.switch_config with invariant; depext_bypass = bypass }
       in
       t_ref := {!t_ref with switch_config};
       OpamSwitchAction.install_switch_config t.switch_global.root t.switch
         switch_config)
  in

  let remove_from_install ?keep_as_root nv =
    t_ref := OpamSwitchAction.remove_from_installed ?keep_as_root !t_ref nv
  in

  let inplace =
    if OpamClientConfig.(!r.inplace_build) || assume_built then
      OpamPackage.Set.fold (fun nv acc ->
          match
            OpamStd.Option.Op.(OpamSwitchState.url t nv >>| OpamFile.URL.url >>=
                               OpamUrl.local_dir)
          with
          | None -> acc
          | Some path -> OpamPackage.Map.add nv path acc)
        requested
        OpamPackage.Map.empty
    else OpamPackage.Map.empty
  in

  let sources_needed =
    let sources_needed = OpamAction.sources_needed t action_graph in
    if not OpamClientConfig.(!r.working_dir) then sources_needed else
      OpamPackage.Set.Op.(sources_needed -- requested)
  in

  (* 1/ process the package actions (fetch, build, installations and removals) *)

  let action_graph = (* Add build actions *)
    let noop_remove nv =
      OpamAction.noop_remove_package t nv in
    PackageActionGraph.explicit
      ~noop_remove
      ~sources_needed:(fun p -> OpamPackage.Set.mem p sources_needed)
      action_graph
  in
  (match OpamSolverConfig.(!r.cudf_file) with
   | None -> ()
   | Some f ->
     let filename = Printf.sprintf "%s-actions-explicit.dot" f in
     let oc = open_out filename in
     OpamSolver.ActionGraph.Dot.output_graph oc action_graph;
     close_out oc);

  let timings = Hashtbl.create 17 in
  (* the child job to run on each action *)
  let job ~pred action =
    let installed, removed, failed =
      List.fold_left (fun (inst,rem,fail) -> function
          | _, `Successful (inst1, rem1) ->
            OpamPackage.Set.Op.(inst ++ inst1, rem ++ rem1, fail)
          | _, `Error (`Aborted a) ->
            inst, rem, PackageAction.Set.Op.(a ++ fail)
          | a, (`Exception _ | `Error _) ->
            inst, rem, PackageAction.Set.add a fail)
        (OpamPackage.Set.empty, OpamPackage.Set.empty, PackageAction.Set.empty)
        pred
    in
    (* Check whether prerequisites failed *)
    let action_is_remove = match action with `Remove _ -> true | _ -> false in
    let has_failure = not (PackageAction.Set.is_empty failed) in
    let has_nonfetch_failure =
      List.exists (function
          | (_, `Successful _) | (`Fetch _, _) -> false
          | _ -> true)
        pred
    in
    if has_failure && (not action_is_remove || has_nonfetch_failure)
    then
      (* fatal error *)
      Done (`Error (`Aborted failed))
    else
    let store_time =
      let t0 = Unix.gettimeofday () in
      fun () -> Hashtbl.add timings action (Unix.gettimeofday () -. t0)
    in
    let not_yet_removed =
      match action with
      | `Remove _ ->
        PackageActionGraph.fold_descendants (function
            | `Remove nv -> OpamPackage.Set.add nv
            | _ -> fun acc -> acc)
          OpamPackage.Set.empty action_graph action
      | _ -> OpamPackage.Set.empty
    in
    let visible_installed =
      OpamPackage.Set.Op.(minimal_install ++ not_yet_removed ++ installed)
    in
    let t =
      { !t_ref with
        installed = visible_installed;
        conf_files = OpamPackage.Name.Map.filter
            (fun name _ -> OpamPackage.Set.exists (fun pkg -> OpamPackage.Name.equal name pkg.name) visible_installed)
            !t_ref.conf_files; }
    in
    let nv = action_contents action in
    let opam = OpamSwitchState.opam t nv in
    let source_dir =
      let raw = OpamSwitchState.source_dir t nv in
      match OpamFile.OPAM.url opam with
      | None -> raw
      | Some url ->
        match OpamFile.URL.subpath url with
        | None -> raw
        | Some subpath -> OpamFilename.Op.(raw / subpath) in
    if OpamClientConfig.(!r.fake) then
      match action with
      | `Build _ | `Fetch _ -> Done (`Successful (installed, removed))
      | `Install nv ->
        OpamConsole.msg "Faking installation of %s\n"
          (OpamPackage.to_string nv);
        add_to_install nv;
        Done (`Successful (OpamPackage.Set.add nv installed, removed))
      | `Remove nv ->
        remove_from_install nv;
        Done (`Successful (installed, OpamPackage.Set.add nv removed))
      | _ -> assert false
    else
    match action with
    | `Fetch nv ->
      log "Fetching sources for %s" (OpamPackage.to_string nv);
      (OpamAction.download_package t nv @@+ function
        | None ->
          store_time (); Done (`Successful (installed, removed))
        | Some (_short_error, long_error) ->
          Done (`Exception (Failure long_error)))

    | `Build nv ->
      if assume_built && OpamPackage.Set.mem nv requested then
        (log "Skipping build for %s, just install%s"
           (OpamPackage.to_string nv)
           (OpamStd.Option.map_default
              (fun p -> " from " ^ OpamFilename.Dir.to_string p)
              "" (OpamPackage.Map.find_opt nv inplace));
         Done (`Successful (installed, removed)))
      else
      let is_inplace, build_dir =
        try true, OpamPackage.Map.find nv inplace
        with Not_found ->
          let dir = OpamPath.Switch.build t.switch_global.root t.switch nv in
          if not OpamClientConfig.(!r.reuse_build_dir) then
            OpamFilename.rmdir dir;
          false, dir
      in
      let test =
        OpamStateConfig.(!r.build_test) && OpamPackage.Set.mem nv requested
      in
      let doc =
        OpamStateConfig.(!r.build_doc) && OpamPackage.Set.mem nv requested
      in
      (if OpamFilename.exists_dir source_dir
       then (if not is_inplace then
               OpamFilename.copy_dir ~src:source_dir ~dst:build_dir)
       else OpamFilename.mkdir build_dir;
       OpamAction.prepare_package_source t nv build_dir @@+ function
       | Some exn -> store_time (); Done (`Exception exn)
       | None ->
         OpamAction.build_package t ~test ~doc build_dir nv @@+ function
         | Some exn -> store_time (); Done (`Exception exn)
         | None -> store_time (); Done (`Successful (installed, removed)))
    | `Install nv ->
      let test =
        OpamStateConfig.(!r.build_test) && OpamPackage.Set.mem nv requested
      in
      let doc =
        OpamStateConfig.(!r.build_doc) && OpamPackage.Set.mem nv requested
      in
      let build_dir = OpamPackage.Map.find_opt nv inplace in
      (OpamAction.install_package t ~test ~doc ?build_dir nv @@+ function
        | None ->
          add_to_install nv;
          store_time ();
          Done (`Successful (OpamPackage.Set.add nv installed, removed))
        | Some exn ->
          store_time ();
          Done (`Exception exn))
    | `Remove nv ->
      (if OpamAction.removal_needs_download t nv then
         let d = OpamPath.Switch.remove t.switch_global.root t.switch nv in
         OpamFilename.rmdir d;
         if OpamFilename.exists_dir source_dir
         then OpamFilename.copy_dir ~src:source_dir ~dst:d
         else OpamFilename.mkdir d;
         OpamAction.prepare_package_source t nv d
       else Done None) @@+ fun _ ->
      OpamProcess.Job.ignore_errors ~default:()
        (fun () -> OpamAction.remove_package ~force:force_remove t nv) @@| fun () ->
      remove_from_install
        ~keep_as_root:(not (OpamPackage.Set.mem nv wished_removed))
        nv;
      store_time ();
      `Successful (installed, OpamPackage.Set.add nv removed)
    | _ -> assert false
  in

  let action_results =
    OpamConsole.header_msg "Processing actions";
    try
      let installs_removes, fetches =
        PackageActionGraph.fold_vertex
          (fun a (installs_removes, fetches as acc) -> match a with
             | `Install _ | `Remove _ as i -> (i::installs_removes, fetches)
             | `Fetch _ as i -> (installs_removes, i::fetches)
             | _ -> acc)
          action_graph ([],[])
      in
      let same_inplace_source =
        OpamPackage.Map.fold (fun nv dir acc ->
            OpamFilename.Dir.Map.update dir (fun l -> nv::l) [] acc)
          inplace OpamFilename.Dir.Map.empty |>
        OpamFilename.Dir.Map.values
      in
      let pools =
        (installs_removes, 1) ::
        (fetches, OpamStateConfig.(!r.dl_jobs)) ::
        OpamStd.List.filter_map
          (fun excl ->
             match
               OpamStd.List.filter_map
                 (fun nv ->
                    let act = `Build nv in
                    if PackageActionGraph.mem_vertex action_graph act
                    then Some act else None)
                 excl
             with [] | [_] -> None | l -> Some (l,1))
          same_inplace_source
      in
      let results =
        PackageActionGraph.Parallel.map
          ~jobs:(Lazy.force OpamStateConfig.(!r.jobs))
          ~command:job
          ~dry_run:OpamStateConfig.(!r.dryrun)
          ~pools
          action_graph
      in
      (* For backwards-compatibility reasons, we separate the json report for
         download failures from the json report for the rest *)
      if OpamClientConfig.(!r.json_out <> None) then begin
        (* Report download failures *)
        let failed_downloads = List.fold_left (fun failed (a, err) ->
            match (a, err) with
            | `Fetch pkg, `Exception (Failure long_error) ->
              OpamPackage.Map.add pkg long_error failed
            | _ ->
              failed
          ) OpamPackage.Map.empty results in
        if not (OpamPackage.Map.is_empty failed_downloads) then
          OpamJson.append "download-failures"
            (`O (List.map (fun (nv, err) -> OpamPackage.to_string nv, `String err)
                   (OpamPackage.Map.bindings failed_downloads)));
        (* Report build/install/remove failures *)
        let j =
          PackageActionGraph.Topological.fold (fun a acc ->
              match a with
              | `Fetch _ -> acc
              | _ ->
                let r = match List.assoc a results with
                  | `Successful _ -> `String "OK"
                  | `Exception e -> Json.exc e
                  | `Error (`Aborted deps) ->
                    let deps = OpamSolver.Action.Set.elements deps in
                    `O ["aborted", `A (List.map OpamSolver.Action.to_json deps)]
                in
                let duration =
                  try [ "duration", `Float (Hashtbl.find timings a) ]
                  with Not_found -> []
                in
                `O ([ "action", PackageAction.to_json a;
                      "result", r ] @
                    duration)
                :: acc
            ) action_graph []
        in
        OpamJson.append "results" (`A (List.rev j))
      end;

      let success, failure, aborted =
        List.fold_left (fun (success, failure, aborted) -> function
            | a, `Successful _ -> a::success, failure, aborted
            | a, `Exception e -> success, (a,e)::failure, aborted
            | a, `Error (`Aborted _) -> success, failure, a::aborted
          ) ([], [], []) results
      in
      let actions_result = {
        actions_successes = success;
        actions_errors = failure;
        actions_aborted = aborted;
      } in
      if failure = [] && aborted = [] then `Successful success
      else (
        List.iter display_error failure;
        `Error (Partial_error actions_result)
      )
    with
    | PackageActionGraph.Parallel.Errors (success, errors, remaining) ->
      let actions_result = {
        actions_successes = success;
        actions_errors = errors;
        actions_aborted = remaining;
      } in
      List.iter display_error errors;
      `Error (Partial_error actions_result)
    | e -> `Exception e
  in
  let t = !t_ref in

  (* 2/ Display errors and finalize *)

  let cleanup_artefacts graph =
    PackageActionGraph.iter_vertex (function
        | `Remove nv
          when not (OpamPackage.has_name t.pinned nv.name) ->
          OpamAction.cleanup_package_artefacts t nv
          (* if reinstalled, only removes build dir *)
        | `Install nv
          when not (OpamPackage.has_name t.pinned nv.name) ->
          let build_dir =
            OpamPath.Switch.build t.switch_global.root t.switch nv in
          if not OpamClientConfig.(!r.keep_build_dir) then
            OpamFilename.rmdir build_dir
        | `Remove _ | `Install _ | `Build _ | `Fetch _ -> ()
        | _ -> assert false)
      graph
  in
  if !invariant_ref <> original_invariant then
    OpamConsole.note "Switch invariant was updated to %s\n\
                      Use `opam switch set-invariant' to change it."
      (match !invariant_ref with
       | OpamFormula.Empty -> "<empty>"
       | f -> OpamFileTools.dep_formula_to_string f);
  match action_results with
  | `Successful successful ->
    cleanup_artefacts action_graph;
    OpamConsole.msg "Done.\n";
    t, OK successful
  | `Exception (OpamStd.Sys.Exit _ | Sys.Break as e) ->
    OpamConsole.msg "Aborting.\n";
    raise e
  | `Exception (OpamSolver.ActionGraph.Parallel.Cyclic cycles as e) ->
    OpamConsole.error "Cycles found during dependency resolution:\n%s"
      (OpamStd.Format.itemize
         (OpamStd.List.concat_map (OpamConsole.colorise `yellow " -> ")
            OpamSolver.Action.to_string)
         cycles);
    raise e
  | `Exception (OpamSystem.Process_error _ | Unix.Unix_error _ as e) ->
    OpamConsole.error "Actions cancelled because of a system error:";
    OpamConsole.errmsg "%s\n" (Printexc.to_string e);
    raise e
  | `Exception e ->
    OpamConsole.error "Actions cancelled because of %s" (Printexc.to_string e);
    raise e
  | `Error err ->
    match err with
    | Aborted -> t, err
    | Partial_error solution_res ->
      let successful = solution_res.actions_successes in
      let failed = List.map fst solution_res.actions_errors in
      let remaining = solution_res.actions_aborted in
      (* Cleanup build/install actions when one of them failed, it's verbose and
         doesn't add information *)
      let successful =
        List.filter (function
            | `Fetch p | `Build p when List.mem (`Install p) failed -> false
            | _ -> true)
          successful
      in
      let remaining =
        List.filter (function
            | `Remove p | `Install p
              when List.mem (`Build p) failed -> false
            | `Remove p | `Install p | `Build p
              when List.mem (`Fetch p) failed -> false
            | _ -> true)
          remaining
      in
      let removes_missing_source =
        List.filter (function
            | `Remove p as rem ->
              let fetch = `Fetch p in
              List.mem fetch failed &&
              PackageActionGraph.mem_edge action_graph fetch rem
            | _ -> false
          )
          successful
      in
      let failed =
        (* Filter out failed fetches that were just for removals, there is a
           shorter message for them *)
        List.filter (function
            | `Fetch _ as a ->
              let succ = PackageActionGraph.succ action_graph a in
              not (List.for_all (fun a -> List.mem a removes_missing_source)
                     succ)
            | _ -> true)
          failed
      in
      let filter_graph l =
        if l = [] then PackageActionGraph.create () else
        let g = PackageActionGraph.copy action_graph in
        PackageActionGraph.iter_vertex (fun v ->
            if not (List.mem v l) then PackageActionGraph.remove_vertex g v)
          g;
        g
      in
      let successful = filter_graph successful in
      cleanup_artefacts successful;
      let successful = PackageActionGraph.reduce successful in
      let failed = PackageActionGraph.reduce (filter_graph failed) in
      let print_actions filter tint header ?empty actions =
        let actions =
          PackageActionGraph.fold_vertex (fun v acc ->
              if filter v then v::acc else acc)
            actions []
        in
        let actions = List.sort PackageAction.compare actions in
        if actions <> [] then
          OpamConsole.(msg "%s%s\n%s%s\n"
            (colorise tint
               (Printf.sprintf "%s%s "
                  (utf8_symbol Symbols.box_drawings_light_down_and_right "+")
                  (utf8_symbol Symbols.box_drawings_light_horizontal "-")))
            header
            (OpamStd.Format.itemize
               ~bullet:(colorise tint
                 (utf8_symbol Symbols.box_drawings_light_vertical "|" ^ " "))
               (fun x -> x)
               (List.map (String.concat " ") @@
                OpamStd.Format.align_table
                  (PackageAction.to_aligned_strings actions)))
            (colorise tint
               (Printf.sprintf "%s%s "
                  (utf8_symbol Symbols.box_drawings_light_up_and_right "+")
                  (utf8_symbol Symbols.box_drawings_light_horizontal "-"))))
        else match empty with
          | Some s ->
            OpamConsole.(msg "%s%s\n"
              (colorise tint
                 (Printf.sprintf "%s%s "
                    (utf8_symbol Symbols.box_drawings_light_right "-")
                    (utf8_symbol Symbols.box_drawings_light_horizontal "")))
              s)
          | None -> ()
      in
      if removes_missing_source <> [] then
        (OpamConsole.msg "\n";
         OpamConsole.warning
                 "The sources of the following couldn't be obtained, they may be \
                  uncleanly removed:\n%s"
                 (OpamStd.Format.itemize
                    (fun rm -> OpamPackage.to_string (action_contents rm))
                    removes_missing_source));
      OpamConsole.msg "\n";
      OpamConsole.header_msg "Error report";
      if OpamConsole.debug () || OpamConsole.verbose () then
        print_actions (fun _ -> true) `yellow
          (Printf.sprintf "The following actions were %s"
             (OpamConsole.colorise `yellow "aborted"))
          (PackageActionGraph.reduce (filter_graph remaining));
      print_actions (fun _ -> true) `red
        (Printf.sprintf "The following actions %s"
           (OpamConsole.colorise `red "failed"))
        failed;
      print_actions
        (function `Build _ | `Fetch _ -> false | _ -> true) `cyan
        ("The following changes have been performed"
         ^ if remaining <> [] then " (the rest was aborted)" else "")
        ~empty:"No changes have been performed"
        successful;
      t, err
    | _ -> assert false

let simulate_new_state state t =
  let installed =
    OpamSolver.ActionGraph.Topological.fold
      (fun action installed ->
        match action with
        | `Install p | `Change (_,_,p) | `Reinstall p ->
          OpamPackage.Set.add p installed
        | `Remove p ->
          OpamPackage.Set.remove p installed
        | `Build _ | `Fetch _ -> installed
      )
      t state.installed in
  { state with installed }

(* Ask confirmation whenever the packages to modify are not exactly
   the packages in the user request *)
let confirmation ?ask requested solution =
  OpamCoreConfig.(!r.answer = Some true) ||
  match ask with
  | Some false -> true
  | Some true -> OpamConsole.confirm "Do you want to continue?"
  | None ->
    let open PackageActionGraph in
    let solution_packages =
      fold_vertex (fun v acc ->
          OpamPackage.Name.Set.add (OpamPackage.name (action_contents v)) acc)
        solution
        OpamPackage.Name.Set.empty in
    OpamPackage.Name.Set.equal requested solution_packages
    || OpamConsole.confirm "Do you want to continue?"

let run_hook_job t name ?(local=[]) w =
  let shell_env = OpamEnv.get_full ~force_path:true t in
  let mk_cmd = function
    | cmd :: args ->
      let text = OpamProcess.make_command_text name ~args cmd in
      Some
        (fun () ->
           OpamSystem.make_command
             ~verbose:(OpamConsole.verbose ())
             ~env:(OpamTypesBase.env_array shell_env)
             ~name ~text cmd args)
    | [] -> None
  in
  let env v =
    try Some (List.assoc v local)
    with Not_found -> OpamPackageVar.resolve_switch t v
  in
  OpamProcess.Job.of_fun_list
    (OpamStd.List.filter_map (fun cmd -> mk_cmd cmd)
       (OpamFilter.commands env w))
  @@+ function
  | Some (cmd, _err) ->
    OpamConsole.error "The %s hook failed at %S"
      name (OpamProcess.string_of_command cmd);
    Done false
  | None ->
    Done true

let syspkgs_to_string spkgs =
  OpamStd.List.concat_map " "
    (fun p -> OpamConsole.colorise `bold (OpamSysPkg.to_string p))
    (OpamSysPkg.Set.elements spkgs)

let print_depext_msg (avail, nf) =
  if not (OpamSysPkg.Set.is_empty nf) then
    OpamConsole.warning
      "These additional system packages are required, but not available on \
       your system: %s"
      (syspkgs_to_string nf);
  if not (OpamSysPkg.Set.is_empty avail) then
    (OpamConsole.formatted_msg
       "\nThe following system packages will first need to be installed:\n";
     OpamConsole.formatted_msg ~indent:4 "    %s\n"
       (syspkgs_to_string avail))

(* Gets depexts from the state, without checking again *)
let get_depexts t packages =
  let sys_packages = Lazy.force t.sys_packages in
  let avail, nf =
    OpamPackage.Set.fold (fun pkg (avail,nf) ->
        match OpamPackage.Map.find_opt pkg sys_packages with
        | Some sys ->
          OpamSysPkg.(Set.union avail sys.s_available),
          OpamSysPkg.(Set.union nf sys.s_not_found)
        | None -> avail, nf)
      packages (OpamSysPkg.Set.empty, OpamSysPkg.Set.empty)
  in
  print_depext_msg (avail, nf);
  avail

let install_depexts t packages sys_packages =
  if OpamSysPkg.Set.is_empty sys_packages ||
     OpamClientConfig.(!r.show) ||
     OpamClientConfig.(!r.assume_depexts)
  then t else
  let print () =
    let commands =
      OpamSysInteract.install_packages_commands sys_packages
      |> List.map (fun (c,a) -> c::a)
    in
    OpamConsole.formatted_msg
      (match commands with
       | [_] -> "This command should get the requirements installed:\n"
       | _ -> "These commands should get the requirements installed:\n");
    OpamConsole.msg "\n    %s\n\n"
      (OpamStd.List.concat_map "\n    " (String.concat " ") commands)
  in
  let map_sysmap f t =
    let sys_packages =
      OpamPackage.Set.fold (fun nv sys_map ->
          match OpamPackage.Map.find_opt nv sys_map with
          | Some status ->
            OpamPackage.Map.add
              nv { status with OpamSysPkg.s_available =
                                 f status.OpamSysPkg.s_available }
              sys_map
          | None -> sys_map)
        packages
        (Lazy.force t.sys_packages)
    in
    { t with sys_packages = lazy sys_packages }
  in
  let recheck t sys_packages =
    let needed, _notfound = OpamSysInteract.packages_status sys_packages in
    let installed = OpamSysPkg.Set.diff sys_packages needed in
    map_sysmap (fun sysp -> OpamSysPkg.Set.diff sysp installed) t, needed
  in
  let rec wait msg sys_packages =
    let give_up () =
      OpamConsole.formatted_msg
        "You can retry with '--assume-depexts' to skip this check, or run \
         'opam config option global depext=false' to permanently disable handling of \
         system packages altogether.\n";
      OpamStd.Sys.exit_because `Aborted
    in
    if not OpamStd.Sys.tty_in || OpamCoreConfig.(!r.answer <> None) then
      give_up ()
    else if OpamConsole.confirm
        "%s\nWhen you are done: check again and continue?"
        msg
    then
      let t, to_install = recheck t sys_packages in
      if OpamSysPkg.Set.is_empty to_install then t else
      let msg =
        Printf.sprintf
          "\nThe following remain to be installed: %s"
          (syspkgs_to_string to_install)
      in
      wait msg to_install
    else if
      OpamConsole.confirm ~default:false
        "Do you want to attempt to proceed anyway?"
    then t
    else give_up ()
  in
  OpamConsole.header_msg "Handling external dependencies";
  if not (OpamFile.Config.depext_run_installs t.switch_global.config) then
    (print ();
     wait "You may now install the packages on your system."
       sys_packages)
  else if OpamClientConfig.(!r.fake) then (print (); t)
  else if
    OpamConsole.confirm
      "Let opam run your package manager to install the required system \
       packages?"
  then
    try
      OpamSysInteract.install sys_packages; (* handles dry_run *)
      map_sysmap (fun _ -> OpamSysPkg.Set.empty) t
    with Failure msg ->
      OpamConsole.error "%s" msg;
      wait "You can now try to get them installed manually."
        sys_packages
  else
    (OpamConsole.note "Use 'opam config option global \
                       depext-run-installs=false' if you don't want to be \
                       prompted again.";
     print ();
     wait
       "You may now install the packages manually on your system."
       sys_packages)

(* Apply a solution *)
let apply ?ask t ~requested ?add_roots ?(assume_built=false) ?force_remove
    solution =
  log "apply";
  if OpamSolver.solution_is_empty solution then
    (* The current state satisfies the request contraints *)
    t, Nothing_to_do
  else (
    (* Otherwise, compute the actions to perform *)
    let stats = OpamSolver.stats solution in
    let show_solution = ask <> Some false in
    let action_graph = OpamSolver.get_atomic_action_graph solution in
    let new_state = simulate_new_state t action_graph in
    OpamPackage.Set.iter
      (fun p ->
         try OpamFile.OPAM.print_errors (OpamSwitchState.opam new_state p)
         with Not_found ->
           OpamConsole.error "No opam file found for %s"
             (OpamPackage.to_string p))
      (OpamSolver.all_packages solution);
    if show_solution then (
      OpamConsole.msg
        "The following actions %s be %s:\n"
        (if OpamClientConfig.(!r.show) then "would" else "will")
        (if OpamStateConfig.(!r.dryrun) then "simulated" else
         if OpamClientConfig.(!r.fake) then "faked"
         else "performed");
      let messages p =
        let opam = OpamSwitchState.opam new_state p in
        let messages = OpamFile.OPAM.messages opam in
        OpamStd.List.filter_map (fun (s,f) ->
          if OpamFilter.opt_eval_to_bool
              (OpamPackageVar.resolve ~opam new_state) f
          then Some s
          else None
        )  messages in
      let append nv =
        (* mark pinned packages with a star *)
        if OpamPackage.Set.mem nv t.pinned then "*"
        else ""
      in
      OpamSolver.print_solution ~messages ~append
        ~requested ~reinstall:(Lazy.force t.reinstall)
        solution;
      let total_actions = sum stats in
      if total_actions >= 2 then
        OpamConsole.msg "===== %s =====\n" (OpamSolver.string_of_stats stats);
    );
    let sys_packages =
      if not (OpamFile.Config.depext t.switch_global.config) then
        OpamSysPkg.Set.empty
      else
        get_depexts t @@ OpamPackage.Set.inter
          new_state.installed
          (OpamSolver.all_packages solution)
    in
    if not OpamClientConfig.(!r.show) &&
       confirmation ?ask requested action_graph
    then (
      let t = install_depexts t new_state.installed sys_packages in
      let requested =
        OpamPackage.packages_of_names new_state.installed requested
      in
      let run_job =
        if OpamStateConfig.(!r.dryrun) || OpamClientConfig.(!r.fake)
        then OpamProcess.Job.dry_run
        else OpamProcess.Job.run
      in
      let var_def name l =
        OpamVariable.Full.of_string name, L l
      in
      let var_def_pset name set =
        var_def name
          (List.map OpamPackage.to_string (OpamPackage.Set.elements set))
      in
      let var_def_spset name set =
        var_def name
          (List.map OpamSysPkg.to_string (OpamSysPkg.Set.elements set))
      in
      let depexts =
        OpamPackage.Set.fold (fun nv depexts ->
            OpamSysPkg.Set.union depexts
              (OpamSwitchState.depexts t nv))
          new_state.installed OpamSysPkg.Set.empty
      in
      let pre_session =
        let open OpamPackage.Set.Op in
        let local = [
          var_def_pset "installed" new_state.installed;
          var_def_pset "new" (new_state.installed -- t.installed);
          var_def_pset "removed" (t.installed -- new_state.installed);
          var_def_spset "depexts" depexts;
        ] in
        run_job @@
        run_hook_job t "pre-session" ~local
          (OpamFile.Wrappers.pre_session
             (OpamFile.Config.wrappers t.switch_global.config))
      in
      if not pre_session then
        OpamStd.Sys.exit_because `Configuration_error;
      let t0 = t in
      let t, r =
        parallel_apply t ~requested ?add_roots ~assume_built ?force_remove
          action_graph
      in
      let success = match r with | OK _ -> true | _ -> false in
      let t =
        if success && (OpamClientConfig.(!r.assume_depexts) ||
                       OpamStateConfig.(!r.no_depexts)) then
          let depext_bypass =
            OpamSysPkg.Set.Op.(t.switch_config.depext_bypass ++ sys_packages)
          in
          { t with switch_config = { t.switch_config with depext_bypass }}
        else t
      in
      let post_session =
        let open OpamPackage.Set.Op in
        let local = [
          var_def_pset "installed" t.installed;
          var_def_pset "new" (t.installed -- t0.installed);
          var_def_pset "removed" (t0.installed -- t.installed);
          OpamVariable.Full.of_string "success", B (success);
          OpamVariable.Full.of_string "failure", B (not success);
        ] in
        run_job @@
        run_hook_job t "post-session" ~local
          (OpamFile.Wrappers.post_session
             (OpamFile.Config.wrappers t.switch_global.config))
      in
      if not post_session then
        OpamStd.Sys.exit_because `Configuration_error;
      t, r
    ) else
      t, Aborted
  )

let resolve t action ~orphans ?reinstall ~requested request =
  if OpamClientConfig.(!r.json_out <> None) then (
    OpamJson.append "command-line"
      (`A (List.map (fun s -> `String s) (Array.to_list Sys.argv)));
    OpamJson.append "switch" (OpamSwitch.to_json t.switch)
  );
  let universe =
    OpamSwitchState.universe t ~requested ?reinstall action
  in
  Json.output_request request action;
  let r = OpamSolver.resolve universe ~orphans request in
  Json.output_solution t r;
  r

let resolve_and_apply ?ask t action ~orphans ?reinstall ~requested ?add_roots
    ?(assume_built=false) ?force_remove request =
  match resolve t action ~orphans ?reinstall ~requested request with
  | Conflicts cs ->
    log "conflict!";
    OpamConsole.msg "%s"
      (OpamCudf.string_of_conflict t.packages
         (OpamSwitchState.unavailable_reason t) cs);
    t, Conflicts cs
  | Success solution ->
    let t, res =
      apply ?ask t ~requested ?add_roots ~assume_built ?force_remove solution
    in
    t, Success res
