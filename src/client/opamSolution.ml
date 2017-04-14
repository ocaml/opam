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

let log fmt = OpamConsole.log "SOLUTION" fmt

open OpamTypes
open OpamTypesBase
open OpamStateTypes
open OpamProcess.Job.Op

module PackageAction = OpamSolver.Action
module PackageActionGraph = OpamSolver.ActionGraph

let post_message ?(failed=false) st action =
  match action, failed with
  | `Remove _, _ | `Reinstall _, _ | `Build _, false -> ()
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
    ) else if failed && OpamFile.OPAM.depexts opam <> None then (
      OpamConsole.header_msg "%s troubleshooting" (OpamPackage.to_string pkg);
      OpamConsole.formatted_msg ~indent:(OpamStd.Format.visual_length mark)
        "%sThis package relies on external (system) dependencies that may \
         be missing. `opam depext %s' may help you find the correct \
         installation for your system.\n"
        mark (OpamPackage.to_string pkg)
    )

let check_solution ?(quiet=false) st = function
  | No_solution ->
    OpamConsole.msg "No solution found, exiting\n";
    OpamStd.Sys.exit 3
  | Partial_error (success, failed, _remaining) ->
    List.iter (post_message st) success;
    List.iter (post_message ~failed:true st) failed;
    OpamEnv.check_and_print_env_warning st;
    OpamStd.Sys.exit 4
  | OK actions ->
    List.iter (post_message st) actions;
    OpamEnv.check_and_print_env_warning st
  | Nothing_to_do ->
    if not quiet then OpamConsole.msg "Nothing to do.\n";
    OpamEnv.check_and_print_env_warning st
  | Aborted     -> OpamStd.Sys.exit 0

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

(* unused ?
let atom_of_name name =
  name, None
*)

let check_availability ?permissive t set atoms =
  let available = OpamPackage.to_map set in
  let check_atom (name, cstr as atom) =
    let exists =
      try
        OpamPackage.Version.Set.exists
          (fun v -> OpamFormula.check atom (OpamPackage.create name v))
          (OpamPackage.Name.Map.find name available)
      with Not_found -> false
    in
    if exists then None
    else if permissive = Some true
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
     OpamStd.Sys.exit 66)

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
let parallel_apply t _action ~requested ?add_roots action_graph =
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

  let add_to_install nv =
    let root = OpamPackage.Name.Set.mem nv.name root_installs in
    t_ref := OpamSwitchAction.add_to_installed !t_ref ~root nv
  in

  let remove_from_install ?keep_as_root nv =
    t_ref := OpamSwitchAction.remove_from_installed ?keep_as_root !t_ref nv
  in

  let inplace =
    if OpamClientConfig.(!r.inplace_build) then
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

  (* 1/ fetch needed package archives *)

  let failed_downloads =
    let sources_needed =
      OpamPackage.Set.Op.
        (OpamAction.sources_needed t action_graph -- OpamPackage.keys inplace)
    in
    let sources_list = OpamPackage.Set.elements sources_needed in
    if OpamPackage.Set.exists (fun nv ->
        not (OpamPackage.Set.mem nv t.pinned &&
             OpamFilename.exists_dir (OpamSwitchState.source_dir t nv)))
        sources_needed
    then OpamConsole.header_msg "Gathering sources";
    let results =
      OpamParallel.map
        ~jobs:OpamStateConfig.(!r.dl_jobs)
        ~command:(OpamAction.download_package t)
        ~dry_run:OpamStateConfig.(!r.dryrun)
        sources_list
    in
    List.fold_left2 (fun failed nv -> function
        | None -> failed
        | Some e -> OpamPackage.Map.add nv e failed)
      OpamPackage.Map.empty sources_list results
  in

  if OpamClientConfig.(!r.json_out <> None) &&
     not (OpamPackage.Map.is_empty failed_downloads) then
    OpamJson.append "download-failures"
      (`O (List.map (fun (nv,err) -> OpamPackage.to_string nv, `String err)
             (OpamPackage.Map.bindings failed_downloads)));

  let fatal_dl_error =
    PackageActionGraph.fold_vertex
      (fun a acc -> acc || match a with
         | `Remove _ -> false
         | _ -> OpamPackage.Map.mem (action_contents a) failed_downloads)
      action_graph false
  in
  if fatal_dl_error then
    OpamConsole.error_and_exit
      "The sources of the following couldn't be obtained, aborting:\n%s"
      (OpamStd.Format.itemize OpamPackage.to_string
         (OpamPackage.Map.keys failed_downloads))
  else if not (OpamPackage.Map.is_empty failed_downloads) then
    OpamConsole.warning
      "The sources of the following couldn't be obtained, they may be \
       uncleanly uninstalled:\n%s"
      (OpamStd.Format.itemize OpamPackage.to_string
         (OpamPackage.Map.keys failed_downloads));


  (* 2/ process the package actions (installations and removals) *)

  let action_graph = (* Add build actions *)
    let noop_remove nv =
      OpamAction.noop_remove_package t nv in
    PackageActionGraph.explicit ~noop_remove action_graph
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
    if not (PackageAction.Set.is_empty failed) then
      Done (`Error (`Aborted failed)) (* prerequisite failed *)
    else
    let store_time =
      let t0 = Unix.gettimeofday () in
      fun () -> Hashtbl.add timings action (Unix.gettimeofday () -. t0)
    in
    let visible_installed =
      OpamPackage.Set.Op.(minimal_install ++ installed)
    in
    let t =
      { !t_ref with
        installed = visible_installed;
        conf_files = OpamPackage.Map.filter
            (fun nv _ -> OpamPackage.Set.mem nv visible_installed)
            !t_ref.conf_files; }
    in
    let nv = action_contents action in
    let source_dir = OpamSwitchState.source_dir t nv in
    if OpamClientConfig.(!r.fake) then
      match action with
      | `Build _ -> Done (`Successful (installed, removed))
      | `Install nv ->
        OpamConsole.msg "Faking installation of %s\n"
          (OpamPackage.to_string nv);
        add_to_install nv;
        Done (`Successful (OpamPackage.Set.add nv installed , removed))
      | `Remove nv ->
        remove_from_install nv;
        Done (`Successful (installed, OpamPackage.Set.add nv removed))
      | _ -> assert false
    else
    match action with
    | `Build nv ->
      let build_dir =
        try OpamPackage.Map.find nv inplace
        with Not_found ->
          let dir = OpamPath.Switch.build t.switch_global.root t.switch nv in
          if not OpamClientConfig.(!r.reuse_build_dir) then
            OpamFilename.rmdir dir;
          dir
      in
      let test =
        OpamStateConfig.(!r.build_test) && OpamPackage.Set.mem nv requested
      in
      let doc =
        OpamStateConfig.(!r.build_doc) && OpamPackage.Set.mem nv requested
      in
      (if OpamFilename.exists_dir source_dir
       then OpamFilename.copy_dir ~src:source_dir ~dst:build_dir
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
        (fun () -> OpamAction.remove_package t nv) @@| fun () ->
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
      let installs_removes =
        PackageActionGraph.fold_vertex
          (fun a acc -> match a with `Install _ | `Remove _ as i -> i::acc
                                   | _ -> acc)
          action_graph []
      in
      let same_inplace_source =
        OpamPackage.Map.fold (fun nv dir acc ->
            OpamFilename.Dir.Map.update dir (fun l -> nv::l) [] acc)
          inplace OpamFilename.Dir.Map.empty |>
        OpamFilename.Dir.Map.values
      in
      let mutually_exclusive =
        installs_removes ::
        OpamStd.List.filter_map
          (fun excl ->
             match
               OpamStd.List.filter_map
                 (fun nv ->
                    let act = `Build nv in
                    if PackageActionGraph.mem_vertex action_graph act
                    then Some act else None)
                 excl
             with [] | [_] -> None | l -> Some l)
          same_inplace_source
      in
      let results =
        PackageActionGraph.Parallel.map
          ~jobs:(Lazy.force OpamStateConfig.(!r.jobs))
          ~command:job
          ~dry_run:OpamStateConfig.(!r.dryrun)
          ~mutually_exclusive
          action_graph
      in
      if OpamClientConfig.(!r.json_out <> None) then
        (let j =
           PackageActionGraph.Topological.fold (fun a acc ->
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
         OpamJson.append "results" (`A (List.rev j)));
      let success, failure, aborted =
        List.fold_left (fun (success, failure, aborted) -> function
            | a, `Successful _ -> a::success, failure, aborted
            | a, `Exception e -> success, (a,e)::failure, aborted
            | a, `Error (`Aborted _) -> success, failure, a::aborted
          ) ([], [], []) results
      in
      if failure = [] && aborted = [] then `Successful ()
      else (
        List.iter display_error failure;
        `Error (Partial_error (success, List.map fst failure, aborted))
      )
    with
    | PackageActionGraph.Parallel.Errors (success, errors, remaining) ->
      List.iter display_error errors;
      `Error (Partial_error (success, List.map fst errors, remaining))
    | e -> `Exception e
  in
  let t = !t_ref in

  (* 3/ Display errors and finalize *)

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
        | `Remove _ | `Install _ | `Build _ -> ()
        | _ -> assert false)
      graph
  in
  match action_results with
  | `Successful () ->
    cleanup_artefacts action_graph;
    OpamConsole.msg "Done.\n";
    t, OK (PackageActionGraph.fold_vertex (fun a b -> a::b) action_graph [])
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
    | Partial_error (successful, failed, remaining) ->
      (* Cleanup build/install actions when one of them failed, it's verbose and
         doesn't add information *)
      let successful =
        List.filter (function
            | `Build p when List.mem (`Install p) failed -> false
            | _ -> true)
          successful
      in
      let remaining =
        List.filter (function
            | `Remove p | `Install p when List.mem (`Build p) failed -> false
            | _ -> true)
          remaining
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
      let print_actions filter color header ?empty actions =
        let actions =
          PackageActionGraph.fold_vertex (fun v acc ->
              if filter v then v::acc else acc)
            actions []
        in
        let actions = List.sort PackageAction.compare actions in
        if actions <> [] then
          OpamConsole.msg "%s%s\n%s%s\n"
            (OpamConsole.colorise color
               (if OpamConsole.utf8 ()
                then "\xe2\x94\x8c\xe2\x94\x80 " (* U+250C U+2500 *)
                else "+- "))
            header
            (OpamStd.Format.itemize
               ~bullet:(OpamConsole.colorise color
                          (if OpamConsole.utf8 ()
                           then "\xe2\x94\x82 " (* U+2503 *) else "| "))
               (fun x -> x)
               (PackageAction.to_aligned_strings actions))
            (OpamConsole.colorise color
               (if OpamConsole.utf8 ()
                then "\xe2\x94\x94\xe2\x94\x80 " (* U+2514 U+2500 *)
                else "+- "))
        else match empty with
          | Some s ->
            OpamConsole.msg "%s%s\n"
              (OpamConsole.colorise color
                 (if OpamConsole.utf8 ()
                  then "\xe2\x95\xb6\xe2\x94\x80 " (* U+2576 U+2500 *)
                  else "- "))
              s
          | None -> ()
      in
      OpamConsole.msg "\n";
      OpamConsole.header_msg "Error report";
      print_actions (fun _ -> true) `yellow
        (Printf.sprintf "The following actions were %s"
           (OpamConsole.colorise `yellow "aborted"))
        (PackageActionGraph.reduce (filter_graph remaining));
      print_actions (fun _ -> true) `red
        (Printf.sprintf "The following actions %s"
           (OpamConsole.colorise `red "failed"))
        failed;
      print_actions
        (function `Build _ -> false | _ -> true) `cyan
        "The following changes have been performed"
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
        | `Build _ -> installed
      )
      t state.installed in
  { state with installed }

let print_external_tags t solution =
  let packages = OpamSolver.new_packages solution in
  let external_tags = OpamStd.String.Set.of_list OpamClientConfig.(!r.external_tags) in
  let values =
    OpamPackage.Set.fold (fun nv accu ->
        let opam = OpamSwitchState.opam t nv in
        match OpamFile.OPAM.depexts opam with
        | None         -> accu
        | Some alltags ->
          OpamStd.String.SetMap.fold (fun tags values accu ->
              if OpamStd.String.Set.(
                  (* A \subseteq B <=> (A U B) / B = 0 *)
                  is_empty (diff (union external_tags tags) external_tags)
                )
              then
                OpamStd.String.Set.union values accu
              else
                accu
            ) alltags accu
      ) packages OpamStd.String.Set.empty in
  let values = OpamStd.String.Set.elements values in
  if values <> [] then
    OpamConsole.msg "%s\n" (String.concat " " values)

(* Ask confirmation whenever the packages to modify are not exactly
   the packages in the user request *)
let confirmation ?ask requested solution =
  OpamCoreConfig.(!r.answer = Some true) ||
  match ask with
  | Some false -> true
  | Some true -> OpamConsole.confirm "Do you want to continue ?"
  | None ->
    let open PackageActionGraph in
    let solution_packages =
      fold_vertex (fun v acc ->
          OpamPackage.Name.Set.add (OpamPackage.name (action_contents v)) acc)
        solution
        OpamPackage.Name.Set.empty in
    OpamPackage.Name.Set.equal requested solution_packages
    || OpamConsole.confirm "Do you want to continue ?"

(* Apply a solution *)
let apply ?ask t action ~requested ?add_roots solution =
  log "apply";
  if OpamSolver.solution_is_empty solution then
    (* The current state satisfies the request contraints *)
    t, Nothing_to_do
  else (
    (* Otherwise, compute the actions to perform *)
    let stats = OpamSolver.stats solution in
    let show_solution = ask <> Some false &&
                        OpamClientConfig.(!r.external_tags) = [] in
    let action_graph = OpamSolver.get_atomic_action_graph solution in
    let new_state = simulate_new_state t action_graph in
    OpamPackage.Set.iter
      (fun p -> OpamFile.OPAM.print_errors (OpamSwitchState.opam new_state p))
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
      OpamSolver.print_solution ~messages ~append ~requested solution;
      let total_actions = sum stats in
      if total_actions >= 2 then
        OpamConsole.msg "===== %s =====\n" (OpamSolver.string_of_stats stats);
      match action with
      | Install | Upgrade | Reinstall
        when not (OpamCudf.external_solver_available ()) &&
             stats.s_remove + stats.s_downgrade >= max 2 (total_actions / 2)
        ->
        OpamConsole.note
          "This solution may not be optimal. You should probably install an \
           external solver (see \
           http://opam.ocaml.org/doc/Install.html#ExternalSolvers for details)"
      | _ -> ()
    );

    if OpamClientConfig.(!r.external_tags) <> [] then (
      print_external_tags t solution;
      t, Aborted
    ) else if not OpamClientConfig.(!r.show) &&
              confirmation ?ask requested action_graph
    then (
      (* print_variable_warnings t; *)
      let requested =
        OpamPackage.packages_of_names new_state.installed requested
      in
      parallel_apply t action ~requested ?add_roots action_graph
    ) else
      t, Aborted
  )

let resolve ?(verbose=true) t action ~orphans ?reinstall ~requested request =
  if OpamClientConfig.(!r.json_out <> None) then (
    OpamJson.append "command-line"
      (`A (List.map (fun s -> `String s) (Array.to_list Sys.argv)));
    OpamJson.append "switch" (OpamSwitch.to_json t.switch)
  );
  Json.output_request request action;
  let r =
    OpamSolver.resolve ~verbose
      (OpamSwitchState.universe t ~requested ?reinstall action)
      ~orphans
      request
  in
  Json.output_solution t r;
  r

let resolve_and_apply ?ask t action ~orphans ?reinstall ~requested ?add_roots request =
  match resolve t action ~orphans ?reinstall ~requested request with
  | Conflicts cs ->
    log "conflict!";
    OpamConsole.msg "%s"
      (OpamCudf.string_of_conflict t.packages
         (OpamSwitchState.unavailable_reason t) cs);
    t, No_solution
  | Success solution -> apply ?ask t action ~requested ?add_roots solution
