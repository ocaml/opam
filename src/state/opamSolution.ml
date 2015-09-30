(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

let log fmt = OpamConsole.log "SOLUTION" fmt

open OpamTypes
open OpamTypesBase
open OpamState.Types
open OpamProcess.Job.Op

module PackageAction = OpamSolver.Action
module PackageActionGraph = OpamSolver.ActionGraph

let post_message ?(failed=false) state action =
  match action with
  | `Remove _ | `Reinstall _ | `Build _ -> ()
  | `Install pkg | `Change (_,_,pkg) ->
    let opam = OpamState.opam state pkg in
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
      let filter_env = OpamState.filter_env ~opam ~local_variables state in
      OpamStd.List.filter_map (fun (message,filter) ->
          if OpamFilter.opt_eval_to_bool filter_env filter
          then Some (OpamFilter.expand_string filter_env message)
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

let check_solution state = function
  | No_solution ->
    OpamConsole.msg "No solution found, exiting\n";
    OpamStd.Sys.exit 3
  | Error (success, failed, _remaining) ->
    List.iter (post_message state) success;
    List.iter (post_message ~failed:true state) failed;
    OpamStd.Sys.exit 4
  | OK actions ->
    List.iter (post_message state) actions
  | Nothing_to_do -> OpamConsole.msg "Nothing to do.\n"
  | Aborted     -> OpamStd.Sys.exit 0

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let eq_atom name version =
  name, Some (`Eq, version)

let eq_atom_of_package nv =
  eq_atom (OpamPackage.name nv) (OpamPackage.version nv)

let eq_atoms_of_packages set =
  List.rev_map eq_atom_of_package (OpamPackage.Set.elements set)

let atom_of_package nv =
  OpamPackage.name nv, None

let atoms_of_packages set =
  List.rev_map (fun n -> n, None)
    (OpamPackage.Name.Set.elements (OpamPackage.names_of_packages set))

(* unused ?
let atom_of_name name =
  name, None
*)

let check_availability ?permissive t set atoms =
  let available = OpamPackage.to_map set in
  let check_atom (name, _ as atom) =
    let exists =
      try
        OpamPackage.Version.Set.exists
          (fun v -> OpamFormula.check atom (OpamPackage.create name v))
          (OpamPackage.Name.Map.find name available)
      with Not_found -> false
    in
    if exists then None
    else if permissive = Some true
    then Some (OpamState.unknown_package t atom)
    else Some (OpamState.unavailable_reason t atom) in
  let errors = OpamStd.List.filter_map check_atom atoms in
  if errors <> [] then
    (List.iter (OpamConsole.error "%s") errors;
     OpamStd.Sys.exit 66)

let sanitize_atom_list ?(permissive=false) t atoms =
  let packages =
    OpamPackage.to_map (OpamPackage.Set.union t.packages t.installed) in
  (* gets back the original capitalization of the package name *)
  let realname name =
    let lc_name name = String.lowercase (OpamPackage.Name.to_string name) in
    let m =
      OpamPackage.Name.Map.filter (fun p _ -> lc_name name = lc_name p)
        packages in
    match OpamPackage.Name.Map.keys m with [name] -> name | _ -> name
  in
  let atoms = List.rev_map (fun (name,cstr) -> realname name, cstr) atoms in
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
    | e -> disp "%s" (Printexc.to_string e)
  in
  match n with
  | `Change (`Up, _, nv)   -> f "upgrading to" nv
  | `Change (`Down, _, nv) -> f "downgrading to" nv
  | `Install nv        -> f "installing" nv
  | `Reinstall nv      -> f "recompiling" nv
  | `Remove nv         -> f "removing" nv
  | `Build nv          -> f "compiling" nv

(* Prettify errors *)
(* unuesed ?
let string_of_errors errors =
  let actions = List.rev_map fst errors in
  let packages = List.rev_map action_contents actions in
  match packages with
  | []  -> assert false
  | [h] -> OpamPackage.to_string h
  | l   -> OpamPackage.Set.to_string (OpamPackage.Set.of_list l)
*)

let new_variables e =
  let e = List.filter (fun (_,s,_) -> s="=") e in
  let e = List.rev_map (fun (v,_,_) -> v) e in
  OpamStd.String.Set.of_list e

let variable_warnings = ref false
let print_variable_warnings t =
  let variables = ref [] in
  if not !variable_warnings then (
    let warn w =
      let is_defined s =
        try let _ = OpamStd.Env.get s in true
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
    if OpamPackage.Set.exists (
        fun nv -> OpamPackage.Name.to_string (OpamPackage.name nv) = "ocamlfind"
      ) t.installed then
      List.iter warn ocamlfind_vars;
    (* 2. Warn about variables possibly set by other compilers *)
    let new_variables comp =
      let comp_f = OpamPath.compiler_comp t.root comp in
      let env = OpamFile.Comp.env (OpamFile.Comp.safe_read comp_f) in
      new_variables env in
    let vars = ref OpamStd.String.Set.empty in
    OpamSwitch.Map.iter (fun _ comp ->
      vars := OpamStd.String.Set.union !vars (new_variables comp)
    ) t.aliases;
    vars := OpamStd.String.Set.diff !vars (new_variables t.compiler);
    OpamStd.String.Set.iter warn !vars;
    if !variables <> [] then (
      OpamConsole.msg "The following variables are set in your environment, it \
                       is advised to unset them for OPAM to work correctly.\n";
      List.iter (OpamConsole.msg " - %s\n") !variables;
      if not (OpamConsole.confirm "Do you want to continue ?") then
        OpamStd.Sys.exit 1;
    );
    variable_warnings := true;
  )

module Json = struct
  let output_request request action =
    if OpamStateConfig.(!r.json_out = None) then () else
    let atoms =
      List.map (fun a -> `String (OpamFormula.short_string_of_atom a))
    in
    let j = `O [
        ("action", match action with
          | Install ns -> `O [ "install", OpamPackage.Name.Set.to_json ns ]
          | Upgrade ps -> `O [ "upgrade", OpamPackage.Set.to_json ps ]
          | Reinstall ps -> `O [ "reinstall", OpamPackage.Set.to_json ps ]
          | Depends -> `String "depends"
          | Init -> `String "init"
          | Remove -> `String "remove"
          | Switch ns -> `O [ "switch", OpamPackage.Name.Set.to_json ns ]
          | Import  ns -> `O [ "import", OpamPackage.Name.Set.to_json ns ]);
        "install", `A (atoms request.wish_install);
        "remove", `A (atoms request.wish_remove);
        "upgrade", `A (atoms request.wish_upgrade);
        "criteria", `String (OpamSolverConfig.criteria request.criteria);
      ]
    in
    OpamJson.append "request" j

  let output_solution t solution =
    if OpamStateConfig.(!r.json_out = None) then () else
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
        OpamCudf.strings_of_conflict (OpamState.unavailable_reason t) cs
      in
      let chains = OpamCudf.conflict_chains cs in
      let jchains =
        `A (List.map (fun c ->
            `A ((List.map (fun f -> `String (OpamFormula.to_string f)) c)))
            chains)
      in
      let toj l = `A (List.map (fun s -> `String s) l) in
      OpamJson.append "conflicts"
        (`O ((if cycles <> [] then ["cycles", toj cycles] else []) @
             (if causes <> [] then ["causes", toj causes] else []) @
             (if chains <> [] then ["broken-deps", jchains] else [])))

  let exc e =
    let lmap f l = List.rev (List.rev_map f l) in
    if OpamStateConfig.(!r.json_out = None) then `O [] else
    match e with
    | OpamSystem.Process_error
        {OpamProcess.r_code; r_duration; r_info; r_stdout; r_stderr; _} ->
      `O [ ("process-error",
            `O [ ("code", `String (string_of_int r_code));
                 ("duration", `Float r_duration);
                 ("info", `O (lmap (fun (k,v) -> (k, `String v)) r_info));
                 ("stdout", `A (lmap (fun s -> `String s) r_stdout));
                 ("stderr", `A (lmap (fun s -> `String s) r_stderr));
               ])]
    | OpamSystem.Internal_error s ->
      `O [ ("internal-error", `String s) ]
    | e -> `O [ ("exception", `String (Printexc.to_string e)) ]

end

(* Process the atomic actions in a graph in parallel, respecting graph order,
   and report to user. Takes a graph of atomic actions *)
let parallel_apply t action action_graph =
  log "parallel_apply";

  (* We keep an imperative state up-to-date and flush it to disk as soon
     as an operation terminates *)
  let t_ref = ref t in

  let root_installs =
    let names = OpamPackage.names_of_packages t.installed_roots in
    match action with
    | Init ->
      OpamPackage.Name.Set.union names (OpamState.base_package_names t)
    | Install r | Import r | Switch r  ->
      OpamPackage.Name.Set.union names r
    | Upgrade _ | Reinstall _ -> names
    | Depends | Remove -> OpamPackage.Name.Set.empty
  in

  let add_to_install nv =
    t_ref :=
      OpamAction.update_metadata t
        ~installed:(OpamPackage.Set.add nv !t_ref.installed)
        ~reinstall:(OpamPackage.Set.remove nv !t_ref.reinstall)
        ~installed_roots:
          (if OpamPackage.Name.Set.mem (OpamPackage.name nv) root_installs
           then OpamPackage.Set.add nv !t_ref.installed_roots
           else !t_ref.installed_roots);
    if not OpamStateConfig.(!r.dryrun) then
      OpamState.install_metadata !t_ref nv
  in

  let remove_from_install nv =
    let rm = OpamPackage.Set.remove nv in
    t_ref :=
      OpamAction.update_metadata t
        ~installed:(rm !t_ref.installed)
        ~installed_roots:(rm !t_ref.installed_roots)
        ~reinstall:(rm !t_ref.reinstall);
  in

  (* 1/ fetch needed package archives *)

  let package_sources, failed_downloads =
    let sources_needed = OpamAction.sources_needed t action_graph in
    let sources_list = OpamPackage.Set.elements sources_needed in
    if sources_list <> [] then
      OpamConsole.header_msg "Gathering sources";
    let results =
      OpamParallel.map
        ~jobs:(OpamState.dl_jobs t)
        ~command:(OpamAction.download_package t)
        ~dry_run:OpamStateConfig.(!r.dryrun)
        sources_list
    in
    List.fold_left2 (fun (sources,failed) nv -> function
        | `Successful None -> sources, failed
        | `Successful (Some dl) -> OpamPackage.Map.add nv dl sources, failed
        | `Error e -> sources, OpamPackage.Map.add nv e failed)
      (OpamPackage.Map.empty,OpamPackage.Map.empty) sources_list results
  in

  if OpamStateConfig.(!r.json_out <> None) &&
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
      "The sources of the following couldn't be obtained, aborting:\n%s\
       (This might be due to outdated metadata, in this case run \
       'opam update')"
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
    PackageActionGraph.explicit action_graph
  in

  let timings = Hashtbl.create 17 in
  (* the child job to run on each action *)
  let job ~pred action =
    let installed, removed, failed =
      List.fold_left (fun (inst,rem,fail) -> function
          | _, `Successful (inst1, rem1) ->
            OpamPackage.Set.Op.(inst ++ inst1, rem ++ rem1, fail)
          | _, `Error (`Aborted a) ->
            inst, rem, a @ fail
          | a, (`Exception _ | `Error _) ->
            inst, rem, a :: fail)
        OpamPackage.Set.(empty, empty, []) pred
    in
    match failed with
    | _::_ -> Done (`Error (`Aborted failed)) (* prerequisite failed *)
    | [] ->
      let store_time =
        let t0 = Unix.gettimeofday () in
        fun () -> Hashtbl.add timings action (Unix.gettimeofday () -. t0)
      in
      let t = (* Local state for this process, only prerequisites are visible *)
        { t with installed =
                   OpamPackage.Set.Op.(t.installed -- removed ++ installed) }
      in
      let nv = action_contents action in
      let source =
        try Some (OpamPackage.Map.find nv package_sources)
        with Not_found -> None in
      if OpamStateConfig.(!r.fake) then
        match action with
        | `Build _ -> Done (`Successful (installed, removed))
        | `Install nv ->
          OpamConsole.msg "Faking installation of %s"
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
          (OpamAction.build_package t source nv @@+ function
            | None -> store_time (); Done (`Successful (installed, removed))
            | Some exn -> store_time (); Done (`Exception exn))
      | `Install nv ->
        (OpamAction.install_package t nv @@+ function
          | None ->
            add_to_install nv;
            store_time ();
            Done (`Successful (OpamPackage.Set.add nv installed, removed))
          | Some exn ->
            store_time ();
            Done (`Exception exn))
      | `Remove nv ->
        if OpamAction.removal_needs_download t nv then
          (try OpamAction.extract_package t source nv
           with e -> OpamStd.Exn.fatal e);
        OpamProcess.Job.catch (fun e -> OpamStd.Exn.fatal e; Done ())
          (OpamAction.remove_package t ~metadata:false nv) @@| fun () ->
        remove_from_install nv;
        store_time ();
        `Successful (installed, OpamPackage.Set.add nv removed)
      | _ -> assert false
  in

  let action_results =
    OpamConsole.header_msg "Processing actions";
    try
      let installs =
        PackageActionGraph.fold_vertex
          (fun a acc -> match a with `Install _ as i -> i::acc | _ -> acc)
          action_graph []
      in
      let results =
        PackageActionGraph.Parallel.map
          ~jobs:(OpamState.jobs t)
          ~command:job
          ~dry_run:OpamStateConfig.(!r.dryrun)
          ~mutually_exclusive:[installs]
          action_graph
      in
      if OpamStateConfig.(!r.json_out <> None) then
        (let j =
           PackageActionGraph.Topological.fold (fun a acc ->
               let r = match List.assoc a results with
                 | `Successful _ -> `String "OK"
                 | `Exception e -> Json.exc e
                 | `Error (`Aborted deps) ->
                   let deps =
                     OpamStd.List.sort_nodup OpamSolver.Action.compare deps
                   in
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
        `Error (Error (success, List.map fst failure, aborted))
      )
    with
    | PackageActionGraph.Parallel.Errors (success, errors, remaining) ->
      List.iter display_error errors;
      `Error (Error (success, List.map fst errors, remaining))
    | e -> `Exception e
  in
  let t = !t_ref in

  (* 3/ Display errors and finalize *)

  let cleanup_artefacts graph =
    PackageActionGraph.iter_vertex (function
        | `Remove nv when not (OpamState.is_pinned t (OpamPackage.name nv)) ->
          OpamAction.cleanup_package_artefacts t nv (* no-op if reinstalled *)
        | `Remove _ | `Install _ | `Build _ -> ()
        | _ -> assert false)
      graph
  in
  match action_results with
  | `Successful () ->
    cleanup_artefacts action_graph;
    OpamConsole.msg "Done.\n";
    OK (PackageActionGraph.fold_vertex (fun a b -> a::b) action_graph [])
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
    | Aborted -> err
    | Error (successful, failed, remaining) ->
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
            | `Install p when List.mem (`Build p) failed -> false
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
      let print_actions filter header ?empty actions =
        let actions =
          PackageActionGraph.fold_vertex (fun v acc ->
              if filter v then v::acc else acc)
            actions []
        in
        let actions = List.sort PackageAction.compare actions in
        if actions <> [] then
          OpamConsole.msg "%s\n%s" header
            (OpamStd.Format.itemize ~bullet:"  " (fun x -> x)
               (PackageAction.to_aligned_strings actions))
        else match empty with
          | Some s -> OpamConsole.msg "%s\n" s
          | None -> ()
      in
      OpamConsole.msg "\n";
      OpamConsole.header_msg "Error report";
      print_actions (fun _ -> true)
        (Printf.sprintf "The following actions were %s"
           (OpamConsole.colorise `yellow "aborted"))
        (PackageActionGraph.reduce (filter_graph remaining));
      print_actions (fun _ -> true)
        (Printf.sprintf "The following actions %s"
           (OpamConsole.colorise `red "failed"))
        failed;
      print_actions
        (function `Reinstall _ -> false | _ -> true)
        "The following changes have been performed"
        ~empty:"No changes have been performed"
        successful;
      err
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
  let external_tags = OpamStd.String.Set.of_list OpamStateConfig.(!r.external_tags) in
  let values =
    OpamPackage.Set.fold (fun nv accu ->
        let opam = OpamState.opam t nv in
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
let apply ?ask t action ~requested solution =
  log "apply";
  if OpamSolver.solution_is_empty solution then
    (* The current state satisfies the request contraints *)
    Nothing_to_do
  else (
    (* Otherwise, compute the actions to perform *)
    let stats = OpamSolver.stats solution in
    let show_solution = ask <> Some false &&
                        OpamStateConfig.(!r.external_tags) = [] in
    let action_graph = OpamSolver.get_atomic_action_graph solution in
    if show_solution then (
      OpamConsole.msg
        "The following actions %s be %s:\n"
        (if OpamStateConfig.(!r.show) then "would" else "will")
        (if OpamStateConfig.(!r.dryrun) then "simulated" else
         if OpamStateConfig.(!r.fake) then "faked"
         else "performed");
      let new_state = simulate_new_state t action_graph in
      let messages p =
        let opam = OpamState.opam new_state p in
        let messages = OpamFile.OPAM.messages opam in
        OpamStd.List.filter_map (fun (s,f) ->
          if OpamFilter.opt_eval_to_bool
              (OpamState.filter_env ~opam new_state) f
          then Some s
          else None
        )  messages in
      let rewrite nv =
        (* mark pinned packages with a star *)
        let n = OpamPackage.name nv in
        if OpamState.is_pinned t n && OpamState.pinned t n = nv then
          OpamPackage.create n
            (OpamPackage.Version.of_string
               (match OpamPackage.version_to_string nv with
                | "~unknown" -> "*" | v -> v ^ "*"))
        else nv
      in
      OpamSolver.print_solution ~messages ~rewrite ~requested solution;
      let total_actions = sum stats in
      if total_actions >= 2 then
        OpamConsole.msg "===== %s =====\n" (OpamSolver.string_of_stats stats);
      match action with
      | Install _ | Upgrade _ | Reinstall _
        when not (OpamCudf.external_solver_available ()) &&
             stats.s_remove + stats.s_downgrade >= max 2 (total_actions / 2)
        ->
        OpamConsole.note
          "This solution may not be optimal. You should probably install an \
           external solver (see \
           http://opam.ocaml.org/doc/Install.html#ExternalSolvers for details)"
      | _ -> ()
    );

    if OpamStateConfig.(!r.external_tags) <> [] then (
      print_external_tags t solution;
      Aborted
    ) else if not OpamStateConfig.(!r.show) &&
              confirmation ?ask requested action_graph
    then (
      print_variable_warnings t;
      parallel_apply t action action_graph
    ) else
      Aborted
  )

let resolve ?(verbose=true) t action ~orphans request =
  if OpamStateConfig.(!r.json_out <> None) then (
    OpamJson.append "opam-version" (`String OpamVersion.(to_string (full ())));
    OpamJson.append "command-line"
      (`A (List.map (fun s -> `String s) (Array.to_list Sys.argv)));
    OpamJson.append "switch" (OpamSwitch.to_json t.switch)
  );
  Json.output_request request action;
  let r =
    OpamSolver.resolve ~verbose (OpamState.universe t action) ~orphans request
  in
  Json.output_solution t r;
  r

let resolve_and_apply ?ask t action ~requested ~orphans request =
  match resolve t action ~orphans request with
  | Conflicts cs ->
    log "conflict!";
    OpamConsole.msg "%s"
      (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
    No_solution
  | Success solution -> apply ?ask t action ~requested solution
