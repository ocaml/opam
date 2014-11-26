(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

let log fmt = OpamGlobals.log "SOLUTION" fmt

open OpamTypes
open OpamTypesBase
open OpamState.Types
open OpamProcess.Job.Op

module PackageAction = OpamSolver.Action
module PackageActionGraph = OpamSolver.ActionGraph

let post_message ?(failed=false) state action =
  match action with
  | To_delete _ | To_recompile _ -> ()
  | To_change (_,pkg) ->
    let opam = OpamState.opam state pkg in
    let messages = OpamFile.OPAM.post_messages opam in
    let local_variables = OpamVariable.Map.empty in
    let local_variables =
      OpamVariable.Map.add (OpamVariable.of_string "success")
        (B (not failed)) local_variables
    in
    let local_variables =
      OpamVariable.Map.add (OpamVariable.of_string "failure")
        (B failed) local_variables
    in
    let messages =
      OpamMisc.filter_map (fun (message,filter) ->
          if OpamState.eval_filter state ~opam local_variables filter then
            Some (OpamState.substitute_string
                    state ~opam local_variables message)
          else None)
        messages
    in
    if messages = [] then () else
    let mark = "=> " in
    let indent = String.make (String.length mark) ' ' in
    let mark = OpamGlobals.colorise (if failed then `red else `green) mark in
    OpamGlobals.header_msg "%s %s"
      (OpamPackage.to_string pkg)
      (if failed then "troubleshooting" else "installed successfully");
    let rex = Re_pcre.regexp "\n" in
    List.iter (fun msg ->
        OpamGlobals.msg "%s%s\n" mark
          (Re_pcre.substitute ~rex ~subst:(fun s -> s^indent) msg))
      messages

let check_solution state = function
  | No_solution ->
    OpamGlobals.msg "No solution found, exiting\n";
    OpamGlobals.exit 3
  | Error (success, failed, _remaining) ->
    List.iter (post_message state) success;
    List.iter (post_message ~failed:true state) failed;
    OpamGlobals.exit 4
  | OK actions ->
    List.iter (post_message state) actions
  | Nothing_to_do -> ()
  | Aborted     -> OpamGlobals.exit 0

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let eq_atom name version =
  name, Some (`Eq, version)

let eq_atoms_of_packages set =
  List.rev_map (fun nv -> eq_atom (OpamPackage.name nv) (OpamPackage.version nv)) (OpamPackage.Set.elements set)

let atom_of_package nv =
  OpamPackage.name nv, None

let atoms_of_packages set =
  List.rev_map (fun n -> n, None)
    (OpamPackage.Name.Set.elements (OpamPackage.names_of_packages set))

let atom_of_name name =
  name, None

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
  let errors = OpamMisc.filter_map check_atom atoms in
  if errors <> [] then
    (List.iter (OpamGlobals.error "%s") errors;
     OpamGlobals.exit 66)

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
      OpamGlobals.header_error "while %s %s" action (OpamPackage.to_string nv) in
    match error with
    | Sys.Break -> ()
    | e -> disp "%s" (Printexc.to_string e)
  in
  match n with
  | To_change (Some o, nv) ->
    if
      OpamPackage.Version.compare
        (OpamPackage.version o) (OpamPackage.version nv) < 0
    then
      f "upgrading to" nv
    else
      f "downgrading to" nv
  | To_change (None, nv) -> f "installing" nv
  | To_recompile nv      -> f "recompiling" nv
  | To_delete nv         -> f "removing" nv

(* Prettify errors *)
let string_of_errors errors =
  let actions = List.rev_map fst errors in
  let packages = List.rev_map action_contents actions in
  match packages with
  | []  -> assert false
  | [h] -> OpamPackage.to_string h
  | l   -> OpamPackage.Set.to_string (OpamPackage.Set.of_list l)


let new_variables e =
  let e = List.filter (fun (_,s,_) -> s="=") e in
  let e = List.rev_map (fun (v,_,_) -> v) e in
  OpamMisc.StringSet.of_list e

let variable_warnings = ref false
let print_variable_warnings t =
  let variables = ref [] in
  if not !variable_warnings then (
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
    if OpamPackage.Set.exists (
        fun nv -> OpamPackage.Name.to_string (OpamPackage.name nv) = "ocamlfind"
      ) t.installed then
      List.iter warn ocamlfind_vars;
    (* 2. Warn about variables possibly set by other compilers *)
    let new_variables comp =
      let comp_f = OpamPath.compiler_comp t.root comp in
      let env = OpamFile.Comp.env (OpamFile.Comp.safe_read comp_f) in
      new_variables env in
    let vars = ref OpamMisc.StringSet.empty in
    OpamSwitch.Map.iter (fun _ comp ->
      vars := OpamMisc.StringSet.union !vars (new_variables comp)
    ) t.aliases;
    vars := OpamMisc.StringSet.diff !vars (new_variables t.compiler);
    OpamMisc.StringSet.iter warn !vars;
    if !variables <> [] then (
      OpamGlobals.msg "The following variables are set in your environment, it \
                       is advised to unset them for OPAM to work correctly.\n";
      List.iter (OpamGlobals.msg " - %s\n") !variables;
      if not (OpamGlobals.confirm "Do you want to continue ?") then
        OpamGlobals.exit 1;
    );
    variable_warnings := true;
  )

(* Transient state (not flushed to disk) *)
type state = {
  mutable s_installed      : package_set;
  mutable s_installed_roots: package_set;
  mutable s_reinstall      : package_set;
}

let output_json_solution solution =
  let to_proceed =
    PackageActionGraph.Topological.fold (fun a to_proceed ->
        match a with
        | To_change(o,p)  ->
          let json = match o with
            | None   -> `O ["install", OpamPackage.to_json p]
            | Some o ->
              if OpamPackage.Version.compare
                  (OpamPackage.version o) (OpamPackage.version p) < 0
              then
                `O ["upgrade", `A [OpamPackage.to_json o; OpamPackage.to_json p]]
              else
                `O ["downgrade", `A [OpamPackage.to_json o; OpamPackage.to_json p]] in
          json :: to_proceed
        | To_recompile p ->
          let json = `O ["recompile", OpamPackage.to_json p] in
          json :: to_proceed
        | To_delete p    ->
          let json = `O ["delete", OpamPackage.to_json p] in
          json :: to_proceed
      ) solution []
  in
  OpamJson.add (`A to_proceed)

let output_json_actions action_errors =
  let json_error = function
    | OpamSystem.Process_error
        {OpamProcess.r_code; r_duration; r_info; r_stdout; r_stderr} ->
      `O [ ("process-error",
            `O [ ("code", `String (string_of_int r_code));
                 ("duration", `Float r_duration);
                 ("info", `O (List.map (fun (k,v) -> (k, `String v)) r_info));
                 ("stdout", `A (List.map (fun s -> `String s) r_stdout));
                 ("stderr", `A (List.map (fun s -> `String s) r_stderr));
               ])]
    | OpamSystem.Internal_error s ->
      `O [ ("internal-error", `String s) ]
    | OpamGlobals.Package_error s ->
      `O [ ("package-error", `String s) ]
    | e -> `O [ ("exception", `String (Printexc.to_string e)) ]
  in
  let json_action (a, e) =
    `O [ ("package", `String (OpamPackage.to_string (action_contents a)));
         ("error"  ,  json_error e) ] in
  List.iter (fun a ->
      let json = json_action a in
      OpamJson.add json
    ) action_errors

(* Process the atomic actions in a graph in parallel, respecting graph order,
   and report to user *)
let parallel_apply t action action_graph =
  log "parallel_apply";

  (* We keep an imperative state up-to-date and flush it to disk as soon
     as an operation terminates *)
  let state = {
    s_installed       = t.installed;
    s_installed_roots = t.installed_roots;
    s_reinstall       = t.reinstall;
  } in
  let t_ref = ref t in
  let update_state () =
    let installed       = state.s_installed in
    let installed_roots = state.s_installed_roots in
    let reinstall       = state.s_reinstall in
    t_ref :=
      OpamAction.update_metadata t ~installed ~installed_roots ~reinstall
  in

  let root_installs =
    let names = OpamPackage.names_of_packages t.installed_roots in
    match action with
    | Init r | Install r | Import r | Switch r  ->
      OpamPackage.Name.Set.union names r
    | Upgrade _ | Reinstall _ -> names
    | Depends | Remove -> OpamPackage.Name.Set.empty
  in

  let add_to_install nv =
    state.s_installed <- OpamPackage.Set.add nv state.s_installed;
    state.s_reinstall <- OpamPackage.Set.remove nv state.s_reinstall;
    if OpamPackage.Name.Set.mem (OpamPackage.name nv) root_installs then
      state.s_installed_roots <- OpamPackage.Set.add nv state.s_installed_roots;
    update_state ();
    if not !OpamGlobals.dryrun then OpamState.install_metadata !t_ref nv
  in

  let remove_from_install deleted =
    let rm = OpamPackage.Set.remove deleted in
    state.s_installed       <- rm state.s_installed;
    state.s_installed_roots <- rm state.s_installed_roots;
    state.s_reinstall       <- rm state.s_reinstall;
    update_state () in

  let cancelled_exn = Failure "cancelled" in

  (* 1/ fetch needed package archives *)

  let _package_sources, failed_downloads =
    let sources_needed = OpamAction.sources_needed t action_graph in
    (* preload http repos indexes *)
    let repos =
      OpamPackage.Set.fold (fun nv repos ->
          try
            let repo_name = fst (OpamPackage.Map.find nv t.package_index) in
            OpamRepositoryName.Set.add repo_name repos
          with Not_found -> repos)
        sources_needed OpamRepositoryName.Set.empty
    in
    OpamRepositoryName.Set.iter (fun repo_name ->
        let repo = OpamState.find_repository t repo_name in
        if repo.repo_kind = `http then OpamHTTP.preload_state repo)
      repos;
    let sources_list = OpamPackage.Set.elements sources_needed in
    if sources_list <> [] then
      OpamGlobals.header_msg "Gathering package archives";
    let results =
      OpamParallel.map
        ~jobs:(OpamState.dl_jobs t)
        ~command:(OpamAction.download_package t)
        sources_list
    in
    List.fold_left2 (fun (sources,failed) nv -> function
        | `Successful None -> sources, failed
        | `Successful (Some dl) -> OpamPackage.Map.add nv dl sources, failed
        | _ -> sources, OpamPackage.Set.add nv failed)
      (OpamPackage.Map.empty,OpamPackage.Set.empty) sources_list results
  in

  let fatal_dl_error =
    PackageActionGraph.fold_vertex
      (fun a acc -> acc || match a with
         | To_delete _ -> false
         | _ -> OpamPackage.Set.mem (action_contents a) failed_downloads)
      action_graph false
  in
  if fatal_dl_error then
    OpamGlobals.error_and_exit
      "The sources of the following couldn't be obtained, aborting:\n  - %s\n\
       (This may be fixed by running 'opam update')"
      (String.concat "\n  - " (List.map OpamPackage.to_string
                                 (OpamPackage.Set.elements failed_downloads)))
  else if not (OpamPackage.Set.is_empty failed_downloads) then
    OpamGlobals.warning
      "The sources of the following couldn't be obtained, they may be \
       uncleanly uninstalled:\n  - %s\n"
      (String.concat "\n  - "
         (List.map OpamPackage.to_string
            (OpamPackage.Set.elements failed_downloads)));


  (* 2/ process the package actions (installations and removals) *)

  (* the child job to run on each action *)
  let job ~pred action =
    if not (List.for_all (fun (_,r) -> r = None) pred) then
      Done (Some cancelled_exn)
    else
    let t = !t_ref in
    match action with
    (* todo: use package_sources rather than guess again *)
    | To_change (_, nv) | To_recompile nv ->
      (OpamAction.build_and_install_package ~metadata:false t nv
       @@+ function
       | None ->  add_to_install nv; Done None
       | Some exn -> Done (Some exn))
    | To_delete nv ->
      if OpamAction.removal_needs_download t nv then
        (try OpamAction.extract_package t nv with e -> OpamMisc.fatal e);
      OpamProcess.Job.catch (fun e -> OpamMisc.fatal e; Done ())
         (OpamAction.remove_package t ~metadata:false nv) @@| fun () ->
      remove_from_install nv;
      None
  in

  let action_results =
    OpamGlobals.header_msg "Processing package actions";
    try
      let results =
        PackageActionGraph.Parallel.map
          ~jobs:(OpamState.jobs t)
          ~command:job
          action_graph
      in
      let successful, failed =
        List.partition (fun (_,r) -> r = None) results
      in
      match failed with
      | [] -> `Successful ()
      | _::_ ->
        let failed =
          List.map (function (act,Some e) -> act, e | _ -> assert false) failed
        in
        let cancelled, failed =
          List.partition (fun (_,e) -> e = cancelled_exn) failed
        in
        let act r = List.map fst r in
        List.iter display_error failed;
        output_json_actions failed;
        `Error (Error (act successful, act failed, act cancelled))
    with
    | PackageActionGraph.Parallel.Errors (successful, errors, remaining) ->
      List.iter display_error errors;
      output_json_actions errors;
      `Error (Error (successful, List.map fst errors, remaining))
    | e -> `Exception e
  in
  let t = !t_ref in

  (* 3/ Display errors and finalize *)

  let cleanup_artefacts graph =
    PackageActionGraph.iter_vertex (function
        | To_delete nv | To_change (Some nv, _)
          when not (OpamState.is_pinned t (OpamPackage.name nv)) ->
          OpamAction.cleanup_package_artefacts t nv (* no-op if reinstalled *)
        | _ -> ())
      graph
  in
  match action_results with
  | `Successful () ->
    cleanup_artefacts action_graph;
    OK (PackageActionGraph.fold_vertex (fun a b -> a::b) action_graph [])
  | `Exception (OpamGlobals.Exit _ | Sys.Break as e) ->
    OpamGlobals.msg "Aborting";
    raise e
  | `Exception e ->
    OpamGlobals.error "Actions cancelled because of %s" (Printexc.to_string e);
    raise e
  | `Error err ->
    match err with
    | Aborted -> err
    | Error (successful, failed, _remaining) ->
      let filter_graph g l =
        if l = [] then PackageActionGraph.create () else
        let g = PackageActionGraph.copy g in
        PackageActionGraph.iter_vertex (fun v ->
            if not (List.mem v l) then PackageActionGraph.remove_vertex g v)
          g;
        PackageActionGraph.reduce g
      in
      let successful = filter_graph action_graph successful in
      cleanup_artefacts successful;
      let failed = filter_graph action_graph failed in
      if PackageActionGraph.(nb_vertex successful + nb_vertex failed) <= 1
      then err else
      let print_actions oc actions =
        let actions =
          PackageActionGraph.Topological.fold (fun v acc -> v::acc) actions []
        in
        List.iter (Printf.fprintf oc "  %s\n")
          (PackageAction.to_aligned_strings actions) in
      OpamGlobals.msg "\n";
      OpamGlobals.header_msg "Error report";
      if not (PackageActionGraph.is_empty failed) then
        OpamGlobals.msg
          "The following %s\n%a"
          (OpamGlobals.colorise `bold "failed")
          print_actions failed;
      if not (PackageActionGraph.is_empty successful) then
        OpamGlobals.msg
          "These actions have been %s\n%a"
          (OpamGlobals.colorise `bold "completed")
          print_actions successful;
      err
    | _ -> assert false

let simulate_new_state state t =
  let installed =
    OpamSolver.ActionGraph.Topological.fold
      (fun action installed ->
        match action with
        | To_change(_,p) | To_recompile p ->
          OpamPackage.Set.add p installed
        | To_delete p ->
          OpamPackage.Set.remove p installed
      )
      t state.installed in
  { state with installed }

let print_external_tags t solution =
  let packages = OpamSolver.new_packages solution in
  let external_tags = OpamMisc.StringSet.of_list !OpamGlobals.external_tags in
  let values =
    OpamPackage.Set.fold (fun nv accu ->
        let opam = OpamState.opam t nv in
        match OpamFile.OPAM.depexts opam with
        | None         -> accu
        | Some alltags ->
          OpamMisc.StringSetMap.fold (fun tags values accu ->
              if OpamMisc.StringSet.(
                  (* A \subseteq B <=> (A U B) / B = 0 *)
                  is_empty (diff (union external_tags tags) external_tags)
                )
              then
                OpamMisc.StringSet.union values accu
              else
                accu
            ) alltags accu
      ) packages OpamMisc.StringSet.empty in
  let values = OpamMisc.StringSet.elements values in
  if values <> [] then
    OpamGlobals.msg "%s\n" (String.concat " " values)

(* Ask confirmation whenever the packages to modify are not exactly
   the packages in the user request *)
let confirmation ?ask requested solution =
  !OpamGlobals.yes ||
  match ask with
  | Some false -> true
  | Some true -> OpamGlobals.confirm "Do you want to continue ?"
  | None ->
    let open PackageActionGraph in
    let solution_packages =
      fold_vertex (fun v acc ->
          OpamPackage.Name.Set.add (OpamPackage.name (action_contents v)) acc)
        solution
        OpamPackage.Name.Set.empty in
    OpamPackage.Name.Set.equal requested solution_packages
    || OpamGlobals.confirm "Do you want to continue ?"

(* Apply a solution *)
let apply ?ask t action ~requested solution =
  log "apply";
  if OpamSolver.solution_is_empty solution then
    (* The current state satisfies the request contraints *)
    Nothing_to_do
  else (
    (* Otherwise, compute the actions to perform *)
    let stats = OpamSolver.stats solution in
    let show_solution = (!OpamGlobals.external_tags = []) in
    let action_graph = OpamSolver.get_atomic_action_graph solution in
    if show_solution then (
      OpamGlobals.msg
        "The following actions %s be %s:\n"
        (if !OpamGlobals.show then "would" else "will")
        (if !OpamGlobals.fake then "simulated" else "performed");
      let new_state = simulate_new_state t action_graph in
      let messages p =
        let opam = OpamState.opam new_state p in
        let messages = OpamFile.OPAM.messages opam in
        OpamMisc.filter_map (fun (s,f) ->
          if OpamState.eval_filter new_state ~opam OpamVariable.Map.empty f
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
      if sum stats >= 2 then
        OpamGlobals.msg "=== %s ===\n" (OpamSolver.string_of_stats stats);
      output_json_solution action_graph;
    );

    if !OpamGlobals.external_tags <> [] then (
      print_external_tags t solution;
      Aborted
    ) else if not !OpamGlobals.show &&
              confirmation ?ask requested action_graph
    then (
      print_variable_warnings t;
      parallel_apply t action action_graph
    ) else
      Aborted
  )

let resolve ?(verbose=true) t action ~orphans request =
  OpamSolver.resolve ~verbose (OpamState.universe t action) ~orphans request

let resolve_and_apply ?ask t action ~requested ~orphans request =
  match resolve t action ~orphans request with
  | Conflicts cs ->
    log "conflict!";
    OpamGlobals.msg "%s"
      (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
    No_solution
  | Success solution -> apply ?ask t action ~requested solution
