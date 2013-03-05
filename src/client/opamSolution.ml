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
open OpamState.Types
open OpamMisc.OP

let check_solution = function
  | No_solution -> OpamGlobals.exit 3
  | Error _     -> OpamGlobals.exit 4
  | Nothing_to_do
  | OK
  | Aborted     -> ()

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let eq_atom name version =
  name, Some (`Eq, version)

let eq_atoms_of_packages set =
  List.rev_map (fun nv -> eq_atom (OpamPackage.name nv) (OpamPackage.version nv)) (OpamPackage.Set.elements set)

let atom_of_package nv =
  OpamPackage.name nv, None

let atoms_of_packages set =
  List.rev_map atom_of_package (OpamPackage.Set.elements set)

let atom_of_name name =
  name, None

(* transform a name into:
   - <name, installed version> package
   - <$n,$v> package when name = $n.$v *)
let atoms_of_names t names =
  let available = OpamPackage.to_map (Lazy.force t.available_packages) in
  let packages = OpamPackage.to_map t.packages in
  let exists name version =
    OpamPackage.Name.Map.mem name packages
    &&
    match version with
    | None   -> true
    | Some v ->
      let versions = OpamPackage.Name.Map.find name packages in
      OpamPackage.Version.Set.mem v versions in
  let available name version =
    OpamPackage.Name.Map.mem name available
    &&
    match version with
    | None  -> true
    | Some v ->
      let versions = OpamPackage.Name.Map.find name available in
      OpamPackage.Version.Set.mem v versions in
  let unavailable name version =
    if OpamPackage.Name.Map.mem name t.pinned then
      OpamPackage.unavailable_because_pinned name version
    else
      OpamPackage.unavailable name version in
  List.rev_map
    (fun name ->
      if exists name None then
        if available name None then
          atom_of_name name
        else
          unavailable name None
      else (
        (* consider 'name' to be 'name.version' *)
        let nv =
          try OpamPackage.of_string (OpamPackage.Name.to_string name)
          with Not_found -> OpamPackage.unknown name None in
        let sname = OpamPackage.name nv in
        let sversion = OpamPackage.version nv in
        log "The raw name %S not found, looking for package %s version %s"
          (OpamPackage.Name.to_string name)
          (OpamPackage.Name.to_string sname)
          (OpamPackage.Version.to_string sversion);
        if exists sname (Some sversion) then
          if available sname (Some sversion) then
            eq_atom sname sversion
          else
            unavailable sname (Some sversion)
        else
          OpamPackage.unknown sname (Some sversion)
      ))
    (OpamPackage.Name.Set.elements names)

(* Pretty-print errors *)
let display_error (n, error) =
  let f action nv =
    OpamGlobals.error "\n==== ERROR [while %s %s] ====" action (OpamPackage.to_string nv);
    match error with
    | OpamParallel.Process_error r  -> OpamGlobals.error "%s" (OpamProcess.string_of_result r)
    | OpamParallel.Internal_error s -> OpamGlobals.error "Internal error:\n  %s" s in
  match n with
  | To_change (Some o, nv) ->
    if OpamPackage.Version.compare (OpamPackage.version o) (OpamPackage.version nv) < 0 then
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
        OpamGlobals.exit 1;
    );
    variable_warnings := true;
  )

(* Is a recovery possible ? *)
let can_try_to_recover_from_error l =
  List.exists (function (n,_) ->
    match n with
    | To_change(Some _,_) -> true
    | To_recompile _
    | To_change _
    | To_delete _         -> false
    ) l

(* Try to recover from errors by installing either the old packages or
   by reinstalling the current ones. This can also fail but if it
   succeeds OPAM should remains in a consistent state. *)
let recover_from_error = function
  | To_delete _          -> ()
  | To_recompile nv
  | To_change (Some nv, _)
  | To_change (None, nv) ->
    let t = OpamState.load_state "recover-from-error" in
    try OpamAction.build_and_install_package t ~metadata:true nv
    with _ -> ()

(* Transient state (not flushed to disk) *)
type state = {
  mutable s_installed      : package_set;
  mutable s_installed_roots: package_set;
  mutable s_reinstall      : package_set;
}

(* Mean function for applying solver solutions. One main process is
   deleting the packages and updating the global state of
   OPAM. Children processes are spawned to deal with parallele builds
   and installations. *)
let parallel_apply t action solution =
  let open PackageActionGraph in
  let state = {
    s_installed       = t.installed;
    s_installed_roots = t.installed_roots;
    s_reinstall       = t.reinstall;
  } in
  let update_state () =
    let installed       = state.s_installed in
    let installed_roots = state.s_installed_roots in
    let reinstall       = state.s_reinstall in
    OpamAction.update_metadata t ~installed ~installed_roots ~reinstall in

  let root_installs =
    let names =
      List.rev_map OpamPackage.name (OpamPackage.Set.elements t.installed_roots) in
    OpamPackage.Name.Set.of_list names in
  let root_installs = match action with
    | Init r
    | Install r
    | Import r
    | Switch r  -> OpamPackage.Name.Set.union root_installs r
    | Upgrade _ -> root_installs
    | _ -> OpamPackage.Name.Set.empty in

  (* flush the contents of installed and installed.root to disk. This
     should be called as often as possible to keep the global state of
     OPAM consistent (as the user is free to kill OPAM at every
     moment). We are not guaranteed to always be consistent, but we
     try hard to be.*)
  let add_to_install nv =
    state.s_installed <- OpamPackage.Set.add nv state.s_installed;
    state.s_reinstall <- OpamPackage.Set.remove nv state.s_reinstall;
    if OpamPackage.Name.Set.mem (OpamPackage.name nv) root_installs then
      state.s_installed_roots <- OpamPackage.Set.add nv state.s_installed_roots;
    update_state () in

  let remove_from_install deleted =
    state.s_installed       <- OpamPackage.Set.diff state.s_installed deleted;
    state.s_installed_roots <- OpamPackage.Set.diff state.s_installed_roots deleted;
    state.s_reinstall       <- OpamPackage.Set.diff state.s_reinstall deleted in

  (* Installation and recompilation are done by child the processes *)
  let child n =
    (* We are guaranteed to load the state when all the dependencies
       have been correctly updated. Thus [t.installed] should be
       up-to-date.
       XXX: do we really need to load the state again here ? *)
    let t = OpamState.load_state "child" in
    match n with
    | To_change (_, nv)
    | To_recompile nv   -> OpamAction.build_and_install_package ~metadata:false t nv
    | To_delete _       -> assert false in

  (* Not pre-condition (yet ?) *)
  let pre _ = () in

  (* Post-condition on the parent process: we modify of the global
     OPAM state to keep the list of installed packages up-to-date. *)
  let post = function
    | To_delete _       -> assert false
    | To_recompile nv
    | To_change (_, nv) -> add_to_install nv in

  try
    let jobs = OpamFile.Config.jobs t.config in
    (* 1/ We remove all installed packages appearing in the solution. *)
    let deleted = OpamAction.remove_all_packages t ~metadata:true solution in
    remove_from_install deleted;

    (* 2/ We install the new packages *)
    PackageActionGraph.Parallel.parallel_iter jobs solution.to_process ~pre ~child ~post;
    if !OpamGlobals.fake then
      OpamGlobals.msg "Simulation complete.\n";

    OK
  with
  | PackageActionGraph.Parallel.Cyclic actions ->
    let packages = List.map (List.map action_contents) actions in
    let strings = List.map (List.map OpamPackage.to_string) packages in
    let mk l = Printf.sprintf " - %s" (String.concat ", " l) in
    OpamGlobals.error
      "Aborting, as the following packages have a cyclic dependency:\n%s"
      (String.concat "\n" (List.map mk strings));
    Aborted
  | PackageActionGraph.Parallel.Errors (errors, remaining) ->
    OpamGlobals.msg "\n";
    if remaining <> [] then (
      OpamGlobals.error
        "Due to some errors while processing %s, the following actions will NOT be proceeded:"
        (string_of_errors errors);
      List.iter (fun n -> OpamGlobals.error "%s" (PackageAction.string_of_action n)) remaining;
    );
    if can_try_to_recover_from_error errors then (
      let pkgs = List.map (fst |> action_contents |> OpamPackage.to_string) errors in
      OpamGlobals.msg "==== ERROR RECOVERY [%s] ====\n" (String.concat ", " pkgs);
      List.iter recover_from_error (List.map fst errors);
      List.iter recover_from_error remaining;
    );
    List.iter display_error errors;
    Error (List.map fst errors @ remaining)

(* Apply a solution *)
let apply ?(force = false) t action solution =
  if OpamSolver.solution_is_empty solution then
    (* The current state satisfies the request contraints *)
    Nothing_to_do
  else (
    (* Otherwise, compute the actions to perform *)
    let stats = OpamSolver.stats solution in
    let show_solution = (!OpamGlobals.external_tags = []) in
    if show_solution then (
      OpamGlobals.msg
        "The following actions will be %s:\n"
        (if !OpamGlobals.fake then "simulated" else "performed");
      OpamSolver.print_solution solution;
      OpamGlobals.msg "%s\n" (OpamSolver.string_of_stats stats)
    );

    let continue =
      if !OpamGlobals.dryrun then (
        OpamGlobals.msg "Dry run: exiting now.\n";
        false
      ) else if !OpamGlobals.external_tags <> [] then (
        let packages = OpamSolver.new_packages solution in
        let external_tags = OpamMisc.StringSet.of_list !OpamGlobals.external_tags in
        let values =
          OpamPackage.Set.fold (fun nv accu ->
            let opam = OpamPackage.Map.find nv t.opams in
            match OpamFile.OPAM.depexts opam with
            | None         -> accu
            | Some alltags ->
              OpamMisc.StringSetMap.fold (fun tags values accu ->
                if OpamMisc.StringSet.(
                    (* A \subseteq B <=> (A U B) / B = 0 *)
                    is_empty (diff (union external_tags tags) tags)
                  )
                then
                  OpamMisc.StringSet.union values accu
                else
                  accu
              ) alltags accu
          ) packages OpamMisc.StringSet.empty in
        let values = OpamMisc.StringSet.elements values in
        if values <> [] then
          OpamGlobals.msg "%s\n" (String.concat " " values);
        false
      ) else if force || !OpamGlobals.fake || !OpamGlobals.yes || sum stats <= 1 then
        true
      else
        OpamState.confirm "Do you want to continue ?" in

    if continue then (
      print_variable_warnings t;
      parallel_apply t action solution
    ) else
      Aborted
  )

let resolve ?(verbose=true) t action request =
  OpamSolver.resolve ~verbose (OpamState.universe t action) request

let resolve_and_apply ?(force=false) t action request =
  match resolve t action request with
  | Conflicts cs ->
    OpamGlobals.msg "%s\n" (cs ());
    No_solution
  | Success solution -> apply ~force t action solution
