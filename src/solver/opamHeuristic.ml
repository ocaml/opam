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
open OpamStd.Op

let log fmt = OpamConsole.log "HEURISTIC" fmt
let slog = OpamConsole.slog

type 'a state = 'a list
type 'a state_space = 'a array list

(* Forget about the changes which are not related to the packages we
   are interested in. We don't have yet computed the transitive
   closure of dependencies: we are processing 'raw' actions which come
   directly from the solver. It shoud be safe to discard
   install/upgrade action outside of the interesting names; delete
   actions are using a different code-path, they should not appear
   here. *)
let minimize_actions interesting_names actions =
  let interesting_names = OpamStd.String.Set.of_list interesting_names in
  List.filter (function
    | `Install p | `Change (_, _, p) | `Recompile p ->
      OpamStd.String.Set.mem p.Cudf.package interesting_names
    | `Remove _ -> true
  ) actions

(* A list of [n] zero. *)
let zero n =
  let rec aux acc n =
    if n > 0 then
      aux (0 :: acc) (n-1)
    else
      acc in
  aux [] n

(* Given a list of bounds, create the least tuple such that the sum of
   components is equal to n.  For instance: init [1;2;1] 3 is
   [0;2;1] *)
let init ~bounds n =
  let rec aux = function
    | 0, []   -> Some []
    | 0, l    -> Some (zero (List.length l))
    | _, []   -> None
    | n, b::t ->
      if n <= b then
        Some (n :: zero (List.length t))
      else match aux (n-b, t) with
        | None   -> None
        | Some l -> Some (b::l) in
  match aux (n, List.rev bounds) with
  | None   -> None
  | Some l -> Some (List.rev l)

(* Given a list of bounds and a tuple, return the next tuple while
   keeping the sum of components of the tuple constant *)
let rec cst_succ ~bounds k l =
  match l, bounds with
  | [] , []  -> None
  | [n], [b] ->
    if n+1 = k && n < b then
      Some [k]
    else
      None
  | n::nt, b::bt ->
    if n >= k then
      None
    else (
      match cst_succ ~bounds:bt (k-n) nt with
      | Some s -> Some (n::s)
      | None   ->
        if n < b then
          match init ~bounds:bt (k-n-1) with
          | None   -> None
          | Some l -> Some (n+1 :: l)
        else
          None)
  | _ ->
    failwith "Bounds and tuple do not have the same size"

(* Given a list of bounds and a tuple, return the next tuple *)
let succ ~bounds l =
  let k = List.fold_left (+) 0 l in
  match cst_succ ~bounds k l with
  | Some t -> Some t
  | None   ->
    let k = List.fold_left (+) 0 l in
    init ~bounds (k+1)

let fallback_msg =
  "You might need to add explicit version constraints to your \
   request to get a better answer.\n"

(* Brute-force exploration of a given space-state:

   [is_consistent] is applied on each possible state of the system,
   where a state is where each pacakge has a fix version. We ensure
   that we apply [is_consistent] in increasing order regarding the
   difference between the maximum version and the current version for
   each package. That is, we apply [is_consistent] first on the state
   where all packages have the maximum version, then on all the states
   where all packages have their maximum version but one which has its
   second maximal version, etc... *)
let brute_force ?(verbose=true) ~dump is_consistent state_space =
  log "brute-force";

  let bounds = List.map (fun v -> Array.length v - 1) state_space in
  List.iter (fun v -> assert (v >= 0)) bounds;
  let mk_state t =
    List.map2 (fun vs i -> vs.(i)) state_space t in
  let t0 = Unix.time () in
  let count = ref 0 in
  let interval = 500 in
  let flush_output () =
    if verbose && !count >= interval then
      OpamConsole.msg
        " an optimal solution has been found after exploring %d states.\n"
        !count in

  (* XXX: need to ensure this is properly transformed into a
     while-loop. *)
  let rec aux = function
    | None ->
      log "no better solution found";
      flush_output ();
      None
    | Some t ->
      let state = mk_state t in
      dump state;
      incr count;
      let t1 = Unix.time () in
      if verbose && !count mod interval = interval - 1 then
        OpamConsole.msg ".";
      if t1 -. t0 > OpamSolverConfig.(!r.solver_timeout) then (
        OpamConsole.msg
          "The brute-force exploration algorithm timed-out [%d states, %.2gs].\n%s\n"
          !count OpamSolverConfig.(!r.solver_timeout) fallback_msg;
        None
      ) else
      if is_consistent state then
        Some state
      else
        aux (succ ~bounds t) in

  aux (init ~bounds 0)

(* Call the solver to check whether a set of packages is
   installable. *)
let consistent_packages universe packages =
  match Algo.Depsolver.edos_coinstall universe packages with
  | { Algo.Diagnostic.result = Algo.Diagnostic.Success _; _ } -> true
  | { Algo.Diagnostic.result = Algo.Diagnostic.Failure _; _ } -> false

let dump_state =
  if (OpamConsole.debug ()) && OpamCoreConfig.(!r.debug_level) > 3 then
    log "dump-state: %a"
      (slog (String.concat ", "
             @* (List.map (OpamPackage.to_string @* OpamCudf.cudf2opam))))
  else
      (fun _ -> ())

(* Explore a given [state_space] to find the optimal solution. Ideally
   the state space should be as small as possible, eg. we rely on
   previous heuristics to reduce its size. *)
let explore ?(verbose=true) universe state_space =
  log "explore";
  let packages_of_state state =
    let filter p =
      List.exists (fun s ->
        s.Cudf.package = p.Cudf.package
        && s.Cudf.version = p.Cudf.version
      ) state
    in
    Cudf.get_packages ~filter universe in
  let is_consistent state =
    let packages = packages_of_state state in
    consistent_packages universe packages in
  brute_force ~verbose ~dump:dump_state is_consistent state_space

(* Build a solution from a given space-state. If a package appears
   in the state, the solution has the package installed with the
   given version. If a package does not appear in the state, but is
   installed in the given universe, its version stays the same.
   Otherwise, if a package appears neither in the state nor is
   installed, it will not appear in the resulting solution. *)
exception Not_reachable of OpamCudf.conflict

let satisfy pkg constrs =
  List.exists (fun (n, v) ->
      n = pkg.Cudf.package && Cudf.version_matches pkg.Cudf.version v
    ) constrs

let actions_of_state ~version_map universe request state =
  log "actions_of_state %a" (slog OpamCudf.string_of_packages) state;
  let installed =
    let filter p =
      p.Cudf.installed
      && List.for_all (fun s -> s.Cudf.package <> p.Cudf.package) state
      && not (satisfy p request.wish_remove) in
    let packages = Cudf.get_packages ~filter universe in
    List.rev_map (fun p -> p.Cudf.package, Some (`Eq, p.Cudf.version)) packages in
  let small_universe =
    let filter p =
      p.Cudf.installed
      || List.exists (fun s -> s.Cudf.package = p.Cudf.package) state in
    let packages = Cudf.get_packages ~filter universe in
    Cudf.load_universe packages in
  let state =
    List.map (fun p -> p.Cudf.package, Some (`Eq, p.Cudf.version)) state in
  let request = { request with
    wish_install = [];
    wish_upgrade = state @ installed
  } in
  match OpamCudf.check_request ~version_map small_universe request with
  | Conflicts c ->
    log "not reachable! universe=%a request=%a"
      (slog OpamCudf.string_of_universe) small_universe
      (slog OpamCudf.string_of_request) request;
    raise (Not_reachable c)
  | Success u   ->
    try
      let diff = OpamCudf.diff universe u in
      let actions = OpamCudf.actions_of_diff diff in
      let actions = minimize_actions (List.map fst state) actions in
      actions
    with Cudf.Constraint_violation s ->
      OpamConsole.error_and_exit "constraint violations: %s" s

(* Find dependencies and installed & reverse dependencies. *)
let find_interesting_names universe request =
  let filter pkg =
    satisfy pkg request.wish_upgrade && not (satisfy pkg request.wish_remove) in
  let packages = Cudf.get_packages ~filter universe in
  let depends = OpamCudf.dependencies universe packages in
  let revdepends =
    let revdepends = OpamCudf.reverse_dependencies universe packages in
    let filter pkg = pkg.Cudf.installed && filter pkg in
    List.filter filter revdepends in
  let set = ref OpamStd.String.Set.empty in
  let add p = set := OpamStd.String.Set.add p.Cudf.package !set in
  List.iter add depends;
  List.iter add revdepends;
  OpamStd.String.Set.elements !set

(* [state_space] returns the packages which will be tested by the
   brute-force state explorer. As we try to minimize the state to
   explore for each package, this means:

   - if the package has a version constraint in the request, that's
   the only we consider (eg. return only one element for this package
   name: the version in the constraint)

   - if the package has no version constraints in the request, or if
   the package does not appear in the initial request, then return
   only the versions greater or equals to the one appearing in the
   given universe.

   - if the package appears in the 'wish_remove' field we do not try
   to test it.
*)
let state_space ?(filters = fun _ -> None) universe wish_remove interesting_names =

  let universe_packages = Cudf.get_packages universe in

  (* Return the version associated to a given package in the
     universe. *)
  let installed_version_of_name =
    let tbl = Hashtbl.create 1024 in
    List.iter
      (fun p ->
        if p.Cudf.installed then
          Hashtbl.add tbl p.Cudf.package p.Cudf.version)
      universe_packages;
    function name ->
      try Some (Hashtbl.find tbl name)
      with Not_found -> None in

  let state_space = Hashtbl.create 1024 in
  let add_state name =
    if not (Hashtbl.mem state_space name) then
      let filter = filters name in
      let packages = Cudf.lookup_packages universe ~filter name in
      let packages =
        match installed_version_of_name name with
        | None   -> packages
        | Some v -> List.filter (fun p -> p.Cudf.version >= v) packages in
      let packages =
        List.sort (fun p1 p2 -> compare p2.Cudf.version p1.Cudf.version) packages in
      let packages = List.filter (fun p -> not (satisfy p wish_remove)) packages in
      if List.length packages <> 0 then
        Hashtbl.add state_space name (Array.of_list packages) in

  List.iter add_state interesting_names;
  Hashtbl.fold (fun _ states acc -> states :: acc) state_space []

(* Find a possible good state which satisfies a request. The idea is
   call iteratively this function while refining the constraints of
   the request until reaching a fix-point. *)
let state_of_request ?(verbose=true) ~version_map current_universe request =
  log "state_of_request";

  match OpamCudf.check_request ~explain:false  ~version_map current_universe request with
  | Conflicts _             ->
    log "state-of-request: %a CONFLICT!"
      (slog OpamCudf.string_of_request) request;
    None
  | Success result_universe ->

    (* This first [result_universe] is a consistent solution which
       contains only installed packages fulfilling the initial
       constraints. It is thus not a complete universe and it is not
       guaranteed to be optimal. So we extend the result with all the
       existing packages. *)
    let result_universe =
      let installed = Cudf.get_packages result_universe in
      let current_universe = OpamCudf.uninstall_all current_universe in
      List.fold_left OpamCudf.install current_universe installed in

    let all_wishes = request.wish_install @ request.wish_upgrade in

    let filters name =
      try List.assoc name all_wishes
      with Not_found ->
        log "state-of-request: %s NOT FOUND!" name;
        None in

    let state_space =
      let names = List.map (fun (n,_) -> n) request.wish_upgrade in
      state_space ~filters result_universe request.wish_remove names in
    explore ~verbose current_universe state_space

(* Unused ?
let same_state s1 s2 =
  let sort l =
    let name p = p.Cudf.package, p.Cudf.version in
    let cmp p1 p2 = compare (name p1) (name p2) in
    List.sort cmp l in
  match s1 with
  | None    -> false
  | Some s1 ->
    List.length s1 = List.length s2
    && sort s1 = sort s2
*)

(* Refine a request with state constraints. *)
let refine state request =
  log "refine request:%a state:%a"
    (slog OpamCudf.string_of_request) request
    (slog OpamCudf.string_of_packages) state;
  let wish_upgrade =
    List.rev_map (fun p -> (p.Cudf.package, Some (`Eq, p.Cudf.version))) state in
  let wish_install =
    let names =
      OpamStd.String.Set.(
        union
          (of_list (List.rev_map fst request.wish_install))
          (of_list (List.rev_map fst request.wish_upgrade))
      ) in
    let set =
      OpamStd.String.Set.filter
        (fun n -> not (List.mem_assoc n wish_upgrade))
        names in
    List.map (fun n -> (n, None)) (OpamStd.String.Set.elements set) in
  { request with wish_install; wish_upgrade }

(* Add a package name to the upgrade list. *)
let add_to_upgrade request name =
  { request with wish_upgrade = (name, None) :: request.wish_upgrade }

(* Compute the 'implicit' packages, ie. the ones which do not appear
   in the request but which are in the transitive closure of
   dependencies, and split them in two categories: already installed
   (which will be kept as much as possible with the same version)
   and not installed (which will be installed to the most recent
   valid version) if they are needed. *)
let implicits universe request =

  let interesting_names = find_interesting_names universe request in

  let implicit_installed, implicit_not_installed =
    let implicits =
      let request_names =
        OpamStd.String.Set.of_list (List.map fst request.wish_upgrade) in
      let all_names = OpamStd.String.Set.of_list interesting_names in
      OpamStd.String.Set.diff all_names request_names in
    let installed =
      let filter p =
        p.Cudf.installed
        && OpamStd.String.Set.mem p.Cudf.package implicits in
      Cudf.get_packages ~filter universe in
    let not_installed =
      let filter n =
        List.for_all (fun p -> p.Cudf.package <> n) installed in
      let set = OpamStd.String.Set.filter filter implicits in
      let list = OpamStd.String.Set.elements set in
      (* Favor packages with higher version number to discard
         deprecated packages. *)
      let max_version name =
        let filter p = p.Cudf.package = name in
        let packages = Cudf.get_packages ~filter universe in
        List.fold_left (fun v p -> max v p.Cudf.version) min_int packages in
      let cmp n1 n2 =
        compare (max_version n1) (max_version n2) in
      List.sort cmp list in

    installed, not_installed in

  log "implicit-installed: %a"
    (slog OpamCudf.string_of_packages) implicit_installed;
  log "implicit-not-installed: %a"
    (slog (String.concat ", ")) implicit_not_installed;

  implicit_installed, implicit_not_installed

(* Remove from the universe all the package versions which are not
   specified on the command-line. For instance:

   $ opam install core.109.13.00

   will cause all versions of core != 109.13.00 to disapear from the
   universe. This is causing [Universe.trim] to remove *a lot* of
   uninstallable packages and will improve the brute-force state
   exploration results.

   We also remove all version stricly less than the one installed as we
   don't downgrade anyway. *)
let trim_universe universe request =
  (* First trim: not very useful, but why not. *)
  let universe = Algo.Depsolver.trim universe in

  (* we compute the cone of interesting packages. *)
  let is_upgrade (n, _) =
    List.exists (fun (p, _) -> p = n) request.wish_upgrade in
  let wish_install =
    List.filter (fun p -> not (is_upgrade p)) request.wish_install in
  let all_wishes = wish_install @ request.wish_upgrade in
  let universe = List.fold_left (fun universe (name, constr) ->
      OpamCudf.remove_all_uninstalled_versions_but universe name constr
    ) universe all_wishes in
  let filter pkg =
    List.exists (fun (n,v) ->
      n = pkg.Cudf.package
      && Cudf.version_matches pkg.Cudf.version v
    ) all_wishes in
  let packages = Cudf.get_packages ~filter universe in
  let packages = OpamCudf.dependencies universe packages in

  (* We manually remove package with invalid constraints (seems that
     trim does not do it properly). *)
  let packages = List.filter (fun pkg ->
      List.for_all (List.exists (fun (name, constr) ->
          let filter p =
            p.Cudf.package = name
            && Cudf.version_matches p.Cudf.version constr in
          Cudf.get_packages ~filter universe <> []
        )) pkg.Cudf.depends
    ) packages in
  let universe = Cudf.load_universe packages in

  (* and we trim again. *)
  Algo.Depsolver.trim universe

(* Various heuristic to transform a solution checker into an optimized
   solver. *)
let optimize ?(verbose=true) ~version_map map_init_u universe request =

  (* We start be specializing the request. *)
  let request =
    let wish_upgrade = List.map (fun (name, constr) ->
        name,
        match constr with
        | Some _ -> constr
        | None   ->
          match Cudf.get_installed universe name with
          | [p] -> Some (`Geq, p.Cudf.version)
          | _   -> None
      ) request.wish_upgrade in
    { request with wish_upgrade } in
  (* We use that request to trim the universe, and keep only the interesting packages. *)
  let full_universe = universe in
  let universe = trim_universe universe request in
  let untrim universe =
    let open OpamCudf.Set in
    let open Op in
    let pkgs =
      of_list (Cudf.get_packages universe) ++
      of_list (Cudf.get_packages ~filter:(fun p -> p.Cudf.installed)
                 full_universe)
    in
    Cudf.load_universe (elements pkgs)
  in
  log "universe: %a"
    (slog (String.concat ", "
           @* List.map (OpamPackage.to_string @* OpamCudf.cudf2opam)
           @* OpamCudf.packages))
    universe;

  (* Upgrade the explicit packages first *)
  match state_of_request ~verbose ~version_map universe request with
  | None       ->
    OpamCudf.to_actions map_init_u (untrim universe)
      (OpamCudf.resolve ~extern:false ~version_map universe request)
  | Some state ->
    log "STATE(0) %a" (slog OpamCudf.string_of_packages) state;

    let request = refine state request in

    (* strategy where we try keep the installed version before trying
       an other one. *)
    let installed_first state p =
      log "installed-first %s" p.Cudf.package;
      if consistent_packages universe (p :: state) then (
        log "keep %s with the same version" p.Cudf.package;
        (p :: state)
      ) else (
        let request = refine state request in
        let request = add_to_upgrade request p.Cudf.package in
        match state_of_request ~verbose ~version_map universe request with
        | None       ->
          log "discard %s" p.Cudf.package;
          state
        | Some state ->
          let p = List.find (fun i -> i.Cudf.package = p.Cudf.package) state in
          log "pick an other version of %s (%d)" p.Cudf.package p.Cudf.version;
          log "request: %a" (slog OpamCudf.string_of_request) request;
          log "state: %a" (slog OpamCudf.string_of_packages) state;
          state
      ) in

    (* Try to keep the installed packages in the dependency cone *)
    let implicit_installed, implicit_not_installed = implicits universe request in
    let state = List.fold_left installed_first state implicit_installed in
    log "STATE(1) %a" (slog OpamCudf.string_of_packages) state;

    (* Minimize the number of new packages to install *)
    (* XXX: if we want to add an interactive mode, we need to do something here *)
    (* XXX: if needed we can sort the packages in a more clever order
       (for instance the size of the dependency cone of packages which
       are not yet installed). *)
    let universe, state = List.fold_left (fun (universe, state) name ->
        let remove_universe = OpamCudf.remove universe name None in
        if consistent_packages remove_universe state then (
          log "%s is not necessary (%a)" name
            (slog OpamCudf.string_of_packages) state;
          (remove_universe, state)
        ) else (
          log "adding %s to the request" name;
          let request = refine state request in
          let request = add_to_upgrade request name in
          match state_of_request ~verbose ~version_map universe request with
          | None       -> (universe, state)
          | Some state -> (universe, state)
        )
      ) (universe, state) implicit_not_installed in

    (* Finally we check that the already installed packages can still
       be installed in the new universe. *)
    let state =
      let filter p =
        p.Cudf.installed
        && List.for_all (fun s -> s.Cudf.package <>  p.Cudf.package) state
        && not (satisfy p request.wish_remove) in
      let packages = Cudf.get_packages ~filter universe in
      List.fold_left installed_first state packages in

    log "STATE(2) %a" (slog OpamCudf.string_of_packages) state;
    let universe = map_init_u (untrim universe) in
    Success (actions_of_state ~version_map universe request state)

let resolve ?(verbose=true) ~version_map map_init_u universe request =
  try
    if request.wish_upgrade <> [] then
      optimize ~verbose ~version_map map_init_u universe request
    else
      let res = OpamCudf.resolve ~extern:false ~version_map universe request in
      OpamCudf.to_actions map_init_u universe res
  with Not_reachable c ->
    Conflicts c
