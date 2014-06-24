(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2014 OCamlPro                                        *)
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

open OpamTypes
open OpamTypesBase
open OpamState.Types
open OpamMisc.OP
open OpamPackage.Set.Op

let log fmt = OpamGlobals.log "CLIENT" fmt
let slog = OpamGlobals.slog

let s_not_installed = "--"

type item = {
  name: name;
  current_version: version;
  installed_version: version option;
  synopsis: string Lazy.t;
  descr: string Lazy.t;
  tags: string list;
}

let names_of_regexp t ~filter ~depends_on ~exact_name ~case_sensitive regexps =
  log "names_of_regexp regexps=%a"
    (slog @@ OpamMisc.string_of_list (fun x -> x)) regexps;
  (* the regexp can also simply be a package. *)
  let fix_versions =
    let packages = OpamMisc.filter_map OpamPackage.of_string_opt regexps in
    OpamPackage.to_map (t.packages %% (OpamPackage.Set.of_list packages)) in
  let regexps =
    OpamMisc.filter_map (fun str ->
      let re =
        match OpamPackage.of_string_opt str with
        | Some nv ->
          if OpamPackage.Set.mem nv t.packages then
            let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
            Re_glob.globx name
          else
            Re_glob.globx str
        | None   -> Re_glob.globx str in
      let re =
        if case_sensitive then re else Re.no_case re in
      try Some (Re.compile re)
      with Re_glob.Parse_error ->
        OpamGlobals.error "%S is not a valid package descriptor." str;
        None
    ) regexps in
  let exact_match str =
    List.exists (fun re -> OpamMisc.exact_match re str) regexps in
  let partial_match str =
    List.exists (fun re -> Re.execp re str) regexps in
  let partial_matchs strs =
    List.exists partial_match strs in
  let is_provided_by providers opam =
    let formula = OpamFile.OPAM.depends opam in
    let depends_on (name,vopt) =
      List.exists (fun (n,_) -> name = n) (OpamFormula.atoms formula) &&
      let open OpamFormula in
      eval
        (fun (n,cstr) ->
           n <> name || match vopt with
           | None -> true
           | Some v -> eval (fun (relop,vref) -> eval_relop relop v vref) cstr)
        formula in
    List.for_all depends_on providers
  in
  let packages = match filter with
    | `all         -> t.packages
    | `installed   -> t.installed
    | `roots       -> t.installed_roots
    | `installable ->
      let installable = OpamSolver.installable (OpamState.universe t Depends) in
      t.installed ++ installable in
  let packages_map = OpamPackage.to_map packages in
  let packages_map =
    OpamPackage.Name.Map.fold (fun name versions map ->
      let installed_version =
        try Some
              (OpamPackage.version
                 (OpamPackage.Set.find
                    (fun nv -> OpamPackage.name nv = name)
                    t.installed))
        with Not_found -> None in
      let versions =
        try OpamPackage.Version.Set.inter versions
              (OpamPackage.Name.Map.find name fix_versions)
        with Not_found -> versions in
      let versions =
        if depends_on = [] then versions else
          OpamPackage.Version.Set.filter (fun v ->
              let nv = OpamPackage.create name v in
              is_provided_by depends_on (OpamState.opam t nv))
            versions in
      if OpamPackage.Version.Set.is_empty versions then map else
      let current_version = match installed_version with
        | Some v when OpamPackage.Version.Set.mem v versions -> v
        | _ -> OpamPackage.Version.Set.max_elt versions in
      let nv = OpamPackage.create name current_version in
      let descr_f = lazy (
        OpamState.descr t nv
      ) in
      let synopsis = lazy (
        OpamFile.Descr.synopsis (Lazy.force descr_f)
      ) in
      let descr = lazy (
        OpamFile.Descr.full (Lazy.force descr_f)
      ) in
      let tags = OpamFile.OPAM.tags (OpamState.opam t nv) in
      OpamPackage.Name.Map.add
        name { name; current_version; installed_version; synopsis; descr; tags }
        map
    ) packages_map OpamPackage.Name.Map.empty in

  (* Filter the list of packages, depending on user predicates *)
  let packages_map =
    OpamPackage.Name.Map.filter
      (fun name { synopsis; descr; tags } ->
         regexps = []
         || exact_match (OpamPackage.Name.to_string name)
         || not exact_name &&
            (partial_match (OpamPackage.Name.to_string name)
             || partial_match (Lazy.force synopsis)
             || partial_match (Lazy.force descr)
             || partial_matchs tags)
      ) packages_map in

  if not (OpamPackage.Set.is_empty t.packages)
  && OpamPackage.Name.Map.is_empty packages_map then
    OpamGlobals.msg "No packages found.\n";
  packages_map

let with_switch_backup command f =
  let t = OpamState.load_state command in
  let file = OpamPath.Switch.backup t.root t.switch in
  OpamFilename.mkdir (OpamPath.Switch.backup_dir t.root t.switch);
  OpamFile.Export.write file (t.installed, t.installed_roots, t.pinned);
  try
    f t;
    OpamFilename.remove file (* We might want to keep it even if successful ? *)
  with
  | OpamGlobals.Exit 0 as e -> raise e
  | err ->
    OpamMisc.register_backtrace err;
    let t1 = OpamState.load_state "switch-backup-err" in
    if OpamPackage.Set.equal t.installed t1.installed &&
       OpamPackage.Set.equal t.installed_roots t1.installed_roots then
      OpamFilename.remove file
    else
     Printf.eprintf "\nThe former state can be restored with \
                     %s switch import %S\n%!"
       Sys.argv.(0) (OpamFilename.to_string file);
    raise err

module API = struct

  let list ~print_short ~filter ~order ~depends_on ~exact_name ~case_sensitive regexp =
    let t = OpamState.load_state "list" in
    let names = names_of_regexp t ~filter ~depends_on ~exact_name ~case_sensitive regexp in
    if not print_short && OpamPackage.Name.Map.cardinal names > 0 then (
      let kind = match filter with
        | `roots
        | `installed -> "Installed"
        | _          -> "Available" in
      OpamGlobals.msg "%s packages for %s:\n" kind (OpamSwitch.to_string t.switch);
    );
    let get_version info =
      if depends_on = [] then info.installed_version
      else Some (info.current_version) in
    let max_n, max_v = (* for alignment *)
      OpamPackage.Name.Map.fold (fun name info (max_n, max_v) ->
        let max_n = max max_n (String.length (OpamPackage.Name.to_string name)) in
        let v_str = match get_version info with
          | None   -> s_not_installed
          | Some v -> OpamPackage.Version.to_string v in
        let max_v = max max_v (String.length v_str) in
        max_n, max_v
      ) names (0,0) in
    let names = OpamPackage.Name.Map.bindings names in
    let names = match order with
      | `normal  -> names
      | `depends ->
        let universe = OpamState.universe t Depends in
        let packages_info =
          List.map (fun (name, info) ->
              (OpamPackage.create name info.current_version, info)
            ) names in
        let packages =
          let packages = OpamPackage.Set.of_list (List.map fst packages_info) in
          OpamSolver.dependencies
            ~depopts:true ~installed:false universe packages in
        List.fold_left (fun acc nv ->
            try (OpamPackage.name nv, List.assoc nv packages_info) :: acc
            with Not_found -> acc
          ) [] packages in
    let roots = OpamPackage.names_of_packages t.installed_roots in
    List.iter (
      if print_short then
        fun (name, _) ->
          let name_str = OpamPackage.Name.to_string name in
          let colored_name =
            if !OpamGlobals.color && OpamPackage.Name.Set.mem name roots then
              OpamGlobals.colorise `underline name_str
            else name_str in
          Printf.printf "%s " colored_name
      else
        let synop_len =
          let col = OpamMisc.terminal_columns () in
          max 0 (col - max_n - max_v - 4) in
        fun (name, info) ->
          let version = get_version info in
          let name_str = OpamPackage.Name.to_string name in
          let colored_name =
            if !OpamGlobals.color && OpamPackage.Name.Set.mem name roots then
              OpamGlobals.colorise `underline name_str
            else name_str in
          let sversion, colored_version, pinned = match version with
            | None   -> s_not_installed, s_not_installed, ""
            | Some v ->
              let vs = OpamPackage.Version.to_string v in
              if OpamState.is_pinned t name then
                vs, OpamGlobals.colorise `blue vs,
                OpamGlobals.colorise `blue " (pinned)"
              else
                vs, OpamGlobals.colorise `magenta vs, ""
          in
          Printf.printf "%s  %s%s  %s\n"
            (OpamMisc.indent_left colored_name ~visual:name_str max_n)
            (OpamMisc.indent_right colored_version ~visual:sversion max_v)
            pinned
            (OpamMisc.sub_at synop_len (Lazy.force info.synopsis))
    ) names;
    if names = [] then OpamGlobals.exit 1

  let info ~fields regexps =
    let t = OpamState.load_state "info" in
    let names =
      names_of_regexp t ~filter:`all ~depends_on:[]
        ~exact_name:true ~case_sensitive:false regexps in

    let show_fields = List.length fields <> 1 in

    let print_one name  { current_version } =

      (* Compute the installed versions, for each switch *)
      let installed = OpamState.installed_versions t name in
      (* let installed = OpamPackage.Map.fold (fun nv alias map -> *)
      (*     OpamPackage.Map.add (OpamState.pinning_version t nv) alias map *)
      (*   ) installed OpamPackage.Map.empty in *)
      let installed_str =
        let one (nv, aliases) =
          Printf.sprintf "%s [%s]"
            (OpamPackage.to_string nv)
            (String.concat " " (List.map OpamSwitch.to_string aliases)) in
        String.concat ", " (List.map one (OpamPackage.Map.bindings installed)) in

      let nv = OpamPackage.create name current_version in
      let opam = OpamState.opam t nv in

      (* where does it come from (eg. which repository) *)
      let repository =
        let repo =
          match OpamState.repository_of_package t nv with
          | None -> []
          | Some r -> [ "repository", OpamRepositoryName.to_string r.repo_name ]
        in
        try
          let pin = OpamPackage.Name.Map.find name t.pinned in
          let kind = kind_of_pin_option pin in
          (if kind = `version then repo else []) @
          ["pinned", (string_of_pin_kind kind)]
        with Not_found ->
          repo
      in

      let url = match OpamState.url t nv with
        | None   -> []
        | Some u ->
          let kind = string_of_repository_kind (OpamFile.URL.kind u) in
          let url = string_of_address (OpamFile.URL.url u) in
          let mirrors =
            OpamMisc.string_of_list string_of_address (OpamFile.URL.mirrors u) in
          let checksum = OpamFile.URL.checksum u in
          [ "upstream-url" , url ]
          @ (if OpamFile.URL.mirrors u = [] then []
             else [ "upstream-mirrors" , mirrors ])
          @ [ "upstream-kind", kind ]
          @ match checksum with
            | None   -> []
            | Some c -> [ "upstream-checksum", c ] in

      (* All the version of the package *)
      let versions = OpamPackage.versions_of_name t.packages name in
      let versions =
        OpamPackage.Version.Set.filter (fun v ->
          OpamPackage.Map.for_all (fun nv _ -> OpamPackage.version nv <> v) installed
        ) versions in

      let installed_version = match OpamPackage.Map.cardinal installed with
        | 0 -> [ "installed-version" , "" ]
        | 1 -> [ "installed-version" , installed_str ]
        | _ -> [ "installed-versions", installed_str ] in

      let available_versions =
        let strings = List.map OpamPackage.Version.to_string
            (OpamPackage.Version.Set.elements versions) in
        match strings with
        | []  -> []
        | [v] -> [ "available-version" , v ]
        | l   -> [ "available-versions", String.concat ", " l ] in

      let mk (empty, get, to_string) name field =
        let v = field opam in
        if empty = v then
          []
        else
          [name, to_string (get v)] in

      let strings = mk (
          [],
          (fun l -> l),
          (String.concat ", ")
        ) in
      let formula = mk (
          Empty,
          (fun f -> f),
          OpamFormula.to_string
        ) in

      let author   = strings "author"   OpamFile.OPAM.author in
      let homepage = strings "homepage" OpamFile.OPAM.homepage in
      let license  = strings "license"  OpamFile.OPAM.license in
      let doc      = strings "doc"      OpamFile.OPAM.doc in
      let tags     = strings "tags"     OpamFile.OPAM.tags in
      let depends  = formula "depends"  OpamFile.OPAM.depends in
      let depopts  = formula "depopts"  OpamFile.OPAM.depopts in

      let libraries = strings "libraries" (fun t -> List.map fst (OpamFile.OPAM.libraries t)) in
      let syntax    = strings "syntax"    (fun t -> List.map fst (OpamFile.OPAM.syntax t)) in

      let os = mk (
        Empty,
        (fun f -> f),
        OpamFormula.string_of_formula (fun (t,s) ->
          if t then s else "!"^s)
      ) "os" OpamFile.OPAM.os in

      let descr =
        let d = OpamState.descr t nv in
        ["description", OpamFile.Descr.full d] in

      let version = OpamPackage.version nv in

      let all_fields =
        [ "package", OpamPackage.Name.to_string name ]
        @ [ "version", OpamPackage.Version.to_string version ]
        @ repository
        @ url
        @ homepage
        @ author
        @ license
        @ doc
        @ tags
        @ libraries
        @ syntax
        @ depends
        @ depopts
        @ os
        @ installed_version
        @ available_versions
        @ descr in

      let all_fields = match fields with
        | [] -> all_fields
        | f  -> List.filter (fun (d,_) -> List.mem d f) all_fields in

      List.iter (fun (f, desc) ->
        if show_fields then
          OpamGlobals.msg "%s "
            (OpamGlobals.colorise `blue (Printf.sprintf "%20s:" f));
        OpamGlobals.msg "%s\n" desc
      ) all_fields in

    OpamPackage.Name.Map.iter print_one names

  (* When packages are removed from upstream, they normally disappear from the
     'available' packages set and can't be seen by the solver anymore. This is a
     problem for several reasons, so we compute the set of orphan packages here:
     - they are checked for conflicts with the user request
     - they are re-added to the universe if (transitively) unrelated to the
       request (the [changes] parameter)
     - they are otherwise put in [wish_remove] in case we use the internal
       solver
     This function separates full orphans (no version of the package available
     anymore) from orphan versions, because they have a different impact on
     the request (needs version change VS needs uninstall).
     See also preprocess_request and check_conflicts *)
  let orphans ?changes ?(transitive=false) t =
    let all = t.packages ++ t.installed in
    let complete_universe =
      OpamState.universe {t with available_packages = lazy all} Reinstall in
    (* Basic definition of orphan packages *)
    let orphans = t.installed -- Lazy.force t.available_packages in
    (* Restriction to the request-related packages *)
    let orphans = match changes with
      | None -> orphans
      | Some ch ->
        let recompile_cone =
          OpamPackage.Set.of_list @@
          OpamSolver.reverse_dependencies ~depopts:true ~installed:false
            complete_universe ch
        in
        orphans %% recompile_cone
    in
    (* Pinned versions of packages remain always available *)
    let orphans = orphans -- OpamState.pinned_packages t in
    (* Closure *)
    let orphans =
      if not transitive then orphans else
        OpamPackage.Set.of_list @@
        OpamSolver.reverse_dependencies ~depopts:false ~installed:false
        complete_universe orphans
    in
    let orphan_names = (* names for which there is no version left *)
      OpamPackage.Name.Set.diff
        (OpamPackage.names_of_packages all)
        (OpamPackage.names_of_packages (all -- orphans)) in
    let full_orphans, orphan_versions =
      OpamPackage.Set.partition
        (fun nv -> OpamPackage.Name.Set.mem (OpamPackage.name nv) orphan_names)
        orphans in
    (* Installed packages outside the set of changes are otherwise safe:
       re-add them to the universe *)
    let t =
      if changes = None then t else
      let available_packages =
        lazy (Lazy.force t.available_packages ++
              (t.installed -- orphans)) in
      { t with available_packages } in
    log "Orphans: full %a, versions %a"
      (slog OpamPackage.Name.Set.to_string) orphan_names
      (slog OpamPackage.Set.to_string) orphan_versions;
    t, full_orphans, orphan_versions

  (* The internal "solver" needs some rewrites of the requests, to make them
     more explicit. This has no effect when using the external solver. *)
  let preprocess_request t full_orphans orphan_versions request =
    if OpamCudf.external_solver_available () then request else
    let { wish_install; wish_remove; wish_upgrade; criteria } = request in
    (* Convert install to upgrade when necessary, request roots installed *)
    let eqnames, neqnames =
      List.partition (function (_,Some(`Eq,_)) -> true | _ -> false)
        wish_install in
    let add_wish_install =
      List.rev_append eqnames
        (OpamSolution.atoms_of_packages
           (t.installed_roots %% (Lazy.force t.available_packages))) in
    let wish_install = List.rev_append add_wish_install wish_install in
    let wish_upgrade = List.rev_append neqnames wish_upgrade in
    (* Remove orphans *)
    let wish_remove =
      OpamSolution.atoms_of_packages full_orphans @
      OpamSolution.eq_atoms_of_packages orphan_versions @
      wish_remove in
    let available =
      OpamPackage.Set.Op.(
        Lazy.force t.available_packages -- orphan_versions -- full_orphans) in
    let still_available atom =
      OpamPackage.Set.exists
        (fun p -> OpamFormula.check atom p)
        available in
    let wish_install = List.filter still_available wish_install in
    let wish_upgrade = List.filter still_available wish_upgrade in
    let nrequest = { wish_install; wish_remove; wish_upgrade; criteria } in
    log "Preprocess request: %a => %a"
      (slog OpamSolver.string_of_request) request
      (slog OpamSolver.string_of_request) nrequest;
    nrequest

  (* Splits a list of atoms into the installed and uninstalled ones*)
  let get_installed_atoms t atoms =
    List.fold_left (fun (packages, not_installed) atom ->
        try
          let nv =
            OpamPackage.Set.find (OpamFormula.check atom) t.installed in
          nv :: packages, not_installed
        with Not_found ->
          packages, atom :: not_installed)
      ([],[]) atoms

  (* Check atoms for pinned packages, and update them. Returns the state that
     may have been reloaded if there were changes *)
  let update_dev_packages_t atoms t =
    let to_update =
      List.fold_left (fun to_update (name,_) ->
          match OpamState.pinned_opt t name with
          | None -> to_update
          | Some nv ->
            OpamPackage.Set.add nv to_update)
        OpamPackage.Set.empty atoms
    in
    if OpamPackage.Set.is_empty to_update then t else (
      OpamGlobals.header_msg "Synchronising pinned packages";
      let updated = OpamState.update_dev_packages t to_update in
      if OpamPackage.Set.is_empty updated then t
      else OpamState.load_state "reload-dev-package-updated"
    )

  let compute_upgrade_t atoms t =
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
    if atoms = [] then
      let to_reinstall = t.reinstall %% t.installed in
      let t, full_orphans, orphan_versions = orphans ~transitive:true t in
      let to_upgrade = t.installed -- full_orphans in
      let requested = OpamPackage.Name.Set.empty in
      let action = Upgrade to_reinstall in
      requested,
      action,
      OpamSolution.resolve t action ~requested
        (preprocess_request t full_orphans orphan_versions
           { wish_install = [];
             wish_remove  = [];
             wish_upgrade = OpamSolution.atoms_of_packages to_upgrade;
             criteria = !OpamGlobals.solver_upgrade_preferences; })
    else
    let to_reinstall =
      OpamPackage.Set.filter
        (fun nv -> OpamPackage.Name.Set.mem (OpamPackage.name nv) names)
        t.reinstall in
    let to_upgrade, not_installed =
      List.fold_left (fun (packages, not_installed) (n,_ as atom) ->
          try
            let nv =
              OpamPackage.Set.find (fun nv -> OpamPackage.name nv = n)
                t.installed in
            OpamPackage.Set.add nv packages, not_installed
          with Not_found ->
            packages, atom :: not_installed)
        (OpamPackage.Set.empty,[]) atoms in
    if not_installed <> [] then
      OpamGlobals.note "%s %s not installed, ignored.\n"
        (OpamMisc.pretty_list
           (List.rev_map OpamFormula.short_string_of_atom not_installed))
        (match not_installed with [_] -> "is" | _ -> "are");
    let t, full_orphans, orphan_versions = orphans ~changes:to_upgrade t in
    let conflicts = to_upgrade %% full_orphans in
    if not (OpamPackage.Set.is_empty conflicts) then
      OpamGlobals.error_and_exit
        "These packages would need to be recompiled, but they are no longer available \
         upstream:\n\
        \  %s\n\
         Please run \"opam upgrade\" without argument to get to a clean state."
        (OpamPackage.Set.to_string conflicts);
    let installed_roots = t.installed -- to_upgrade in
    let requested = names in
    let action = Upgrade to_reinstall in
    let upgrade_atoms =
      (* packages corresponds to the currently installed versions.
         Not what we are interested in, recover the original atom constraints *)
      List.map (fun nv ->
          let name = OpamPackage.name nv in
          try name, List.assoc name atoms
          with Not_found -> name, None)
        (OpamPackage.Set.elements to_upgrade) in
    requested,
    action,
    OpamSolution.resolve t action ~requested
      (preprocess_request t full_orphans orphan_versions
         { wish_install = OpamSolution.eq_atoms_of_packages installed_roots;
           wish_remove  = [];
           wish_upgrade = upgrade_atoms;
           criteria = !OpamGlobals.solver_upgrade_preferences; })

  let upgrade_t ?ask atoms t =
    log "UPGRADE %a"
      (slog @@ function [] -> "<all>" | a -> OpamFormula.string_of_atoms a)
      atoms;
    match compute_upgrade_t atoms t with
    | _requested, _action, Conflicts cs ->
      log "conflict!";
      let reasons, chains, _cycles =
        OpamCudf.strings_of_conflict (OpamState.unavailable_reason t) cs in
      OpamGlobals.warning
        "This is a consistency problem with the currently \
         installed packages:";
      List.iter (OpamGlobals.msg "  - %s\n") reasons;
      if chains <> [] then (
        OpamGlobals.msg "The following dependencies are in cause:\n";
        List.iter (OpamGlobals.msg "  - %s\n") chains);
      if OpamCudf.external_solver_available () then
        OpamGlobals.msg
          "\nYou may run \"opam upgrade --fixup\" to let OPAM fix your \
           installation.\n"
    | requested, action, Success solution ->
      let result = OpamSolution.apply ?ask t action ~requested solution in
      if result = Nothing_to_do then OpamGlobals.msg "Already up-to-date.\n";
      OpamSolution.check_solution t result

  let upgrade names =
    with_switch_backup "upgrade" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    let t = update_dev_packages_t atoms t in
    upgrade_t atoms t

  let fixup_t t =
    log "FIXUP";
    if not (OpamCudf.external_solver_available ()) then
      (OpamGlobals.msg
         "Sorry, \"--fixup\" is not available without an external solver. \n\
          You'll have to select the packages to change or remove by hand, \
          or install aspcud or another solver on your system.\n";
       OpamGlobals.exit 1)
    else
    let t, _full_orphans, _orphan_versions = orphans ~transitive:true t in
    let requested = OpamPackage.Name.Set.empty in
    let action = Upgrade OpamPackage.Set.empty in
    let solution =
      OpamSolution.resolve_and_apply ~ask:true t action ~requested
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = [];
          criteria = !OpamGlobals.solver_fixup_preferences; }
    in
    OpamSolution.check_solution t solution

  let fixup () = with_switch_backup "fixup" fixup_t

  let update ~repos_only names =
    let t = OpamState.load_state ~save_cache:true "update" in
    log "UPDATE %a" (slog @@ String.concat ", ") names;
    let repositories =
      if names = [] then
        t.repositories
      else
        let aux r _ =
          List.mem (OpamRepositoryName.to_string r) names in
        OpamRepositoryName.Map.filter aux t.repositories in
    let repositories_need_update =
      not (OpamRepositoryName.Map.is_empty repositories) in

    let dev_packages =
      if repos_only then OpamPackage.Set.empty
      else
        let all = t.installed %% OpamState.dev_packages t in
        if names = [] then
          all
        else
          OpamPackage.Set.filter (fun nv ->
              let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
              let pkg = OpamPackage.to_string nv in
              List.exists (fun s -> s = name || s = pkg) names
            ) all in
    let dev_packages_need_update =
      not (OpamPackage.Set.is_empty dev_packages) in

    let valid_repositories =
      OpamMisc.StringSet.of_list
        (List.rev_map OpamRepositoryName.to_string
           (OpamRepositoryName.Map.keys repositories)) in
    let valid_pinned_packages =
      OpamMisc.StringSet.of_list
        (List.rev_map OpamPackage.Name.to_string
           (OpamPackage.Name.Map.keys t.pinned)) in
    let unknown_names, not_pinned =
      if names = [] then
        [], []
      else
        let all = OpamMisc.StringSet.of_list names in
        let valid_names =
          OpamMisc.StringSet.of_list
            (List.rev_map
               (OpamPackage.name @> OpamPackage.Name.to_string)
               (OpamPackage.Set.elements t.packages)) in
        let (--) = OpamMisc.StringSet.diff in
        let unknown_names = all -- valid_repositories -- valid_names in
        let not_pinned =
          (OpamMisc.StringSet.inter all valid_names)
          -- valid_pinned_packages
          -- valid_repositories in
        OpamMisc.StringSet.elements unknown_names,
        OpamMisc.StringSet.elements not_pinned in

    begin
      let valid_repositories =
        match OpamMisc.StringSet.elements valid_repositories with
        | []  -> ""
        | [s] -> Printf.sprintf " Valid repository is %s." s
        | l   ->
          Printf.sprintf
            " Valid repositories are %s."
            (OpamMisc.pretty_list l) in
      match unknown_names with
      | []  -> ()
      | [s] ->
        OpamGlobals.error_and_exit
          "Cannot update the repository %s.%s"
          s valid_repositories
      | _   ->
        OpamGlobals.error_and_exit
          "Cannot update the repositories %s.%s"
          (OpamMisc.pretty_list unknown_names) valid_repositories
    end;
    begin
      let valid_pinned_packages =
        match OpamMisc.StringSet.elements valid_pinned_packages with
        | []  -> ""
        | [s] -> Printf.sprintf "Only %s is currently pinned.\n" s
        | l   ->
          Printf.sprintf
            "The currently pinned packages are %s.\n"
            (OpamMisc.pretty_list l) in
      match not_pinned with
      | []  -> ()
      | [s] ->
        OpamGlobals.msg
          "Cannot update the package %s because it is not pinned.\n%s"
          s valid_pinned_packages
      | _   ->
        OpamGlobals.msg
          "Cannot update %s because none of them is pinned.%s\n"
          (OpamMisc.pretty_list not_pinned) valid_pinned_packages
    end;

    if repositories_need_update then (
      let repos = OpamRepositoryName.Map.values repositories in
      let child repo =
        try OpamRepositoryCommand.update t repo
        with e ->
          OpamMisc.fatal e;
          OpamGlobals.error "Skipping %s as the repository is not available.\n"
            (string_of_address repo.repo_address) in

      (* Update each remote backend *)
      OpamRepository.Parallel.iter_l (2 * OpamState.jobs t) repos
        ~child ~post:ignore ~pre:ignore;

      let t, compiler_updates =
        let t = OpamRepositoryCommand.update_compiler_index t in
        t, OpamRepositoryCommand.fix_compiler_descriptions t ~verbose:true in
      let package_updates =
        let t = OpamRepositoryCommand.update_package_index t in
        OpamRepositoryCommand.fix_package_descriptions t ~verbose:true in

      (* Eventually output some JSON file *)
      if OpamJson.verbose () then
        let json to_json update =
          `O [ ("created", to_json update.created);
               ("updated", to_json update.updated);
               ("deleted", to_json update.deleted);
               ("changed", to_json update.changed); ] in
        let updates = `O [
            "package-updates" , (json OpamPackage.Set.to_json package_updates);
            "compiler-updates", (json OpamCompiler.Set.to_json compiler_updates);
          ] in
        OpamJson.add updates;
    );

    if dev_packages_need_update then (
      let updates =
        OpamRepositoryCommand.update_dev_packages ~verbose:true t dev_packages in
      let json = `O [ "dev-packages-update", OpamPackage.Set.to_json updates ] in
      OpamJson.add json
    );

    OpamState.rebuild_state_cache ();

    log "dry-upgrade";
    let t = OpamState.load_state ~save_cache:false "dry-upgrade" in
    match compute_upgrade_t [] t with
    | _, _, Success upgrade ->
      let stats = OpamSolver.stats upgrade in
      if OpamSolution.sum stats > 0 then
        (OpamGlobals.msg "=== %s ===\n" (OpamSolver.string_of_stats stats);
         OpamGlobals.msg
           "You can now run 'opam upgrade' to upgrade your system.\n")
    | _, _, Conflicts cs ->
      let reasons, chains, _cycles =
        OpamCudf.strings_of_conflict (OpamState.unavailable_reason t) cs in
      OpamGlobals.warning
        "A conflict was detected in your installation. This can be caused by updated\n\
         constraints in your installed packages:";
      List.iter (OpamGlobals.msg "  - %s\n") reasons;
      if chains <> [] then (
        OpamGlobals.msg "The following dependencies are in cause:\n";
        List.iter (OpamGlobals.msg "  - %s\n") chains);
      if OpamCudf.external_solver_available () then
        OpamGlobals.msg
          "\nYou should run \"opam upgrade --fixup\" to resolve the situation.\n"

  let init repo compiler ~jobs shell dot_profile update_config =
    log "INIT %a" (slog OpamRepository.to_string) repo;
    let root = OpamPath.root () in
    let config_f = OpamPath.config root in
    let dot_profile_o = Some dot_profile in
    let user = { shell; ocamlinit = true; dot_profile = dot_profile_o } in
    let root_empty =
      not (OpamFilename.exists_dir root) || OpamFilename.dir_is_empty root in
    let update_setup t =
      let updated = match update_config with
        | `ask -> OpamState.update_setup_interactive t shell dot_profile
        | `no  -> false
        | `yes ->
          let global = { complete = true; switch_eval = true } in
          OpamState.update_setup t (Some user) (Some global);
          true in
      if updated then OpamState.print_env_warning_at_switch t
      else OpamState.print_env_warning_at_init t user in

    if OpamFilename.exists config_f then (
      OpamGlobals.msg "OPAM has already been initialized.";
      let t = OpamState.load_state "init" in
      update_setup t
    ) else (
      if not root_empty then (
        OpamGlobals.warning "%s exists and is not empty"
          (OpamFilename.Dir.to_string root);
        if not (OpamState.confirm "Proceed ?") then OpamGlobals.exit 1);
      try
        (* Create (possibly empty) configuration files *)
        let switch =
          if compiler = OpamCompiler.system then
            OpamSwitch.default
          else
            OpamSwitch.of_string (OpamCompiler.to_string compiler) in

        (* Create ~/.opam/compilers/system.comp *)
        let system_version = OpamCompiler.Version.current () in
        OpamState.create_system_compiler_description root system_version;

        (* Create ~/.opam/config *)
        let config =
          OpamFile.Config.create switch [repo.repo_name] jobs
            OpamGlobals.default_dl_jobs in
        OpamFile.Config.write config_f config;

        (* Create ~/.opam/aliases *)
        OpamFile.Aliases.write
          (OpamPath.aliases root)
          (OpamSwitch.Map.add switch compiler OpamSwitch.Map.empty);

        (* Init repository *)
        OpamFile.Package_index.write (OpamPath.package_index root)
          OpamPackage.Map.empty;
        OpamFile.Compiler_index.write (OpamPath.compiler_index root)
          OpamCompiler.Map.empty;
        OpamFile.Repo_config.write (OpamPath.Repository.config repo) repo;
        OpamRepository.init repo;

        (* Init global dirs *)
        OpamFilename.mkdir (OpamPath.packages_dir root);
        OpamFilename.mkdir (OpamPath.compilers_dir root);

        (* Load the partial state, and update the global state *)
        log "updating repository state";
        let t = OpamState.load_state ~save_cache:false "init-1" in
        OpamRepositoryCommand.fix_descriptions t ~save_cache:false ~verbose:false;

        (* Load the partial state, and install the new compiler if needed *)
        log "updating package state";
        let t = OpamState.load_state ~save_cache:false "init-2" in
        let switch = OpamSwitch.of_string (OpamCompiler.to_string compiler) in
        let quiet = (compiler = OpamCompiler.system) in
        OpamState.install_compiler t ~quiet switch compiler;
        OpamState.update_switch_config t switch;

        (* Finally, load the complete state and install the compiler packages *)
        log "installing compiler packages";
        let t = OpamState.load_state "init-3" in
        let compiler_packages = OpamState.get_compiler_packages t compiler in
        let compiler_names =
          OpamPackage.Name.Set.of_list (List.rev_map fst compiler_packages) in
        let solution =
          OpamSolution.resolve_and_apply ~ask:false t (Init compiler_names)
            ~requested:compiler_names
            { wish_install = [];
              wish_remove  = [];
              wish_upgrade = compiler_packages;
              criteria = !OpamGlobals.solver_preferences; }
        in
        OpamSolution.check_solution t solution;
        update_setup t

      with e ->
        if not !OpamGlobals.debug && root_empty then
          OpamFilename.rmdir root;
        raise e)

  let packages_of_atoms t atoms =
    let check_atoms nv =
      let name = OpamPackage.name nv in
      let atoms = List.filter (fun (n,_) -> n = name) atoms in
      atoms <> [] && List.for_all (fun a -> OpamFormula.check a nv) atoms in
    (* All packages satisfying [atoms] *)
    OpamPackage.Set.filter check_atoms t.packages

  (* Checks a request for [atoms] for conflicts with the orphan packages *)
  let check_conflicts t atoms =
    let changes = packages_of_atoms t atoms in
    let t, full_orphans, orphan_versions = orphans ~changes t in
    (* packages which still have local data are OK for install/reinstall *)
    let has_no_local_data nv =
      not (OpamFilename.exists_dir (OpamPath.packages t.root nv)) in
    let orphans =
      OpamPackage.Set.filter has_no_local_data
        (full_orphans ++ orphan_versions) in
    let available = lazy (t.packages -- orphans) in
    let conflict_atoms =
      List.filter
        (fun (name,_ as a) ->
           not (OpamState.is_pinned t name) &&
           OpamPackage.Set.exists (OpamFormula.check a) orphans && (*optim*)
           not (OpamPackage.Set.exists (OpamFormula.check a) (* real check *)
                  (Lazy.force available)))
        atoms in
    if conflict_atoms <> [] then
      OpamGlobals.error_and_exit
        "Sorry, these packages are no longer available \
         from the repositories: %s"
        (OpamMisc.pretty_list
           (List.map OpamFormula.string_of_atom conflict_atoms))
    else
      t, full_orphans, orphan_versions

  let install_t ?ask atoms add_to_roots deps_only t =
    log "INSTALL %a" (slog OpamFormula.string_of_atoms) atoms;
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

    let t, full_orphans, orphan_versions = check_conflicts t atoms in

    let pkg_skip, pkg_new =
      get_installed_atoms t atoms in

    (* Add the packages to the list of package roots and display a
       warning for already installed package roots. *)
    let current_roots = t.installed_roots in
    let t =
      List.fold_left (fun t nv ->
          if OpamPackage.Set.mem nv t.installed then
            match add_to_roots with
            | None ->
              OpamGlobals.note
                "Package %s is already installed (current version is %s)."
                (OpamPackage.Name.to_string (OpamPackage.name nv))
                (OpamPackage.Version.to_string (OpamPackage.version nv));
              t
            | Some true ->
              if OpamPackage.Set.mem nv t.installed_roots then
                OpamGlobals.note
                  "Package %s is already installed as a root."
                  (OpamPackage.Name.to_string (OpamPackage.name nv));
              { t with installed_roots =
                         OpamPackage.Set.add nv t.installed_roots }
            | Some false ->
              if OpamPackage.Set.mem nv t.installed_roots then
                { t with installed_roots =
                           OpamPackage.Set.remove nv t.installed_roots }
              else
                (OpamGlobals.note
                   "Package %s is already marked as 'installed automatically'."
                   (OpamPackage.Name.to_string (OpamPackage.name nv));
                 t)
          else t
        )  t pkg_skip in
    if t.installed_roots <> current_roots then (
      let diff = t.installed_roots -- current_roots in
      if not (OpamPackage.Set.is_empty diff) then
        let diff = OpamPackage.Set.elements diff in
        let diff = List.rev (List.rev_map OpamPackage.to_string diff) in
        OpamGlobals.msg
          "Adding %s to the list of installed roots.\n"
          (OpamMisc.pretty_list diff)
      else (
        let diff = current_roots -- t.installed_roots in
        let diff = OpamPackage.Set.elements diff in
        let diff = List.rev (List.rev_map OpamPackage.to_string diff) in
        OpamGlobals.msg
          "Removing %s from the list of installed roots.\n"
          (OpamMisc.pretty_list diff)
      );
      let file = OpamPath.Switch.installed_roots t.root t.switch in
      OpamFile.Installed_roots.write file t.installed_roots;
    );

    OpamSolution.check_availability t (Lazy.force t.available_packages) atoms;

    if pkg_new <> [] then (

      let request =
        preprocess_request t full_orphans orphan_versions
          { wish_install = atoms;
            wish_remove  = [];
            wish_upgrade = [];
            criteria = !OpamGlobals.solver_preferences; }
      in
      let action =
        if add_to_roots = Some false || deps_only then
          Install OpamPackage.Name.Set.empty
        else Install names in
      let solution = OpamSolution.resolve t action ~requested:names request in
      let solution = match solution with
        | Conflicts cs ->
          log "conflict!";
          OpamGlobals.msg "%s"
            (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
          No_solution
        | Success solution ->
          if deps_only then (
            let to_install =
              OpamSolver.ActionGraph.fold_vertex (fun act acc -> match act with
                  | To_change (_, p) -> OpamPackage.Set.add p acc
                  | _ -> acc)
                solution.to_process OpamPackage.Set.empty in
            let all_deps =
              let universe = OpamState.universe t (Install names) in
              OpamPackage.Name.Set.fold (fun name deps ->
                  let nvs = OpamPackage.packages_of_name to_install name in
                  let deps_nv =
                    OpamSolver.dependencies ~depopts:false ~installed:false
                      universe nvs in
                  let deps_only = OpamPackage.Set.of_list deps_nv -- nvs in
                  deps ++ deps_only)
                names OpamPackage.Set.empty in
            OpamSolver.ActionGraph.iter_vertex (function
                | To_change (_, p) as v ->
                  if not (OpamPackage.Set.mem p all_deps) then
                    OpamSolver.ActionGraph.remove_vertex
                      solution.to_process v
                | _ -> ())
              solution.to_process
          );
          OpamSolution.apply ?ask t action ~requested:names solution in
      OpamSolution.check_solution t solution
    )

  let install names add_to_roots deps_only =
    with_switch_backup "install" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list ~permissive:true t names in
    let t = update_dev_packages_t atoms t in
    install_t atoms add_to_roots deps_only t

  let remove_t ?ask ~autoremove ~force atoms t =
    log "REMOVE autoremove:%b %a" autoremove
      (slog OpamFormula.string_of_atoms) atoms;

    let t, full_orphans, orphan_versions =
      let changes =
        if autoremove then None
        else Some (packages_of_atoms t atoms) in
      orphans ?changes t
    in

    let nothing_to_do = ref true in
    let atoms =
      List.filter (fun (n,_) ->
        if n = OpamPackage.Name.global_config then (
          OpamGlobals.msg "Package %s can not be removed.\n"
            (OpamPackage.Name.to_string OpamPackage.Name.global_config);
          false
        ) else
          true
      ) atoms in
    let packages, not_installed =
      get_installed_atoms t atoms in
    if not_installed <> [] then (
      if force then
        let force_remove atom =
          let candidates = OpamPackage.Set.filter (OpamFormula.check atom) t.packages in
          try
            let nv = OpamPackage.max_version candidates (fst atom) in
            OpamGlobals.note "Forcing removal of (uninstalled) %s" (OpamPackage.to_string nv);
            OpamAction.remove_package ~metadata:false t nv;
            OpamAction.cleanup_package_artefacts t nv;
            nothing_to_do := false
          with Not_found ->
            OpamGlobals.error "No package %s found for (forced) removal.\n"
              (OpamFormula.short_string_of_atom atom)
        in
        List.iter force_remove not_installed
      else
        OpamGlobals.note "%s %s not installed.\n"
          (OpamMisc.pretty_list
             (List.map OpamFormula.short_string_of_atom not_installed))
          (match not_installed with [_] -> "is" | _ -> "are")
    );

    if autoremove || packages <> [] then (
      let packages = OpamPackage.Set.of_list packages in
      let universe = OpamState.universe t Remove in
      let to_remove =
        OpamPackage.Set.of_list
          (OpamSolver.reverse_dependencies
             ~depopts:false ~installed:true universe packages) in
      let to_keep =
        (if autoremove then t.installed_roots else t.installed)
        -- to_remove -- full_orphans -- orphan_versions
      in
      let to_keep =
        OpamPackage.Set.of_list
          (OpamSolver.dependencies
             ~depopts:true ~installed:true universe to_keep) in
      (* to_keep includes the depopts, because we don't want to autoremove
         them. But that may re-include packages that we wanted removed, so we
         need to remove them again *)
      let to_keep = to_keep -- to_remove in
      let requested = OpamPackage.names_of_packages packages in
      let to_remove =
        if autoremove then
          let to_remove = t.installed -- to_keep in
          if atoms = [] then to_remove
          else (* restrict to the dependency cone of removed pkgs *)
            to_remove %%
            (OpamPackage.Set.of_list
               (OpamSolver.dependencies
                  ~depopts:true ~installed:true universe to_remove))
        else to_remove in
      let solution = OpamSolution.resolve_and_apply ?ask t Remove ~requested
          { wish_install = OpamSolution.eq_atoms_of_packages to_keep;
            wish_remove  = OpamSolution.atoms_of_packages to_remove;
            wish_upgrade = [];
            criteria = !OpamGlobals.solver_preferences; } in
      OpamSolution.check_solution t solution
    ) else if !nothing_to_do then
      OpamGlobals.msg "Nothing to do.\n"

  let remove ~autoremove ~force names =
    with_switch_backup "remove" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    remove_t ~autoremove ~force atoms t

  let reinstall_t ?ask ?(force=false) atoms t =
    log "reinstall %a" (slog OpamFormula.string_of_atoms) atoms;

    let t, _, _ = check_conflicts t atoms in

    let reinstall, not_installed =
      get_installed_atoms t atoms in
    let t =
      if not_installed <> [] then
        if
          force ||
          (OpamGlobals.warning "%s %s not installed."
             (OpamMisc.pretty_list
                (List.map OpamFormula.short_string_of_atom not_installed))
             (match not_installed with [_] -> "is" | _ -> "are");
           OpamState.confirm "Install ?")
        then
          (install_t atoms None false t;
           if reinstall = [] then OpamGlobals.exit 0
           else OpamState.load_state "reinstall-installed")
        else
          OpamGlobals.exit 1
      else t
    in

    let reinstall = OpamPackage.Set.of_list reinstall in
    let universe = OpamState.universe t Depends in
    let depends = (* Do not cast to a set, we need to keep the order *)
      OpamSolver.reverse_dependencies
        ~depopts:true ~installed:true universe reinstall in
    let to_process =
      List.map (fun pkg -> To_recompile pkg) depends in
    let requested =
      OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
    let solution =
      OpamSolver.sequential_solution universe ~requested to_process in
    let solution = match solution with
      | Conflicts cs ->
        log "conflict!";
        OpamGlobals.msg "%s"
          (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
        No_solution
      | Success solution ->
        OpamSolution.apply ?ask t Reinstall ~requested solution in
    OpamSolution.check_solution t solution

  let reinstall names =
    with_switch_backup "reinstall" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    let t = update_dev_packages_t atoms t in
    reinstall_t atoms t

  module PIN = struct
    open OpamPinCommand

    let post_pin_action t name =
      let nv = try Some (OpamState.pinned t name) with Not_found -> None in
      match nv with
      | Some nv ->
        let v = OpamPackage.version nv in
        OpamGlobals.msg "%s needs to be %sinstalled.\n"
          (OpamPackage.Name.to_string name)
          (if OpamPackage.has_name t.installed name then "re" else "");
        if OpamPackage.Set.mem nv t.installed
        then reinstall_t ~ask:true [name, Some (`Eq,v)] t (* same version *)
        else install_t ~ask:true [name, Some (`Eq,v)] None false t (* != version or new *)
      | None ->
        try
          let nv = OpamPackage.max_version t.installed name in
          if OpamPackage.has_name (Lazy.force t.available_packages) name then
            (OpamGlobals.msg "%s needs to be reinstalled.\n"
               (OpamPackage.Name.to_string name);
             if OpamPackage.Set.mem nv (Lazy.force t.available_packages)
             then reinstall_t ~ask:true [name, Some (`Eq, OpamPackage.version nv)] t
             else upgrade_t ~ask:true [name, None] t)
          else
            (OpamGlobals.msg "%s needs to be removed.\n" (OpamPackage.to_string nv);
             (* Package no longer available *)
             remove_t ~ask:true ~autoremove:false ~force:false [name, None] t)
        with Not_found -> ()

    let get_upstream t name =
      try
        let nv = OpamPackage.max_version t.packages name in
        match OpamState.opam_opt t nv with
        | None -> raise Not_found
        | Some o -> match OpamFile.OPAM.dev_repo o with
          | None -> raise Not_found
          | Some pin -> pin
      with Not_found ->
        OpamGlobals.error_and_exit
          "\"dev-repo\" field missing in %s metadata, you'll need to specify \
           the pinning location"
          (OpamPackage.Name.to_string name)

    let pin name ?(edit=false) ?(action=true) pin_option_opt =
      let pin_option = match pin_option_opt with
        | Some o -> o
        | None ->
          let t = OpamState.load_state "pin-get-upstream" in
          get_upstream t name
      in
      let needs_reinstall = pin name pin_option in
      with_switch_backup "pin-reinstall" @@ fun t ->
      OpamGlobals.msg "\n";
      let nv = OpamState.pinned t name in
      ignore (OpamState.update_dev_package t nv);
      OpamGlobals.msg "\n";
      let needs_reinstall2 =
        let empty_opam = OpamState.has_empty_opam t nv in
        if empty_opam then
          OpamState.add_pinned_overlay ~template:true t name;
        if edit || empty_opam then
          OpamPinCommand.edit t name else None
      in
      if action then
        let t = OpamState.load_state "pin-reinstall-2" in
        if not (OpamPackage.has_name t.installed name) ||
           needs_reinstall <> None ||
           needs_reinstall2 <> None
        then post_pin_action t name

    let edit ?(action=true) name =
      with_switch_backup "pin-edit" @@ fun t ->
      match edit t name with
      | None -> ()
      | Some true ->
        if action then post_pin_action t name
      | Some false ->
        (* Version changed: reload the state to ensure consistency *)
        let t = OpamState.load_state "pin-edit-2" in
        if action then post_pin_action t name

    let unpin ?(action=true) name =
      if unpin name then
        with_switch_backup "pin-reinstall" @@ fun t ->
        if action then post_pin_action t name

    let list = list
  end

  module REPOSITORY = OpamRepositoryCommand
  module CONFIG     = OpamConfigCommand
  module SWITCH     = OpamSwitchCommand

end

let read_lock f =
  OpamState.check (Read_lock f)

let switch_lock f =
  OpamState.check (Switch_lock f)

let global_lock f =
  OpamState.check (Global_lock f)

(** We protect each main functions with a lock depending on its access
    on some read/write data. *)

module SafeAPI = struct

  let init = API.init

  let list ~print_short ~filter ~order ~depends_on
      ~exact_name ~case_sensitive pkg_str =
    read_lock (fun () ->
      API.list ~print_short ~filter ~order ~depends_on
        ~exact_name ~case_sensitive pkg_str
    )

  let info ~fields regexps =
    read_lock (fun () -> API.info ~fields regexps)

  let install names add_to_roots deps_only =
    switch_lock (fun () -> API.install names add_to_roots deps_only)

  let reinstall names =
    switch_lock (fun () -> API.reinstall names)

  let upgrade names =
    switch_lock (fun () -> API.upgrade names)

  let fixup () =
    switch_lock API.fixup

  let remove ~autoremove ~force names =
    switch_lock (fun () -> API.remove ~autoremove ~force names)

  let update ~repos_only repos =
    global_lock (fun () -> API.update ~repos_only repos)

  module CONFIG = struct

    let env ~csh ~sexp ~fish =
      read_lock (fun () -> API.CONFIG.env ~csh ~sexp ~fish)

    let setup local global =
      global_lock (fun () -> API.CONFIG.setup local global)

    let setup_list shell dot_profile =
      read_lock (fun () -> API.CONFIG.setup_list shell dot_profile)

    let exec command =
      read_lock (fun () -> API.CONFIG.exec command)

    let list names =
      read_lock (fun () -> API.CONFIG.list names)

    let variable var =
      read_lock (fun () -> API.CONFIG.variable var)

    let subst files =
      read_lock (fun () -> API.CONFIG.subst files)

  end

  module REPOSITORY = struct

    let list ~short =
      global_lock (fun () -> API.REPOSITORY.list ~short)

    let add name kind address ~priority =
      global_lock (fun () -> API.REPOSITORY.add name kind address ~priority)

    let remove name =
      global_lock (fun () -> API.REPOSITORY.remove name)

    let priority name ~priority =
      global_lock (fun () -> API.REPOSITORY.priority name ~priority)

  end

  module SWITCH = struct

    let switch ~quiet ~warning name =
      global_lock (fun () -> API.SWITCH.switch ~quiet ~warning name)

    let install ~quiet ~warning ~update_config switch ocaml_version =
      global_lock (fun () ->
        API.SWITCH.install ~quiet ~warning ~update_config switch ocaml_version)

    let import filename =
      switch_lock (fun () -> API.SWITCH.import filename)

    let export filename =
      read_lock (fun () -> API.SWITCH.export filename)

    let remove switch =
      global_lock (fun () -> API.SWITCH.remove switch)

    let reinstall switch =
      global_lock (fun () -> API.SWITCH.reinstall switch)

    let list ~print_short ~installed ~all =
      read_lock (fun () -> API.SWITCH.list ~print_short ~installed ~all)

    let show () =
      read_lock API.SWITCH.show

  end

  module PIN = struct

    let pin name ?edit ?action pin_option =
      switch_lock (fun () -> API.PIN.pin name ?edit ?action pin_option)

    let edit ?action name =
      switch_lock (fun () -> API.PIN.edit ?action name)

    let unpin ?action name =
      switch_lock (fun () -> API.PIN.unpin ?action name)

    let list () =
      read_lock API.PIN.list

  end

end
