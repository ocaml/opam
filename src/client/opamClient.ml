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

open OpamTypes
open OpamTypesBase
open OpamState.Types
open OpamStd.Op
open OpamPackage.Set.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "CLIENT" fmt
let slog = OpamConsole.slog

let s_not_installed = "--"

type package_details = {
  name: name;
  current_version: version;
  installed_version: version option;
  synopsis: string Lazy.t;
  descr: string Lazy.t;
  tags: string list;
  syntax: string list Lazy.t;
  libraries: string list Lazy.t;
  extension: (string * string) list Lazy.t;
  others: string list Lazy.t; (* words in lines in files *)
}

let details_of_package t name versions =
  let installed_version =
    try Some
          (OpamPackage.version
             (OpamPackage.Set.find
                (fun nv -> OpamPackage.name nv = name)
                t.installed))
    with Not_found -> None in
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
  let opam = OpamState.opam t nv in
  let tags = OpamFile.OPAM.tags opam in
  let syntax = lazy (
    OpamStd.List.filter_map (fun (s,filter) ->
        if OpamFilter.opt_eval_to_bool (OpamState.filter_env ~opam t) filter
        then Some s else None)
      (OpamFile.OPAM.syntax opam)) in
  let libraries = lazy (
    OpamStd.List.filter_map (fun (s,filter) ->
        if OpamFilter.opt_eval_to_bool (OpamState.filter_env ~opam t) filter
        then Some s else None)
      (OpamFile.OPAM.libraries opam)) in
  let extension = lazy (
    OpamStd.String.Map.fold (fun fld v acc ->
        (fld, OpamFormat.Print.value v) :: acc)
      (OpamFile.OPAM.extensions opam) []
    |> List.rev) in
  let others = lazy (
    match OpamState.repository_and_prefix_of_package t nv with
    | None  -> []
    | Some (repo, prefix) ->
      List.fold_left (fun acc filename ->
          let file = OpamRepositoryPath.packages repo prefix nv // filename in
          let file = OpamFile.Lines.safe_read file in
          List.flatten file @ acc
        ) [] OpamClientConfig.search_files
  ) in
  { name; current_version; installed_version;
    synopsis; descr; tags;
    syntax; libraries; extension; others; }

let details_of_package_regexps t packages ~exact_name ~case_sensitive regexps =
  log "names_of_regexp regexps=%a"
    (slog @@ OpamStd.List.to_string (fun x -> x)) regexps;
  (* the regexp can also simply be a package. *)
  let fix_versions =
    let fix_packages = OpamStd.List.filter_map OpamPackage.of_string_opt regexps in
    OpamPackage.to_map (packages %% (OpamPackage.Set.of_list fix_packages)) in
  let regexps =
    OpamStd.List.filter_map (fun str ->
      let re =
        match OpamPackage.of_string_opt str with
        | Some nv ->
          if OpamPackage.Set.mem nv packages then
            let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
            Re_glob.globx name
          else
            Re_glob.globx str
        | None   -> Re_glob.globx str in
      let re =
        if case_sensitive then re else Re.no_case re in
      try Some (Re.compile re)
      with Re_glob.Parse_error ->
        OpamConsole.error "%S is not a valid package descriptor." str;
        None
    ) regexps in
  let exact_match str =
    List.exists (fun re -> OpamStd.String.exact_match re str) regexps in
  let partial_match str =
    List.exists (fun re -> Re.execp re str) regexps in
  let partial_matchs strs =
    List.exists partial_match strs in
  let packages_map = OpamPackage.to_map packages in
  let packages_map =
    OpamPackage.Name.Map.fold (fun name versions map ->
        let versions =
          try OpamPackage.Version.Set.inter versions
                (OpamPackage.Name.Map.find name fix_versions)
          with Not_found -> versions in
        if OpamPackage.Version.Set.is_empty versions then map else
          OpamPackage.Name.Map.add name (details_of_package t name versions) map
    ) packages_map OpamPackage.Name.Map.empty in

  (* Filter the list of packages, depending on user predicates *)
  let packages_map =
    OpamPackage.Name.Map.filter
      (fun name
        { synopsis; descr; tags; syntax; libraries; extension; others; _ } ->
         regexps = []
         || exact_match (OpamPackage.Name.to_string name)
         || not exact_name &&
            (partial_match (OpamPackage.Name.to_string name)
             || partial_match (Lazy.force synopsis)
             || partial_match (Lazy.force descr)
             || partial_matchs tags
             || partial_matchs (Lazy.force libraries)
             || partial_matchs (Lazy.force syntax)
             || partial_matchs (List.map snd (Lazy.force extension))
             || partial_matchs (Lazy.force others))
      ) packages_map in
  packages_map

let with_switch_backup command f =
  let t = OpamState.load_state command OpamStateConfig.(!r.current_switch) in
  let file = OpamPath.Switch.backup t.root t.switch in
  OpamFilename.mkdir (OpamPath.Switch.backup_dir t.root t.switch);
  OpamFile.Export.write file (t.installed, t.installed_roots, t.pinned);
  try
    f t;
    OpamFilename.remove file (* We might want to keep it even if successful ? *)
  with
  | OpamStd.Sys.Exit 0 as e -> raise e
  | err ->
    OpamStd.Exn.register_backtrace err;
    let t1 = OpamState.load_state "switch-backup-err"
        OpamStateConfig.(!r.current_switch) in
    if OpamPackage.Set.equal t.installed t1.installed &&
       OpamPackage.Set.equal t.installed_roots t1.installed_roots then
      OpamFilename.remove file
    else
      (prerr_string
         (OpamStd.Format.reformat
            (Printf.sprintf
               "\nThe former state can be restored with:\n    \
                %s switch import %S\n%!"
               Sys.argv.(0) (OpamFilename.prettify file))));
    raise err

module API = struct

  (* Prints a list of package details in the 'opam list' format *)
  let print_list t ~uninst_versions ~short ~shortv ~order names =
    let get_version info =
      if uninst_versions then Some (info.current_version)
      else info.installed_version in
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
            ~depopts:true ~installed:false ~unavailable:true ~build:true
            universe packages
        in
        List.fold_left (fun acc nv ->
            try (OpamPackage.name nv, List.assoc nv packages_info) :: acc
            with Not_found -> acc
          ) [] packages in
    let roots = OpamPackage.names_of_packages t.installed_roots in
    List.iter (
      if short then
        fun (name, d) ->
          let name_str =
            if shortv then
              OpamPackage.to_string (OpamPackage.create name d.current_version)
            else
              OpamPackage.Name.to_string name
          in
          let colored_name =
            if OpamPackage.Name.Set.mem name roots then
              OpamConsole.colorise `underline name_str
            else name_str in
          Printf.printf "%s\n" colored_name
      else
        let synop_len =
          let col = OpamStd.Sys.terminal_columns () in
          max 0 (col - max_n - max_v - 4) in
        fun (name, info) ->
          let version = get_version info in
          let name_str = OpamPackage.Name.to_string name in
          let colored_name =
            if OpamConsole.color () && OpamPackage.Name.Set.mem name roots then
              OpamConsole.colorise `underline name_str
            else name_str in
          let sversion, colored_version, pinned = match version with
            | None   ->
              s_not_installed, OpamConsole.colorise `cyan s_not_installed, ""
            | Some v ->
              let vs = OpamPackage.Version.to_string v in
              if OpamState.pinned_opt t name = Some (OpamPackage.create name v)
              then
                vs, OpamConsole.colorise `blue vs,
                OpamConsole.colorise `blue " (pinned)"
              else if info.installed_version <> Some v then
                vs, OpamConsole.colorise `cyan vs, ""
              else
                vs, OpamConsole.colorise `magenta vs, ""
          in
          Printf.printf "%s  %s%s  %s\n"
            (OpamStd.Format.indent_left colored_name ~visual:name_str max_n)
            (OpamStd.Format.indent_right colored_version ~visual:sversion max_v)
            pinned
            (OpamStd.String.sub_at synop_len (Lazy.force info.synopsis))
    ) names

  let list ~print_short ~filter ~order ~exact_name ~case_sensitive
      ?(depends=[]) ?(reverse_depends=false) ?(recursive_depends=false)
      ?(resolve_depends=false) ?(depopts=false) ?depexts ?dev
      regexp =
    let t = OpamState.load_state "list"
        OpamStateConfig.(!r.current_switch) in
    let depends_mode = depends <> [] in
    let get_version name =
      (* We're generally not interested in the aggregated deps for all versions
         of the package. Take installed or max version only when there is no
         version constraint *)
      OpamState.get_package t name
    in
    let depends_atoms =
      let atoms = OpamSolution.sanitize_atom_list ~permissive:true t depends in
      if resolve_depends then atoms else
        List.map (function
            | _, Some _ as atom -> atom
            | n, None ->
              try OpamSolution.eq_atom n (OpamPackage.version (get_version n))
              with Not_found -> n, None)
          atoms
    in
    let depends = OpamState.packages_of_atoms t depends_atoms in
    let packages =
      if not depends_mode then t.packages
      else if resolve_depends then
        let universe =
          let u = OpamState.universe t Depends in
          match filter with
          | `all -> { u with u_available = u.u_packages }
          | `installed -> u
          | _ ->  { u with u_installed = OpamPackage.Set.empty;
                           u_installed_roots = OpamPackage.Set.empty }
        in
        let req = OpamSolver.request ~install:depends_atoms () in
        match
          OpamSolver.resolve universe ~orphans:OpamPackage.Set.empty req
        with
        | Success s -> OpamSolver.new_packages s
        | Conflicts cs ->
          if not print_short then
            OpamConsole.msg "No solution%s for %s:\n%s"
              (if depopts then " including optional dependencies" else "")
              (OpamFormula.string_of_atoms depends_atoms)
              (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
          OpamStd.Sys.exit 1
      else if recursive_depends then
        let universe = OpamState.universe t Depends in
        let deps =
          if reverse_depends then OpamSolver.reverse_dependencies
          else OpamSolver.dependencies in
        deps ~depopts
          ~installed:(filter=`installed)
          ~unavailable:(filter<>`installable)
          ~build:true
          universe depends
        |> OpamPackage.Set.of_list
      else if reverse_depends then
        let is_dependent_on deps nv =
          let opam = OpamState.opam t nv in
          let formula =
            OpamStateConfig.filter_deps ?dev (OpamFile.OPAM.depends opam) in
          let formula =
            if depopts then
              OpamFormula.ands
                [formula;
                 OpamStateConfig.filter_deps ?dev (OpamFile.OPAM.depopts opam)]
            else formula in
          let depends_on nv =
            let name = OpamPackage.name nv in
            let v = OpamPackage.version nv in
            List.exists (fun (n,_) -> name = n) (OpamFormula.atoms formula) &&
            OpamFormula.eval
              (fun (n,cstr) ->
                 n <> name ||
                 OpamFormula.eval
                   (fun (relop,vref) -> OpamFormula.eval_relop relop v vref)
                   cstr)
              formula in
          OpamPackage.Set.for_all depends_on deps
        in
        OpamPackage.Set.filter (is_dependent_on depends) t.packages
      else
      let deps nv =
        let opam = OpamState.opam t nv in
        let deps =
          OpamState.packages_of_atoms t @@ OpamFormula.atoms @@
          OpamStateConfig.filter_deps ?dev (OpamFile.OPAM.depends opam) in
        if depopts then
          deps ++ (OpamState.packages_of_atoms t @@ OpamFormula.atoms @@
                   OpamStateConfig.filter_deps ?dev (OpamFile.OPAM.depopts opam))
        else deps
      in
      OpamPackage.Set.fold (fun nv acc -> acc ++ deps nv)
        depends OpamPackage.Set.empty
    in
    let depends =
      (* Filter to keep only the relevant versions *)
      if resolve_depends then
        packages %% depends ++ depends %% t.installed
      else depends
    in
    let packages =
      if resolve_depends then packages else
        packages %% match filter with
        | `all         -> t.packages
        | `installed   -> t.installed
        | `roots       -> t.installed_roots
        | `installable ->
          t.installed ++
          OpamSolver.installable (OpamState.universe t Depends) in
    let packages =
      if resolve_depends then packages
      else if depexts <> None then packages ++ depends
      else if depends_mode then packages -- depends
      else packages
    in
    let details =
      details_of_package_regexps t packages ~exact_name ~case_sensitive regexp
    in
    if not print_short && not (OpamPackage.Set.is_empty packages) &&
       OpamPackage.Name.Map.is_empty details
    then
      OpamConsole.msg "No packages found.\n";
    match depexts with
    | Some tags_list ->
      let required_tags = OpamStd.String.Set.of_list tags_list in
      let packages =
        OpamPackage.Name.Map.fold (fun name details acc ->
            let nv = OpamPackage.create name details.current_version in
            OpamPackage.Set.add nv acc)
          details OpamPackage.Set.empty
      in
      if not print_short then
        OpamConsole.msg "# Known external dependencies for %s %s%s\n"
          (OpamStd.Format.pretty_list ?last:None @@
           List.map (OpamConsole.colorise `bold @* OpamPackage.to_string) @@
           OpamPackage.Set.elements packages)
          (if tags_list <> [] then "on " else "")
          (OpamConsole.colorise `cyan @@ String.concat "," tags_list);
      let depexts =
        OpamPackage.Set.fold (fun nv acc ->
            let opam = OpamState.opam t nv in
            match OpamFile.OPAM.depexts opam with
            | None -> acc
            | Some tags ->
              OpamStd.String.SetMap.fold (fun tags values acc ->
                  if tags_list = [] then
                    let line =
                      Printf.sprintf "%s: %s"
                        (String.concat " " (OpamStd.String.Set.elements tags))
                        (String.concat " " (OpamStd.String.Set.elements values))
                    in
                    OpamStd.String.Set.add line acc
                  else if OpamStd.String.Set.for_all
                      (fun tag -> OpamStd.String.Set.mem tag required_tags)
                      tags
                  then OpamStd.String.Set.union acc values
                  else acc)
                tags acc)
          packages OpamStd.String.Set.empty
      in
      OpamConsole.msg "%s\n" @@
      String.concat "\n" @@ OpamStd.String.Set.elements depexts
    | None ->
      let print_header () =
        if resolve_depends then
          OpamConsole.msg "# Consistent installation providing %s %s\n"
            (OpamStd.Format.pretty_list @@
             List.map (OpamConsole.colorise `bold @* OpamPackage.to_string) @@
             OpamPackage.Set.elements depends)
            (if filter = `installed then "from current install"
             else "from scratch")
        else
        let kind = match filter with
          | `roots
          | `installed -> "Installed"
          | `all       -> "Existing"
          | _          -> "Available" in
        let results =
          if not depends_mode then "" else
            Printf.sprintf " %s %s %s"
              (if recursive_depends then "recursively" else "directly")
              (if reverse_depends then "depending on" else "required by")
              (OpamStd.Format.pretty_list ~last:"or" @@
               List.map (OpamConsole.colorise `bold @* OpamPackage.to_string) @@
               OpamPackage.Set.elements depends)
        in
        OpamConsole.msg "# %s packages%s for %s:\n" kind results
          (OpamSwitch.to_string t.switch) in
      if not print_short && OpamPackage.Name.Map.cardinal details > 0 then
        print_header ();
      print_list t ~uninst_versions:depends_mode ~short:print_short
        ~shortv:resolve_depends ~order details;
      if regexp <> [] &&
         OpamPackage.Name.Map.is_empty details
      then OpamStd.Sys.exit 1

  let info ~fields ~raw_opam ~where atoms =
    let t = OpamState.load_state "info"
        OpamStateConfig.(!r.current_switch) in
    let atoms = OpamSolution.sanitize_atom_list t ~permissive:true atoms in
    let details =
      let map = OpamPackage.to_map (OpamState.packages_of_atoms t atoms) in
      OpamPackage.Name.Map.mapi (details_of_package t) map
    in

    let show_fields = List.length fields <> 1 in

    let print_one name
        { current_version; tags; syntax; libraries; extension; _ } =

      (* Compute the installed versions, for each switch *)
      let installed = OpamState.installed_versions t name in

      let installed_str =
        let one (nv, aliases) =
          Printf.sprintf "%s [%s]"
            (OpamPackage.Version.to_string (OpamPackage.version nv))
            (String.concat " " (List.map OpamSwitch.to_string aliases)) in
        String.concat ", " (List.map one (OpamPackage.Map.bindings installed)) in

      let nv = OpamPackage.create name current_version in
      let opam = OpamState.opam t nv in
      let opam_f () =
        (* The above gives the opam structure, but the location of the orig file
           is lost: re-compute *)
        let overlay = OpamPath.Switch.Overlay.opam t.root t.switch name in
        if OpamFilename.exists overlay &&
           OpamFile.OPAM.(version (read overlay)) = current_version
        then overlay else
        let global = OpamPath.opam t.root nv in
        if OpamFilename.exists global then global else
        match OpamState.repository_and_prefix_of_package t nv with
        | Some (repo,pfx) -> OpamRepositoryPath.opam repo pfx nv
        | None ->
          OpamSystem.internal_error "opam file location for %s not found"
            (OpamPackage.to_string nv)
      in
      if where then OpamConsole.msg "%s\n" (OpamFilename.to_string (opam_f ()));

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
          let revision =
            match repository_kind_of_pin_kind kind with
            | Some kind ->
              let repo = OpamRepositoryBackend.default () in
              let repo =
                {repo with
                 repo_kind = kind;
                 repo_root = OpamPath.Switch.dev_package t.root t.switch name;
                 repo_address = address_of_string @@ string_of_pin_option pin} in
              (match OpamProcess.Job.run (OpamRepository.revision repo) with
               | Some v -> Printf.sprintf " (%s)" (OpamPackage.Version.to_string v)
               | None -> "")
            | None -> ""
          in
          (if kind = `version then repo else []) @
          ["pinned", (string_of_pin_kind kind) ^ revision]
        with Not_found ->
          repo
      in

      let url = match OpamState.url t nv with
        | None   -> []
        | Some u ->
          let kind = string_of_repository_kind (OpamFile.URL.kind u) in
          let url = string_of_address (OpamFile.URL.url u) in
          let mirrors =
            OpamStd.List.to_string string_of_address (OpamFile.URL.mirrors u) in
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

      let mk empty to_string name field =
        let v = field opam in
        if empty = v then []
        else [name, to_string v] in

      let strings = mk [] (String.concat ", ") in
      let formula = mk Empty OpamFormula.to_string in
      let option f = mk None (function None -> "" | Some x -> f x) in

      let author   = strings "authors"  OpamFile.OPAM.author in
      let homepage = strings "homepage" OpamFile.OPAM.homepage in
      let bug_reports = strings "bug-reports" OpamFile.OPAM.bug_reports in
      let dev_repo = option string_of_pin_option "dev-repo" OpamFile.OPAM.dev_repo in
      let license  = strings "license"  OpamFile.OPAM.license in
      let doc      = strings "doc"      OpamFile.OPAM.doc in
      let tags     = strings "tags"     (fun _ -> tags) in
      let depends  = formula "depends"  (OpamStateConfig.filter_deps @* OpamFile.OPAM.depends) in
      let depopts  = formula "depopts"  (OpamStateConfig.filter_deps @* OpamFile.OPAM.depopts) in

      let libraries = strings "libraries" (fun _ -> Lazy.force libraries) in
      let syntax    = strings "syntax"    (fun _ -> Lazy.force syntax) in

      let os =
        mk Empty
          (OpamFormula.string_of_formula (fun (t,s) ->
               if t then s else "!"^s))
          "os" OpamFile.OPAM.os in

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
        @ bug_reports
        @ dev_repo
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
        @ Lazy.force extension
        @ descr in

      let all_fields = match fields with
        | [] when not (raw_opam || where) -> all_fields
        | f  -> List.filter (fun (d,_) -> List.mem d f) all_fields in

      List.iter (fun (f, desc) ->
        if show_fields then
          OpamConsole.msg "%s "
            (OpamConsole.colorise `blue (Printf.sprintf "%20s:" f));
        OpamConsole.msg "%s\n" desc
      ) all_fields;

      if raw_opam then OpamFile.OPAM.write_to_channel stdout opam
    in

    OpamPackage.Name.Map.iter print_one details

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
    let allnames = OpamPackage.names_of_packages all in
    let universe = OpamState.universe t (Reinstall OpamPackage.Set.empty) in
    (* Basic definition of orphan packages *)
    let orphans = t.installed -- Lazy.force t.available_packages in
    (* Restriction to the request-related packages *)
    let orphans = match changes with
      | None -> orphans
      | Some ch ->
        if OpamPackage.Set.is_empty orphans then orphans else
        let recompile_cone =
          OpamPackage.Set.of_list @@
          OpamSolver.reverse_dependencies
            ~depopts:true ~installed:true ~unavailable:true ~build:true
            universe ch
        in
        orphans %% recompile_cone
    in
    (* Pinned versions of packages remain always available *)
    let orphans = orphans -- OpamState.pinned_packages t in
    (* Splits between full orphans (no version left) and partial ones *)
    let full_partition orphans =
      let orphan_names = (* names for which there is no version left *)
        OpamPackage.Name.Set.diff
          allnames
          (OpamPackage.names_of_packages (all -- orphans)) in
      OpamPackage.Set.partition
        (fun nv -> OpamPackage.Name.Set.mem (OpamPackage.name nv) orphan_names)
        orphans
    in
    let full_orphans, orphan_versions = full_partition orphans in
    (* Closure *)
    let full_orphans, orphan_versions =
      if not transitive then full_orphans, orphan_versions else
        let rec add_trans full_orphans orphan_versions =
          (* fixpoint to check all packages with no available version *)
          let new_orphans =
            OpamPackage.Set.of_list @@
              OpamSolver.reverse_dependencies
                ~depopts:false ~installed:false ~unavailable:true ~build:true
                universe full_orphans
          in
          let full, versions = full_partition (new_orphans++orphan_versions) in
          if OpamPackage.Set.equal full_orphans full
          then full, versions
          else add_trans full versions
        in
        add_trans full_orphans orphan_versions
    in
    (* Installed packages outside the set of changes are otherwise safe:
       re-add them to the universe *)
    let t =
      if changes = None then t else
      let available_packages =
        lazy (Lazy.force t.available_packages ++
              (t.installed -- orphans)) in
      { t with available_packages } in
    log "Orphans: full %a, versions %a"
      (slog @@ OpamPackage.Name.Set.to_string @* OpamPackage.names_of_packages)
      full_orphans
      (slog OpamPackage.Set.to_string) orphan_versions;
    t, full_orphans, orphan_versions

  (* The internal "solver" needs some rewrites of the requests, to make them
     more explicit. This has no effect when using the external solver. *)
  let preprocessed_request t full_orphans orphan_versions
    ?wish_install ?wish_remove ?wish_upgrade ?criteria () =
    let request =
      OpamSolver.request ?install:wish_install ?remove:wish_remove
        ?upgrade:wish_upgrade ?criteria ()
    in
    if OpamCudf.external_solver_available () then request else
    let { wish_install; wish_remove; wish_upgrade; criteria; _ } = request in
    (* Convert install to upgrade when necessary, request roots installed *)
    let eqnames, neqnames =
      List.partition (function (_,Some(`Eq,_)) -> true | _ -> false)
        wish_install in
    let add_wish_install =
      List.rev_append eqnames
        (OpamSolution.atoms_of_packages
           (t.installed_roots %% Lazy.force t.available_packages)) in
    let base_packages =
      let comp = OpamState.compiler_comp t t.compiler in
      OpamFormula.to_conjunction (OpamFile.Comp.packages comp) in
    let base_packages =
      List.map (fun atom ->
          try OpamPackage.Set.find (OpamFormula.check atom) t.installed
              |> OpamSolution.eq_atom_of_package
          with Not_found -> atom)
        base_packages in
    let wish_install = List.rev_append add_wish_install wish_install in
    let wish_install = List.rev_append base_packages wish_install in
    let uninstalled_eqnames =
      List.filter (fun (name,_) -> not (OpamState.is_name_installed t name))
        eqnames in
    let wish_upgrade = List.rev_append neqnames wish_upgrade in
    let wish_upgrade = List.rev_append uninstalled_eqnames wish_upgrade in
    (* Remove orphans *)
    let wish_remove =
      OpamSolution.atoms_of_packages full_orphans @
      OpamSolution.eq_atoms_of_packages orphan_versions @
      wish_remove in
    let available =
      Lazy.force t.available_packages -- orphan_versions -- full_orphans in
    let still_available ?(up=false) (name,_ as atom) =
      let installed =
        if up then
          try Some (OpamPackage.version @@ OpamPackage.Set.choose_one @@
                    OpamPackage.packages_of_name t.installed name)
          with Not_found -> None
        else None in
       OpamPackage.Set.exists
        (fun p -> OpamFormula.check atom p &&
                  match installed with Some i -> OpamPackage.version p >= i
                                     | None -> true)
         available in
    let upgradeable, non_upgradeable =
      List.partition (still_available ~up:true) wish_upgrade in
    let wish_install =
      List.filter (still_available ~up:false)
        (non_upgradeable @ wish_install) in
    let wish_upgrade =
      List.filter (still_available ~up:true) upgradeable in
    let nrequest = { wish_install; wish_remove; wish_upgrade;
                     criteria; extra_attributes = [] } in
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
      OpamConsole.header_msg "Synchronising pinned packages";
      try
        let updated = OpamState.update_dev_packages t to_update in
        if OpamPackage.Set.is_empty updated then t
        else OpamState.load_state "reload-dev-package-updated"
            OpamStateConfig.(!r.current_switch)
      with e ->
        OpamStd.Exn.fatal e;
        t
    )

  let compute_upgrade_t atoms t =
    let names = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
    if atoms = [] then
      let to_reinstall = t.reinstall %% t.installed in
      let t, full_orphans, orphan_versions = orphans ~transitive:true t in
      let to_upgrade = t.installed -- full_orphans -- orphan_versions in
      let to_install = t.installed -- full_orphans in
      let requested = OpamPackage.Name.Set.empty in
      let action = Upgrade to_reinstall in
      requested,
      action,
      OpamSolution.resolve t action
        ~orphans:(full_orphans ++ orphan_versions)
        (preprocessed_request t full_orphans orphan_versions
           ~wish_install:(OpamSolution.atoms_of_packages to_install)
           ~wish_upgrade:(OpamSolution.atoms_of_packages to_upgrade)
           ~criteria:`Upgrade ())
    else
    let atoms =
      List.map (function
          | (n,None) ->
            (* force strict update for unchanged, non dev or pinned packages
               (strict update makes no sense for pinned packages which have
               a fixed version) *)
            (try
               let nv = OpamState.find_installed_package_by_name t n in
               if OpamState.is_dev_package t nv ||
                  OpamState.is_pinned t n ||
                  OpamPackage.Set.mem nv t.reinstall
               then (n, None)
               else
                 let atom = (n, Some (`Gt, OpamPackage.version nv)) in
                 if OpamPackage.Set.exists (OpamFormula.check atom)
                     (Lazy.force t.available_packages)
                 then atom
                 else (n, None)
             with Not_found -> (n,None))
          | atom -> atom
        ) atoms in
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
      OpamConsole.note "%s %s not installed, ignored.\n"
        (OpamStd.Format.pretty_list
           (List.rev_map OpamFormula.short_string_of_atom not_installed))
        (match not_installed with [_] -> "is" | _ -> "are");
    let t, full_orphans, orphan_versions = orphans ~changes:to_upgrade t in
    let to_remove = to_upgrade %% full_orphans in
    let to_upgrade = to_upgrade -- full_orphans in
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
    OpamSolution.resolve t action
      ~orphans:(full_orphans ++ orphan_versions)
      (preprocessed_request t full_orphans orphan_versions
         ~wish_remove:(OpamSolution.atoms_of_packages to_remove)
         ~wish_upgrade:upgrade_atoms
         ())

  let upgrade_t ?ask atoms t =
    log "UPGRADE %a"
      (slog @@ function [] -> "<all>" | a -> OpamFormula.string_of_atoms a)
      atoms;
    match compute_upgrade_t atoms t with
    | requested, _action, Conflicts cs ->
      log "conflict!";
      if not (OpamPackage.Name.Set.is_empty requested) then
        (OpamConsole.msg "%s"
           (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
         OpamStd.Sys.exit 3);
      let reasons, chains, cycles =
        OpamCudf.strings_of_conflict (OpamState.unavailable_reason t) cs in
      if cycles <> [] then begin
        OpamConsole.error
          "Dependency errors in the upgrade actions. Please update, and \
           report the following to the package maintainers if the error \
           persists:";
        OpamConsole.errmsg "%s\n%s\n"
          (OpamStd.Format.itemize (fun x -> x) cycles)
          "You may try upgrading packages individually to work around this."
      end else begin
        OpamConsole.warning
          "Upgrade is not possible because of conflicts or packages that \
           are no longer available:";
        OpamConsole.errmsg "%s" (OpamStd.Format.itemize (fun x -> x) reasons);
        if chains <> [] then
          OpamConsole.errmsg
            "The following dependencies are in cause:\n%s"
            (OpamStd.Format.itemize (fun x -> x) chains);
        if OpamCudf.external_solver_available () then
          OpamConsole.errmsg
            "\nYou may run \"opam upgrade --fixup\" to let OPAM fix the \
             current state.\n"
      end;
      OpamStd.Sys.exit 3
    | requested, action, Success solution ->
      let result = OpamSolution.apply ?ask t action ~requested solution in
      if result = Nothing_to_do then (
        let to_check =
          if OpamPackage.Name.Set.is_empty requested then t.installed
          else OpamPackage.packages_of_names t.installed requested
        in
        let latest =
          OpamPackage.Name.Set.fold (fun name acc ->
              OpamPackage.Set.add (OpamPackage.max_version t.packages name) acc)
            (OpamPackage.names_of_packages to_check)
            OpamPackage.Set.empty in
        let notuptodate = latest -- to_check in
        if OpamPackage.Set.is_empty notuptodate then
          OpamConsole.msg "Already up-to-date.\n"
        else
          (let hdmsg = "Everything as up-to-date as possible" in
           let unav = notuptodate -- Lazy.force t.available_packages in
           let unopt = notuptodate %% Lazy.force t.available_packages in
           let base =
             OpamPackage.packages_of_names unopt
               (OpamState.base_package_names t)
           in
           let unopt = unopt -- base in
           if (OpamConsole.verbose ()) && not (OpamPackage.Set.is_empty unav) then
             OpamConsole.formatted_msg
               "%s.\n\
                The following newer versions couldn't be installed:\n%s"
               hdmsg
               (OpamStd.Format.itemize (fun p ->
                    OpamState.unavailable_reason t
                      (OpamSolution.eq_atom
                         (OpamPackage.name p) (OpamPackage.version p)))
                   (OpamPackage.Set.elements unav))
           else
             OpamConsole.formatted_msg
               "%s (run with --verbose to show unavailable upgrades).\n" hdmsg;
           if not (OpamPackage.Set.is_empty unopt) then
             (OpamConsole.formatted_msg
                "The following would require downgrades or uninstalls, but \
                 you may upgrade them explicitly:\n%s"
                (OpamStd.Format.itemize OpamPackage.to_string
                   (OpamPackage.Set.elements unopt)));
          )
      );
      OpamSolution.check_solution t result

  let upgrade names =
    with_switch_backup "upgrade" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    let t = update_dev_packages_t atoms t in
    upgrade_t atoms t

  let fixup_t t =
    log "FIXUP";
    if not (OpamCudf.external_solver_available ()) then
      (OpamConsole.formatted_msg
         "Sorry, \"--fixup\" is not available without an external solver. \
          You'll have to select the packages to change or remove by hand, \
          or install aspcud or another solver on your system.\n";
       OpamStd.Sys.exit 1)
    else
    let t, full_orphans, orphan_versions = orphans ~transitive:true t in
    let action = Upgrade OpamPackage.Set.empty in
    let all_orphans = full_orphans ++ orphan_versions in
    let resolve pkgs =
      pkgs,
      OpamSolution.resolve t action ~orphans:all_orphans
        (OpamSolver.request
           ~install:(OpamSolution.atoms_of_packages pkgs)
           ~criteria:`Fixup
           ())
    in
    let is_success = function
      | _, Success _ -> true
      | _, Conflicts cs ->
        log "conflict: %a"
          (slog (OpamCudf.string_of_conflict @@ OpamState.unavailable_reason t))
          cs;
        false
    in
    let requested, solution =
      let s =
        log "fixup-1/ keep installed packages with orphaned versions and roots";
        resolve (t.installed_roots -- full_orphans ++ orphan_versions)
      in
      if is_success s then s else
      let s =
        log "fixup-2/ keep just roots";
        resolve (t.installed_roots -- full_orphans)
      in
      if is_success s then s else
      let s =
        log "fixup-3/ keep packages with orphaned versions";
        resolve orphan_versions
      in
      if is_success s then s else
      let s =
        log "fixup-4/ last resort: no constraints. This should never fail";
        resolve OpamPackage.Set.empty
      in
      s
      (* Could still fail with uninstallable base packages actually, but we
         can only fix so far *)
    in
    let result = match solution with
      | Conflicts cs -> (* ouch... *)
        OpamConsole.msg "%s"
          (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
        No_solution
      | Success solution ->
        let _, req_rm, _ = orphans ~transitive:false t in
        OpamSolution.apply ~ask:true t action
          ~requested:(OpamPackage.names_of_packages (requested ++ req_rm))
          solution
    in
    OpamSolution.check_solution t result

  let fixup () = with_switch_backup "fixup" fixup_t

  let update ~repos_only ~dev_only ?(no_stats=false) names =
    let t = OpamState.load_state ~save_cache:true "update"
        OpamStateConfig.(!r.current_switch) in
    log "UPDATE %a" (slog @@ String.concat ", ") names;
    let repositories =
      if dev_only then OpamRepositoryName.Map.empty
      else if names = [] then
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
        if names = [] then
          t.installed %% OpamState.dev_packages t
        else
          OpamPackage.Set.filter (fun nv ->
              let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
              let pkg = OpamPackage.to_string nv in
              List.exists (fun s -> s = name || s = pkg) names
            ) (OpamState.dev_packages t) in
    let dev_packages_need_update =
      not (OpamPackage.Set.is_empty dev_packages) in

    let valid_repositories =
      OpamStd.String.Set.of_list
        (List.rev_map OpamRepositoryName.to_string
           (OpamRepositoryName.Map.keys repositories)) in
    let valid_pinned_packages =
      OpamStd.String.Set.of_list
        (List.rev_map OpamPackage.Name.to_string
           (OpamPackage.Name.Map.keys t.pinned)) in
    let unknown_names, not_pinned =
      if names = [] then
        [], []
      else
        let all = OpamStd.String.Set.of_list names in
        let valid_names =
          OpamStd.String.Set.of_list
            (List.rev_map
               (OpamPackage.name @> OpamPackage.Name.to_string)
               (OpamPackage.Set.elements t.packages)) in
        let open OpamStd.String.Set.Op in
        let unknown_names = all -- valid_repositories -- valid_names in
        let not_pinned =
          (all %% valid_names)
          -- valid_pinned_packages
          -- valid_repositories
          -- (OpamPackage.Set.fold (fun nv acc ->
              OpamStd.String.Set.add (OpamPackage.name_to_string nv) acc)
              dev_packages OpamStd.String.Set.empty) in
        OpamStd.String.Set.elements unknown_names,
        OpamStd.String.Set.elements not_pinned in

    begin
      let valid_repositories =
        match OpamStd.String.Set.elements valid_repositories with
        | []  -> ""
        | [s] -> Printf.sprintf " Valid repository is %s." s
        | l   ->
          Printf.sprintf
            " Valid repositories are %s."
            (OpamStd.Format.pretty_list l) in
      match unknown_names with
      | []  -> ()
      | [s] ->
        OpamConsole.error_and_exit
          "Cannot update the repository %s.%s"
          s valid_repositories
      | _   ->
        OpamConsole.error_and_exit
          "Cannot update the repositories %s.%s"
          (OpamStd.Format.pretty_list unknown_names) valid_repositories
    end;
    begin
      let valid_pinned_packages =
        match OpamStd.String.Set.elements valid_pinned_packages with
        | []  -> ""
        | [s] -> Printf.sprintf "Only %s is currently pinned.\n" s
        | l   ->
          Printf.sprintf
            "The currently pinned packages are %s.\n"
            (OpamStd.Format.pretty_list l) in
      match not_pinned with
      | []  -> ()
      | [s] ->
        OpamConsole.msg
          "Cannot update the package %s because it is not pinned.\n%s"
          s valid_pinned_packages
      | _   ->
        OpamConsole.msg
          "Cannot update %s because none are pinned.%s\n"
          (OpamStd.Format.pretty_list not_pinned) valid_pinned_packages
    end;

    if repositories_need_update then (
      OpamConsole.header_msg "Updating package repositories";
      let repos = OpamRepositoryName.Map.values repositories in
      let command repo =
        OpamProcess.Job.ignore_errors ~default:(fun t -> t)
          ~message:("Could not update repository " ^
                    OpamRepositoryName.to_string repo.repo_name) @@
        OpamRepositoryCommand.update t repo
      in
      let t =
        OpamParallel.reduce
          ~jobs:(OpamState.dl_jobs t)
          ~command
          ~merge:(fun f1 f2 x -> f1 (f2 x))
          ~nil:(fun x -> x)
          repos
          t
      in
      let t, compiler_updates =
        let t = OpamRepositoryCommand.update_compiler_index t in
        t, OpamRepositoryCommand.fix_compiler_descriptions t
          ~verbose:(OpamCoreConfig.(!r.verbose_level) >= 2) in
      let package_updates =
        let t = OpamRepositoryCommand.update_package_index t in
        OpamRepositoryCommand.fix_package_descriptions t
          ~verbose:(OpamCoreConfig.(!r.verbose_level) >= 2) in

      (* If necessary, output a JSON file *)
      if OpamStateConfig.(!r.json_out <> None) then
        let json to_json update =
          `O [ ("created", to_json update.created);
               ("updated", to_json update.updated);
               ("deleted", to_json update.deleted);
               ("changed", to_json update.changed); ] in
        OpamJson.append "package-updates"
          (json OpamPackage.Set.to_json package_updates);
        OpamJson.append "compiler-updates"
          (json OpamCompiler.Set.to_json compiler_updates);
    );

    if dev_packages_need_update then (
      OpamConsole.header_msg "Synchronizing development packages";
      let updates =
        OpamRepositoryCommand.update_dev_packages ~verbose:(OpamConsole.verbose ())
          t dev_packages in
      OpamJson.append "dev-packages-updates" (OpamPackage.Set.to_json updates)
    );

    OpamState.Cache.remove ();
    let t =
      OpamState.load_state "dry-upgrade" OpamStateConfig.(!r.current_switch)
    in
    OpamState.Cache.save t;

    log "dry-upgrade";
    let broken_state_message ~need_fixup conflicts =
      let reasons, chains, _cycles =
        OpamCudf.strings_of_conflict (OpamState.unavailable_reason t) conflicts
      in
      OpamConsole.warning
        "A conflict was detected in your installation. \
         This can be caused by updated constraints or conflicts in your \
         installed packages:\n%s"
        (OpamStd.Format.itemize (fun x -> x) reasons);
      if chains <> [] then (
        OpamConsole.formatted_msg "The following dependencies are in cause:\n";
        List.iter (OpamConsole.msg "  - %s\n") chains);
      OpamConsole.formatted_msg
        "\nYou should run \"opam upgrade%s\" to resolve the situation.\n"
        (if need_fixup && OpamCudf.external_solver_available () then " --fixup"
         else "")
    in
    if not no_stats then
    let universe = OpamState.universe t (Upgrade OpamPackage.Set.empty) in
    match OpamSolver.check_for_conflicts universe with
    | Some cs ->
      let need_fixup = match compute_upgrade_t [] t with
        | _, _, Success _ -> false
        | _, _, Conflicts _ -> true
      in
      broken_state_message ~need_fixup cs
    | None ->
      match compute_upgrade_t [] t with
      | _, _, Success upgrade ->
        let stats = OpamSolver.stats upgrade in
        if OpamSolution.sum stats > 0 then
          OpamConsole.msg
            "\nUpdates available for %s, apply them with 'opam upgrade':\n\
             ===== %s =====\n"
            (OpamSwitch.to_string t.switch)
            (OpamSolver.string_of_stats stats)
      | _, _, Conflicts cs ->
        log "State isn't broken but upgrade fails: something might be wrong.";
        broken_state_message ~need_fixup:true cs

  let init repo compiler shell dot_profile update_config =
    log "INIT %a" (slog OpamRepositoryBackend.to_string) repo;
    let root = OpamStateConfig.(!r.root_dir) in
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
      OpamConsole.msg "OPAM has already been initialized.";
    ) else (
      if not root_empty then (
        OpamConsole.warning "%s exists and is not empty"
          (OpamFilename.Dir.to_string root);
        if not (OpamConsole.confirm "Proceed ?") then OpamStd.Sys.exit 1);
      try

        (* Check for the external dependencies *)
        let check_external_dep name =
          OpamSystem.command_exists name
        in
        OpamConsole.msg "Checking for available remotes: ";
        let repo_types =
          ["rsync", "rsync and local";
           "git", "git"; "hg", "mercurial"; "darcs", "darcs"]
        in
        let available_repos, unavailable_repos =
          List.partition (check_external_dep @* fst) repo_types in
        OpamConsole.msg "%s.%s\n"
          (match available_repos with
           | [] -> "none"
           | r -> String.concat ", " (List.map snd r))
          (if unavailable_repos = [] then " Perfect!" else
             "\n" ^ OpamStd.Format.itemize (fun (cmd,msg) ->
                 Printf.sprintf
                   "you won't be able to use %s repositories unless you \
                    install the %s command on your system."
                   msg (OpamConsole.colorise `bold cmd))
               unavailable_repos);
        if not (check_external_dep "aspcud") then
          OpamConsole.warning
            "Recommended external solver %s not found."
            (OpamConsole.colorise `bold "aspcud");
        let advised_deps =
          [OpamStateConfig.(Lazy.force !r.makecmd); "m4"; "cc"]
        in
        (match List.filter (not @* check_external_dep) advised_deps with
         | [] -> ()
         | missing ->
           OpamConsole.warning
             "Recommended dependencies -- \
              most packages rely on these:\n%s"
             (OpamStd.Format.itemize (OpamConsole.colorise `bold) missing));
        let fetch_cmd_user =
          let open OpamStd.Option.Op in
          match
            OpamStd.Env.getopt "OPAMCURL",
            OpamStd.Env.getopt "OPAMFETCH" >>| fun s ->
            OpamStd.String.split s ' '
          with
          | Some cmd, _ | _, Some (cmd::_) -> check_external_dep cmd
          | _ -> false
        in
        let required_deps =
          ["curl or wget",
           fetch_cmd_user ||
           check_external_dep "curl" ||
           check_external_dep "wget";
           "patch", check_external_dep "patch";
           "tar", check_external_dep "tar";
           "unzip", check_external_dep "unzip" ]
        in
        (match List.filter (not @* snd) required_deps with
         | [] -> ()
         | missing ->
           OpamConsole.error_and_exit
             "Missing dependencies -- \
              the following commands are required for OPAM to operate:\n%s"
             (OpamStd.Format.itemize (OpamConsole.colorise `bold @* fst) missing));

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
        log "updating repository state";
        let t = OpamState.load_state ~save_cache:false "init-1" switch in
        OpamConsole.header_msg "Fetching repository information";
        let t = OpamProcess.Job.run (OpamRepositoryCommand.update t repo) t in
        OpamRepositoryCommand.fix_descriptions t
          ~save_cache:false ~verbose:false;

        (* Load the partial state, and install the new compiler if needed *)
        log "updating package state";
        let quiet = (compiler = OpamCompiler.system) in
        OpamState.install_compiler t ~quiet switch compiler;

        (* Finally, load the complete state and install the compiler packages *)
        log "installing compiler packages";
        OpamSwitchCommand.install_packages switch compiler

      with e ->
        OpamStd.Exn.register_backtrace e;
        OpamConsole.error "Initialisation failed";
        OpamConsole.errmsg "%s\n" (Printexc.to_string e);
        if not (OpamConsole.debug ()) && root_empty then
          OpamFilename.rmdir root;
        raise e);
    let t = OpamState.load_state "init"
        OpamStateConfig.(!r.current_switch) in
    update_setup t


  (* Checks a request for [atoms] for conflicts with the orphan packages *)
  let check_conflicts t atoms =
    let changes = OpamState.packages_of_atoms t atoms in
    let t, full_orphans, orphan_versions = orphans ~changes t in
    (* packages which still have local data are OK for install/reinstall *)
    let has_no_local_data nv =
      not (OpamFilename.exists_dir (OpamPath.packages t.root nv)) in
    let full_orphans, full_orphans_with_local_data =
      OpamPackage.Set.partition has_no_local_data
        full_orphans in
    let orphan_versions, orphan_versions_with_local_data =
      OpamPackage.Set.partition has_no_local_data
        orphan_versions in
    let available = lazy (t.packages -- full_orphans -- orphan_versions) in
    let orphans = full_orphans ++ orphan_versions in
    let conflict_atoms =
      List.filter
        (fun (name,_ as a) ->
           not (OpamState.is_pinned t name) &&
           OpamPackage.Set.exists (OpamFormula.check a) orphans && (*optim*)
           not (OpamPackage.Set.exists (OpamFormula.check a) (* real check *)
                  (Lazy.force available)))
        atoms in
    if conflict_atoms <> [] then
      OpamConsole.error_and_exit
        "Sorry, these packages are no longer available \
         from the repositories: %s"
        (OpamStd.Format.pretty_list
           (List.map OpamFormula.string_of_atom conflict_atoms))
    else
      {t with available_packages = lazy
                (Lazy.force t.available_packages ++
                 full_orphans_with_local_data ++
                 orphan_versions_with_local_data )},
      full_orphans,
      orphan_versions

  let install_t ?ask atoms add_to_roots ~deps_only ~upgrade t =
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
              if not upgrade then
                OpamConsole.note
                  "Package %s is already installed (current version is %s)."
                  (OpamPackage.Name.to_string (OpamPackage.name nv))
                  (OpamPackage.Version.to_string (OpamPackage.version nv));
              t
            | Some true ->
              if OpamPackage.Set.mem nv t.installed_roots then
                OpamConsole.note
                  "Package %s is already installed as a root."
                  (OpamPackage.Name.to_string (OpamPackage.name nv));
              { t with installed_roots =
                         OpamPackage.Set.add nv t.installed_roots }
            | Some false ->
              if OpamPackage.Set.mem nv t.installed_roots then
                { t with installed_roots =
                           OpamPackage.Set.remove nv t.installed_roots }
              else
                (OpamConsole.note
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
        OpamConsole.msg
          "Adding %s to the list of installed roots.\n"
          (OpamStd.Format.pretty_list diff)
      else (
        let diff = current_roots -- t.installed_roots in
        let diff = OpamPackage.Set.elements diff in
        let diff = List.rev (List.rev_map OpamPackage.to_string diff) in
        OpamConsole.msg
          "Removing %s from the list of installed roots.\n"
          (OpamStd.Format.pretty_list diff)
      );
      let file = OpamPath.Switch.installed_roots t.root t.switch in
      OpamFile.Installed_roots.write file t.installed_roots;
    );

    let available_packages = Lazy.force t.available_packages in
    let available_packages =
      if deps_only then
        (* Assume the named packages are available *)
        OpamPackage.Name.Set.fold (fun name avail ->
            if OpamPackage.has_name available_packages name then avail
            else avail ++ OpamPackage.packages_of_name t.packages name)
          names available_packages
      else
        (OpamSolution.check_availability t available_packages atoms;
         available_packages) in
    let t = {t with available_packages = lazy available_packages} in

    let wish_upgrade =
      if upgrade then List.filter (fun at -> not (List.mem at pkg_new)) atoms
      else [] in

    if pkg_new <> [] || wish_upgrade <> [] then (

      let request =
        preprocessed_request t full_orphans orphan_versions
          ~wish_install:atoms ~wish_upgrade ();
      in
      let action =
        if wish_upgrade <> [] then Upgrade (OpamPackage.Set.of_list pkg_skip)
        (* Fixme: the above won't properly handle setting as a root *)
        else match add_to_roots, deps_only with
          | Some false, _ | None, true ->
            Install OpamPackage.Name.Set.empty
          | _ -> Install names in
      let solution =
        OpamSolution.resolve t action
          ~orphans:(full_orphans ++ orphan_versions)
          request in
      let solution = match solution with
        | Conflicts cs ->
          log "conflict!";
          OpamConsole.msg "%s"
            (OpamCudf.string_of_conflict (OpamState.unavailable_reason t) cs);
          No_solution
        | Success solution ->
          let solution =
            if deps_only then
              OpamSolver.filter_solution (fun nv ->
                  not (OpamPackage.Name.Set.mem (OpamPackage.name nv) names))
                solution
            else solution in
          OpamSolution.apply ?ask t action ~requested:names solution in
      OpamSolution.check_solution t solution
    )

  let install names add_to_roots ~deps_only ~upgrade =
    with_switch_backup "install" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list ~permissive:true t names in
    let t = update_dev_packages_t atoms t in
    install_t atoms add_to_roots ~deps_only ~upgrade t

  let remove_t ?ask ~autoremove ~force atoms t =
    log "REMOVE autoremove:%b %a" autoremove
      (slog OpamFormula.string_of_atoms) atoms;

    let t, full_orphans, orphan_versions =
      let changes =
        if autoremove then None
        else Some (OpamState.packages_of_atoms t atoms) in
      orphans ?changes t
    in

    let nothing_to_do = ref true in
    let packages, not_installed =
      get_installed_atoms t atoms in
    if not_installed <> [] then (
      if force then
        let force_remove atom =
          let candidates = OpamPackage.Set.filter (OpamFormula.check atom) t.packages in
          try
            let nv = OpamPackage.max_version candidates (fst atom) in
            OpamConsole.note "Forcing removal of (uninstalled) %s" (OpamPackage.to_string nv);
            OpamProcess.Job.run (OpamAction.remove_package ~metadata:false t nv);
            OpamAction.cleanup_package_artefacts t nv;
            nothing_to_do := false
          with Not_found ->
            OpamConsole.error "No package %s found for (forced) removal.\n"
              (OpamFormula.short_string_of_atom atom)
        in
        List.iter force_remove not_installed
      else
        OpamConsole.note "%s %s not installed.\n"
          (OpamStd.Format.pretty_list
             (List.map OpamFormula.short_string_of_atom not_installed))
          (match not_installed with [_] -> "is" | _ -> "are")
    );

    if autoremove || packages <> [] then (
      let packages = OpamPackage.Set.of_list packages in
      let universe = OpamState.universe t Remove in
      let to_remove =
        OpamPackage.Set.of_list
          (OpamSolver.reverse_dependencies ~build:true
             ~depopts:false ~installed:true universe packages) in
      let to_keep =
        (if autoremove then t.installed_roots else t.installed)
        -- to_remove -- full_orphans -- orphan_versions
      in
      let to_keep =
        OpamPackage.Set.of_list
          (OpamSolver.dependencies ~build:true
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
               (OpamSolver.dependencies ~build:true
                  ~depopts:true ~installed:true universe to_remove))
        else to_remove in
      let solution =
        OpamSolution.resolve_and_apply ?ask t Remove ~requested
          ~orphans:(full_orphans ++ orphan_versions)
          (OpamSolver.request
             ~install:(OpamSolution.eq_atoms_of_packages to_keep)
             ~remove:(OpamSolution.atoms_of_packages to_remove)
             ())
      in
      OpamSolution.check_solution t solution
    ) else if !nothing_to_do then
      OpamConsole.msg "Nothing to do.\n"

  let remove ~autoremove ~force names =
    with_switch_backup "remove" @@ fun t ->
    let atoms = OpamSolution.sanitize_atom_list t names in
    remove_t ~autoremove ~force atoms t

  let reinstall_t ?ask ?(force=false) atoms t =
    log "reinstall %a" (slog OpamFormula.string_of_atoms) atoms;

    let reinstall, not_installed =
      get_installed_atoms t atoms in
    let to_install =
      if not_installed <> [] then
        if
          force ||
          (OpamConsole.warning "%s %s not installed."
             (OpamStd.Format.pretty_list
                (List.map OpamFormula.short_string_of_atom not_installed))
             (match not_installed with [_] -> "is" | _ -> "are");
           OpamConsole.confirm "Install ?")
        then not_installed
        else OpamStd.Sys.exit 1
      else []
    in

    let reinstall = OpamPackage.Set.of_list reinstall in

    let atoms =
      to_install @ OpamSolution.eq_atoms_of_packages reinstall in

    let t, full_orphans, orphan_versions = check_conflicts t atoms in

    let requested =
      OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in

    let request =
      preprocessed_request t full_orphans orphan_versions
        ~wish_install:atoms
        ~criteria:`Fixup
        ()
    in

    let solution =
      OpamSolution.resolve_and_apply ?ask t (Reinstall reinstall) ~requested
        ~orphans:(full_orphans ++ orphan_versions)
        request in

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
      try match nv with
      | Some nv ->
        let v = OpamPackage.version nv in
        OpamConsole.msg "%s needs to be %sinstalled.\n"
          (OpamPackage.Name.to_string name)
          (if OpamPackage.has_name t.installed name then "re" else "");
        if OpamPackage.Set.mem nv t.installed then
          reinstall_t ~ask:true [name, Some (`Eq,v)] t (* same version *)
        else
          install_t ~ask:true [name, Some (`Eq,v)] None
            ~deps_only:false ~upgrade:false t
          (* != version or new *)
      | None ->
        try
          let nv = OpamPackage.max_version t.installed name in
          if OpamPackage.has_name (Lazy.force t.available_packages) name then
            (OpamConsole.msg "%s needs to be reinstalled.\n"
               (OpamPackage.Name.to_string name);
             if OpamPackage.Set.mem nv (Lazy.force t.available_packages)
             then reinstall_t ~ask:true [name, Some (`Eq, OpamPackage.version nv)] t
             else upgrade_t ~ask:true [name, Some (`Neq, OpamPackage.version nv)] t)
          else
            (OpamConsole.msg "%s needs to be removed.\n" (OpamPackage.to_string nv);
             (* Package no longer available *)
             remove_t ~ask:true ~autoremove:false ~force:false [name, None] t)
        with Not_found -> ()
      with e ->
        OpamConsole.note
          "Pinning command successful, but your installed packages \
           may be out of sync.";
        raise e

    let get_upstream t name =
      try
        let nv = OpamPackage.max_version t.packages name in
        match OpamState.opam_opt t nv with
        | None -> raise Not_found
        | Some o -> match OpamFile.OPAM.dev_repo o with
          | None -> raise Not_found
          | Some pin -> pin
      with Not_found ->
        OpamConsole.error_and_exit
          "\"dev-repo\" field missing in %s metadata, you'll need to specify \
           the pinning location"
          (OpamPackage.Name.to_string name)

    let pin name ?(edit=false) ?version ?(action=true) pin_option_opt =
      let pin_option = match pin_option_opt with
        | Some o -> o
        | None ->
          let t = OpamState.load_state "pin-get-upstream"
              OpamStateConfig.(!r.current_switch) in
          get_upstream t name
      in
      let needs_reinstall = pin name ?version pin_option in
      with_switch_backup "pin-reinstall" @@ fun t ->
      OpamConsole.msg "\n";
      let updated =
        OpamProcess.Job.run
          (OpamState.update_pinned_package t ?fixed_version:version name)
      in
      if not updated then
        (ignore (unpin ~state:t [name]);
         OpamStd.Sys.exit 1);
      OpamConsole.msg "\n";
      let opam_f = OpamPath.Switch.Overlay.opam t.root t.switch name in
      let empty_opam = OpamFile.OPAM.(
          empty = with_name_opt (with_version_opt (read opam_f) None) None
        ) in
      let needs_reinstall2 =
        if edit || empty_opam then
          try OpamPinCommand.edit t name
          with Not_found ->
            (OpamConsole.error "No valid metadata available.";
             ignore (unpin ~state:t [name]);
             OpamStd.Sys.exit 1)
        else None
      in
      if action then
        let t = OpamState.load_state "pin-reinstall-2" t.switch in
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
        let t = OpamState.load_state "pin-edit-2" t.switch in
        if action then post_pin_action t name

    let unpin ?(action=true) names =
      let reinstall = unpin names in
      if action && reinstall <> [] then
        with_switch_backup "pin-reinstall" @@ fun t ->
        let t,atoms =
          List.fold_left (fun (t,atoms) name ->
              try
                let nv = OpamPackage.max_version t.installed name in
                let avail = Lazy.force t.available_packages in
                if OpamPackage.Set.mem nv avail then
                  {t with reinstall = OpamPackage.Set.add nv t.reinstall},
                  (name, Some (`Eq, OpamPackage.version nv))::atoms
                else
                  t, (name, None)::atoms
              with Not_found -> t, atoms
            )
            (t,[]) names
        in
        upgrade_t ~ask:true atoms t

    let list = list
  end

  module REPOSITORY = OpamRepositoryCommand
  module CONFIG     = OpamConfigCommand
  module SWITCH     = OpamSwitchCommand

end

let read_lock f =
  OpamState.check (Read_lock f)

let switch_lock f =
  OpamState.check
    (Switch_lock ((fun () -> OpamStateConfig.(!r.current_switch)), f))

let global_lock f =
  OpamState.check (Global_lock f)

let global_then_switch_lock f =
  OpamState.check (Global_with_switch_cont_lock f)

(** We protect each main functions with a lock depending on its access
    on some read/write data. *)

module SafeAPI = struct

  let init = API.init

  let list ~print_short ~filter ~order ~exact_name ~case_sensitive
      ?depends ?reverse_depends ?recursive_depends ?resolve_depends
      ?depopts ?depexts ?dev
      pkg_str =
    read_lock (fun () ->
      API.list ~print_short ~filter ~order ~exact_name ~case_sensitive
        ?depends ?reverse_depends ?recursive_depends ?resolve_depends
        ?depopts ?depexts ?dev
        pkg_str
    )

  let info ~fields ~raw_opam ~where regexps =
    read_lock (fun () -> API.info ~fields ~raw_opam ~where regexps)

  let install names add_to_roots ~deps_only ~upgrade =
    switch_lock (fun () -> API.install names add_to_roots ~deps_only ~upgrade)

  let reinstall names =
    switch_lock (fun () -> API.reinstall names)

  let upgrade names =
    switch_lock (fun () -> API.upgrade names)

  let fixup () =
    switch_lock API.fixup

  let remove ~autoremove ~force names =
    switch_lock (fun () -> API.remove ~autoremove ~force names)

  let update ~repos_only ~dev_only ?no_stats repos =
    global_lock (fun () -> API.update ~repos_only ~dev_only ?no_stats repos)

  module CONFIG = struct

    let env ~csh ~sexp ~fish ~inplace_path =
      API.CONFIG.env ~csh ~sexp ~fish ~inplace_path

    let setup local global =
      global_lock (fun () -> API.CONFIG.setup local global)

    let setup_list shell dot_profile =
      read_lock (fun () -> API.CONFIG.setup_list shell dot_profile)

    let exec ~inplace_path command =
      API.CONFIG.exec ~inplace_path command

    let list names =
      read_lock (fun () -> API.CONFIG.list names)

    let set var value =
      switch_lock (fun () -> API.CONFIG.set var value)

    let expand str =
      read_lock (fun () -> API.CONFIG.expand str)

    let variable var =
      read_lock (fun () -> API.CONFIG.variable var)

    let subst files =
      read_lock (fun () -> API.CONFIG.subst files)

  end

  module REPOSITORY = struct

    let list ~short =
      read_lock (fun () -> API.REPOSITORY.list ~short)

    let add name kind address ~priority =
      global_lock (fun () -> API.REPOSITORY.add name kind address ~priority)

    let remove name =
      global_lock (fun () -> API.REPOSITORY.remove name)

    let priority name ~priority =
      global_lock (fun () -> API.REPOSITORY.priority name ~priority)

    let set_url name address =
      global_lock (fun () -> API.REPOSITORY.set_url name address)

  end

  module SWITCH = struct

    let switch ?compiler ~quiet ~warning name =
      global_then_switch_lock (fun () ->
        API.SWITCH.switch_cont ?compiler ~quiet ~warning name)

    let install ~quiet ~warning ~update_config switch ocaml_version =
      global_then_switch_lock (fun () ->
        API.SWITCH.install_cont ~quiet ~warning ~update_config switch ocaml_version)

    let import filename =
      switch_lock (fun () -> API.SWITCH.import filename)

    let export filename =
      read_lock (fun () -> API.SWITCH.export filename)

    let remove switch =
      global_lock (fun () -> API.SWITCH.remove switch)

    let reinstall switch =
      global_then_switch_lock (fun () ->
          switch, (fun () -> API.SWITCH.reinstall switch))

    let list ~print_short ~installed ~all =
      read_lock (fun () -> API.SWITCH.list ~print_short ~installed ~all)

    let show () =
      read_lock API.SWITCH.show

  end

  module PIN = struct

    let pin name ?edit ?version ?action pin_option =
      switch_lock (fun () -> API.PIN.pin name ?edit ?version ?action pin_option)

    let edit ?action name =
      switch_lock (fun () -> API.PIN.edit ?action name)

    let unpin ?action names =
      switch_lock (fun () -> API.PIN.unpin ?action names)

    let list ~short () =
      read_lock (fun () -> API.PIN.list ~short ())

  end

end
