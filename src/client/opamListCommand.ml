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
open OpamState.Types
open OpamStd.Op
open OpamPackage.Set.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "LIST" fmt
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
        | `installable -> t.installed ++ Lazy.force t.available_packages
        (* OpamSolver.installable (OpamState.universe t Depends) -- too expensive *)
    in
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

  let print_summary name =

    (* All the version of the package *)
    let versions = OpamPackage.versions_of_name t.packages name in

    (* Compute the installed versions, for each switch *)
    let installed = OpamState.installed_versions t name in

    let installed_str =
      let one (nv, aliases) =
        Printf.sprintf "%s [%s]"
          (OpamPackage.Version.to_string (OpamPackage.version nv))
          (String.concat " " (List.map OpamSwitch.to_string aliases)) in
      String.concat ", " (List.map one (OpamPackage.Map.bindings installed)) in

    let installed_version = match OpamPackage.Map.cardinal installed with
      | 0 -> [ "installed-version" , "none" ]
      | 1 -> [ "installed-version" , installed_str ]
      | _ -> [ "installed-versions", installed_str ] in

    let available_versions =
      let strings = List.map OpamPackage.Version.to_string
          (OpamPackage.Version.Set.elements versions) in
      match strings with
      | []  -> []
      | [v] -> [ "available-version" , v ]
      | l   -> [ "available-versions", String.concat ", " l ] in

    let all_fields =
      [ "package", OpamPackage.Name.to_string name ]
      @ installed_version
      @ available_versions
    in

    List.iter (fun (f, desc) ->
        if show_fields then
          OpamConsole.msg "%s "
            (OpamConsole.colorise `blue (Printf.sprintf "%20s:" f));
        OpamConsole.msg "%s\n" desc
      ) all_fields;
  in

  let print_one name
      { current_version; tags; syntax; libraries; extension; _ } =

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
        match OpamPackage.Name.Map.find name t.pinned with
        | Version v -> repo @ ["pinned", OpamPackage.Version.to_string v]
        | Source url ->
          let revision =
            let repo =
              { repo_name = OpamRepositoryName.of_string "tmp";
                repo_url = url;
                repo_priority = 0;
                repo_root = OpamPath.Switch.dev_package t.root t.switch name; }
            in
            (match OpamProcess.Job.run (OpamRepository.revision repo) with
             | Some v -> Printf.sprintf " (%s)" (OpamPackage.Version.to_string v)
             | None -> "")
          in
          ["pinned", OpamUrl.string_of_backend url.OpamUrl.backend ^ revision]
      with Not_found ->
        repo
    in

    let url = match OpamState.url t nv with
      | None   -> []
      | Some u ->
        let url = OpamFile.URL.url u in
        let mirrors =
          OpamStd.List.to_string OpamUrl.to_string (OpamFile.URL.mirrors u)
        in
        let checksum = OpamFile.URL.checksum u in
        [ "upstream-url" , OpamUrl.to_string url ]
        @ (if OpamFile.URL.mirrors u = [] then []
           else [ "upstream-mirrors" , mirrors ])
        @ [ "upstream-kind", OpamUrl.string_of_backend url.OpamUrl.backend ]
        @ match checksum with
        | None   -> []
        | Some c -> [ "upstream-checksum", c ] in

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
    let dev_repo = option OpamUrl.to_string "dev-repo" OpamFile.OPAM.dev_repo in
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
      [ "metadata of version", OpamPackage.Version.to_string version ]
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

  OpamPackage.Name.Map.iter (fun k v -> 
    print_summary k;
    OpamConsole.msg "----------------------------------------------------------------------\n";
    print_one k v
  )details
