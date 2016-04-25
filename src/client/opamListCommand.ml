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
open OpamStateTypes
open OpamStd.Op
open OpamPackage.Set.Op

let log fmt = OpamConsole.log "LIST" fmt
let slog = OpamConsole.slog

type dependency_toggles = {
  recursive: bool;
  depopts: bool;
  build: bool;
  test: bool;
  doc: bool;
  dev: bool;
}

let default_dependency_toggles = {
  recursive = false;
  depopts = false;
  build = true;
  test = false;
  doc = false;
  dev = false;
}

type pattern_selector = {
  case_sensitive: bool;
  exact: bool;
  glob: bool;
  fields: string list;
  ext_fields: bool;
}

let default_pattern_selector = {
  case_sensitive = false;
  exact = false;
  glob = true;
  fields = ["name"; "descr"; "flags"];
  ext_fields = false;
}

type selector =
  | Installed
  | Root
  | Available
  | Installable
  | Depends_on of dependency_toggles * atom list
  | Required_by of dependency_toggles * atom list
  | Solution of dependency_toggles * atom list
  | Pattern of pattern_selector * string
  | Atoms of atom list
  | Flag of package_flag

let string_of_selector = function
  | Installed -> "installed"
  | Root -> "root"
  | Available -> "available"
  | Installable -> "installable"
  | Depends_on (tog,atoms) ->
    Printf.sprintf "%sdepends-on(%s)"
      (if tog.recursive then "rec-" else "")
      (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom atoms)
  | Required_by (tog,atoms) ->
    Printf.sprintf "%srequired-by(%s)"
      (if tog.recursive then "rec-" else "")
      (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom atoms)
  | Solution (_tog,atoms) ->
    Printf.sprintf "solution(%s)"
      (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom atoms)
  | Pattern (_,str) -> Printf.sprintf "matches(%S)" str
  | Atoms atoms ->
    OpamStd.List.concat_map ~left:"(" ~right:")" " | "
      OpamFormula.short_string_of_atom atoms
  | Flag fl ->
    Printf.sprintf "has-flag(%s)" (OpamTypesBase.string_of_pkg_flag fl)

let string_of_formula =
  OpamFormula.string_of_formula string_of_selector

let packages_of_atoms st atoms =
  atoms |>
  OpamSolution.sanitize_atom_list ~permissive:true st |>
  OpamSwitchState.packages_of_atoms st

let package_dependencies st tog nv =
  OpamSwitchState.opam st nv |>
  OpamPackageVar.all_depends
    ~build:tog.build ~test:tog.test ~doc:tog.doc ~dev:tog.dev
    ~depopts:tog.depopts
    st

let atom_dependencies st tog atoms =
  atoms |>
  OpamSwitchState.packages_of_atoms st |> fun pkgs ->
  OpamPackage.Set.fold (fun nv acc ->
      OpamFormula.ors [acc; package_dependencies st tog nv])
    pkgs OpamFormula.Empty

let get_universe st tog =
  let universe = OpamSwitchState.universe st Depends in
  { universe with
    u_test = tog.test;
    u_doc = tog.doc;
    u_dev = st.packages }

let rec value_strings value =
  let module SS = OpamStd.String.Set in
  match value with
  | Bool _ | Int _ -> SS.empty
  | Ident (_, s) -> SS.singleton s
  | String (_, s) -> SS.singleton s
  | Relop (_, _, v1, v2)
  | Logop (_, _, v1, v2)
  | Env_binding (_, v1, _, v2) ->
    SS.union (value_strings v1) (value_strings v2)
  | Prefix_relop (_, _, v)
  | Pfxop (_, _, v) ->
    value_strings v
  | List (_, l)
  | Group (_, l) ->
    List.fold_left (fun acc v -> SS.union acc (value_strings v))
      SS.empty l
  | Option (_, v, vl) ->
    List.fold_left (fun acc v -> SS.union acc (value_strings v))
      (value_strings v) vl

let apply_selector st = function
  | Installed -> st.installed
  | Root -> st.installed_roots
  | Available -> Lazy.force st.available_packages
  | Installable -> OpamSolver.installable (OpamSwitchState.universe st Depends)
  | (Required_by ({recursive=true; _} as tog, atoms)
    | Depends_on ({recursive=true; _} as tog, atoms)) as direction ->
    let deps_fun = match direction with
      | Required_by _ -> OpamSolver.dependencies
      | Depends_on _ -> OpamSolver.reverse_dependencies
      | _ -> assert false
    in
    deps_fun ~depopts:tog.depopts ~build:tog.build
      ~installed:false ~unavailable:true
      (get_universe st tog)
      (packages_of_atoms st atoms)
    |> OpamPackage.Set.of_list
  | Required_by (tog, atoms) ->
    atom_dependencies st tog atoms |>
    OpamFormula.atoms |>
    OpamSwitchState.packages_of_atoms st
  | Depends_on (tog, atoms) ->
    let packages = packages_of_atoms st atoms in
    OpamPackage.Set.filter (fun nv ->
        let deps = package_dependencies st tog nv |> OpamFormula.atoms in
        OpamPackage.Set.exists
          (fun nv -> List.exists (fun at -> OpamFormula.check at nv) deps)
          packages)
      st.packages
  | Solution (tog, atoms) ->
    let universe = get_universe st tog in
    let universe =
      { universe
        with u_installed = OpamPackage.Set.empty;
             u_installed_roots = OpamPackage.Set.empty }
    in
    (match OpamSolver.resolve universe ~orphans:OpamPackage.Set.empty
             (OpamSolver.request ~install:atoms ()) with
    | Success s -> OpamSolver.new_packages s
    | Conflicts cs ->
      failwith @@
      Printf.sprintf "No solution%s for %s:\n%s"
        (if tog.depopts then " including optional dependencies" else "")
        (OpamFormula.string_of_atoms atoms)
        (OpamCudf.string_of_conflict
           (OpamSwitchState.unavailable_reason st) cs))
  | Pattern (psel, pat) ->
    let re =
      if psel.glob then Re_glob.glob ~expand_braces:true pat
      else Re.str pat
    in
    let re = if psel.case_sensitive then Re.case re else re in
    let re = if psel.exact then Re.seq [Re.bos; re; Re.eos] else re in
    let re = Re.compile re in
    let content_strings nv =
      OpamSwitchState.opam st nv |>
      OpamFile.OPAM.to_list |>
      OpamStd.List.filter_map (fun (f, v) ->
          if psel.fields = [] ||
             List.mem f psel.fields ||
             psel.ext_fields && OpamStd.String.starts_with ~prefix:"x-" f
          then Some (value_strings v)
          else None)
    in
    OpamPackage.Set.filter
      (fun nv -> List.exists (OpamStd.String.Set.exists (Re.execp re))
          (content_strings nv))
      st.packages
  | Atoms atoms ->
    OpamSwitchState.packages_of_atoms st atoms
  | Flag f ->
    OpamPackage.Set.filter (fun nv ->
        OpamSwitchState.opam st nv |> OpamFile.OPAM.has_flag f)
      st.packages

let rec filter st = function
  | Empty -> st.packages
  | Atom select -> apply_selector st select
  | Block b -> filter st b
  | And (a, b) -> filter st a %% filter st b
  | Or (a, b) -> filter st a ++ filter st b

type output_format =
  | Name
  | Package
  | Synopsis
  | Synopsis_or_target
  | Field of string
  | Installed_version
  | Pinning_target
  | Raw
  | All_installed_versions
  | Available_versions
  | All_versions

let default_list_format = [Name; Installed_version; Synopsis_or_target]

let disp_header = function
  | Name -> "Name"
  | Package -> "Package"
  | Synopsis | Synopsis_or_target -> "Synopsis"
  | Field s -> s
  | Installed_version -> "Installed"
  | Pinning_target -> "Pin"
  | Raw -> "Metadata"
  | All_installed_versions -> "Installed versions"
  | Available_versions -> "Available versions"
  | All_versions -> "Versions"

let detail_printer st nv =
  let open OpamStd.Option.Op in
  let (%) s cols = OpamConsole.colorise' cols s in
  let root_sty =
    if OpamPackage.has_name st.installed_roots nv.name then [`underline]
    else []
  in
  function
  | Name -> OpamPackage.Name.to_string nv.name % (`bold :: root_sty)
  | Package ->
    (OpamPackage.name_to_string nv % (`bold :: root_sty)) ^
    ("." ^ OpamPackage.version_to_string nv) % root_sty
  | Synopsis ->
    (OpamSwitchState.opam st nv |>
     OpamFile.OPAM.descr >>| OpamFile.Descr.synopsis)
    +! ""
  | Synopsis_or_target ->
    (match OpamPinned.package_opt st nv.name with
     | Some nv ->
       let opam = OpamSwitchState.opam st nv in
       if Some opam = OpamPackage.Map.find_opt nv st.repos_package_index then
         Printf.sprintf "pinned to version %s"
           (OpamPackage.Version.to_string nv.version % [`blue])
       else
         Printf.sprintf "pinned to version %s at %s"
           (OpamPackage.Version.to_string nv.version % [`blue])
           (OpamStd.Option.to_string ~none:"(local metadata only)"
              (fun u -> OpamUrl.to_string u % [`underline])
              (OpamFile.OPAM.get_url opam))
     | None ->
       (OpamSwitchState.opam st nv |>
        OpamFile.OPAM.descr >>| OpamFile.Descr.synopsis)
       +! "")
  | Field f ->
    (try List.assoc f (OpamFile.OPAM.to_list (OpamSwitchState.opam st nv)) |>
         OpamFormat.Print.value
     with Not_found -> "")
  | Installed_version ->
    (try OpamPackage.package_of_name st.installed nv.name |> fun inst_nv ->
         OpamPackage.version_to_string inst_nv |> fun s ->
         if OpamPackage.Set.mem inst_nv st.pinned then s % [`blue] else
         if OpamPackage.has_name st.pinned nv.name then s % [`bold;`red] else
         if nv <> inst_nv &&
            not (OpamPackage.Set.mem inst_nv st.compiler_packages)
         then s % [`bold;`yellow] else
           s % [`magenta]
     with Not_found -> "--" % [`cyan])
  | Pinning_target ->
    if OpamPackage.Set.mem nv st.pinned then
      let opam = OpamSwitchState.opam st nv in
      OpamStd.Option.to_string ~none:"--" OpamUrl.to_string
        (OpamFile.OPAM.get_url opam)
    else ""
  | Raw -> OpamFile.OPAM.write_to_string (OpamSwitchState.opam st nv)
  | All_installed_versions ->
    OpamGlobalState.installed_versions st.switch_global nv.name |>
    OpamPackage.Map.mapi (fun nv switches ->
        Printf.sprintf "%s [%s]" (OpamPackage.version_to_string nv)
          (String.concat " " (List.map OpamSwitch.to_string switches))) |>
    OpamPackage.Map.values |>
    String.concat ", "
  | Available_versions ->
    let available =
      OpamPackage.packages_of_name (Lazy.force st.available_packages) nv.name
    in
    let installed =
      OpamGlobalState.installed_versions st.switch_global nv.name
    in
    OpamStd.List.concat_map " " (fun nv ->
        OpamPackage.Version.to_string nv.version %
        if OpamPackage.Map.mem nv installed then [`bold] else [])
      (OpamPackage.Set.elements available)
  | All_versions ->
    let pkgs = OpamPackage.packages_of_name st.packages nv.name in
    let installed =
      OpamGlobalState.installed_versions st.switch_global nv.name
    in
    let is_available nv = (* Ignore unavailability due to pinning *)
      OpamFilter.eval_to_bool ~default:false
        (OpamPackageVar.resolve_switch_raw st.switch_global
           st.switch st.switch_config)
        (OpamFile.OPAM.available (OpamSwitchState.opam st nv))
    in
    OpamStd.List.concat_map " " (fun nv ->
        OpamPackage.Version.to_string nv.version %
        (if OpamPackage.Set.mem nv st.installed then [`bold;`magenta] else
           (if OpamPackage.Map.mem nv installed then [`bold] else []) @
           (if is_available nv then [] else [`crossed;`red])))
      (OpamPackage.Set.elements pkgs)

let display st ~format ~dependency_order ~all_versions packages =
  let packages =
    if all_versions then packages else
      OpamPackage.Name.Map.fold (fun n vs acc ->
          OpamPackage.Set.add
            (OpamPackage.create n (OpamPackage.Version.Set.max_elt vs))
            acc)
        (OpamPackage.to_map packages)
        OpamPackage.Set.empty
  in
  let packages =
    if dependency_order then
      let universe = OpamSwitchState.universe st Depends in
      let deps_packages =
        OpamSolver.dependencies
          ~depopts:true ~installed:false ~unavailable:true ~build:true
          universe packages
      in
      List.filter (fun nv -> OpamPackage.Set.mem nv packages) deps_packages
    else
      OpamPackage.Set.elements packages
  in
  List.rev_map (fun nv -> List.map (detail_printer st nv) format) packages |>
  List.rev |>
  OpamStd.Format.align_table |>
  OpamStd.Format.print_table ~cut:`Truncate stdout ~sep:" "

let load_maybe gt =
  let rt = OpamRepositoryState.load `Lock_none gt in
  match OpamStateConfig.(!r.current_switch) with
  | None -> OpamSwitchState.load_virtual gt rt
  | Some sw -> OpamSwitchState.load `Lock_none gt rt sw

let list gt
    ~print_short ~filter:filter_arg ~order ~exact_name ~case_sensitive
    ?depends
    ?(reverse_depends=false) ?(recursive_depends=false) ?(resolve_depends=false)
    ?(depopts=false) ?depexts ?(dev=false)
    patterns
  =
  let module F = OpamFormula in
  let filter_f =
    match filter_arg with
    | `all -> F.Empty
    | `installed -> F.Atom Installed
    | `roots -> F.Atom Root
    | `installable -> F.Atom Available (* /!\ *)
  in
  let filter_deps =
    match depends with
    | None | Some [] -> F.Empty
    | Some deps ->
      let tog = {
        recursive = recursive_depends;
        depopts;
        build=true;
        test=false;
        doc=false;
        dev
      } in
      let atom =
        if resolve_depends then Solution (tog, deps)
        else if reverse_depends
        then Depends_on (tog, deps)
        else Required_by (tog, deps)
      in
      if depexts <> None then
        F.ors [F.Atom atom; F.Atom (Atoms deps)]
      else F.Atom atom
  in
  let patt_f =
    F.ors @@
    List.map (fun pat ->
        F.Atom (Pattern ({case_sensitive; exact = exact_name;
                          fields =
                            if exact_name then ["name"]
                            else ["name"; "descr"; "tags"];
                          glob = true;
                          ext_fields = false}, pat)))
      patterns
  in
  let formula = F.ands [filter_f; filter_deps; patt_f] in
  let st = load_maybe gt in
  log "Package selector: %a" (slog string_of_formula) formula;
  let packages = filter st formula in
  if OpamPackage.Set.is_empty packages then
    (if not print_short then OpamConsole.error "No matches";
     OpamStd.Sys.exit 1);
  let format = if print_short then [Name] else default_list_format in
  match depexts with
  | None ->
    display st ~format ~dependency_order:(order=`depends)
      ~all_versions:false packages
  | Some tags_list ->
    let required_tags = OpamStd.String.Set.of_list tags_list in
    OpamPackage.Name.Set.fold (fun name acc ->
        let nv = OpamSwitchState.get_package st name in
        let nv =
          if OpamPackage.Set.mem nv packages then nv else
            OpamPackage.Set.max_elt (OpamPackage.packages_of_name packages name)
        in
        let opam = OpamSwitchState.opam st nv in
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
      (OpamPackage.names_of_packages packages)
      OpamStd.String.Set.empty
    |> OpamStd.String.Set.iter print_endline

let info gt ~fields ~raw_opam ~where atoms =
  let st = load_maybe gt in
  let packages = OpamSwitchState.packages_of_atoms st atoms in
  let all_versions nv =
    OpamConsole.header_msg "%s: information on all versions"
      (OpamPackage.Name.to_string nv.name);
    let tbl =
      List.map (fun item -> [disp_header item; detail_printer st nv item]) [
        Name;
        All_installed_versions;
        All_versions;
      ]
    in
    OpamStd.Format.align_table tbl |>
    OpamStd.Format.print_table stdout ~sep:" ";
  in
  let one_version nv =
    OpamConsole.header_msg "%s: version-specific details"
      (OpamPackage.to_string nv)
  in
  OpamPackage.names_of_packages packages |>
  OpamPackage.Name.Set.iter (fun name ->
      if where then
      let nvs = OpamPackage.packages_of_name packages name in
      let choose =
        try OpamPackage.Set.choose (nvs %% st.pinned) with Not_found ->
        try OpamPackage.Set.choose (nvs %% st.installed) with Not_found ->
        try OpamPackage.Set.max_elt (nvs %% Lazy.force st.available_packages)
        with Not_found ->
          OpamPackage.Set.max_elt nvs
      in
      all_versions choose;
      one_version choose)

(*

type package_details = {
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

let load_maybe gt =
  let rt = OpamRepositoryState.load `Lock_none gt in
  match OpamStateConfig.(!r.current_switch) with
  | None -> OpamSwitchState.load_virtual gt rt
  | Some sw -> OpamSwitchState.load `Lock_none gt rt sw

let details_of_package t name versions =
  let installed_version =
    try Some
          (OpamPackage.version
             (OpamPackage.Set.find
                (fun nv -> nv.name = name)
                t.installed))
    with Not_found -> None in
  let current_version = match installed_version with
    | Some v when OpamPackage.Version.Set.mem v versions -> v
    | _ -> OpamPackage.Version.Set.max_elt versions in
  let nv = OpamPackage.create name current_version in
  let descr_f = lazy (
    OpamSwitchState.descr t nv
  ) in
  let synopsis = lazy (
    OpamFile.Descr.synopsis (Lazy.force descr_f)
  ) in
  let descr = lazy (
    OpamFile.Descr.full (Lazy.force descr_f)
  ) in
  let opam = OpamSwitchState.opam t nv in
  let tags = OpamFile.OPAM.tags opam in
  let syntax = lazy (
    OpamStd.List.filter_map (fun (s,filter) ->
        if OpamFilter.opt_eval_to_bool (OpamPackageVar.resolve ~opam t) filter
        then Some s else None)
      (OpamFile.OPAM.syntax opam)) in
  let libraries = lazy (
    OpamStd.List.filter_map (fun (s,filter) ->
        if OpamFilter.opt_eval_to_bool (OpamPackageVar.resolve ~opam t) filter
        then Some s else None)
      (OpamFile.OPAM.libraries opam)) in
  let extension = lazy (
    OpamStd.String.Map.fold (fun fld v acc ->
        (fld, OpamFormat.Print.value v) :: acc)
      (OpamFile.OPAM.extensions opam) []
    |> List.rev) in
  let others = lazy (
    match OpamFile.OPAM.metadata_dir opam with
    | None  -> []
    | Some dir ->
      List.fold_left (fun acc filename ->
          let file =
            OpamFile.Lines.safe_read (OpamFile.make (dir // filename))
          in
          List.flatten file @ acc
        ) [] OpamClientConfig.search_files
  ) in
  { current_version; installed_version;
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
            let name = OpamPackage.Name.to_string nv.name in
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
      let universe = OpamSwitchState.universe t Depends in
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
          try (nv.name, List.assoc nv packages_info) :: acc
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
          if OpamPinned.package_opt t name = Some (OpamPackage.create name v)
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

  let list gt ~print_short ~filter ~order ~exact_name ~case_sensitive
      ?(depends=[]) ?(reverse_depends=false) ?(recursive_depends=false)
      ?(resolve_depends=false) ?(depopts=false) ?depexts ?dev
      regexp =
    let st = load_maybe gt in
    let depends_mode = depends <> [] in
    let get_version name =
      (* We're generally not interested in the aggregated deps for all versions
         of the package. Take installed or max version only when there is no
         version constraint *)
      OpamSwitchState.get_package st name
    in
    let depends_atoms =
      let atoms = OpamSolution.sanitize_atom_list ~permissive:true st depends in
      if resolve_depends then atoms else
        List.map (function
            | _, Some _ as atom -> atom
            | n, None ->
              try OpamSolution.eq_atom n (OpamPackage.version (get_version n))
              with Not_found -> n, None)
          atoms
    in
    let depends = OpamSwitchState.packages_of_atoms st depends_atoms in
    let packages =
      if not depends_mode then st.packages
      else if resolve_depends then
        let universe =
          let u = OpamSwitchState.universe st Depends in
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
              (OpamCudf.string_of_conflict (OpamSwitchState.unavailable_reason st) cs);
          OpamStd.Sys.exit 1
      else if recursive_depends then
        let universe = OpamSwitchState.universe st Depends in
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
          let opam = OpamSwitchState.opam st nv in
          let atoms =
            OpamFormula.atoms
              (OpamPackageVar.all_depends ?dev ~depopts st opam)
          in
          let depends_on nv =
            List.exists (fun atom -> OpamFormula.check atom nv) atoms
          in
          OpamPackage.Set.for_all depends_on deps
        in
        OpamPackage.Set.filter (is_dependent_on depends) st.packages
      else
      let deps nv =
        let opam = OpamSwitchState.opam st nv in
        OpamSwitchState.packages_of_atoms st @@
        OpamFormula.atoms @@
        OpamPackageVar.all_depends ?dev ~depopts st opam
      in
      OpamPackage.Set.fold (fun nv acc -> acc ++ deps nv)
        depends OpamPackage.Set.empty
    in
    let depends =
      (* Filter to keep only the relevant versions *)
      if resolve_depends then
        packages %% depends ++ depends %% st.installed
      else depends
    in
    let packages =
      if resolve_depends then packages else
        packages %% match filter with
        | `all         -> st.packages
        | `installed   -> st.installed
        | `roots       -> st.installed_roots
        | `installable -> st.installed ++ Lazy.force st.available_packages
        (* OpamSolver.installable (OpamSwitchState.universe st Depends) -- too expensive *)
    in
    let packages =
      if resolve_depends then packages
      else if depexts <> None then packages ++ depends
      else if depends_mode then packages -- depends
      else packages
    in
    let details =
      details_of_package_regexps st packages ~exact_name ~case_sensitive regexp
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
            let opam = OpamSwitchState.opam st nv in
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
          (OpamSwitch.to_string st.switch) in
      if not print_short && OpamPackage.Name.Map.cardinal details > 0 then
        print_header ();
      print_list st ~uninst_versions:depends_mode ~short:print_short
        ~shortv:resolve_depends ~order details;
      if regexp <> [] &&
         OpamPackage.Name.Map.is_empty details
      then OpamStd.Sys.exit 1

let info gt ~fields ~raw_opam ~where atoms =
  let t = load_maybe gt in
  let atoms = OpamSolution.sanitize_atom_list t ~permissive:true atoms in
  let details =
    let map = OpamPackage.to_map (OpamSwitchState.packages_of_atoms t atoms) in
    OpamPackage.Name.Map.mapi (details_of_package t) map
  in

  let show_fields = List.length fields <> 1 in

  let print_summary name =

    (* All the version of the package *)
    let versions = OpamPackage.versions_of_name t.packages name in

    (* Compute the installed versions, for each switch *)
    let installed = OpamGlobalState.installed_versions t.switch_global name in

    let installed_str =
      let one (nv, aliases) =
        Printf.sprintf "%s [%s]"
          (OpamPackage.Version.to_string nv.version)
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
    let opam = OpamSwitchState.opam t nv in

    if where then begin
      match OpamFile.OPAM.metadata_dir opam with
      | Some dir ->
        OpamConsole.msg "%s\n" (OpamFilename.to_string (dir // "opam"))
      | None ->
        OpamSystem.internal_error "opam file location for %s not found"
          (OpamPackage.to_string nv)
    end;

    (* where does it come from (eg. which repository) *)
    let repository =
      let repo =
        match OpamRepositoryState.find_package_opt t.switch_repos
                (OpamRepositoryState.repos_list t.switch_repos) nv with
        | None -> []
        | Some (r,_) -> [ "repository", OpamRepositoryName.to_string r ]
      in
      if OpamPackage.Set.mem nv t.pinned then
        if OpamPackage.Map.find_opt nv t.repos_package_index = Some opam then
          repo @ ["pinned", OpamPackage.Version.to_string nv.version]
        else
        match OpamFile.OPAM.url opam with
        | None ->
          repo @ ["pinned",
                  Printf.sprintf "%s (locally defined)"
                    (OpamPackage.Version.to_string nv.version)]
        | Some urlf ->
          let url = OpamFile.URL.url urlf in
          let revision =
            let repo =
              { repo_name = OpamRepositoryName.of_string "tmp";
                repo_url = url;
                repo_priority = 0;
                repo_root = OpamPath.Switch.dev_package t.switch_global.root
                    t.switch name; }
            in
            OpamProcess.Job.run (OpamRepository.revision repo)
          in
          let msg =
            Printf.sprintf "%s %s%s"
              (OpamPackage.Version.to_string nv.version)
              (OpamUrl.string_of_backend url.OpamUrl.backend)
              (OpamStd.Option.to_string OpamPackage.Version.to_string revision)
          in
          ["pinned", msg ]
      else repo
    in

    let url = match OpamSwitchState.url t nv with
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
    let formula = mk Empty OpamFilter.string_of_filtered_formula in
    let option f = mk None (function None -> "" | Some x -> f x) in

    let author   = strings "authors"  OpamFile.OPAM.author in
    let homepage = strings "homepage" OpamFile.OPAM.homepage in
    let bug_reports = strings "bug-reports" OpamFile.OPAM.bug_reports in
    let dev_repo = option OpamUrl.to_string "dev-repo" OpamFile.OPAM.dev_repo in
    let license  = strings "license"  OpamFile.OPAM.license in
    let doc      = strings "doc"      OpamFile.OPAM.doc in
    let tags     = strings "tags"     (fun _ -> tags) in
    let depends  = formula "depends"  OpamFile.OPAM.depends in
    let depopts  = formula "depopts"  OpamFile.OPAM.depopts in

    let libraries = strings "libraries" (fun _ -> Lazy.force libraries) in
    let syntax    = strings "syntax"    (fun _ -> Lazy.force syntax) in

    let descr =
      let d = OpamSwitchState.descr t nv in
      ["description", OpamFile.Descr.full d] in

    let version = nv.version in

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
      if not (raw_opam || where) then (
        OpamConsole.header_msg "Information on all versions";
        print_summary k;
        OpamConsole.header_msg "Version-specific details";
      );
      print_one k v
  )details
*)
