(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes.FullPos
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
  post: bool;
  test: bool;
  dev_setup: bool;
  doc: bool;
  dev: bool;
}

let default_dependency_toggles = {
  recursive = false;
  depopts = false;
  build = true;
  post = false;
  test = false;
  dev_setup = false;
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
  fields = ["name"; "synopsis"; "descr"; "tags"];
  ext_fields = false;
}

type selector =
  | Any
  | Installed
  | Root
  | Compiler
  | Available
  | Installable
  | Pinned
  | Depends_on of dependency_toggles * atom list
  | Required_by of dependency_toggles * atom list
  | Conflicts_with of package list
  | Coinstallable_with of dependency_toggles * package list
  | Solution of dependency_toggles * atom list
  | Pattern of pattern_selector * string
  | Atoms of atom list
  | Flag of package_flag
  | Tag of string
  | From_repository of repository_name list
  | Owns_file of filename

let string_of_selector =
  let (%) s col = OpamConsole.colorise col s in
  function
  | Any -> "any" % `cyan
  | Installed -> "installed" % `cyan
  | Root -> "root" % `cyan
  | Compiler -> "base" % `cyan
  | Available -> "available" % `cyan
  | Installable -> "installable" % `cyan
  | Pinned -> "pinned" % `cyan
  | Depends_on (tog,atoms) ->
    Printf.sprintf "%s(%s)"
      ((if tog.recursive then "rec-depends-on" else "depends-on") % `blue)
      (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom atoms
       % `bold)
  | Required_by (tog,atoms) ->
    Printf.sprintf "%s(%s)"
      ((if tog.recursive then "rec-required-by" else "required-by") % `blue)
      (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom atoms
       % `bold)
  | Conflicts_with packages ->
    Printf.sprintf "%s(%s)"
      ("conflicts" % `blue)
      ((OpamStd.List.concat_map " " OpamPackage.to_string packages) % `bold)
  | Coinstallable_with (_,packages) ->
    Printf.sprintf "%s(%s)"
      ("coinstallable" % `blue)
      ((OpamStd.List.concat_map " " OpamPackage.to_string packages) % `bold)
  | Solution (_tog,atoms) ->
    Printf.sprintf "%s(%s)"
      ("solution" % `blue)
      (OpamStd.List.concat_map " " OpamFormula.short_string_of_atom atoms
       % `bold)
  | Pattern (sel,str) ->
    let str = if sel.exact then str else Printf.sprintf "*%s*" str in
    let fctname = if sel.glob then "match" else "exact-match" in
    let fctname =
      match sel.fields with
      | [] ->  Printf.sprintf "none-%s" fctname
      | [fld] ->  Printf.sprintf "%s-%s" fld fctname
      | _ -> fctname
    in
    Printf.sprintf "%s(%s)" (fctname % `green) (str % `bold)
  | Atoms atoms ->
    OpamStd.List.concat_map ~left:"(" ~right:")" " | "
      (fun a -> OpamFormula.short_string_of_atom a % `bold) atoms
  | Flag fl ->
    Printf.sprintf "%s(%s)" ("has-flag" % `green)
      (OpamTypesBase.string_of_pkg_flag fl % `bold)
  | Tag t ->
    Printf.sprintf "%s(%s)" ("has-tag" % `green) (t % `bold)
  | From_repository r ->
    Printf.sprintf "%s(%s)" ("from-repository" % `magenta)
      (OpamStd.List.concat_map " " OpamRepositoryName.to_string r % `bold)
  | Owns_file f ->
    Printf.sprintf "%s(%s)" ("owns-file" % `magenta)
      (OpamFilename.prettify f % `bold)

let string_of_formula =
  OpamFormula.string_of_formula string_of_selector

let get_opam st nv =
  match OpamSwitchState.opam_opt st nv with
  | Some o ->
    OpamFile.OPAM.(with_name nv.OpamPackage.name
                     (with_version nv.OpamPackage.version o))
  | None -> OpamFile.OPAM.create nv

let packages_of_atoms st atoms =
  atoms |>
  OpamSolution.sanitize_atom_list ~permissive:true st |>
  OpamFormula.packages_of_atoms (st.packages ++ st.installed)

let package_dependencies st tog nv =
  get_opam st nv |>
  OpamPackageVar.all_depends
    ~build:tog.build ~post:tog.post
    ~test:tog.test ~doc:tog.doc ~dev_setup:tog.dev_setup ~dev:tog.dev
    ~depopts:tog.depopts
    st

let atom_dependencies st tog atoms =
  atoms |>
  OpamFormula.packages_of_atoms (st.packages ++ st.installed) |> fun pkgs ->
  OpamPackage.Set.fold (fun nv acc ->
      OpamFormula.ors [acc; package_dependencies st tog nv])
    pkgs OpamFormula.Empty

let get_universe st ?requested tog =
  let requested =
    match requested with
    | Some r -> r
    | None -> st.packages
  in
  OpamSwitchState.universe st
    ~test:tog.test ~doc:tog.doc ~dev_setup:tog.dev_setup ~force_dev_deps:tog.dev
    ~requested Query

let rec value_strings value =
  let module SS = OpamStd.String.Set in
  match value with
  | Bool _ | Int _ -> SS.empty
  | Ident s -> SS.singleton s
  | String s -> SS.singleton s
  | Relop (_, v1, v2)
  | Logop (_, v1, v2)
  | Env_binding (v1, _, v2) ->
    SS.union (value_strings v1.pelem) (value_strings v2.pelem)
  | Prefix_relop (_, v) | Pfxop (_, v) ->
    value_strings v.pelem
  | List l | Group l ->
    List.fold_left (fun acc v -> SS.union acc (value_strings v.pelem))
      SS.empty l.pelem
  | Option (v, vl) ->
    List.fold_left (fun acc v -> SS.union acc (value_strings v.pelem))
      (value_strings v.pelem) vl.pelem

let pattern_selector patterns =
  let name_patt =
    { default_pattern_selector with exact = true; fields = ["name"] }
  in
  let version_patt =
    { default_pattern_selector with exact = true; fields = ["version"] }
  in
  OpamFormula.ors
    (List.map (fun patt ->
         match OpamStd.String.cut_at patt '.' with
         | None ->
           Atom (Pattern (name_patt, patt))
         | Some (name, version) ->
           OpamFormula.ands
             [Atom (Pattern (name_patt, name));
              Atom (Pattern (version_patt, version))])
        patterns)

let apply_selector ~base st = function
  | Any -> base
  | Installed -> st.installed
  | Root -> st.installed_roots
  | Compiler -> st.compiler_packages
  | Available -> Lazy.force st.available_packages
  | Installable ->
    OpamSolver.installable_subset
      (OpamSwitchState.universe st ~requested:OpamPackage.Set.empty Query)
      base
  | Pinned -> OpamPinned.packages st
  | (Required_by ({recursive=true; _} as tog, atoms)
    | Depends_on ({recursive=true; _} as tog, atoms)) as direction ->
    let deps_fun = match direction with
      | Required_by _ -> OpamSolver.dependencies
      | Depends_on _ -> OpamSolver.reverse_dependencies
      | _ -> assert false
    in
    deps_fun ~depopts:tog.depopts ~build:tog.build ~post:tog.post
      ~installed:false ~unavailable:true
      (get_universe st tog)
      (packages_of_atoms st atoms)
  | Required_by (tog, atoms) ->
    atom_dependencies st tog atoms |>
    OpamFormula.packages base
  | Depends_on (tog, atoms) ->
    let packages = packages_of_atoms st atoms in
    OpamPackage.Set.filter (fun nv ->
        OpamPackage.Set.exists
          (OpamFormula.verifies (package_dependencies st tog nv))
          packages)
      base
  | Conflicts_with packages ->
    OpamSwitchState.conflicts_with st (OpamPackage.Set.of_list packages)
      base
  | Coinstallable_with (tog, packages) ->
    let universe = get_universe st tog in
    let set = OpamPackage.Set.of_list packages in
    OpamSolver.coinstallable_subset universe set base
  | Solution (tog, atoms) ->
    let universe =
      let requested =
        OpamFormula.packages_of_atoms st.packages atoms
      in
      get_universe st tog ~requested
    in
    let universe =
      { universe
        with u_installed = OpamPackage.Set.empty;
             u_installed_roots = OpamPackage.Set.empty }
    in
    (match OpamSolver.resolve universe
             (OpamSolver.request ~install:atoms ()) with
    | Success s -> OpamSolver.new_packages s
    | Conflicts cs ->
      OpamConsole.error_and_exit `No_solution
        "No solution%s for %s: %s"
        (if tog.depopts then " including optional dependencies" else "")
        (OpamFormula.string_of_atoms atoms)
        (OpamCudf.string_of_conflicts st.packages
           (OpamSwitchState.unavailable_reason st) cs))
  | Pattern (psel, pat) ->
    let re =
      if psel.glob then Re.Glob.glob ~expand_braces:true pat
      else Re.str pat
    in
    let re = if psel.case_sensitive then Re.case re else Re.no_case re in
    let re = if psel.exact then Re.seq [Re.bos; re; Re.eos] else re in
    let re = Re.compile re in
    let content_strings nv =
      let opam = get_opam st nv in
      if psel.fields = [] then
        List.map (fun (_,v) -> value_strings v.pelem)
          (OpamFile.OPAM.to_list opam)
      else
      try
        List.map
          (fun f -> match OpamFile.OPAM.print_field_as_syntax f opam with
             | None -> OpamStd.String.Set.empty
             | Some v -> value_strings v.pelem)
          psel.fields
      with Not_found ->
        OpamConsole.error_and_exit `Bad_arguments
          "Unrecognised field in selection %s"
          (String.concat ", " psel.fields)
    in
    OpamPackage.Set.filter
      (fun nv -> List.exists (OpamStd.String.Set.exists (Re.execp re))
          (content_strings nv))
      base
  | Atoms atoms ->
    OpamFormula.packages_of_atoms base atoms
  | Flag f ->
    OpamPackage.Set.filter (fun nv ->
        get_opam st nv |> OpamFile.OPAM.has_flag f)
      base
  | Tag t ->
    OpamPackage.Set.filter (fun nv ->
        get_opam st nv |> List.mem t @* OpamFile.OPAM.tags)
      base
  | From_repository repos ->
    let rt = st.switch_repos in
    let rec aux = function
      | [] -> OpamPackage.Set.empty
      | r :: rl ->
        let packages =
          OpamPackage.keys (OpamRepositoryName.Map.find r rt.repo_opams)
        in
        if List.mem r repos then OpamPackage.Set.union packages (aux rl)
        else OpamPackage.Set.diff (aux rl) packages
    in
    aux (OpamSwitchState.repos_list st)
  | Owns_file file ->
    (try
       let root = st.switch_global.root in
       let switch =
         List.find (fun sw ->
             OpamFilename.remove_prefix (OpamPath.Switch.root root sw) file
             <> OpamFilename.to_string file)
           (OpamFile.Config.installed_switches st.switch_global.config)
       in
       let rel_name =
         OpamFilename.remove_prefix (OpamPath.Switch.root root switch) file
       in
       let matching_change_files =
         List.filter (fun change_f ->
             OpamFilename.check_suffix change_f ".changes" &&
             let changes =
               OpamFile.Changes.safe_read (OpamFile.make change_f)
             in
             OpamStd.String.Map.exists
               (fun f -> function
                  | OpamDirTrack.Removed -> false
                  | _ -> rel_name = f)
               changes)
           (OpamFilename.files (OpamPath.Switch.install_dir root switch))
       in
       let selections =
         if switch = st.switch then OpamSwitchState.selections st
         else OpamSwitchState.load_selections st.switch_global switch
       in
       List.fold_left (fun acc f ->
           let name =
             OpamPackage.Name.of_string @@
             OpamFilename.(Base.to_string (basename (chop_extension f)))
           in
           try
             OpamPackage.Set.add
               (OpamPackage.package_of_name selections.sel_installed name)
               acc
           with Not_found -> acc)
         OpamPackage.Set.empty matching_change_files
     with Not_found ->
       log "%a doesn't belong to a known opam switch"
         (slog OpamFilename.to_string) file;
       OpamPackage.Set.empty)


let rec filter ~base st = function
  | Empty -> base
  | Atom select -> base %% apply_selector ~base st select
  | Block b -> filter ~base st b
  | And (a, b) ->
    let base = filter ~base st a in
    filter ~base st b
  | Or (a, b) -> filter ~base st a ++ filter ~base st b

type output_format =
  | Name
  | Version
  | Package
  | Synopsis
  | Synopsis_or_target
  | Description
  | Field of string
  | Raw_field of string
  | Installed_version
  | Pinning_target
  | Source_hash
  | Raw
  | All_installed_versions
  | Available_versions
  | All_versions
  | Repository
  | Installed_files
  | VC_ref
  | Depexts

let default_list_format = [Name; Installed_version; Synopsis_or_target]

let disp_header = function
  | Name -> "Name"
  | Version -> "Version"
  | Package -> "Package"
  | Synopsis | Synopsis_or_target -> "Synopsis"
  | Description -> "Description"
  | Field s | Raw_field s -> String.capitalize_ascii s
  | Installed_version -> "Installed"
  | Pinning_target -> "Pin"
  | Source_hash -> "Source hash"
  | Raw -> "Metadata"
  | All_installed_versions -> "Installed versions"
  | Available_versions -> "Available versions"
  | All_versions -> "Versions"
  | Repository -> "Repository"
  | Installed_files -> "Installed files"
  | VC_ref -> "VC ref"
  | Depexts -> "Depexts"

let field_names = [
  Name, "name";
  Version, "version";
  Package, "package";
  Synopsis, "synopsis";
  Synopsis_or_target, "synopsis-or-target";
  Description, "description";
  Field "<field>", "<field>";
  Raw_field "<field>:", "<field>:";
  Installed_version, "installed-version";
  Pinning_target, "pin";
  Source_hash, "source-hash";
  Raw, "opam-file";
  All_installed_versions, "all-installed-versions";
  Available_versions, "available-versions";
  All_versions, "all-versions";
  Repository, "repository";
  Installed_files, "installed-files";
  VC_ref, "vc-ref";
  Depexts, "depexts";
]

let raw_field_names =
  List.filter (function Field _, _ -> false | _ -> true) field_names

let string_of_field ?(raw=false) = function
  | Field s -> if raw then s ^":" else s
  | Raw_field s -> s ^":"
  | f -> List.assoc f field_names

let field_of_string ~raw =
  let names_fields = List.map (fun (a,b) -> b, a) field_names in
  let opam_fields = List.map fst OpamFile.OPAM.fields in
  if raw then
    fun s -> Raw_field s
  else
  fun s ->
    if OpamStd.String.ends_with ~suffix:":" s then
      Raw_field (OpamStd.String.remove_suffix ~suffix:":" s)
    else
    try
      List.assoc s names_fields
    with Not_found ->
    match OpamStd.List.find_opt (fun x -> s = x) opam_fields with
    | Some f -> Field f
    | None -> OpamConsole.error_and_exit `Bad_arguments "No printer for %S" s

let version_color st nv =
  let installed = (* (in any switch) *)
    OpamGlobalState.installed_versions st.switch_global nv.name
  in
  let is_available nv = (* Ignore unavailability due to pinning *)
    try
      OpamFilter.eval_to_bool ~default:false
        (OpamPackageVar.resolve_switch_raw ~package:nv st.switch_global
           st.switch st.switch_config)
        (OpamFile.OPAM.available (get_opam st nv))
    with Not_found -> false
  in
  if OpamPackage.Set.mem nv st.installed then [`bold;`magenta] else
    (if OpamPackage.Map.mem nv installed then [`bold] else []) @
    (if is_available nv then [] else [`crossed;`red])

let mini_field_printer ?(prettify=false) ?(normalise=false) =
  let module OpamPrinter = OpamPrinter.FullPos in
  if normalise then OpamPrinter.Normalise.value else
  fun v -> match v.pelem with
  | String s when prettify -> s
  | List l when prettify &&
                     List.for_all (function {pelem=String _;_} -> true | _ -> false) l.pelem ->
    OpamStd.List.concat_map ", "  (function {pelem=String s;_} -> s | _ -> assert false) l.pelem
  | List l -> OpamPrinter.value_list l
  | _ -> OpamPrinter.Normalise.value v

let detail_printer ?prettify ?normalise ?(sort=false) st nv =
  let open OpamStd.Option.Op in
  let (%) s cols = OpamConsole.colorise' cols s in
  let root_sty =
    if OpamPackage.Set.mem nv st.installed_roots then [`underline]
    else []
  in
  let get_opam =
    if not sort then get_opam else
    fun st nv ->
      OpamFileTools.sort_opam (get_opam st nv)
  in
  function
  | Name -> OpamPackage.Name.to_string nv.name % (`bold :: root_sty)
  | Version -> OpamPackage.Version.to_string nv.version % version_color st nv
  | Package ->
    (OpamPackage.name_to_string nv % (`bold :: root_sty)) ^
    ("." ^ OpamPackage.version_to_string nv) % root_sty
  | Synopsis ->
    (get_opam st nv |>
     OpamFile.OPAM.descr >>| OpamFile.Descr.synopsis)
    +! ""
  | Synopsis_or_target ->
    (match OpamPinned.package_opt st nv.name with
     | Some nv ->
       let opam = get_opam st nv in
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
       (get_opam st nv |>
        OpamFile.OPAM.descr >>| OpamFile.Descr.synopsis)
       +! "")
  | Description ->
    (get_opam st nv |>
     OpamFile.OPAM.descr >>|
     OpamFile.Descr.body)
    +! ""
  | Raw_field f | Field f ->
    (try
       List.assoc f (OpamFile.OPAM.to_list (get_opam st nv)) |>
       mini_field_printer ?prettify ?normalise
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
      let opam = get_opam st nv in
      OpamStd.Option.to_string ~none:"--" OpamUrl.to_string
        (OpamFile.OPAM.get_url opam)
    else ""
  | Source_hash ->
    let hash_opt =
      let open OpamStd.Option.Op in
      OpamSwitchState.url st nv >>| OpamFile.URL.url >>= fun url ->
      OpamSwitchState.source_dir st nv |>
      OpamFilename.opt_dir >>= fun srcdir ->
      OpamProcess.Job.run (OpamRepository.revision srcdir url) >>|
      OpamPackage.Version.to_string
    in
    OpamStd.Option.default "" hash_opt
  | Raw -> OpamFile.OPAM.write_to_string (get_opam st nv)
  | All_installed_versions ->
    OpamGlobalState.installed_versions st.switch_global nv.name |>
    OpamPackage.Map.mapi (fun nv switches ->
        Printf.sprintf "%s [%s]"
          (OpamPackage.version_to_string nv % version_color st nv)
          (String.concat " " (List.map OpamSwitch.to_string switches))) |>
    OpamPackage.Map.values |>
    String.concat "  "
  | Available_versions ->
    let available =
      OpamPackage.packages_of_name (Lazy.force st.available_packages) nv.name
    in
    OpamStd.List.concat_map "  " (fun nv ->
        OpamPackage.Version.to_string nv.version % version_color st nv)
      (OpamPackage.Set.elements available)
  | All_versions ->
    let pkgs = OpamPackage.packages_of_name st.packages nv.name in
    OpamStd.List.concat_map "  " (fun nv ->
        OpamPackage.Version.to_string nv.version % version_color st nv)
      (OpamPackage.Set.elements pkgs)
  | Repository ->
    OpamRepositoryState.find_package_opt st.switch_repos
      (OpamSwitchState.repos_list st) nv |>
    OpamStd.Option.to_string (fun (r, _) -> OpamRepositoryName.to_string r)
  | Installed_files ->
    let changes_f =
      OpamPath.Switch.changes st.switch_global.root st.switch nv.name
    in
    (match OpamFile.Changes.read_opt changes_f with
     | None ->
       OpamConsole.error_and_exit `Not_found
         "Cannot list installed files: %s is not installed"
         (OpamPackage.Name.to_string nv.name)
     | Some c ->
       OpamStd.Format.itemize ~bullet:""
         (fun (file, status) ->
            OpamFilename.to_string file ^ match status with
            | `Unchanged -> ""
            | `Removed -> " (absent)" % [`red]
            | `Changed -> " (modified since)" % [`yellow])
         (OpamDirTrack.check
            (OpamPath.Switch.root st.switch_global.root st.switch)
            c))
  | VC_ref ->
    OpamStd.Option.Op.(
      (OpamSwitchState.url st nv >>| OpamFile.URL.url >>= fun url ->
       url.OpamUrl.hash)
      +! ""
    )
  | Depexts ->
    OpamStd.List.concat_map " " OpamSysPkg.to_string
      (OpamSysPkg.Set.elements (OpamSwitchState.depexts st nv))

type package_listing_format = {
  short: bool;
  header: bool;
  columns: output_format list;
  all_versions: bool;
  wrap: [`Wrap of string | `Truncate | `None] option;
  separator: string;
  value_printer: [`Normal | `Pretty | `Normalised];
  order: [`Standard | `Dependency | `Custom of package -> package -> int];
}

let default_package_listing_format = {
  short = false;
  header = true;
  columns = default_list_format;
  all_versions = false;
  wrap = None;
  separator = " ";
  value_printer = `Normal;
  order = `Standard;
}

let display st format packages =
  let packages =
    if format.all_versions then packages else
      OpamPackage.Name.Set.fold (fun name ->
          let pkgs = OpamPackage.packages_of_name packages name in
          let nv =
            let get = OpamPackage.Set.max_elt in
            try get (pkgs %% st.installed) with Not_found ->
            try get (pkgs %% st.pinned) with Not_found ->
            try get (pkgs %% Lazy.force st.available_packages) with Not_found ->
              get pkgs
          in
          OpamPackage.Set.add nv)
        (OpamPackage.names_of_packages packages)
        OpamPackage.Set.empty
  in
  let packages =
    if format.order = `Dependency then
      let universe =
        OpamSwitchState.universe st
          ~requested:packages
          Query
      in
      OpamSolver.dependency_sort ~depopts:true ~build:true ~post:false
        universe packages
    else match format.order with
      | `Custom o -> List.sort o (OpamPackage.Set.elements packages)
      | _ -> OpamPackage.Set.elements packages
  in
  let add_head l =
    if format.header then
      (List.map (fun f -> "# "^disp_header f) format.columns)
      :: l
    else l
  in
  let prettify = format.value_printer = `Pretty in
  let normalise = format.value_printer = `Normalised in
  if packages = [] then
    (if format.header then
       OpamConsole.errmsg "%s\n"
         (OpamConsole.colorise `red "# No matches found"))
  else
    List.rev_map (fun nv ->
        List.map (detail_printer ~prettify ~normalise st nv) format.columns)
      packages |>
    List.rev |>
    add_head |>
    OpamStd.Format.align_table |>
    OpamConsole.print_table ?cut:format.wrap stdout ~sep:format.separator

let get_switch_state gt rt =
  match OpamStateConfig.get_switch_opt () with
  | None -> OpamSwitchState.load_virtual gt rt
  | Some sw -> OpamSwitchState.load `Lock_none gt rt sw

let get_depexts st packages =
  OpamPackage.Name.Set.fold
    (fun name acc ->
       let nv = OpamSwitchState.get_package st name in
       let nv =
         if OpamPackage.Set.mem nv packages then nv else
           OpamPackage.Set.max_elt (OpamPackage.packages_of_name packages name)
       in
       OpamSysPkg.Set.union acc
         (OpamSwitchState.depexts st nv))
    (OpamPackage.names_of_packages packages)
    OpamSysPkg.Set.empty

let print_depexts =
  OpamSysPkg.Set.iter (fun d -> OpamConsole.msg "%s\n" (OpamSysPkg.to_string d))

let info st ~fields ~raw ~where ?normalise ?(show_empty=false)
    ?(all_versions=false) ?(sort=false) atoms =
  let packages =
    OpamFormula.packages_of_atoms ~disj:all_versions
      (st.packages ++ st.installed) atoms
  in
  let atoms, missing_atoms =
    List.partition (fun (n,_) -> OpamPackage.has_name packages n) atoms
  in
  if missing_atoms <> [] then
    (OpamConsole.error "No package matching %s found"
       (OpamStd.List.concat_map " or " OpamFormula.short_string_of_atom
          missing_atoms);
     if OpamPackage.Set.is_empty packages then
       OpamStd.Sys.exit_because `Not_found);
  let fields = List.map (field_of_string ~raw) fields in
  let all_versions_fields = [
    Name;
    All_installed_versions;
    All_versions;
  ] in
  let one_version_fields = [
    Version;
    Repository;
    Pinning_target;
    Source_hash;
    Field "url.src";
    Field "url.checksum";
    Field "homepage";
    Field "doc";
    Field "bug-reports";
    Field "dev-repo";
    Field "authors";
    Field "maintainer";
    Field "license";
    Field "tags";
    Field "flags";
    Field "depends";
    Field "depopts";
    Field "conflicts";
    Field "conflict-class";
    Field "depexts";
    Synopsis;
    Description;
  ] in
  let output_table fields nv =
    let tbl =
      List.fold_left (fun acc item ->
          let contents = detail_printer ?normalise ~sort st nv item in
          if show_empty || contents <> "" then
            [ OpamConsole.colorise `blue (string_of_field ~raw item); contents ]
            :: acc
          else acc)
        [] (List.rev fields)
    in
    let cut =
      match normalise with
      | Some true -> Some `None
      | _  -> None
    in
    OpamStd.Format.align_table tbl |>
    OpamConsole.print_table ?cut stdout ~sep:" ";
  in
  let header pkg =
    if fields = [] && not raw && not where then
      (OpamConsole.header_msg "%s: information on all versions"
         (OpamPackage.Name.to_string pkg.name);
       output_table all_versions_fields pkg)
  in
  let output_package pkg =
    let opam = get_opam st pkg in
    let opam =
      if not sort then opam else
        OpamFileTools.sort_opam opam
    in
    OpamFile.OPAM.print_errors opam;
    if where then
      OpamConsole.msg "%s\n"
        (match OpamFile.OPAM.metadata_dir opam with
         | Some (None, dir) -> Filename.concat dir "opam"
         | Some (Some repo, rdir) ->
           let repo_dir = OpamRepositoryPath.root st.switch_global.root repo in
           let tar = OpamRepositoryPath.tar st.switch_global.root repo in
           if OpamFilename.exists tar &&
              not (OpamFilename.exists_dir repo_dir) then
             Printf.sprintf "<%s>%s%s"
               (OpamFilename.to_string tar)
               Filename.dir_sep
               rdir
           else
             OpamFilename.Dir.to_string OpamFilename.Op.(repo_dir / rdir)
         | None -> "<nowhere>")
    else if raw && fields = [] then
      OpamFile.OPAM.write_to_channel stdout opam
    else
    match fields with
    | [] ->
      OpamConsole.header_msg "Version-specific details";
      output_table one_version_fields pkg
    | [f] -> OpamConsole.msg "%s\n" (detail_printer ?normalise ~sort st pkg f)
    | fields -> output_table fields pkg
  in
  List.iter (fun (name,_) ->
      (* Like OpamSwitchState.get_package, but restricted to [packages] *)
      let nvs = OpamPackage.packages_of_name packages name in
      if OpamPackage.Set.is_singleton nvs || all_versions then
        (header (OpamPackage.Set.max_elt nvs);
         OpamPackage.Set.iter output_package nvs)
      else
        (let choose =
           try OpamPackage.Set.choose (nvs %% st.pinned) with Not_found ->
           try OpamPackage.Set.choose (nvs %% st.installed) with Not_found ->
           try OpamPackage.Set.max_elt (nvs %% Lazy.force st.available_packages)
           with Not_found ->
             OpamPackage.Set.max_elt nvs
         in
         header choose;
         output_package choose)
    ) atoms
