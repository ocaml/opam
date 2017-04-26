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

open OpamStd.Op

open OpamTypes
open OpamStateTypes

(* Lists of defined variables, for documentation *)

let global_variable_names = [
  "opam-version",         "The currently running opam version";
  "switch",               "The local name (alias) of the current switch";
  "jobs",                 "The number of parallel jobs set up in opam \
                           configuration";
  "arch",                 "The current arch, as returned by \"uname -m\"";
  "root",                 "The current opam root directory";
]

let package_variable_names = [
  "name",      "Name of the package";
  "version",   "Version of the package";
  "depends",   "Resolved direct dependencies of the package";
  "installed", "Whether the package is installed";
  "enable",    "Takes the value \"enable\" or \"disable\" depending on whether \
                the package is installed";
  "pinned",    "Whether the package is pinned";
  "bin",       "Binary directory for this package";
  "sbin",      "System binary directory for this package";
  "lib",       "Library directory for this package";
  "man",       "Man directory for this package";
  "doc",       "Doc directory for this package";
  "share",     "Share directory for this package";
  "etc",       "Etc directory for this package";
  "build",     "Directory where the package was built";
  "hash",      "Hash of the package archive";
  "dev",       "True if this is a development package";
]

let predefined_depends_variables =
  List.map OpamVariable.Full.of_string [
    "build"; "with-test"; "with-doc"; "dev";
  ]

let resolve_global gt full_var =
  let module V = OpamVariable in
  if V.Full.(scope full_var <> Global) then None else
  let var = V.Full.variable full_var in
  match V.Full.read_from_env full_var with
  | Some _ as c -> c
  | None ->
    match OpamVariable.Map.find_opt var gt.global_variables with
    | Some (lazy (Some _ as some), _) -> some
    | _ ->
      match V.to_string var with
      | "opam-version"  -> Some (V.string OpamVersion.(to_string current))
      | "jobs"          -> Some (V.int (OpamFile.Config.jobs gt.config))
      | "root"          -> Some (V.string (OpamFilename.Dir.to_string gt.root))
      | _               -> None

(** Resolve switch-global variables only, as allowed by the 'available:'
    field *)
let resolve_switch_raw ?package gt switch switch_config full_var =
  let module V = OpamVariable in
  let var = V.Full.variable full_var in
  let allowed_package_variables =
    match V.Full.scope full_var, package with
    | _, None -> None
    | V.Full.Package n, Some nv when n <> nv.name -> None
    | _, Some nv -> match V.to_string var with
      | "name" -> Some (S (OpamPackage.Name.to_string nv.name))
      | "version" -> Some (S (OpamPackage.Version.to_string nv.version))
      | _ -> None
  in
  if allowed_package_variables <> None then allowed_package_variables
  else if V.Full.scope full_var <> V.Full.Global then None
  else
  match V.Full.read_from_env full_var with
  | Some _ as c -> c
  | None ->
    try
      let stdpath = OpamTypesBase.std_path_of_string (V.to_string var) in
      let dir =
        OpamPath.Switch.get_stdpath gt.root switch switch_config stdpath
      in
      Some (V.string (OpamFilename.Dir.to_string dir))
    with Failure _ ->
    match OpamFile.Switch_config.variable switch_config var with
    | Some _ as c -> c
    | None ->
      match resolve_global gt full_var with
      | Some _ as c -> c
      | None ->
        match V.to_string var with
        | "switch" -> Some (V.string (OpamSwitch.to_string switch))
        | _ -> None

let resolve_switch ?package st full_var =
  resolve_switch_raw ?package
    st.switch_global st.switch st.switch_config full_var

open OpamVariable

let is_dev_package st opam =
  match OpamFile.OPAM.url opam with
  | None -> false
  | Some urlf ->
    match OpamFile.URL.(url urlf, checksum urlf) with
    | { OpamUrl.backend = `http; _ }, _
      when not (OpamPackage.Set.mem (OpamFile.OPAM.package opam) st.pinned) ->
      false
    | _, _::_ -> false
    | _, [] -> true

let filter_depends_formula
    ?(build=true)
    ?(test=OpamStateConfig.(!r.build_test))
    ?(doc=OpamStateConfig.(!r.build_doc))
    ?(dev=false)
    ?default
    ~env
    ff
  =
  ff |>
  OpamFilter.partial_filter_formula (fun v ->
      if List.mem v predefined_depends_variables then None
      else env v) |>
  OpamFilter.filter_deps ~build ~test ~doc ~dev ?default

let all_depends ?build ?test ?doc ?dev ?(filter_default=false)
    ?(depopts=true) st opam =
  let dev = match dev with None -> is_dev_package st opam | Some d -> d in
  let deps =
    OpamFormula.ands
      (OpamFile.OPAM.depends opam ::
       if depopts then [OpamFile.OPAM.depopts opam] else [])
  in
  filter_depends_formula ?build ?test ?doc ~dev
    ~default:filter_default
    ~env:(resolve_switch ~package:(OpamFile.OPAM.package opam) st) deps

(* filter handling *)
let rec resolve st ?opam:opam_arg ?(local=OpamVariable.Map.empty) v =
  let dirname dir = string (OpamFilename.Dir.to_string dir) in
  let pkgname = OpamStd.Option.map OpamFile.OPAM.name opam_arg in
  let read_package_var v =
    let get name =
      try
        let cfg =
          OpamPackage.Map.find
            (OpamPackage.package_of_name st.installed name)
            st.conf_files
        in
        OpamFile.Dot_config.variable cfg (OpamVariable.Full.variable v)
      with Not_found -> None
    in
    match OpamVariable.Full.scope v with
    | OpamVariable.Full.Global -> None
    | OpamVariable.Full.Package n -> get n
    | OpamVariable.Full.Self ->
      OpamStd.Option.Op.(pkgname >>= get)
  in
  let get_local_var v =
    match OpamVariable.Full.package v with
    | Some _ -> None
    | None ->
      let var = OpamVariable.Full.variable v in
      try match OpamVariable.Map.find var local with
        | None -> raise Exit (* Variable explicitly undefined *)
        | some -> some
      with Not_found -> None
  in
  let get_features_var opam v =
    let to_str opam =
      OpamPackage.to_string @@
      OpamPackage.create (OpamFile.OPAM.name opam) (OpamFile.OPAM.version opam)
    in
    let features = OpamFile.OPAM.features opam in
    try
      let v, _descr, filt = List.find (fun (id,_,_) -> id = v) features in
      let local = (* Avoid recursion *)
        OpamVariable.Map.add v None local in
      try Some (OpamFilter.eval (resolve st ~opam ~local) filt)
      with Failure _ ->
        OpamConsole.warning "Feature %s of %s didn't resolve%s"
          (OpamVariable.to_string v) (to_str opam)
          (match opam_arg with None -> "" | Some o ->
            Printf.sprintf " (referred to from %s)" (to_str o));
        None
    with Not_found -> None
  in
  let get_package_var v =
    if OpamVariable.Full.is_global v then None else
    let var_str = OpamVariable.to_string (OpamVariable.Full.variable v) in
    let name =
      match OpamVariable.Full.scope v with
      | OpamVariable.Full.Global -> assert false
      | OpamVariable.Full.Package n -> n
      | OpamVariable.Full.Self ->
        match pkgname with Some n -> n | None -> raise Exit
    in
    let opam = (* ensure opam, if not None, corresponds to name *)
      match opam_arg with
      | Some o when OpamFile.OPAM.name o = name -> opam_arg
      | _ ->
        try
          let nv = OpamPackage.package_of_name st.installed name in
          Some (OpamPackage.Map.find nv st.opams)
        with Not_found -> None
    in
    let feat = match opam with
      | Some o -> get_features_var o (OpamVariable.Full.variable v)
      | None -> None in
    if feat <> None then feat else
    let get_nv opam = OpamPackage.create name (OpamFile.OPAM.version opam) in
    let root = st.switch_global.root in
    match var_str, opam with
    | "installed", Some _ ->
      Some (bool (OpamPackage.has_name st.installed name))
    | "installed", None ->
      Some (bool false)
    | "pinned", _ ->
      Some (bool (OpamPackage.has_name st.pinned name))
    | "name", _ ->
      if OpamPackage.has_name st.packages name
      then Some (string (OpamPackage.Name.to_string name))
      else None
    | _, None -> None
    | "bin", _ ->
      Some (dirname (OpamPath.Switch.bin root st.switch st.switch_config))
    | "sbin", _ ->
      Some (dirname (OpamPath.Switch.sbin root st.switch st.switch_config))
    | "lib", _ ->
      Some (dirname (OpamPath.Switch.lib root st.switch st.switch_config name))
    | "man", _ ->
      Some (dirname (OpamPath.Switch.man_dir root st.switch st.switch_config))
    | "doc", _ ->
      Some (dirname (OpamPath.Switch.doc root st.switch st.switch_config name))
    | "share", _ ->
      Some (dirname (OpamPath.Switch.share root st.switch st.switch_config name))
    | "etc", _ ->
      Some (dirname (OpamPath.Switch.etc root st.switch st.switch_config name))
    | "build", Some opam ->
      Some (dirname (OpamPath.Switch.build root st.switch (get_nv opam)))
    | "version", Some opam ->
      Some (string (OpamPackage.Version.to_string (OpamFile.OPAM.version opam)))
    | "depends", Some opam ->
      let deps = OpamFormula.atoms (all_depends st opam) in
      let installed_deps =
        OpamStd.List.filter_map
          (fun (n,cstr) ->
             try
               let nv =
                 OpamPackage.Set.find (fun nv -> nv.name = n)
                   st.installed
               in
               let version = nv.version in
               match cstr with
               | None -> Some nv
               | Some (op,v) when OpamFormula.eval_relop op version v -> Some nv
               | Some _ -> None
             with Not_found -> None)
          deps
      in
      let str_deps =
        OpamStd.List.concat_map " " OpamPackage.to_string installed_deps
      in
      Some (string str_deps)
    | "hash", Some opam ->
      (try
         let nv = get_nv opam in
         let f = OpamPath.archive root nv in
         if OpamFilename.exists f then
           Some (string (OpamHash.to_string
                           (OpamHash.compute ~kind:`MD5
                              (OpamFilename.to_string f))))
         else Some (string "")
       with Not_found -> Some (string ""))
    | "dev", Some opam -> Some (bool (is_dev_package st opam))
    | _, _ -> None
  in
  let make_package_local v =
    (* [var] within the opam file of [pkg] is tried as [pkg:var] *)
    match OpamVariable.Full.is_global v, pkgname with
    | true, Some name ->
      OpamVariable.Full.create name (OpamVariable.Full.variable v)
    | _ -> v
  in
  let skip _ = None in
  let v' = make_package_local v in
  let contents =
    try
      List.fold_left
        (function None -> (fun (f,v) -> f v) | r -> (fun _ -> r))
        None
        [
          get_local_var, v;
          Full.read_from_env, v;
          (if v' <> v then Full.read_from_env else skip), v';
          read_package_var, v;
          resolve_switch st, v;
          (if v' <> v then read_package_var else skip), v';
          get_package_var, v';
        ]
    with Exit -> None
  in
  contents
