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

open OpamStd.Op

open OpamTypes
open OpamStateTypes

(* Lists of defined variables, for documentation *)

let global_variable_names = [
  "opam-version",         "The currently running OPAM version";
  "switch",               "The local name (alias) of the current switch";
  "jobs",                 "The number of parallel jobs set up in OPAM \
                           configuration";
  "arch",                 "The current arch, as returned by \"uname -m\"";
]

(* Obsolete ocaml variables, for compat *)
let ocaml_variable_names = [
  "ocaml-version",        "The version of the currently used OCaml compiler";
  "preinstalled",         "Whether the compiler was preinstalled on the system, \
                           or installed by OPAM";
  "compiler",             "The name of the current OCaml compiler (may be more \
                           specific than the version, eg: \"4.01.0+fp\", or \
                           \"system\")";
  "ocaml-native",         "Whether the OCaml native compilers are available";
  "ocaml-native-tools",   "Whether the native \".opt\" version of the OCaml \
                           toolchain is available";
  "ocaml-native-dynlink", "Whether native dynlink is available on this \
                           installation";
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
]


let resolve_compat_ocaml_variables _gt _switch _switch_config _ocaml_var =
  raise Not_found
  (* backwards-compat, these are replaced by variables exported from the
     'ocaml' package.
     !X
     /!\ reloads files for every variable resolution ! *)
(*
  let module V = OpamVariable in
  let compiler = OpamSwitch.Map.find switch gt.aliases in
  let comp = OpamFile.Comp.read (OpamPath.compiler_comp gt.root compiler) in
  let preinstalled_comp = OpamFile.Comp.preinstalled comp in
  match ocaml_var with
  | "ocaml-version" -> V.string (OpamCompiler.Version.to_string
                                   (OpamFile.Comp.version comp))
  | "compiler" -> V.string (OpamCompiler.to_string
                              (OpamFile.Comp.name comp))
  | "preinstalled" -> V.bool preinstalled_comp
  | "ocaml-native" ->
    if preinstalled_comp then
      V.bool (Lazy.force OpamOCaml.ocaml_native_available)
    else
      V.bool (OpamFilename.exists
                (OpamPath.Switch.bin gt.root switch switch_config
                 // "ocamlopt"))
  | "ocaml-native-tools" ->
    if preinstalled_comp then
      V.bool (Lazy.force OpamOCaml.ocaml_opt_available)
    else
      V.bool (OpamFilename.exists
                (OpamPath.Switch.bin gt.root switch switch_config
                 // "ocamlc.opt"))
  | "ocaml-native-dynlink" ->
    if preinstalled_comp then
      V.bool (Lazy.force OpamOCaml.ocaml_natdynlink_available)
    else
      V.bool (OpamFilename.exists
                (OpamPath.Switch.lib_dir gt.root switch switch_config
                 / "ocaml" // "dynlink.cmxa"))
  | _ -> raise Not_found
*)

let resolve_global gt full_var =
  let module V = OpamVariable in
  if V.Full.(scope full_var <> Global) then None else
  let var = V.Full.variable full_var in
  match V.Full.read_from_env full_var with
  | Some _ as c -> c
  | None ->
    match V.to_string var with
    | "opam-version"  -> Some (V.string OpamVersion.(to_string current))
    | "jobs"          -> Some (V.int (OpamFile.Config.jobs gt.config))
    | "arch"          -> Some (V.string (OpamStd.Sys.arch ()))
    | _               -> None

(** Resolve switch-global variables only, as allowed by the 'available:' field *)
let resolve_switch_raw gt switch switch_config full_var =
  let module V = OpamVariable in
  if V.Full.(scope full_var <> Global) then None else
  let var = V.Full.variable full_var in
  match V.Full.read_from_env full_var with
  | Some _ as c -> c
  | None ->
    (* !X Variables from packages forming the current compiler should be
       accessible here *)
    match OpamFile.Dot_config.variable switch_config var with
    | Some _ as c -> c
    | None ->
      match resolve_global gt full_var with
      | Some _ as c -> c
      | None ->
        match V.to_string var with
        | "switch" -> Some (V.string (OpamSwitch.to_string switch))
        | var_name ->
          try
            Some (resolve_compat_ocaml_variables gt switch switch_config var_name)
          with Not_found ->
            None

let resolve_switch st full_var =
  resolve_switch_raw st.switch_global st.switch st.switch_config full_var

open OpamVariable

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
      let dev =
        OpamStd.Option.Op.(
          (OpamFile.OPAM.get_url opam >>| fun u ->
           u.OpamUrl.backend <> `http)
          +! false
        ) in
      let deps =
        OpamFormula.atoms
          (OpamStateConfig.filter_deps ~dev (OpamFile.OPAM.depends opam)) @
        OpamFormula.atoms
          (OpamStateConfig.filter_deps ~dev (OpamFile.OPAM.depopts opam))
      in
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
         if OpamFilename.exists f then Some (string (OpamFilename.digest f))
         else Some (string "")
       with Not_found -> Some (string ""))
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
