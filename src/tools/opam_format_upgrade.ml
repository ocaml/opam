(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamProcess.Job.Op
open OpamStd.Option.Op

module O = OpamFile.OPAM

type args = {
  clear_cache: bool;
}

let args =
  let open Cmdliner in
  let clear_cache =
    let doc = "Remove the cache of compiler archive hashes" in
    Arg.(value & flag & info ["clear-cache"] ~doc)
  in
  Term.(pure (fun clear_cache -> { clear_cache; }) $ clear_cache)


let ocaml_pkgname = OpamPackage.Name.of_string "ocaml"

let ocaml_wrapper_pkgname = OpamPackage.Name.of_string "ocaml"
let ocaml_official_pkgname = OpamPackage.Name.of_string "ocaml-base-compiler"
let ocaml_variants_pkgname = OpamPackage.Name.of_string "ocaml-variants"
let ocaml_system_pkgname = OpamPackage.Name.of_string "ocaml-system"

let ocaml_package_names =
  [ocaml_wrapper_pkgname;
   ocaml_official_pkgname;
   ocaml_variants_pkgname;
   ocaml_system_pkgname]

(* OCaml script that generates the .config file for a given ocaml compiler *)
let wrapper_conf_script =
  "let () =\n\
  \  if Sys.ocaml_version <> \"%{_:version}%\" then\n\
  \    (Printf.eprintf\n\
  \       \"OCaml version mismatch: %%s, expected %{_:version}%\"\n\
  \       Sys.ocaml_version;\n\
  \     exit 1)\n\
  \  else\n\
  \  let oc = open_out \"%{_:name}%.config\" in\n\
  \  let ocaml = Sys.executable_name in\n\
  \  let ocamlc = ocaml^\"c\" in\n\
  \  let libdir =\n\
  \    let ic = Unix.open_process_in (ocamlc^\" -where\") in\n\
  \    let r = input_line ic in\n\
  \    Unix.close_process_in ic;\n\
  \    r\n\
  \  in\n\
  \  let stubsdir =\n\
  \    let ic = open_in (Filename.concat libdir \"ld.conf\") in\n\
  \    let rec r acc = try r (input_line ic::acc) with End_of_file -> acc in\n\
  \    let lines = List.rev (r []) in\n\
  \    close_in ic;\n\
  \    String.concat \":\" lines\n\
  \  in\n\
  \  let p fmt = Printf.fprintf oc (fmt ^^ \"\\n\") in\n\
  \  p \"opam-version: \\\"2.0~alpha\\\"\";\n\
  \  p \"variables {\";\n\
  \  p \"  native: %%b\"\n\
  \    (Sys.file_exists (ocaml^\"opt\"));\n\
  \  p \"  native-tools: %%b\"\n\
  \    (Sys.file_exists (ocamlc^\".opt\"));\n\
  \  p \"  native-dynlink: %%b\"\n\
  \    (Sys.file_exists (Filename.concat libdir \"dynlink.cmxa\"));\n\
  \  p \"  stubsdir: %%S\"\n\
  \    stubsdir;\n\
  \  p \"}\";\n\
  \  close_out oc\n\
  "

let system_conf_script =
  "let () =\n\
  \  let ocamlc = Sys.executable_name ^ \"c\" in\n\
  \  if Sys.ocaml_version <> \"%{_:version}%\" then\n\
  \    (Printf.eprintf\n\
  \       \"ERROR: The compiler found at %%s has version %%s,\n\\\n\
  \        and this package requires %{_:version}%.\\n\\\n\
  \        You should use e.g. 'opam switch %{_:name}%.%{_:version}%' \\\n\
  \        instead.\"\n\
  \       Sys.ocaml_version;\n\
  \     exit 1)\n\
  \  else\n\
  \  let oc = open_out \"%{_:name}%.config\" in\n\
  \  Printf.fprintf oc \"opam-version: \\\"2.0~alpha\\\"\\n\\\n\
  \                     file-depends: [ %%S %%S ]\\n\"\n\
  \    ocamlc (Digest.to_hex (Digest.file ocamlc));\n\
  \  close_out oc\n\
  "

let conf_script_name = "gen_ocaml_config.ml"

let all_base_packages =
  OpamPackage.Name.Set.of_list (List.map OpamPackage.Name.of_string [
      "base-bigarray";
      "base-threads";
      "base-unix";
    ])

let process args =

  let cache_file : string list list OpamFile.t =
    OpamFile.make @@
    OpamFilename.of_string "~/.cache/opam-compilers-to-packages/url-hashes"
  in

  if args.clear_cache then
    OpamFilename.remove (OpamFile.filename cache_file)
  else

  let repo = OpamRepositoryBackend.local (OpamFilename.cwd ()) in

  let write_opam ?(add_files=[]) opam =
    let nv = O.package opam in
    let pfx = Some (OpamPackage.name_to_string nv) in
    let files_dir = OpamRepositoryPath.files repo pfx nv in
    O.write (OpamRepositoryPath.opam repo pfx nv) opam;
    List.iter (fun (base,contents) ->
        OpamFilename.(write Op.(files_dir // base) contents))
      add_files
  in

  let compilers =
    let compilers_dir = OpamFilename.Op.(repo.repo_root / "compilers") in
    if OpamFilename.exists_dir compilers_dir then (
      List.fold_left (fun map f ->
          if OpamFilename.check_suffix f ".comp" then
            let c = OpamFilename.(Base.to_string (basename (chop_extension f))) in
            OpamStd.String.Map.add c f map
          else
            map)
        OpamStd.String.Map.empty (OpamFilename.rec_files compilers_dir)
    ) else
      OpamStd.String.Map.empty
  in

  let ocaml_versions =
    OpamStd.String.Map.fold (fun c comp_file ocaml_versions ->
      let comp = OpamFile.Comp.read (OpamFile.make comp_file) in
      let descr_file =
        OpamFilename.(opt_file (add_extension (chop_extension comp_file) "descr"))
      in
      let descr = descr_file >>| fun f -> OpamFile.Descr.read (OpamFile.make f) in
      let comp =
        let drop_names = [ OpamPackage.Name.of_string "base-ocamlbuild" ] in
        (* ocamlbuild has requirements on variable ocaml-version: it can't be in
           the dependencies *)
        OpamFile.Comp.with_packages
          (OpamFormula.map
             (fun ((name, _) as atom) ->
                if List.mem name drop_names then OpamFormula.Empty
                else Atom atom)
             (OpamFile.Comp.packages comp))
          comp
      in

      let nv, ocaml_version, variant =
        match OpamStd.String.cut_at c '+' with
        | None ->
          OpamPackage.create ocaml_official_pkgname
            (OpamPackage.Version.of_string c),
          c, None
        | Some (version,variant) ->
          OpamPackage.create ocaml_variants_pkgname
            (OpamPackage.Version.of_string (version^"+"^variant)),
          version, Some variant
      in

      (* (Some exotic compiler variants have e.g. 'lwt' as base package, which
         won't work in our current setup. They'll need to be rewritten, but
         break the following detection of all base packages, which isn't
         idempotent anyway...)

         List.iter (fun (name, _) ->
             all_base_packages := OpamPackage.Name.Set.add name !all_base_packages)
           (OpamFormula.atoms (OpamFile.Comp.packages comp));
      *)

      let opam = OpamFile.Comp.to_package ~package:nv comp descr in
      let opam =
        if variant = None then
          opam |>
          O.with_conflicts (OpamFormula.ors [
              Atom (ocaml_system_pkgname, Empty);
              Atom (ocaml_variants_pkgname, Empty);
            ])
        else opam
      in
      let patches = OpamFile.Comp.patches comp in
      if patches <> [] then
        OpamConsole.msg "Fetching patches of %s to check their checksums...\n"
          (OpamPackage.to_string nv);
      let url_md5 =
        (OpamFile.Lines.read_opt cache_file +! [] |> List.map @@ function
          | [url; md5] -> OpamUrl.of_string url, md5
          | _ -> failwith "Bad cache") |>
        OpamUrl.Map.of_list
      in
      let extra_sources =
        (* Download them just to get their mandatory MD5 *)
        OpamParallel.map
          ~jobs:3
          ~command:(fun url ->
              try Done (Some (url, OpamUrl.Map.find url url_md5, None))
              with Not_found ->
                let err e =
                  OpamConsole.error
                    "Could not get patch file for %s from %s (%s), skipping"
                    (OpamPackage.to_string nv) (OpamUrl.to_string url)
                    (Printexc.to_string e);
                  Done None
                in
                OpamFilename.with_tmp_dir_job @@ fun dir ->
                try
                  OpamProcess.Job.catch err
                    (OpamDownload.download ~overwrite:false url dir @@| fun f ->
                     Some (url, OpamFilename.digest f, None))
                with e -> err e)
          (OpamFile.Comp.patches comp)
      in
      List.fold_left
        (fun url_md5 -> function
           | Some (url,md5,_) -> OpamUrl.Map.add url md5 url_md5
           | None -> url_md5)
        url_md5 extra_sources |>
      OpamUrl.Map.bindings |>
      List.map (fun (url,m) -> [OpamUrl.to_string url; m]) |>
      OpamFile.Lines.write cache_file;
      if List.mem None extra_sources then ocaml_versions
      else
      let opam =
        opam |>
        OpamFile.OPAM.with_extra_sources
          (OpamStd.List.filter_some extra_sources)
      in
      write_opam opam;

      if variant = None then begin
        (* "official" compiler release: generate a system compiler package *)
        let sys_nv = OpamPackage.create ocaml_system_pkgname nv.version in
        let system_opam =
          O.create sys_nv |>
          O.with_substs [OpamFilename.Base.of_string conf_script_name] |>
          O.with_build [
            List.map (fun s -> CString s, None)
              [ "ocaml"; conf_script_name ],
            None
          ] |>
          O.with_conflicts (OpamFormula.ors [
              Atom (ocaml_official_pkgname, Empty);
              Atom (ocaml_variants_pkgname, Empty);
            ]) |>
          O.with_maintainer [ "platform@lists.ocaml.org" ] |>
          O.with_flags [Pkgflag_Compiler] |>
          O.with_descr
            (OpamFile.Descr.create
                 "The OCaml compiler (system version, from outside of opam)")
            (* add depext towards an 'ocaml' package ? *)
        in
        write_opam ~add_files:[conf_script_name^".in", system_conf_script]
          system_opam
      end;

      (* cleanup *)
      OpamFilename.remove comp_file;
      OpamStd.Option.iter OpamFilename.remove descr_file;
      OpamFilename.rmdir_cleanup (OpamFilename.dirname comp_file);
      OpamConsole.status_line
        "Compiler %s successfully converted to package %s"
        c (OpamPackage.to_string nv);
      OpamStd.String.Set.add ocaml_version ocaml_versions
      )
    compilers OpamStd.String.Set.empty
  in

  (* Generate "ocaml" package wrappers depending on one of the implementations
     at the appropriate version *)
  let gen_ocaml_wrapper str_version =
    let version = OpamPackage.Version.of_string str_version in
    let wrapper_nv = OpamPackage.create ocaml_wrapper_pkgname version in
    let upper_bound_v =
      let g = Re.(exec @@ compile @@ seq [rep digit; eos]) str_version in
      try
        let sn = Re.Group.get g 0 in
        String.sub str_version 0 (fst (Re.Group.offset g 0)) ^
        (string_of_int (1 + int_of_string sn)) ^ "~"
      with Not_found -> str_version ^ "a"
    in
    let wrapper_opam =
      O.create wrapper_nv |>
      O.with_substs [OpamFilename.Base.of_string conf_script_name] |>
      O.with_build [
        List.map (fun s -> CString s, None)
          [ "ocaml"; "unix.cma"; conf_script_name ],
        None
      ] |>
      O.with_maintainer [ "platform@lists.ocaml.org" ] |>
      O.with_env [
        "CAML_LD_LIBRARY_PATH", Eq, "%{_:stubsdir}%", None;
        "CAML_LD_LIBRARY_PATH", PlusEq, "%{lib}%", None
      ] |>
      (* leave the Compiler flag to the implementations (since the user
         needs to select one)
         O.with_flags [Pkgflag_Compiler] |> *)
      O.with_descr
        (OpamFile.Descr.create
           "The OCaml compiler (virtual package)\n\
            This package requires a matching implementation of OCaml,\n\
            and polls it to initialise specific variables like \
            `ocaml:native-dynlink`") |>
      O.with_depends
        (OpamFormula.ors [
            Atom (
              ocaml_official_pkgname,
              Atom (Constraint (`Eq, FString str_version))
            );
            Atom (
              ocaml_variants_pkgname,
              OpamFormula.ands [
                Atom (Constraint (`Geq, FString str_version));
                Atom (Constraint (`Lt, FString upper_bound_v));
              ]
            );
            Atom (
              ocaml_system_pkgname,
              Atom (Constraint (`Eq, FString str_version))
            )
          ]) |>
      O.with_features [
        OpamVariable.of_string "preinstalled",
        "The concrete compiler was installed outside of opam",
        FIdent ([ocaml_system_pkgname], OpamVariable.of_string "installed",
                None);
        OpamVariable.of_string "compiler",
        "Detailed OCaml or variant compiler name",
        FString
          (Printf.sprintf "%%{%s:installed?system:}%%\
                           %%{%s:installed?%s:}%%\
                           %%{%s:version}%%"
             (OpamPackage.Name.to_string ocaml_system_pkgname)
             (OpamPackage.Name.to_string ocaml_official_pkgname) str_version
             (OpamPackage.Name.to_string ocaml_variants_pkgname));
      ]
    in
    write_opam ~add_files:[conf_script_name^".in", wrapper_conf_script]
      wrapper_opam
  in
  OpamStd.String.Set.iter gen_ocaml_wrapper ocaml_versions;

  let packages = OpamRepository.packages_with_prefixes repo in

  OpamConsole.msg "Will not update base packages: %s\n"
    (OpamPackage.Name.Set.to_string all_base_packages);

  OpamPackage.Map.iter (fun package prefix ->
      let opam_file = OpamRepositoryPath.opam repo prefix package in
      let opam0 = OpamFile.OPAM.read opam_file in
      let opam = opam0 in
      let nv = OpamFile.OPAM.package opam in
      let available =
        OpamFilter.distribute_negations (OpamFile.OPAM.available opam)
      in
      let sym_op = function
        | (`Eq | `Neq) as op -> op
        | `Gt -> `Lt
        | `Geq -> `Leq
        | `Lt -> `Gt
        | `Leq -> `Geq
      in
      let mk_constraint op v = Atom (Constraint (op, FString v)) in
      let get_atom v =
        if v = "system" then
          ocaml_system_pkgname, Empty
        else
          (if String.contains v '+' then ocaml_variants_pkgname
           else ocaml_official_pkgname),
          mk_constraint `Eq v
      in
      let module NMap = OpamPackage.Name.Map in
      let pkg_deps, pkg_conflicts, available_opt =
        let rec aux avail = match avail with
          | FOp (FString _ as fs, op, (FIdent _ as fid)) ->
            aux (FOp (fid, sym_op op, fs))
          | FOp (FIdent ([],var,None), op, FString v) ->
            (match OpamVariable.to_string var, op with
             | "ocaml-version", _ ->
               NMap.singleton ocaml_wrapper_pkgname (mk_constraint op v),
               NMap.empty,
               None
             | "compiler", `Eq ->
               NMap.of_list [get_atom v], NMap.empty, None
             | "compiler", `Neq ->
               NMap.empty, NMap.of_list [get_atom v], None
             | _ -> NMap.empty, NMap.empty, Some avail)
          | FIdent ([], v, None) when
              OpamVariable.to_string v = "preinstalled" ->
            NMap.singleton ocaml_system_pkgname Empty,
            NMap.empty,
            None
          | FNot (FIdent ([], v, None)) when
              OpamVariable.to_string v = "preinstalled" ->
            NMap.empty,
            NMap.singleton ocaml_system_pkgname Empty,
            None
          | FNot f ->
            let pkg_deps, pkg_conflicts, available_opt = aux f in
            pkg_conflicts, pkg_deps,
            OpamStd.Option.map (fun f -> FNot f) available_opt
          | FAnd (f1,f2) ->
            let deps1, cflt1, f1 = aux f1 in
            let deps2, cflt2, f2 = aux f2 in
            (NMap.union (fun d1 d2 -> OpamFormula.ands [d1; d2]) deps1 deps2,
             NMap.union (fun c1 c2 -> OpamFormula.ors [c1; c2]) cflt1 cflt2,
             match f1, f2 with
             | Some f1, Some f2 -> Some (FAnd (f1, f2))
             | None, f | f, None -> f)
          | FOr (f1,f2) ->
            let deps1, cflt1, f1 = aux f1 in
            let deps2, cflt2, f2 = aux f2 in
            let err () =
              OpamConsole.error "Unconvertible 'available:' disjunction in %s"
                (OpamFile.to_string opam_file)
            in
            (NMap.union (fun d1 d2 -> OpamFormula.ors [d1; d2]) deps1 deps2,
             NMap.union (fun c1 c2 -> OpamFormula.ands [c1; c2]) cflt1 cflt2,
             match f1, f2 with
             | Some f1, Some f2 -> Some (FOr (f1,f2))
             | None, None -> None
             | None, f | f, None -> err (); f)
          | f -> NMap.empty, NMap.empty, Some f
        in
        aux available
      in
      let pkg_deps =
        if not (NMap.mem ocaml_wrapper_pkgname pkg_deps) then
          NMap.add ocaml_wrapper_pkgname Empty pkg_deps
        else pkg_deps
      in
      let available =
        OpamStd.Option.default (FBool true) available_opt
      in
      if List.exists (fun v -> match OpamVariable.Full.to_string v with
          | "ocaml-version" | "compiler" | "preinstalled"
          | "ocaml-native" | "ocaml-native-tools"
          | "ocaml-native-dynlink" -> true
          | _ -> false)
          (OpamFilter.variables available)
      then OpamConsole.warning
          "Could not translate some OCaml variables in the 'available:' \
           field of %s: %s"
          (OpamFile.to_string opam_file) (OpamFilter.to_string available);
      let depends =
        let to_add,conj =
          List.fold_left (fun (to_add,conj) -> function
            | Atom (pkgname, cstr) as atom ->
              (try
                 NMap.remove pkgname to_add,
                 Atom (pkgname,
                       OpamFormula.ands [cstr; NMap.find pkgname to_add])
                 :: conj
               with Not_found -> to_add, atom :: conj)
            | f -> to_add, f::conj)
            (pkg_deps, [])
            (OpamFormula.ands_to_list (O.depends opam))
        in
        let remain =
          List.map (fun (name, cstr) -> Atom (name, cstr))
            (NMap.bindings to_add)
        in
        OpamFormula.ands (remain @ List.rev conj)
      in
      let conflicts =
        let pkg_conflicts =
          NMap.map (OpamFormula.map (function
              | Constraint (op, FString v) ->
                Atom (op, OpamPackage.Version.of_string v)
              | _ -> assert false))
            pkg_conflicts
        in
        let to_add, disj =
          List.fold_left (fun (to_add,disj) -> function
            | Atom (pkgname, cstr) as atom ->
              (try
                 NMap.remove pkgname to_add,
                 Atom (pkgname,
                       OpamFormula.ors [cstr; NMap.find pkgname to_add])
                 :: disj
               with Not_found -> to_add, atom :: disj)
            | f -> to_add, f::disj)
            (pkg_conflicts,[])
            (OpamFormula.ors_to_list (O.conflicts opam))
        in
        let remain =
          List.map (fun (name, cstr) -> Atom (name, cstr))
            (NMap.bindings to_add)
        in
        OpamFormula.ors (remain @ List.rev disj)
      in
      let rewrite_var v =
        let mkvar s =
          OpamVariable.Full.create ocaml_pkgname (OpamVariable.of_string s)
        in
        match OpamVariable.Full.scope v with
        | OpamVariable.Full.Global ->
          (match OpamVariable.(to_string (Full.variable v)) with
           | "compiler" -> mkvar "compiler"
           | "preinstalled" -> mkvar "preinstalled"
           | "ocaml-version" -> mkvar "version"
           | "ocaml-native" -> mkvar "native"
           | "ocaml-native-tools" -> mkvar "native-tools"
           | "ocaml-native-dynlink" -> mkvar "native-dynlink"
           | _ -> v)
        | _ -> v
      in
      if not (List.mem nv.name ocaml_package_names) &&
         not (OpamPackage.Name.Set.mem nv.name all_base_packages) then
        opam |>
        OpamFile.OPAM.with_depends depends |>
        OpamFile.OPAM.with_conflicts conflicts |>
        OpamFile.OPAM.with_available available |>
        OpamFileTools.map_all_variables rewrite_var |>
        fun opam ->
          if opam <> opam0 then
            (OpamFile.OPAM.write_with_preserved_format opam_file opam;
             OpamConsole.status_line "Updated %s" (OpamFile.to_string opam_file))
    )
    packages
