#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
#directory "+../re";;

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2013 OCamlPro                                             *)
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
open OpamProcess.Job.Op
open Opam_admin_top
open OpamStd.Option.Op
;;

let () =
  let error_printer = function
    | OpamParallel.Errors (_, (_,exc)::_, _) -> Some (Printexc.to_string exc)
    | _ -> None
  in
  Printexc.register_printer error_printer
;;

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
;;

let official_ocaml_name = OpamPackage.Name.of_string "ocaml"

let provides_map =
  OpamStd.String.Map.fold (fun c comp_file provides_map ->
    let comp = OpamFile.Comp.read (OpamFile.make comp_file) in
    let comp_name = OpamFile.Comp.name comp in
    let is_official, name, version =
      match OpamStd.String.cut_at comp_name '+' with
      | None -> (* Official compiler *)
        true, official_ocaml_name, OpamPackage.Version.of_string comp_name
      | Some (version, variant) ->
        if Re.(execp (compile @@ seq [ bos; str "pr"; rep1 digit; eos ]) variant)
        then
          false,
          OpamPackage.Name.of_string "ocaml+dev",
          OpamPackage.Version.of_string (version ^ "+" ^ variant)
        else
          false,
          OpamPackage.Name.of_string ("ocaml+"^variant),
          OpamPackage.Version.of_string version
    in
    let conflicts =
      if is_official then OpamFormula.Empty else
        OpamFormula.of_disjunction [official_ocaml_name, None]
    in
    let path_prefix = Some (OpamPackage.Name.to_string name) in
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
    let opam =
      OpamFile.Comp.to_package (OpamPackage.Name.of_string "ocaml")
        comp descr
    in
    let nv = OpamFile.OPAM.package opam in
    let patches = OpamFile.Comp.patches comp in
    if patches <> [] then
      OpamConsole.msg "Fetching patches of %s to check their checksums...\n"
        (OpamPackage.to_string nv);
    let cache_file : string list list OpamFile.t =
      OpamFile.make @@
      OpamFilename.of_string "~/.cache/opam-compilers-to-packages/url-hashes"
    in
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
    if List.mem None extra_sources then provides_map
    else
    let provides_map =
      if is_official then provides_map else
      OpamPackage.Map.update
          (OpamPackage.create official_ocaml_name version)
          (OpamPackage.Set.add (OpamPackage.create name version))
          OpamPackage.Set.empty
          provides_map
    in
    let provides_map =
      let provides =
        OpamFormula.packages_of_atoms
          (OpamFormula.atoms (OpamFile.Comp.packages comp))
          packages
      in
      OpamPackage.Set.fold (fun base provides_map ->
          OpamPackage.Map.update base
            (OpamPackage.Set.add nv) OpamPackage.Set.empty
            provides_map)
        provides provides_map
    in
    let opam =
      opam |>
      OpamFile.OPAM.with_conflicts conflicts |>
      OpamFile.OPAM.with_extra_sources
        (OpamStd.List.filter_some extra_sources) |>
      OpamFile.OPAM.with_substs
        [OpamFilename.Base.of_string (OpamPackage.Name.to_string name ^".config")]
    in
    OpamFile.OPAM.write (OpamRepositoryPath.opam repo path_prefix nv) opam;
    let config =
      OpamFile.Dot_config.create @@
      List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
      [ "ocaml-version",
        S (OpamFile.Comp.version comp);
        "compiler", S (OpamFile.Comp.name comp);
        "preinstalled", B false;
        (* fixme: generate those from build/config artifacts using a script ?
           Guess from os and arch vars and use static 'features' + variable
           expansion ?
           ... or just let them be fixed by hand ? *)
        "ocaml-native", B true;
        "ocaml-native-tools", B true;
        "ocaml-native-dynlink", B true;
        "ocaml-stubsdir", S "%{lib}%/stublibs"; ]
    in
    let files_dir = OpamRepositoryPath.files repo path_prefix nv in
(* Were those ever supposed to exist ? We do have a few in the repo...
    OpamStd.Option.iter (fun files ->
        OpamFilename.move_dir files files_dir)
      (OpamFilename.opt_dir
         OpamFilename.Op.(OpamFilename.dirname comp_file / "files"));
*)
    OpamFile.Dot_config.write
      (OpamFile.make OpamFilename.Op.(
           files_dir // (OpamPackage.Name.to_string name ^".config.in")))
      config;
    OpamFilename.remove comp_file;
    OpamStd.Option.iter OpamFilename.remove descr_file;
    OpamFilename.rmdir_cleanup (OpamFilename.dirname comp_file);
    OpamConsole.msg "Compiler %s successfully converted to package %s\n"
      c (OpamPackage.to_string nv);
    provides_map)
  compilers OpamPackage.Map.empty
;;

OpamPackage.Map.iter (fun nv provided_by ->
    OpamConsole.msg "Updating 'provided-by:' field of %s\n"
      (OpamPackage.to_string nv);
    (OpamRepositoryPath.opam repo
       (Some (OpamPackage.name_to_string nv)) nv
     |> fun f ->
     OpamFile.OPAM.read_opt f >>|
     OpamFile.OPAM.with_provided_by
       (OpamFormula.of_disjunction @@
        OpamSolution.eq_atoms_of_packages provided_by) >>|
     OpamFile.OPAM.write_with_preserved_format f)
    +! ())
  provides_map
;;
