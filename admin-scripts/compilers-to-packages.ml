#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;
#directory "+../re";;

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2013 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
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

OpamStd.String.Map.iter (fun c comp_file ->
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
    if List.mem None extra_sources then ()
    else
    let opam =
      opam |>
      OpamFile.OPAM.with_extra_sources
        (OpamStd.List.filter_some extra_sources) |>
      OpamFile.OPAM.with_substs
        [OpamFilename.Base.of_string "ocaml.config"]
    in
    OpamFile.OPAM.write (OpamRepositoryPath.opam repo (Some "ocaml") nv) opam;
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
    OpamFile.Dot_config.write
      (OpamFile.make
         OpamFilename.Op.(OpamRepositoryPath.files repo (Some "ocaml") nv
                          // "ocaml.config.in"))
      config;
    OpamFilename.remove comp_file;
    OpamStd.Option.iter OpamFilename.remove descr_file;
    OpamFilename.rmdir_cleanup (OpamFilename.dirname comp_file);
    OpamConsole.msg "Compiler %s successfully converted to package %s\n"
      c (OpamPackage.to_string nv))
  compilers
;;
