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

OpamCompiler.Map.iter (fun c prefix ->
    let comp_file = OpamRepositoryPath.compiler_comp repo prefix c in
    let comp = OpamFile.Comp.read comp_file in
    let descr_file =
      OpamRepositoryPath.compiler_descr repo prefix c |>
      OpamFilename.opt_file
    in
    let descr = descr_file >>| OpamFile.Descr.read in
    let opam =
      OpamFile.Comp.to_package (OpamPackage.Name.of_string "ocaml")
        comp descr
    in
    let nv = OpamFile.OPAM.package opam in
    let patches = OpamFile.Comp.patches comp in
    if patches <> [] then
      OpamConsole.msg "Fetching patches of %s to check their checksums...\n"
        (OpamPackage.to_string nv);
    let extra_sources =
      (* Download them just to get their mandatory MD5 *)
      OpamParallel.map
        ~jobs:3
        ~command:(fun url ->
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
                (OpamDownload.download ~overwrite:false url dir
                 @@| fun f -> Some (url, OpamFilename.digest f, None))
            with e -> err e)
        (OpamFile.Comp.patches comp)
    in
    if List.mem None extra_sources then ()
    else
    let opam =
      OpamFile.OPAM.with_extra_sources opam
        (OpamStd.List.filter_some extra_sources)
    in
    OpamFile.OPAM.write (OpamRepositoryPath.opam repo (Some "ocaml") nv) opam;
    let config =
      OpamFile.Dot_config.create @@
      List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
      [ "ocaml-version",
        S (OpamCompiler.Version.to_string (OpamFile.Comp.version comp));
        "compiler", S (OpamCompiler.to_string (OpamFile.Comp.name comp));
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
      OpamFilename.Op.(OpamRepositoryPath.files repo prefix nv // "ocaml.config")
      config;
    OpamFilename.remove comp_file;
    OpamStd.Option.iter OpamFilename.remove descr_file;
    OpamFilename.rmdir_cleanup (OpamFilename.dirname comp_file);
    OpamConsole.msg "Compiler %s successfully converted to package %s\n"
      (OpamCompiler.to_string c) (OpamPackage.to_string nv))
  (OpamRepository.compilers_with_prefixes repo)
;;
