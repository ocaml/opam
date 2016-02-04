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
;;

let () =
  let error_printer = function
    | OpamParallel.Errors (_, (_,exc)::_, _) -> Some (Printexc.to_string exc)
    | _ -> None
  in
  Printexc.register_printer error_printer
;;

iter_compilers_gen @@ fun c ~prefix ~comp ~descr ->
  let opam = OpamFile.Comp.to_package (OpamFile.Comp.name comp) comp descr in
  let nv = OpamFile.OPAM.package opam in
  let extra_sources =
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
  if List.mem None extra_files then
    comp, `Keep
  else
  let opam = OpamFile.OPAM.with_extra_sources opam extra_sources in
  OpamFile.OPAM.write (OpamRepositoryPath.opam repo prefix nv) opam;
  let config =
    OpamFile.Dot_config.create @@
    List.map (fun (v,c) -> OpamVariable.of_string v, c) @@
    [ "ocaml-version",
      S (OpamCompiler.Version.to_string (OpamFile.Comp.version comp));
      "compiler", S (OpamCompiler.to_string (OpamFile.Comp.name comp));
      "preinstalled", B false;
      (* fixme: generate those from build/config artifacts using a script ?
         Guess from os and arch vars and use static 'features' + variable
         expansion ? *)
      "ocaml-native", B true;
      "ocaml-native-tools", B true;
      "ocaml-native-dynlink", B true;
      "ocaml-stubsdir", S "%{lib}%/stublibs"; ]
  in
  OpamFile.Dot_config.write
    OpamFilename.Op.(OpamRepositoryPath.files repo prefix nv // "ocaml.config")
    config;
  comp, `Remove
;;

module OF = OpamFile.OPAM
;;

let rec filter_of_formula atom_f = function
  | Empty -> FBool true
  | Atom at -> atom_f at
  | Block f -> filter_of_formula atom_f f
  | And (a, b) -> FAnd (filter_of_formula atom_f a, filter_of_formula atom_f b)
  | Or (a, b) -> FOr (filter_of_formula atom_f a, filter_of_formula atom_f b)
;;

iter_packages ~opam:(fun nv opam ->
    if OpamPackage.name_to_string nv <> "ocaml" then
      let available =
        match OF.ocaml_version opam with
        | None -> OF.available opam
        | Some cstr ->
          let filter =
            filter_of_formula
              (fun (op,v) ->
                 FOp
                   (FIdent ([], OpamVariable.of_string "ocaml-version", None),
                    op,
                    FString (OpamCompiler.Version.to_string v)))
              cstr
          in
          match OF.available opam with
          | FBool true -> filter
          | f -> FAnd (filter, f)
      in
      let opam = OpamFile.OPAM.with_ocaml_version opam OpamFormula.Empty in
      let opam = OF.with_available opam available in
      let opam = OF.with_opam_version opam (OpamVersion.of_string "1.3~dev4") in
      opam
    else opam)
  ()
;;

