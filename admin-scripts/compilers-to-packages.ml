#!/usr/bin/env opam-admin.top

#directory "+../opam-lib";;

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

iter_compilers_gen @@ fun c ~prefix ~comp ~descr ->
  let version =
    OpamPackage.Version.of_string
      (OpamCompiler.to_string (OpamFile.Comp.name comp))
  in
  let nv = OpamPackage.create (OpamPackage.Name.of_string "ocaml") version in
  (* OpamGlobals.msg "Processing compiler %s => package %s\n" *)
  (*   (OpamCompiler.to_string (OpamFile.Comp.name comp)) *)
  (*   (OpamPackage.to_string nv); *)
  let nofilter x = x, (None: filter option) in
  let build =
    OpamFile.Comp.(
      match build comp with
      | [] ->
        List.map (fun l -> nofilter (List.map nofilter l)) [
          (List.map (fun s -> CString s) ("./configure" :: configure comp ))
          @ [ CString "-prefix"; CIdent "prefix"];
          CIdent "make" :: List.map (fun s -> CString s) (make comp);
          [ CIdent "make"; CString "install"];
        ]
      | cl -> cl)
  in
  let prefix = Some (OpamPackage.Name.to_string (OpamPackage.name nv)) in
  let patches =
    OpamParallel.map
      ~jobs:3
      ~command:(fun f ->
        OpamFilename.download ~overwrite:true f
          (OpamPath.Repository.files repo prefix nv)
        @@| OpamFilename.basename)
      (OpamFile.Comp.patches comp)
  in
  let (@) f x y = f y x in
  let opam =
    let module O = OpamFile.OPAM in
    O.create nv
    |> O.with_build @ build
    |> O.with_maintainer @ [ "contact@ocamlpro.com" ]
    |> O.with_patches @ List.map nofilter patches
  in
  (match OpamFile.Comp.src comp with
   | None -> ()
   | Some address ->
     let adr,kind = OpamTypesBase.parse_url address in
     let url = OpamFile.URL.create kind adr in
     OpamFile.URL.write (OpamPath.Repository.url repo prefix nv) url);
  OpamFile.OPAM.write (OpamPath.Repository.opam repo prefix nv) opam;
  OpamMisc.Option.iter
    (OpamFile.Descr.write (OpamPath.Repository.descr repo prefix nv))
    descr;
  let comp =
    let module C = OpamFile.Comp in
    comp
    |> C.with_src @ None
    |> C.with_patches @ []
    |> C.with_configure @ []
    |> C.with_make @ []
    |> C.with_build @ []
    |> C.with_packages @
       OpamFormula.(
         And (Atom (OpamPackage.name nv, Atom (`Eq, OpamPackage.version nv)),
              C.packages comp)
       )
  in
  comp, `Keep
;;

iter_packages ~opam:(fun nv opam ->
    let ocaml_version = OpamFile.OPAM.ocaml_version opam in
    let ocaml_version_formula, available =
      match ocaml_version with
      | None ->
        let available = OpamFile.OPAM.available opam in
        let rec aux = function
          | FOp (FIdent "ocaml-version", op, FString v) ->
            Atom (op, OpamPackage.Version.of_string v)
          | FNot f ->
            OpamFormula.neg (fun (op,v) -> OpamFormula.neg_relop op, v)
              (aux f)
          | FAnd (f1,f2) -> OpamFormula.ands [aux f1; aux f2]
          | FOr (f1,f2) -> OpamFormula.ors [aux f1; aux f2]
          | _ -> Empty
        in
        let ocaml_dep_formula = aux available in
        let rec aux =
          function
          | FOp (FIdent "ocaml-version", op, FString v) -> None
          | FNot f -> OpamMisc.Option.map (fun f -> FNot f) (aux f)
          | FAnd (f1,f2) -> (match aux f1, aux f2 with
              | Some f1, Some f2 -> Some (FAnd (f1,f2))
              | None, f | f, None -> f)
          | FOr (f1,f2) -> (match aux f1, aux f2 with
              | Some f1, Some f2 -> Some (FOr (f1,f2))
              | None, f | f, None ->
                OpamGlobals.error_and_exit "Unconvertible 'available' field in %s"
                  (OpamPackage.to_string nv))
          | f -> Some f
        in
        let rem_available =
          OpamMisc.Option.default (FBool true) (aux available)
        in
        ocaml_dep_formula, rem_available
      | Some f ->
        OpamFormula.map (fun (op,v) ->
            Atom (op, OpamPackage.Version.of_string
                    (OpamCompiler.Version.to_string v))
          ) f,
        OpamFile.OPAM.available opam
    in
    let depends = OpamFormula.(
        And (Atom (OpamPackage.Name.of_string "ocaml",
                   ([],ocaml_version_formula)),
             OpamFile.OPAM.depends opam)
      )
    in
    let opam = OpamFile.OPAM.with_ocaml_version opam None in
    if OpamPackage.name_to_string nv <> "ocaml" then
      let opam = OpamFile.OPAM.with_depends opam depends in
      let opam = OpamFile.OPAM.with_available opam available in
      opam
    else opam)
  ()
  (* Warning : no conversion done on the _variable_ ocaml-version *)
;;

