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
    List.map (fun f ->
        OpamFilename.download ~overwrite:true f
          (OpamPath.Repository.files repo prefix nv)
        |> OpamFilename.basename
      )
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

iter_packages ~opam:(fun _ opam ->
    let ocaml_version = OpamFile.OPAM.ocaml_version opam in
    match ocaml_version with
    | None -> opam
    | Some v ->
      let depends = OpamFormula.(
          And (Atom (OpamPackage.Name.of_string "ocaml",
                     ([],
                      map (fun (op,v) ->
                          Atom (op, OpamPackage.Version.of_string (OpamCompiler.Version.to_string v))
                        ) v)),
               OpamFile.OPAM.depends opam)
        )
      in
      let opam = OpamFile.OPAM.with_ocaml_version opam None in
      let opam = OpamFile.OPAM.with_depends opam depends in
      opam)
  ()
  (* Warning : no conversion done on the _variable_ ocaml-version *)
;;

