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

let vars_new_1_2 = [ "compiler"; "ocaml-native"; "ocaml-native-tools";
                     "ocaml-native-dynlink"; "arch" ]

let replace_vars_str = function
  | "compiler" -> "ocaml:compiler"
  | "preinstalled" -> "ocaml:preinstalled"
  | "ocaml-version" -> "ocaml:version"
  | "ocaml-native" -> "ocaml:native"
  | "ocaml-native-tools" -> "ocaml:native-tools"
  | "ocaml-native-dynlink" -> "ocaml:native-dynlink"
  | v -> v

let replace_vars v = OpamVariable.of_string (replace_vars_str (OpamVariable.to_string v))

let filter_string =
  let rex =
    Re.(compile ( seq [
        str "%{";
        rep (seq [opt (char '%'); opt (char '}'); diff notnl (set "}%")]);
        str "}%";
      ]))
  in
  Re_pcre.substitute ~rex ~subst:(fun s ->
      "%{" ^ replace_vars_str (String.sub s 2 (String.length s - 4)) ^ "}%")

let rec map_filter = function
  | FIdent ([],i,df) -> FIdent ([], replace_vars i, df)
  | FString s -> FString (filter_string s)
  | FBool _ | FIdent _ | FUndef as f -> f
  | FOp (f1,op,f2) -> FOp (map_filter f1, op, map_filter f2)
  | FAnd (f1,f2) -> FAnd (map_filter f1, map_filter f2)
  | FOr (f1,f2) -> FOr (map_filter f1, map_filter f2)
  | FNot f -> FNot (map_filter f)

let filter_vars_optlist ol =
  List.map (fun (x, filter) -> x, OpamStd.Option.Op.(filter >>| map_filter)) ol

let filter_args sl =
  List.map
    (fun (s, filter) ->
       (match s with
        | CString s -> CString (filter_string s)
        | CIdent i -> CIdent (replace_vars_str i)),
       OpamStd.Option.Op.(filter >>| map_filter))
    sl

let filter_vars_commands ol =
  List.map
    (fun (args, filter) -> filter_args args,
                           OpamStd.Option.Op.(filter >>| map_filter))
    ol
;;

iter_compilers_gen @@ fun c ~prefix ~comp ~descr ->
  let version =
    OpamPackage.Version.of_string
      (OpamCompiler.to_string (OpamFile.Comp.name comp))
  in
  let nv = OpamPackage.create (OpamPackage.Name.of_string "ocaml") version in
  (* OpamConsole.msg "Processing compiler %s => package %s\n" *)
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
        OpamDownload.download ~overwrite:true f
          (OpamRepositoryPath.files repo prefix nv)
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
    |> O.with_env @ OpamFile.Comp.env comp
    |> O.with_flags @ [Pkgflag_Compiler]
  in
  (match OpamFile.Comp.src comp with
   | None -> ()
   | Some address ->
     let url = OpamFile.URL.create address in
     OpamFile.URL.write (OpamRepositoryPath.url repo prefix nv) url);
  OpamFile.OPAM.write (OpamRepositoryPath.opam repo prefix nv) opam;
  OpamStd.Option.iter
    (OpamFile.Descr.write (OpamRepositoryPath.descr repo prefix nv))
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

module OF = OpamFile.OPAM
;;

iter_packages ~opam:(fun nv opam ->
    if OpamPackage.name_to_string nv <> "ocaml" then
      let available = map_filter (OF.available opam) in
      let available =
        match OF.ocaml_version opam with
        | None -> available
        | Some cstr ->
          let filter = OpamFormula.
          OpamFormula.ands [cstr; available]
      in
      let opam = OpamFile.OPAM.with_ocaml_version opam OpamFormula.Empty in
      let opam = OF.with_available opam available in
      let opam =
        OF.with_build opam (filter_vars_commands (OF.build opam)) in
      let opam =
        OF.with_install opam (filter_vars_commands (OF.install opam)) in
      let opam =
        OF.with_features opam
          (List.map (fun (a,b,f) -> a, b, map_filter f) (OF.features opam)) in
      let opam = OF.with_opam_version opam (OpamVersion.of_string "1.3") in
      let opam = OF.with_patches opam (filter_vars_optlist (OF.patches opam)) in
      let opam = OF.with_libraries opam (filter_vars_optlist (OF.libraries opam)) in
      let opam = OF.with_syntax opam (filter_vars_optlist (OF.syntax opam)) in
      let opam = OF.with_messages opam (filter_vars_optlist (OF.messages opam)) in
      let opam = OF.with_post_messages opam (filter_vars_optlist (OF.post_messages opam)) in
      opam
    else opam)
  ()
;;

