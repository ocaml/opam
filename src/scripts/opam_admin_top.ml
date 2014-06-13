(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
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

(* To be used for quick repo scripts using the toplevel *)
open OpamFilename.OP

let identity = fun x -> x

let repo = OpamRepository.local (OpamFilename.cwd ())
let packages = OpamRepository.packages repo
let compilers = OpamRepository.compilers repo 

let map_packages ?(opam=identity) ?(descr=identity) ?(url=identity) ?(dot_install=identity) () =

  let packages = OpamRepository.packages_with_prefixes repo in

  (** packages *)
  OpamPackage.Map.iter (fun package prefix ->
      OpamGlobals.msg "Processing package %s\n" (OpamPackage.to_string package);

      let opam_file = OpamPath.Repository.opam repo prefix package in
      let opam1 = OpamFile.OPAM.read opam_file in
      let opam2 = opam opam1 in
      if opam1 <> opam2 then OpamFile.OPAM.write opam_file opam2;

      let descr_file = OpamPath.Repository.descr repo prefix package in
      if OpamFilename.exists descr_file then (
        let descr1 = OpamFile.Descr.read descr_file in
        let descr2 = descr descr1 in
        if descr1 <> descr2 then OpamFile.Descr.write descr_file descr2
      );

      let url_file = OpamPath.Repository.url repo prefix package in
      if OpamFilename.exists url_file then (
        let url1 = OpamFile.URL.read url_file in
        let url2 = url url1 in
        if url1 <> url2 then OpamFile.URL.write url_file url2
      );

      let dot_install_file =
        OpamPath.Repository.files repo prefix package
        // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install") in
      if OpamFilename.exists dot_install_file then (
        let dot_install1 = OpamFile.Dot_install.read dot_install_file in
        let dot_install2 = dot_install dot_install1 in
        if dot_install1 <> dot_install2 then
          OpamFile.Dot_install.write dot_install_file dot_install2
      );
    ) packages;
  OpamGlobals.msg "Done.\n"

let map_compilers ?(comp=identity) ?(descr=identity) () =
  (** compilers *)
  let compilers = OpamRepository.compilers_with_prefixes repo in

  OpamCompiler.Map.iter (fun c prefix ->
      OpamGlobals.msg "Processing compiler %s\n" (OpamCompiler.to_string c);

      let comp_file = OpamPath.Repository.compiler_comp repo prefix c in
      let comp1 = OpamFile.Comp.read comp_file in
      let comp2 = comp comp1 in
      if comp1 <> comp2 then OpamFile.Comp.write comp_file comp2;

      let descr_file = OpamPath.Repository.compiler_descr repo prefix c in
      if OpamFilename.exists descr_file then (
        let descr1 = OpamFile.Descr.read descr_file in
        let descr2 = descr descr1 in
        if descr2 <> descr1 then OpamFile.Descr.write descr_file descr2
      );
    ) compilers;
  OpamGlobals.msg "Done.\n"
