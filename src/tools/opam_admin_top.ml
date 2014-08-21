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

let wopt w f = function
  | None -> OpamFilename.remove f
  | Some contents -> w f contents

let map_packages_gen f =
  let packages = OpamRepository.packages_with_prefixes repo in
  let changed_pkgs = ref 0 in
  let changed_files = ref 0 in

  (** packages *)
  OpamPackage.Map.iter (fun package prefix ->
      OpamGlobals.msg "Processing package %s... " (OpamPackage.to_string package);

      let opam_file = OpamPath.Repository.opam repo prefix package in
      let opam = OpamFile.OPAM.read opam_file in

      let descr_file = OpamPath.Repository.descr repo prefix package in
      let descr =
        if OpamFilename.exists descr_file then
          Some (OpamFile.Descr.read descr_file)
        else None
      in

      let url_file = OpamPath.Repository.url repo prefix package in
      let url =
        if OpamFilename.exists url_file then
          Some (OpamFile.URL.read url_file)
        else None
      in

      let dot_install_file =
        OpamPath.Repository.files repo prefix package
        // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install") in
      let dot_install =
        if OpamFilename.exists dot_install_file then
          Some (OpamFile.Dot_install.read dot_install_file)
        else None
      in

      let opam2, descr2, url2, dot_install2 =
        f package ~opam ~descr ~url ~dot_install
      in

      let changed = ref false in
      let upd () = changed := true; incr changed_files in
      if opam <> opam2 then
        (upd (); OpamFile.OPAM.write opam_file opam2);
      if descr <> descr2 then
        (upd (); wopt OpamFile.Descr.write descr_file descr2);
      if url <> url2 then
        (upd (); wopt OpamFile.URL.write url_file url2);
      if dot_install <> dot_install2 then
        (upd (); wopt OpamFile.Dot_install.write dot_install_file dot_install2);

      if !changed then
        (incr changed_pkgs;
         OpamGlobals.msg "\r\027[KUpdated %s\n" (OpamPackage.to_string package))
      else
        OpamGlobals.msg "\r\027[K";
    ) packages;
  OpamGlobals.msg "Done. Updated %d files in %d packages.\n"
    !changed_files !changed_pkgs

let map_packages ?(opam=identity) ?(descr=identity) ?(url=identity) ?(dot_install=identity) () =
  let omap = OpamMisc.Option.map in
  map_packages_gen (fun _ ~opam:o ~descr:d ~url:u ~dot_install:i ->
      opam o, omap descr d, omap url u, omap dot_install i)

let map_compilers_gen f =
  let compilers = OpamRepository.compilers_with_prefixes repo in

  let changed_comps = ref 0 in
  let changed_files = ref 0 in

  OpamCompiler.Map.iter (fun c prefix ->
      OpamGlobals.msg "Processing compiler %s... " (OpamCompiler.to_string c);

      let comp_file = OpamPath.Repository.compiler_comp repo prefix c in
      let comp = OpamFile.Comp.read comp_file in

      let descr_file = OpamPath.Repository.compiler_descr repo prefix c in
      let descr =
        if OpamFilename.exists descr_file then
          Some (OpamFile.Descr.read descr_file)
        else None
      in

      let comp2, descr2 =
        f c ~comp ~descr
      in

      let changed = ref false in
      let upd () = changed := true; incr changed_files in

      if comp <> comp2 then
        (upd (); OpamFile.Comp.write comp_file comp2);
      if descr <> descr2 then
        (upd (); wopt OpamFile.Descr.write descr_file descr2);

      if !changed then
        (incr changed_comps;
         OpamGlobals.msg "\r\027[KUpdated %s\n" (OpamCompiler.to_string c))
      else
        OpamGlobals.msg "\r\027[K";
    ) compilers;
  OpamGlobals.msg "Done. Updated %d files in %d compiler descriptions.\n"
    !changed_files !changed_comps

let map_compilers ?(comp=identity) ?(descr=identity) () =
  map_compilers_gen (fun _ ~comp:c ~descr:d ->
      comp c, OpamMisc.Option.map descr d)
