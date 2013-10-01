(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
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

(* Script to check that a given repository is well-typed (or well-parsed) *)
open OpamFilename.OP

type args = {
  normalize: bool;
}

let args =
  let open Cmdliner in
  let normalize =
    let doc = "Normalize all files in the repository." in
    Arg.(value & flag & info ["n";"normalize"] ~doc)
  in
  Term.(pure (fun globals normalize ->
      OpamArg.apply_global_options globals; { normalize }
    ) $ OpamArg.global_options $ normalize)

let process args =

  let write f_write fic st =
    if args.normalize then f_write fic st in

  let repo = OpamRepository.local (OpamFilename.cwd ()) in

  let packages = OpamRepository.packages_with_prefixes repo in

  (** packages *)
  OpamPackage.Map.iter (fun package prefix ->
    OpamGlobals.msg "Processing package %s\n" (OpamPackage.to_string package);

    (** OPAM *)
    let opam = OpamPath.Repository.opam repo prefix package in
    write OpamFile.OPAM.write opam (OpamFile.OPAM.read opam);

    (** Descr *)
    let descr = OpamPath.Repository.descr repo prefix package in
    if OpamFilename.exists descr then
      write OpamFile.Descr.write descr (OpamFile.Descr.read descr);

    (** URL *)
    let url = OpamPath.Repository.url repo prefix package in
    if OpamFilename.exists url then
      write OpamFile.URL.write url (OpamFile.URL.read url);

    (** Dot_install *)
    let dot_install =
      OpamPath.Repository.files repo prefix package
      // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install") in
    if OpamFilename.exists dot_install then
      write
        OpamFile.Dot_install.write dot_install
        (OpamFile.Dot_install.read dot_install);

  ) packages;

  (** compilers *)
  let compilers = OpamRepository.compilers_with_prefixes repo in
  OpamCompiler.Map.iter (fun c prefix ->
      let comp = OpamPath.Repository.compiler_comp repo prefix c in
      let descr = OpamPath.Repository.compiler_descr repo prefix c in
      OpamGlobals.msg "Processing compiler %s\n" (OpamCompiler.to_string c);
      write OpamFile.Comp.write comp (OpamFile.Comp.read comp);
      if OpamFilename.exists descr then
        write OpamFile.Descr.write descr (OpamFile.Descr.read descr);
  ) compilers
