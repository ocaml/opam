(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Script to check that a given repository is well-typed (or well-parsed) *)
open OpamTypes
open OpamFilename.Op

type args = {
  normalize: bool;
}

let args =
  let open Cmdliner in
  let normalize =
    let doc = "Normalize all files in the repository." in
    Arg.(value & flag & info ["n";"normalize"] ~doc)
  in
  Term.(pure (fun normalize -> { normalize }) $ normalize)

let process args =

  let write f_write fic st =
    if args.normalize then f_write fic st in

  let repo = OpamRepositoryBackend.local (OpamFilename.cwd ()) in

  let packages = OpamRepository.packages_with_prefixes repo in

  (* packages *)
  OpamPackage.Map.iter (fun package prefix ->
    OpamConsole.msg "Processing package %s\n" (OpamPackage.to_string package);

    (* OPAM *)
    let opam = OpamRepositoryPath.opam repo.repo_root prefix package in
    write OpamFile.OPAM.write opam (OpamFile.OPAM.read opam);

    (* Descr *)
    let descr = OpamRepositoryPath.descr repo.repo_root prefix package in
    if OpamFile.exists descr then
      write OpamFile.Descr.write descr (OpamFile.Descr.read descr);

    (* URL *)
    let url = OpamRepositoryPath.url repo.repo_root prefix package in
    if OpamFile.exists url then
      write OpamFile.URL.write url (OpamFile.URL.read url);

    (* Dot_install *)
    let dot_install : OpamFile.Dot_install.t OpamFile.t =
      OpamFile.make
        (OpamRepositoryPath.files repo.repo_root prefix package
         // (OpamPackage.Name.to_string (OpamPackage.name package) ^ ".install"))
    in
    if OpamFile.exists dot_install then
      write
        OpamFile.Dot_install.write dot_install
        (OpamFile.Dot_install.read dot_install);

  ) packages
