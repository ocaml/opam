(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 Thomas Gazagnaire <thomas@gazagnaire.org>            *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Script to add findlib info *)
open OpamTypes

module StringSet = OpamStd.String.Set

type args = {
  src: name;
  dst: name;
}

let args =
  let open Cmdliner in
  let src =
    let doc = "Name of the source package." in
    Arg.(required & pos 0 (some string) None & info ~doc []) in
  let dst =
    let doc = "Name of the destination package." in
    Arg.(required & pos 1 (some string) None & info ~doc []) in
  Term.(pure (fun src dst ->
      let src = OpamPackage.Name.of_string src in
      let dst = OpamPackage.Name.of_string dst in
      { src; dst }
    ) $ src $ dst)

let process args =
  let repo = OpamRepositoryBackend.local (OpamFilename.cwd ()) in
  let packages = OpamRepository.packages_with_prefixes repo in
  OpamPackage.Map.iter (fun package prefix ->
      if OpamPackage.name package = args.src then (
        let new_pkg =
          OpamPackage.create args.dst (OpamPackage.version package)
        in
        if OpamPackage.Map.mem new_pkg packages then (
          OpamConsole.warning
            "Cannot rename %s to %s as the package already exists, skipping."
            (OpamPackage.to_string package) (OpamPackage.to_string new_pkg);
        ) else (
          OpamConsole.msg "Processing %s\n" (OpamPackage.to_string package);
          let path = OpamRepositoryPath.packages repo.repo_root prefix package in
          let new_path =
            let prefix = match prefix with
              | None   -> None
              | Some _ -> Some (OpamPackage.Name.to_string args.dst)
            in
            OpamRepositoryPath.packages repo.repo_root prefix new_pkg
          in
          OpamFilename.move_dir ~src:path ~dst:new_path;
          (* XXX: do we want to rename the findlib packages as well ?? *)
        )
      ) else (
        let opam_f = OpamRepositoryPath.opam repo.repo_root prefix package in
        let opam = OpamFile.OPAM.read opam_f in
        let rename (n, c) =
          if n = args.src then Atom (args.dst, c) else Atom (n, c)
        in
        let depends = OpamFormula.map rename (OpamFile.OPAM.depends opam) in
        let depopts = OpamFormula.map rename (OpamFile.OPAM.depopts opam) in
        let new_opam =
          OpamFile.OPAM.with_depends depends
            (OpamFile.OPAM.with_depopts depopts opam)
        in

        if opam <> new_opam then (
          OpamConsole.msg "Processing %s\n" (OpamPackage.to_string package);
          OpamFile.OPAM.write opam_f new_opam
        );
      ))
    packages
