(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamTypes

type args = {
  pkg: name;
  os: string list;
  deps: string list;
}

let package_name =
  let parse str =
    try `Ok (OpamPackage.Name.of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf pkg = Format.pp_print_string ppf (OpamPackage.Name.to_string pkg) in
  parse, print

let args =
  let open Cmdliner in
  let os =
    let doc = "Operating system tag" in
    Arg.(value & opt_all string [] & info ["os"] ~doc)
  in
  let deps =
    let doc = "Extdep tag" in
    Arg.(value & opt_all string [] & info ["dep"] ~doc)
  in
  let pkg =
    let doc = "OPAM package name" in
    Arg.(required & pos 1 (some package_name) None & info [] ~doc)
  in
  Term.(pure (fun os deps pkg -> { os; deps; pkg }) $ os $ deps $ pkg)

let process args =

  let repo = OpamRepositoryBackend.local (OpamFilename.cwd ()) in

  let packages = OpamRepository.packages_with_prefixes repo in

  (* packages *)
  OpamPackage.Map.iter (fun package prefix ->
      OpamConsole.msg "Processing (package) %s\n" (OpamPackage.to_string package);
      (* OPAM *)
      let opam_f = OpamRepositoryPath.opam repo prefix package in
      let opam = OpamFile.OPAM.read opam_f in
      let pkgname = OpamFile.OPAM.name opam in
      if pkgname = args.pkg then begin
        let depexts =
          let os = OpamStd.String.Set.of_list args.os in
          let deps = OpamStd.String.Set.of_list args.deps in
          match OpamFile.OPAM.depexts opam with
          | None -> OpamStd.String.SetMap.of_list [ os, deps ]
          | Some depexts' -> (* TODO: Replace existing entry? *)
            OpamStd.String.SetMap.add os deps depexts'
        in
        let opam = OpamFile.OPAM.with_depexts opam depexts in
        OpamFile.OPAM.write opam_f opam;
      end;
    ) packages
