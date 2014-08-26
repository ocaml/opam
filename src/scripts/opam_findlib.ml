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

(* Script to add findlib info *)
open OpamTypes
open OpamFilename.OP
open OpamMisc.OP

module StringSet = OpamMisc.StringSet

type args = {
  opam_pkg: name;
  findlib_pkgs: string list;
  infer: bool;
}

let package_name =
  let parse str =
    try `Ok (OpamPackage.Name.of_string str)
    with Failure msg -> `Error msg
  in
  let print ppf pkg =
    Format.pp_print_string ppf (OpamPackage.Name.to_string pkg) in
  parse, print

let args =
  let open Cmdliner in
  let infer =
    let doc = "Infer the `findlib' file by looking at the contents of the \
               `remove` field." in
    Arg.(value & flag & info ~doc ["infer"]) in
  let findlib_pkgs =
    let doc = "Findlib package name" in
    Arg.(value & opt (list string) [] & info ["pkg"] ~doc
           ~docv:"FINDLIB-PKGS")
  in
  let opam_pkg =
    let doc = "OPAM package name" in
    Arg.(required & pos 0 (some package_name) None & info [] ~doc
           ~docv:"OPAM-PKG")
  in
  Term.(pure (fun infer findlib_pkgs opam_pkg ->
      { infer; findlib_pkgs; opam_pkg }
    ) $ infer $ findlib_pkgs $ opam_pkg)

let process args =
  let repo = OpamRepository.local (OpamFilename.cwd ()) in

  let packages = OpamRepository.packages_with_prefixes repo in

  OpamPackage.Map.iter (fun package prefix ->
      let opam_f = OpamPath.Repository.opam repo prefix package in
      let opam = OpamFile.OPAM.read opam_f in
      let pkgname = OpamFile.OPAM.name opam in
      if pkgname = args.opam_pkg then (
        OpamGlobals.msg "Processing (package) %s\n" (OpamPackage.to_string package);
        let filename = OpamFilename.dirname opam_f // "findlib" in
        let pkgs0 =
          OpamFile.Lines.safe_read filename
          |> List.flatten
          |> StringSet.of_list
        in
        let pkgs1 =
          if args.infer then (
            let cmds = OpamFile.OPAM.remove opam in
            List.fold_left (fun acc (cmd: OpamTypes.command) ->
                match fst cmd with
                | (CString "ocamlfind",_) :: l ->
                  let pkgs = OpamMisc.filter_map (function
                      | CString s, _ -> Some s
                      | _ -> None
                    ) (List.tl l) in
                  StringSet.union acc (StringSet.of_list pkgs);
                | _ -> acc
              ) StringSet.empty cmds
          ) else StringSet.of_list args.findlib_pkgs
        in
        let pkgs = StringSet.union pkgs0 pkgs1 in
        let contents = List.map (fun x -> [x]) (StringSet.elements pkgs) in
        OpamFile.Lines.write filename contents)
    ) packages
