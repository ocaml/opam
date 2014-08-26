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
  opam_pkgs: string list;
  findlib_pkgs: string list;
  infer: bool;
}

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
  let opam_pkgs =
    let doc = "OPAM package pattern" in
    Arg.(value & pos_all string [] & info [] ~doc
           ~docv:"OPAM-PKG")
  in
  Term.(pure (fun infer findlib_pkgs opam_pkgs ->
      { infer; findlib_pkgs; opam_pkgs }
    ) $ infer $ findlib_pkgs $ opam_pkgs)

let process args =
  let repo = OpamRepository.local (OpamFilename.cwd ()) in
  let packages = OpamRepository.packages_with_prefixes repo in
  let regexps =
    List.map (fun pattern ->
        if OpamPackage.Map.exists (fun pkg _ ->
            OpamPackage.Name.to_string (OpamPackage.name pkg) = pattern
          ) packages
        then pattern ^ ".*"
        else pattern
      ) args.opam_pkgs
    |> List.map (fun pattern -> Re.compile (Re_glob.globx pattern))
  in
  let should_process package = match regexps with
    | [] -> true
    | _  ->
      let str = OpamPackage.to_string package in
      List.exists (fun re -> OpamMisc.exact_match re str) regexps
  in
  OpamPackage.Map.iter (fun package prefix ->
      if should_process package then (
        OpamGlobals.msg "Processing (package) %s\n"
          (OpamPackage.to_string package);
        let opam_f = OpamPath.Repository.opam repo prefix package in
        let opam = OpamFile.OPAM.read opam_f in
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
        if contents <> [] then OpamFile.Lines.write filename contents)
    ) packages
