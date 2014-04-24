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

(* Script to add library/syntax info *)
open OpamTypes

module StringSet = OpamMisc.StringSet

type args = {
  pkg: name;
  lib: StringSet.t;
  syntax: StringSet.t;
  infer: bool;
}

let args =
  let open Cmdliner in
  let infer =
    let doc = "Infer the library list by looking at the contents of the \
               `remove` field." in
    Arg.(value & flag & info ~doc ["infer"]) in
  let lib =
    let doc = "Library name" in
    Arg.(value & opt_all string [] & info ["lib"] ~doc)
  in
  let syntax =
    let doc = "Syntax extension name" in
    Arg.(value & opt_all string [] & info ["syntax"] ~doc)
  in
  let pkg =
    let doc = "OPAM package name" in
    Arg.(required & pos 0 (some OpamArg.package_name) None & info [] ~doc)
  in
  Term.(pure (fun infer lib syntax pkg ->
      let lib = StringSet.of_list lib in
      let syntax = StringSet.of_list syntax in
      { infer; lib; syntax; pkg }
    ) $ infer $ lib $ syntax $ pkg)

let process args =
  let repo = OpamRepository.local (OpamFilename.cwd ()) in

  let packages = OpamRepository.packages_with_prefixes repo in

  (** packages *)
  OpamPackage.Map.iter (fun package prefix ->
      let opam_f = OpamPath.Repository.opam repo prefix package in
      let opam = OpamFile.OPAM.read opam_f in
      let pkgname = OpamFile.OPAM.name opam in
      if pkgname = args.pkg then begin
        OpamGlobals.msg "Processing (package) %s\n" (OpamPackage.to_string package);
        let libs   = StringSet.of_list (List.map fst (OpamFile.OPAM.syntax opam)) in
        let libs   = StringSet.union libs args.lib in
        let syntax = StringSet.of_list (List.map fst (OpamFile.OPAM.syntax opam)) in
        let syntax = StringSet.union syntax args.syntax in

        let libs = ref libs in
        let syntax = ref syntax in
        if args.infer then (
          let cmds = OpamFile.OPAM.remove opam in
          List.iter (fun (cmd: OpamTypes.command) ->
              match fst cmd with
              | (CString "ocamlfind",_) :: l ->
                let l = OpamMisc.filter_map (function
                    | CString s, _ -> Some s
                    | _ -> None
                  ) (List.tl l) in
                let ls, ss = List.partition (OpamMisc.ends_with ~suffix:".syntax") l in
                List.iter (fun l -> libs := OpamMisc.StringSet.add l !libs) ls;
                List.iter (fun s -> syntax := OpamMisc.StringSet.add s !syntax) ss;
              | _ -> ()
            ) cmds;
        );

        let libs = List.map (fun l -> l, None) (StringSet.elements !libs) in
        let syntax = List.map (fun s -> s, None) (StringSet.elements !syntax) in
        let opam = OpamFile.OPAM.with_libraries opam libs in
        let opam = OpamFile.OPAM.with_syntax opam syntax in
        OpamFile.OPAM.write opam_f opam;
      end;
    ) packages
