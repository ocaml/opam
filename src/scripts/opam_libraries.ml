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

module StringMap = OpamMisc.StringMap

type args = {
  pkg: name;
  lib: filter option StringMap.t;
  syntax: filter option StringMap.t;
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
      let mk x = StringMap.of_list (List.map (fun l -> l, None) x) in
      let lib =  mk lib in
      let syntax = mk syntax in
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
        let merge x y = match x,y with
          | None  , None   -> None
          | Some x, None
          | None  , Some x -> Some x
          | Some x, Some _ -> Some x in
        let libs   = StringMap.of_list (OpamFile.OPAM.libraries opam) in
        let libs   = StringMap.union merge args.lib libs in
        let syntax = StringMap.of_list (OpamFile.OPAM.syntax opam) in
        let syntax = StringMap.union merge args.syntax syntax in

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
                let ss, ls = List.partition (OpamMisc.ends_with ~suffix:".syntax") l in
                List.iter (fun l -> libs := StringMap.add l None !libs) ls;
                List.iter (fun s -> syntax := StringMap.add s None !syntax) ss;
              | _ -> ()
            ) cmds;
        );

        let libs = StringMap.bindings !libs in
        let syntax = StringMap.bindings !syntax in
        let opam = OpamFile.OPAM.with_libraries opam libs in
        let opam = OpamFile.OPAM.with_syntax opam syntax in
        OpamFile.OPAM.write opam_f opam;
      end;
    ) packages
