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

open OpamTypesBase
open OpamFilename.Op

let read_opam dir =
  let (opam_file: OpamFile.OPAM.t OpamFile.t) = OpamFile.make (dir // "opam") in
  let (url_file: OpamFile.URL.t OpamFile.t) = OpamFile.make (dir // "url") in
  let (descr_file: OpamFile.Descr.t OpamFile.t)  = OpamFile.make (dir // "descr") in
  let opam =
    try Some (OpamFile.OPAM.read opam_file) with
    | OpamSystem.Internal_error _ | Not_found -> None
    | Parsing.Parse_error | OpamFormat.Bad_format _ | Lexer_error _ ->
      OpamConsole.warning "Errors while parsing %s, skipping."
        (OpamFilename.to_string opam_file);
      None
  in
  match opam with
  | Some opam ->
    let opam =
      if OpamFilename.exists url_file then
        try OpamFile.OPAM.with_url opam (OpamFile.URL.read url_file)
        with e ->
          OpamStd.Exn.fatal e;
          OpamConsole.warning "Errors while parsing %s, skipping."
            (OpamFilename.to_string url_file);
          opam
      else opam
    in
    let opam =
      if OpamFilename.exists descr_file then
        try OpamFile.OPAM.with_descr opam (OpamFile.Descr.read descr_file)
        with e ->
          OpamStd.Exn.fatal e;
          OpamConsole.warning "Errors while parsing %s, skipping."
            (OpamFilename.to_string descr_file);
          opam
      else opam
    in
    Some opam
  | None -> None

