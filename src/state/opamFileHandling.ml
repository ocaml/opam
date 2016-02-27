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

let log = OpamConsole.log "opam-file"

let read_opam dir =
  let (opam_file: OpamFile.OPAM.t OpamFile.t) =
    OpamFile.make (dir // "opam")
  in
  let (url_file: OpamFile.URL.t OpamFile.t) =
    OpamFile.make (dir // "url")
  in
  let (descr_file: OpamFile.Descr.t OpamFile.t)  =
    OpamFile.make (dir // "descr")
  in
  let try_read rd f =
    try rd f with
    | (OpamSystem.Internal_error _ | Not_found) as exc ->
      (if OpamFormatConfig.(!r.strict) then
         OpamConsole.error_and_exit
           "Could not read file %s: %s.\nAborting (strict mode)."
       else
         OpamConsole.warning
           "Could not read file %s: %s. Skipping.")
        (OpamFile.to_string f) (Printexc.to_string exc);
      None
    | (Parsing.Parse_error | OpamFormat.Bad_format _ | Lexer_error _) as exc ->
      (if OpamFormatConfig.(!r.strict) then
         OpamConsole.error_and_exit
           "Errors while parsing %s: %s.\nAborting (strict mode)."
       else
         OpamConsole.warning
           "Errors while parsing  %s: %s. Skipping.")
        (OpamFile.to_string f) (Printexc.to_string exc);
      None
  in
  let open OpamStd.Option.Op in
  try_read OpamFile.OPAM.read_opt opam_file >>| fun opam ->
  let opam =
    match try_read OpamFile.URL.read_opt url_file with
    | Some url ->
      if OpamFile.OPAM.url opam <> None then
        log "Overriding url of %s through external url file"
          (OpamFile.to_string opam_file);
      OpamFile.OPAM.with_url opam url
    | None -> opam
  in
  let opam =
    match try_read OpamFile.Descr.read_opt descr_file with
    | Some descr ->
      if OpamFile.OPAM.descr opam <> None then
        log "Overriding descr of %s through external descr file"
          (OpamFile.to_string opam_file);
      OpamFile.OPAM.with_descr opam descr
    | None -> opam
  in
  let extra_files =
    OpamFile.OPAM.metadata_dir opam >>=
    OpamFilename.opt_dir >>| fun dir ->
    let files_dir = OpamFilename.Op.(dir / "files") in
    List.map
      (fun f ->
         OpamFilename.Base.of_string (OpamFilename.remove_prefix files_dir f),
         OpamFilename.digest f)
      (OpamFilename.rec_files files_dir)
  in
  let opam =
    if OpamFile.OPAM.extra_files opam <> None then
      OpamFile.OPAM.with_extra_files opam (extra_files +! [])
    else opam
  in
  opam
