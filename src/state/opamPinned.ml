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

open OpamTypes
open OpamStateTypes
open OpamFilename.Op

let package st name = OpamPackage.package_of_name st.pinned name

let package_opt st name = try Some (package st name) with Not_found -> None

let version st name = (package st name).version

let packages st = st.pinned

let possible_definition_filenames dir name = [
  dir / (OpamPackage.Name.to_string name ^ ".opam") // "opam";
  dir // (OpamPackage.Name.to_string name ^ ".opam");
  dir / "opam" // "opam";
  dir // "opam"
]

let find_opam_file_in_source name dir =
  OpamStd.Option.map OpamFile.make
    (OpamStd.List.find_opt OpamFilename.exists
       (possible_definition_filenames dir name))

let orig_opam_file opam =
  let open OpamStd.Option.Op in
  OpamFile.OPAM.metadata_dir opam >>= fun dir ->
  OpamStd.List.find_opt OpamFilename.exists [
    dir // (OpamPackage.Name.to_string (OpamFile.OPAM.name opam) ^ ".opam");
    dir // "opam"
  ] >>|
  OpamFile.make
