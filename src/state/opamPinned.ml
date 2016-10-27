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

let files_in_source d =
  let baseopam = OpamFilename.Base.of_string "opam" in
  let files =
    List.filter (fun f ->
        OpamFilename.basename f = baseopam ||
        OpamFilename.check_suffix f ".opam")
      (OpamFilename.files d) @
    OpamStd.List.filter_map (fun d ->
        if OpamFilename.(basename_dir d = Base.of_string "opam") ||
           OpamStd.String.ends_with ~suffix:".opam"
             (OpamFilename.Dir.to_string d)
        then OpamFilename.opt_file OpamFilename.Op.(d//"opam")
        else None)
      (OpamFilename.dirs d)
  in
  List.map
    (fun f ->
       let name =
         let b =
           if OpamFilename.(basename f = baseopam) then
             OpamFilename.(Base.to_string (basename_dir (dirname f)))
           else
             OpamFilename.(Base.to_string (basename f))
         in
         if b = "opam" then None else
         try
           Some (OpamPackage.Name.of_string
                   (OpamStd.String.remove_suffix ~suffix:".opam" b))
         with Failure _ -> None
       in
       name, OpamFile.make f)
    files

let orig_opam_file opam =
  let open OpamStd.Option.Op in
  OpamFile.OPAM.metadata_dir opam >>= fun dir ->
  OpamStd.List.find_opt OpamFilename.exists [
    dir // (OpamPackage.Name.to_string (OpamFile.OPAM.name opam) ^ ".opam");
    dir // "opam"
  ] >>|
  OpamFile.make
