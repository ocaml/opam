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
  dir / "opam" / (OpamPackage.Name.to_string name ^ ".opam") // "opam";
  dir / "opam" // (OpamPackage.Name.to_string name ^ ".opam");
  dir / "opam" // "opam";
  dir // "opam"
]

let find_opam_file_in_source name dir =
  let opt =
    OpamStd.List.find_opt OpamFilename.exists
      (possible_definition_filenames dir name)
  in
  (match opt, OpamStateConfig.(!r.locked) with
   | Some base, Some ext ->
     let fl = OpamFilename.add_extension base ext in
     if OpamFilename.exists fl then Some fl else opt
   | _ -> opt)
  |> OpamStd.Option.map OpamFile.make

let name_of_opam_filename dir file =
  let open OpamStd.Option.Op in
  let suffix = ".opam" in
  let get_name s =
    if Filename.check_suffix s suffix
    then Some Filename.(chop_suffix (basename s) suffix)
    else None
  in
  let rel = OpamFilename.remove_prefix dir file in
  let rel =
    match OpamStateConfig.(!r.locked) with
    | None -> rel
    | Some suf ->
      let ext = "."^suf in
      if OpamStd.String.ends_with ~suffix:(suffix^ext) rel then
        OpamStd.String.remove_suffix ~suffix:ext rel
      else rel
  in
  (get_name (Filename.basename rel) >>+ fun () ->
   get_name (Filename.dirname rel)) >>= fun name ->
  try Some (OpamPackage.Name.of_string name)
  with Failure _ -> None

let files_in_source d =
  let baseopam = OpamFilename.Base.of_string "opam" in
  let files d =
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
  files d @ files (d / "opam") |>
  List.map
    (fun f ->
       match OpamStateConfig.(!r.locked) with
       | None -> f
       | Some ext ->
         let fl = OpamFilename.add_extension f ext in
         if OpamFilename.exists fl then fl else f) |>
  OpamStd.List.filter_map
    (fun f ->
       try
         (* Ignore empty files *)
         if (Unix.stat (OpamFilename.to_string f)).Unix.st_size = 0 then None
         else Some (name_of_opam_filename d f, OpamFile.make f)
       with Unix.Unix_error _ ->
         OpamConsole.error "Can not read %s, ignored."
           (OpamFilename.to_string f);
         None)

let orig_opam_file name opam =
  let open OpamStd.Option.Op in
  OpamFile.OPAM.metadata_dir opam >>= fun dir ->
  OpamStd.List.find_opt OpamFilename.exists [
    dir // (OpamPackage.Name.to_string name ^ ".opam");
    dir // "opam"
  ] >>|
  OpamFile.make
