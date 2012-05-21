(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(* A script helper to initialize an OPAM repo.
   It takes as input a directory where:
   * opam/                 contains some OPAM files
   * descr/                contains some description files
   * archives/             might contain some archive
   * url/$name.$version    contains archive address
   * files/$name.$version  contains some files to include in
                           the archives (typically .config.in
                           and .install)

   After the script is run, archives/ contains all the package archives
   for the available descr and OPAM files *)

module F = Filename

let tmp_dir0 = F.concat F.temp_dir_name "opam-mk-repo"

open Types

let root = Path.R.of_path (Dirname.cwd ())

let opams = Path.R.available root

let opams =
  NV.Set.filter (fun nv ->
    not (Filename.exists (Path.R.archive root nv))
  ) opams

let url nv =
  let f = Path.R.root root / "url" // NV.to_string nv in
  if Filename.exists f then
    Some (Utils.string_strip (Raw.to_string (Filename.read f)))
  else
    None

let files nv =
  let d = Path.R.root root / "files" / NV.to_string nv in
  if Dirname.exists d then
    Filename.list d
  else
    []

let tmp nv =
  Path.R.root root / "tmp" / NV.to_string nv

let tmp_dir nv =
  Dirname.of_string tmp_dir0 / NV.to_string nv

let wget src =
  match Globals.os with
  | Globals.Darwin -> Printf.sprintf "ftp %s" src
  | _ -> Printf.sprintf "wget %s" src

let () =
  Dirname.mkdir (Path.R.archive_dir root);
  NV.Set.iter (fun nv ->
    let tmp_dir = tmp_dir nv in
    Dirname.rmdir tmp_dir;
    Dirname.mkdir tmp_dir;
    begin match url nv with
    | None     -> ()
    | Some url ->
        Dirname.mkdir (tmp nv);
        let err = Dirname.exec (tmp nv) [ wget url ] in
        if err = 0 then (
          Filename.extract (tmp nv // F.basename url) tmp_dir;
        ) else
          Globals.error_and_exit "Cannot get %s" url;
    end;
    List.iter (fun f ->
      Filename.copy_in f tmp_dir
    ) (files nv);
    let err = Dirname.exec (Dirname.of_string tmp_dir0)
      [ Printf.sprintf
          "tar cz %s/ > %s"
          (NV.to_string nv)
          (Filename.to_string (Path.R.archive root nv))
      ] in
    if err <> 0 then
      Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_dir)
  ) opams
