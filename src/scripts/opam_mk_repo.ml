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

let tmp_dir = F.concat F.temp_dir_name "opam-mk-repo"

open Types

let root = Path.R.of_path (Dirname.cwd ())

let opams = Path.R.available root

let archives =
  NV.Set.filter (fun nv ->
    not (Filename.exists (Path.R.archive root nv))
  ) opams

let url nv =
  Raw.to_string (Filename.read (Path.R.root root / "url" // NV.to_string nv))

let files nv =
  Path.R.root root / "files" / NV.to_string nv

let tmp nv =
  Path.R.root root / "tmp" / NV.to_string nv

let tmp_dir nv =
  Dirname.of_string tmp_dir / NV.to_string nv

let wget src =
  match Globals.os with
  | Globals.Darwin -> Printf.sprintf "ftp %s" src
  | _ -> Printf.sprintf "wget %s" src

let () =
  Dirname.mkdir (Path.R.archive_dir root);
  NV.Set.iter (fun nv ->
    let url = url nv in
    let tmp_dir = tmp_dir nv in
    let err = Dirname.exec (tmp nv) [ wget url ] in
    if err = 0 then (
      Dirname.rmdir tmp_dir;
      Filename.extract (tmp nv // F.basename url) tmp_dir;
      List.iter (fun f ->
        Filename.copy_in f tmp_dir
      ) (Filename.list (files nv));
      let err = Dirname.exec (Path.R.archive_dir root)
        [ Printf.sprintf
            "tar cz %s > %s.tar.gz"
            (Dirname.to_string tmp_dir)
            (NV.to_string nv)
        ] in
      if err <> 0 then
        Globals.error_and_exit "Cannot compress %s" (Dirname.to_string tmp_dir)
    ) else
      Globals.error_and_exit "Cannot get %s" url;
  ) opams
