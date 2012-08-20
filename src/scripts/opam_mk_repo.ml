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

open Types

let all, index, packages =
  let usage = Printf.sprintf "%s [-all] [<package>]*" (Stdlib_filename.basename Sys.argv.(0)) in
  let all = ref true in
  let index = ref false in
  let packages = ref [] in
  let specs = Arg.align [
    ("-all"  , Arg.Set all  , Printf.sprintf " Build all package archives (default is %b)" !all);
    ("-index", Arg.Set index, Printf.sprintf " Build indexes only (default is %b)" !index);
  ] in
  let ano p = packages := p :: !packages in
  Arg.parse specs ano usage;
  !all, !index, NV.Set.of_list (List.map NV.of_string !packages)

let () =
  let local_path = Dirname.cwd () in
  Globals.root_path := Dirname.to_string local_path;
  let local_repo = Path.R.of_dirname local_path in

  (* Read urls.txt *)
  let local_index_file = Filename.of_string "urls.txt" in
  let old_index = File.Urls_txt.safe_read local_index_file in

  let new_index = Remote_file.Set.of_list (List.map (fun f ->
    let basename =
      Basename.of_string (Filename.remove_prefix ~prefix:(Dirname.cwd()) f) in
    let perm =
      let s = Unix.stat (Filename.to_string f) in
      s.Unix.st_perm in
    let digest =
      Digest.to_hex (Digest.file (Filename.to_string f)) in
    Remote_file.create basename digest perm
  ) (Filename.rec_list (Path.R.packages_dir local_repo)
   @ Filename.list (Path.R.archives_dir local_repo)
   @ Filename.list (Path.R.compilers_dir local_repo)
  )) in
  File.Urls_txt.write local_index_file new_index;

  let nv_set_of_remotes remotes =
    let aux r = Filename.create (Dirname.cwd ()) (Remote_file.base r) in
    let list = List.map aux (Remote_file.Set.elements remotes) in
    NV.Set.of_list (Utils.filter_map NV.of_filename list) in
  let to_remove = nv_set_of_remotes (Remote_file.Set.diff old_index new_index) in
  let to_add = nv_set_of_remotes (Remote_file.Set.diff new_index old_index) in

  (* Remove the old archive files *)
  NV.Set.iter (fun nv ->
    let archive = Path.R.archive local_repo nv in
    Globals.msg "Removing %s" (Filename.to_string archive);
    Filename.remove archive
  ) to_remove;

  let to_add = NV.Set.inter packages to_add in
  NV.Set.iter Repositories.make_archive to_add;

  (* Create index.tar.gz *)
  let err = Run.command [
    "sh"; "-c"; "tar cz compilers opam descr > index.tar.gz"
  ] in
  if err <> 0 then
    Globals.error_and_exit "Cannot create index.tar.gz"
