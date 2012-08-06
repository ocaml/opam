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

open Repo_helpers
open Types
open Curl

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
  let local_repo = Path.R.of_dirname local_path in
  let state = {
    local_path; local_repo;
    remote_path = local_path;
    remote_repo = local_repo;
  } in

  (* Create urls.txt *)
  let local_index_file = Filename.of_string "urls.txt" in
  let urls = List.map (fun f ->
    let basename =
      Basename.of_string (Filename.remove_prefix ~prefix:(Dirname.cwd()) f) in
    let perm =
      let s = Unix.stat (Filename.to_string f) in
      s.Unix.st_perm in
    let digest =
      Digest.to_hex (Digest.file (Filename.to_string f)) in
    basename, perm, digest
  ) (Filename.rec_list (Path.R.packages_dir local_repo)
   @ Filename.list (Path.R.archives_dir local_repo)
   @ Filename.list (Path.R.compilers_dir local_repo)
  ) in
  File.Urls_txt.write local_index_file urls;

  let updates = Curl.get_updates state in

  (* Update the archive files *)
  NV.Set.iter (fun nv ->
    Globals.msg "Updating %s as some file have changed\n" (NV.to_string nv);
    if Filename.exists (Path.R.archive local_repo nv) then
      let _archive = Curl.make_archive state nv in
      ()
  ) updates;

  (* Create the archives asked by the user *)
  NV.Set.iter (fun nv ->
    if not index && (all || NV.Set.mem nv packages) then begin
      Globals.msg "Creating archive for %s\n" (NV.to_string nv);
      let _archive = Curl.make_archive state nv in
      ()
    end
  ) updates;

  (* Create index.tar.gz *)
  let err = Run.command [
    "sh"; "-c"; "tar cz compilers opam descr > index.tar.gz"
  ] in
  if err <> 0 then
    Globals.error_and_exit "Cannot create index.tar.gz"
