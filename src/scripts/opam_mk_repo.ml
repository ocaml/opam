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
   * packages/             contains packages meta-files
   * archives/             might contain some archive
   * compilers/            contains compiler descriptions

   After the script is run with -all, archives/ contains all the
   package archives for the available package meta-files *)

open Types

let log fmt = Globals.log "OPAM-MK-REPO" fmt

let version () =
  Printf.printf "%s: version %s\n" Sys.argv.(0) Globals.version;
  exit 1

let all, index, packages =
  let usage = Printf.sprintf "%s [-all] [<package>]*" (Stdlib_filename.basename Sys.argv.(0)) in
  let all = ref true in
  let index = ref false in
  let packages = ref [] in
  let specs = Arg.align [
    ("-v"       , Arg.Unit version, " Display version information");
    ("--version", Arg.Unit version, " Display version information");
    ("-all"  , Arg.Set all  , Printf.sprintf " Build all package archives (default is %b)" !all);
    ("-index", Arg.Set index, Printf.sprintf " Build indexes only (default is %b)" !index);
  ] in
  let ano p = packages := p :: !packages in
  Arg.parse specs ano usage;
  !all, !index, NV.Set.of_list (List.map NV.of_string !packages)

let () =
  let local_path = Dirname.cwd () in
  log "local_path=%s" (Dirname.to_string local_path);

  Globals.root_path := Dirname.to_string local_path;
  let local_repo = Path.R.of_dirname local_path in

  (* Read urls.txt *)
  log "Reading urls.txt";
  let local_index_file = Filename.of_string "urls.txt" in
  let old_index = File.Urls_txt.safe_read local_index_file in
  let new_index = Curl.make_urls_txt local_repo in

  let to_remove = Remote_file.Set.diff old_index new_index in
  let to_add = Remote_file.Set.diff new_index old_index in

  let nv_set_of_remotes remotes =
    let aux r = Filename.create (Dirname.cwd ()) (Remote_file.base r) in
    let list = List.map aux (Remote_file.Set.elements remotes) in
    NV.Set.of_list (Utils.filter_map NV.of_filename list) in
  let new_index = nv_set_of_remotes new_index in
  let missing_archive =
    NV.Set.filter (fun nv ->
      let archive = Path.R.archive local_repo nv in
      not (Filename.exists archive)
    ) new_index in
  let to_remove = nv_set_of_remotes to_remove in
  let to_add = NV.Set.union (nv_set_of_remotes to_add) missing_archive in
  let to_add =
    if NV.Set.is_empty packages then
      to_add
    else
      NV.Set.inter packages to_add in

  if not (NV.Set.is_empty to_remove) then
    Globals.msg "Packages to remove: %s\n" (NV.Set.to_string to_remove);
  if not (NV.Set.is_empty to_add) then
    Globals.msg "Packages to build: %s\n" (NV.Set.to_string to_add);

  (* Remove the old archive files *)
  NV.Set.iter (fun nv ->
    let archive = Path.R.archive local_repo nv in
    Globals.msg "Removing %s ...\n" (Filename.to_string archive);
    Filename.remove archive
  ) to_remove;

  let errors = ref [] in
  if not index then
    NV.Set.iter (fun nv ->
      try Repositories.make_archive nv
      with _ -> errors := nv :: !errors;
    ) to_add;

  (* Create index.tar.gz *)
  if not (NV.Set.is_empty to_add) && not (NV.Set.is_empty to_remove) then (
    Globals.msg "Creating index.tar.gz ...\n";
    Curl.make_index_tar_gz local_repo;
  ) else
    Globals.msg "OPAM Repository already up-to-date\n";

  Run.remove "log";
  Run.remove "tmp";

  if !errors <> [] then
    Globals.msg "Got some errors while processing: %s"
      (String.concat ", " (List.map NV.to_string !errors))
