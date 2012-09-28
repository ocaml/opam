(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
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

let all, index, packages, gener_digest, dryrun =
  let usage = Printf.sprintf "%s [-all] [<package>]*" (Stdlib_filename.basename Sys.argv.(0)) in
  let all = ref true in
  let index = ref false in
  let packages = ref [] in
  let gener_digest = ref false in
  let dryrun = ref false in
  let specs = Arg.align [
    ("-v"       , Arg.Unit Globals.version, " Display version information");
    ("--version", Arg.Unit Globals.version, " Display version information");

    ("-a"   , Arg.Set all, "");
    ("--all", Arg.Set all  , Printf.sprintf " Build all package archives (default is %b)" !all);

    ("-i"     , Arg.Set index, "");
    ("--index", Arg.Set index, Printf.sprintf " Build indexes only (default is %b)" !index);

    ("-g"                  , Arg.Set gener_digest, "");
    ("--generate-checksums", Arg.Set gener_digest,
     Printf.sprintf " Automatically correct the wrong archive checksums (default is %b)" !gener_digest);

    ("-d"      , Arg.Set dryrun, "");
    ("--dryrun", Arg.Set dryrun, " Simply display the possible actions instead of executing them")
  ] in
  let ano p = packages := p :: !packages in
  Arg.parse specs ano usage;
  !all, !index, N.Set.of_list (List.map N.of_string !packages), !gener_digest, !dryrun

let () =
  let local_path = Dirname.cwd () in
  log "local_path=%s" (Dirname.to_string local_path);

  Globals.root_path := Dirname.to_string local_path;
  let local_repo = Path.R.of_dirname local_path in

  (* Read urls.txt *)
  log "Reading urls.txt";
  let local_index_file = Filename.of_string "urls.txt" in
  let old_index = OpamFile.Urls_txt.safe_read local_index_file in
  let new_index = Curl.make_urls_txt local_repo in

  let to_remove = Remote_file.Set.diff old_index new_index in
  let to_add = Remote_file.Set.diff new_index old_index in

  (* Compute the transitive closure of packages *)
  let get_dependencies n =
    let versions = Path.R.available_versions local_repo n in
    let depends v =
      let nv = NV.create n v in
      let opam_f = Path.R.opam local_repo nv in
      if Filename.exists opam_f then (
        let opam = OpamFile.OPAM.read opam_f in
        let deps = OpamFile.OPAM.depends opam in
        let depopts = OpamFile.OPAM.depopts opam in
        List.fold_left (fun accu l ->
          List.fold_left (fun accu ((n,_),_) ->
            N.Set.add (N.of_string n) accu
          ) accu l
        ) N.Set.empty (deps @ depopts)
      ) else
        N.Set.empty in
    V.Set.fold (fun v set ->
      N.Set.union (depends v) set
    ) versions (N.Set.singleton n) in
  let rec get_transitive_dependencies names =
    let new_names =
      N.Set.fold (fun n set -> N.Set.union (get_dependencies n) set) names N.Set.empty in
    if N.Set.cardinal names = N.Set.cardinal new_names then
      names
    else
      get_transitive_dependencies new_names in
  let packages = get_transitive_dependencies packages in
  let packages =
    N.Set.fold (fun n set ->
      let versions = Path.R.available_versions local_repo n in
      V.Set.fold (fun v set -> NV.Set.add (NV.create n v) set) versions set
    ) packages NV.Set.empty in

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

  let errors = ref [] in
  if not index then (

    (* Remove the old archive files *)
    if not (NV.Set.is_empty to_remove) then
      Globals.msg "Packages to remove: %s\n" (NV.Set.to_string to_remove);
    NV.Set.iter (fun nv ->
      let archive = Path.R.archive local_repo nv in
      Globals.msg "Removing %s ...\n" (Filename.to_string archive);
      if not dryrun then
        Filename.remove archive
    ) to_remove;

    (* build the new archives *)
    if not (NV.Set.is_empty to_add) then
      Globals.msg "Packages to build: %s\n" (NV.Set.to_string to_add);
    NV.Set.iter (fun nv ->
      let archive = Path.R.archive local_repo nv in
      try
        if not dryrun then (
          Filename.remove archive;
          Repositories.make_archive ~gener_digest nv
        ) else
          Globals.msg "Building %s\n" (Filename.to_string archive)
      with e ->
        Filename.remove archive;
        errors := (nv, e) :: !errors;
    ) to_add;
  );

  (* Create index.tar.gz *)
  if not (Filename.exists (local_path // "index.tar.gz"))
  || not (NV.Set.is_empty to_add)
  || not (NV.Set.is_empty to_remove) then (
    Globals.msg "Rebuilding index.tar.gz ...\n";
    if not dryrun then
      Curl.make_index_tar_gz local_repo;
  ) else
    Globals.msg "OPAM Repository already up-to-date.\n";

  Run.remove "log";
  Run.remove "tmp";

  (* Rebuild urls.txt now the archives have been updated *)
  if dryrun then
    Globals.msg "Rebuilding urls.txt\n"
  else
    let _index = Curl.make_urls_txt local_repo in

    if !errors <> [] then
      let display_error (nv, error) =
        Globals.error "[ERROR] Error while processing %s" (NV.to_string nv);
        match error with
        | Run.Process_error r  -> Process.display_error_message r
        | Run.Internal_error s -> Globals.error "  %s" s
        | _ -> Globals.error "%s" (Printexc.to_string error) in
      let all_errors = List.map fst !errors in
      Globals.error "Got some errors while processing: %s"
        (String.concat ", " (List.map NV.to_string all_errors));
      List.iter display_error !errors
