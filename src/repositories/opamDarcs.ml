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

(* Note: this module is heavily inspired by (and tries to stay close to) OpamGit *)

open OpamTypes
open OpamFilename.OP

let log fmt = OpamGlobals.log "Darcs" fmt

let darcs_fetch local_path remote_address =
  OpamGlobals.msg "Synchronizing %s with %s.\n"
    (OpamFilename.Dir.to_string local_path)
    (OpamFilename.Dir.to_string remote_address);
  OpamFilename.in_dir local_path (fun () ->
    (* Fetch the changes and save them to a temporary patch bundle *)
    OpamSystem.command [ "darcs" ; "fetch"; "--all"; "--output=opam_update.bundle"]
  )

let darcs_merge local_path =
  OpamFilename.in_dir local_path (fun () ->
    let patches_bundle = OpamFilename.of_string "opam_update.bundle" in
    if OpamFilename.exists patches_bundle then
      OpamSystem.command [ "darcs" ; "apply"; "opam_update.bundle" ];
      OpamFilename.remove patches_bundle
  )

(* Look for file pathes {packages,compilers}/* in a set of XML lines. *)
let files_of_xmlchanges lines =
  let rex = Re_perl.compile_pat "((packages|compilers)((/[\\.\\w]+))+)" in
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
      try
        let subs = Re.exec rex h in
        let file = Re.get subs 1 in
        aux (file :: acc) t
      with Not_found ->
        aux acc t
  in
  aux [] lines

(* Return the list of modified files of the darcs repository located
   at [dirname].
   There is no simple way to get a diff of files with differences between the
   local and a remote repository, with darcs, as 'git diff --name-only'. We use
   the following workaround:
   1. Tag the current state of the repo 'opam_update'
   2. Pull (fetch and apply) all new patches
   3. Get the changes made to the repo since the 'opam_update' tag, in a XML format
   4. Back to initial state: obliterate the 'opam_update' tag and all subsequent patches *)
let darcs_diff local_path =
  OpamFilename.in_dir local_path (fun () ->
    OpamSystem.commands [
      [ "darcs" ; "tag" ; "--author=opam@ocamlpro.com" ; "opam_update" ] ;
      [ "darcs" ; "pull"; "--all" ]
    ];
    let xml_changes = OpamSystem.read_command_output
      [ "darcs" ; "changes" ; "--xml-output" ; "--summary" ; "--from-tag=opam_update" ] in
    let files = files_of_xmlchanges xml_changes in
    OpamSystem.command [ "darcs" ; "obliterate" ; "--all" ; "--from-tag=opam_update" ];
    OpamFilename.Set.of_list (List.map OpamFilename.of_string files)
  )

let darcs_init address =
  let repo = OpamFilename.Dir.to_string address in
  OpamSystem.commands [
    (* Initialize a new darcs repository, and set a default source repository.
       The dummy tag prevents the patches from being actually fetched. We use
       the 'fetch' command because there is no dedicated command with darcs to set
       a remote repository, as 'git remote'. *)
    [ "darcs" ; "initialize" ];
    [ "darcs" ; "fetch" ; "--tags=ThisIsADummyTag#00"; "--set-default" ; repo ];
  ]

let check_updates local_path remote_address=
  if OpamFilename.exists_dir (local_path / "_darcs") then begin
    darcs_fetch local_path remote_address;
    let files = darcs_diff local_path in
    darcs_merge local_path;
    Some files
  end else
    None

module B = struct

  let updates r =
    OpamPath.Repository.root r // "last-darcs-updates"

  let check_file file =
    let local_repo = OpamRepository.local_repo () in
    let updates = OpamFile.Filenames.read (updates local_repo) in
    if OpamFilename.Set.mem file updates then
      Result file
    else if OpamFilename.exists file then
      Up_to_date file
    else
      Not_available

  let init ~address =
    log "init";
    let local_repo = OpamRepository.local_repo () in
    darcs_init address;
    OpamFile.Filenames.write (updates local_repo) (OpamFilename.Set.empty)

  let download_archive ~address:_ nv =
    log "download_archive";
    let local_repo = OpamRepository.local_repo () in
    let archive = OpamPath.Repository.archive local_repo nv in
    check_file archive

  let download_file ?checksum:_ nv filename =
    log "download_file";
    let local_repo = OpamRepository.local_repo () in
    let basename = OpamFilename.basename filename in
    let file = OpamPath.Repository.tmp_dir local_repo nv // OpamFilename.Base.to_string basename in
    check_file file

  let rec download_dir nv ?dst remote_address =
    log "download_dir";
    let local_repo = OpamRepository.local_repo () in
    let dir = match dst with
      | None   ->
        let basename = OpamFilename.Base.to_string (OpamFilename.basename_dir remote_address) in
        OpamPath.Repository.tmp_dir local_repo nv / basename
      | Some d -> d in
    match check_updates dir remote_address with
    | None ->
        OpamFilename.mkdir dir;
        OpamFilename.in_dir dir (fun () -> darcs_init remote_address);
        download_dir nv ?dst remote_address
    | Some f ->
        if OpamFilename.Set.empty = f then
          Up_to_date dir
        else
          Result dir

  let update ~address =
    log "update";
    let local_repo = OpamRepository.local_repo () in
    let local_dir = OpamPath.Repository.root local_repo in
    match check_updates local_dir address with
    | Some f -> OpamFile.Filenames.write (updates local_repo) f; f
    | None   ->
        OpamGlobals.error_and_exit
          "The repository %s is not initialized correctly"
          (OpamFilename.Dir.to_string local_dir)

  let upload_dir ~address:_ dirname =
    log "upload_dir";
    let files = OpamFilename.list_files dirname in
    try
      OpamSystem.commands [
        [ "darcs"; "add"; OpamFilename.Dir.to_string dirname; ];
        [ "darcs"; "record"; "--all"; "-m"; "upload new files" ];
        [ "darcs"; "push"; "--all"]
      ];
      OpamFilename.Set.of_list files
    with _ ->
      OpamFilename.Set.empty

end

let register () =
  OpamRepository.register_backend `darcs (module B: OpamRepository.BACKEND)
