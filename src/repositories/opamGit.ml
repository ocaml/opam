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

open OpamTypes
open OpamFilename.OP

let log fmt = OpamGlobals.log "GIT" fmt

let git_fetch local_path remote_address =
  OpamGlobals.msg "Fetching %s ...\n" (OpamFilename.Dir.to_string remote_address);
  OpamFilename.in_dir local_path (fun () ->
    OpamSystem.command [ "git" ; "fetch" ; "origin" ]
  )

let git_merge local_path =
  OpamFilename.in_dir local_path (fun () ->
    OpamSystem.command [ "git" ; "merge" ; "origin/master" ]
  )

(* Return the list of modified files of the git repository located
   at [dirname] *)
let git_diff local_path =
  OpamFilename.in_dir local_path (fun () ->
    let lines = OpamSystem.read_command_output
      [ "git" ; "diff" ; "remotes/origin/master" ; "--name-only" ] in
    OpamFilename.Set.of_list (List.map OpamFilename.of_string lines)
  )

let git_init address =
  let repo = OpamFilename.Dir.to_string address in
  OpamSystem.commands [
    [ "git" ; "init" ] ;
    [ "git" ; "remote" ; "add" ; "origin" ; repo ] ;
  ]

let check_updates local_path remote_address=
  if OpamFilename.exists_dir (local_path / ".git") then begin
    git_fetch local_path remote_address;
    let files = git_diff local_path in
    git_merge local_path;
    Some files
  end else
    None

module B = struct

  let updates r =
    OpamPath.Repository.root r // "last-git-updates"

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
    let local_repo = OpamRepository.local_repo () in
    git_init address;
    OpamFile.Filenames.write (updates local_repo) (OpamFilename.Set.empty)

  let download_archive ~address nv =
    let local_repo = OpamRepository.local_repo () in
    let archive = OpamPath.Repository.archive local_repo nv in
    check_file archive

  let download_file nv filename =
    let local_repo = OpamRepository.local_repo () in
    let basename = OpamFilename.basename filename in
    let file = OpamPath.Repository.tmp_dir local_repo nv // OpamFilename.Base.to_string basename in
    check_file file

  let rec download_dir nv ?dst remote_address =
    let local_repo = OpamRepository.local_repo () in
    let dir = match dst with
      | None   ->
        let basename = OpamFilename.Base.to_string (OpamFilename.basename_dir remote_address) in
        OpamPath.Repository.tmp_dir local_repo nv / basename
      | Some d -> d in
    match check_updates dir remote_address with
    | None ->
        OpamFilename.mkdir dir;
        OpamFilename.in_dir dir (fun () -> git_init remote_address);
        download_dir nv ?dst remote_address
    | Some f ->
        if OpamFilename.Set.empty = f then
          Up_to_date dir
        else
          Result dir

  let update ~address =
    let local_repo = OpamRepository.local_repo () in
    let local_dir = OpamPath.Repository.root local_repo in
    match check_updates local_dir address with
    | Some f -> OpamFile.Filenames.write (updates local_repo) f; f
    | None   ->
        OpamGlobals.error_and_exit
          "The repository %s is not initialized correctly"
          (OpamFilename.Dir.to_string local_dir)

  let upload_dir ~address dirname =
    let files = OpamFilename.list_files dirname in
    try
      OpamSystem.commands [
        [ "git"; "add"; OpamFilename.Dir.to_string dirname; ];
        [ "git"; "commit"; "-a"; "-m"; "upload new files" ];
        [ "git"; "push"; "origin"; "master" ]
      ];
      OpamFilename.Set.of_list files
    with _ ->
      OpamFilename.Set.empty

end

let register () =
  OpamRepository.register_backend "git" (module B: OpamRepository.BACKEND)
