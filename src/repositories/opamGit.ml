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

let git_fetch local_path remote_address commit =
  OpamGlobals.msg "Synchronizing %s with %s%s.\n"
    (OpamFilename.Dir.to_string local_path)
    (OpamFilename.Dir.to_string remote_address)
    (match commit with
    | None   -> ""
    | Some c -> Printf.sprintf " [%s]" c);
  OpamFilename.in_dir local_path (fun () ->
    OpamSystem.command [ "git" ; "fetch" ; "origin" ]
  )

let git_merge local_path commit =
  let commit = match commit with
    | None   -> "origin/master"
    | Some c -> c in
  OpamFilename.in_dir local_path (fun () ->
    OpamSystem.command [ "git" ; "merge" ; commit ]
  )

(* Return the list of modified files of the git repository located
   at [dirname] *)
let git_diff local_path commit =
  let commit = match commit with
    | None   -> "remotes/origin/master"
    | Some c -> c in
  OpamFilename.in_dir local_path (fun () ->
    let lines = OpamSystem.read_command_output
      [ "git" ; "diff" ; commit ; "--name-only" ] in
    OpamFilename.Set.of_list (List.map OpamFilename.of_string lines)
  )

let git_init address =
  let repo = OpamFilename.Dir.to_string address in
  OpamSystem.commands [
    [ "git" ; "init" ] ;
    [ "git" ; "remote" ; "add" ; "origin" ; repo ] ;
  ]

let check_updates local_path remote_address commit =
  if OpamFilename.exists_dir (local_path / ".git") then begin
    git_fetch local_path remote_address commit;
    let files = git_diff local_path commit in
    git_merge local_path commit;
    Some files
  end else
    None

(* A git address could be of the form: git://path/to/the/repository/#SHA *)
let git_of_string address =
  let address = OpamFilename.Dir.to_string address in
  let address, commit = OpamMisc.git_of_string address in
  OpamFilename.raw_dir address, commit

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
    log "init %s" (OpamFilename.Dir.to_string address);
    let local_repo = OpamRepository.local_repo () in
    git_init address;
    OpamFile.Filenames.write (updates local_repo) (OpamFilename.Set.empty)

  let download_archive ~address:_ _ =
    log "downnload_archive";
    Not_available

  let download_file ?checksum:_ nv filename =
    log "download_file";
    let local_repo = OpamRepository.local_repo () in
    let basename = OpamFilename.basename filename in
    let file = OpamPath.Repository.tmp_dir local_repo nv // OpamFilename.Base.to_string basename in
    check_file file

  let rec download_dir nv ?dst remote_address =
    log "download_dir";
    let local_repo = OpamRepository.local_repo () in
    let address, commit = git_of_string remote_address in
    let dir = match dst with
      | None   ->
        let basename = OpamFilename.Base.to_string (OpamFilename.basename_dir address) in
        OpamPath.Repository.tmp_dir local_repo nv / basename
      | Some d -> d in
    match check_updates dir address commit with
    | None ->
        OpamFilename.mkdir dir;
        OpamFilename.in_dir dir (fun () -> git_init address);
        download_dir nv ?dst remote_address
    | Some f ->
        if OpamFilename.Set.empty = f then
          Up_to_date dir
        else
          Result dir

  let update ~address =
    log "update";
    let local_repo = OpamRepository.local_repo () in
    let address, commit = git_of_string address in
    let local_dir = OpamPath.Repository.root local_repo in
    match check_updates local_dir address commit with
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
        [ "git"; "add"; OpamFilename.Dir.to_string dirname; ];
        [ "git"; "commit"; "-a"; "-m"; "upload new files" ];
        [ "git"; "push"; "origin"; "master" ]
      ];
      OpamFilename.Set.of_list files
    with _ ->
      OpamFilename.Set.empty

end

let register () =
  OpamRepository.register_backend `git (module B: OpamRepository.BACKEND)
