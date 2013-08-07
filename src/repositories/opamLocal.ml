(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let log fmt = OpamGlobals.log "RSYNC" fmt

let rsync_arg = "-rLptgoDrvc"

let rsync src dst =
  log "rsync: src=%s dst=%s" src dst;
  if Sys.file_exists src then (
    if src <> dst then (
      OpamSystem.mkdir src;
      OpamSystem.mkdir dst;
      let lines = OpamSystem.read_command_output (
          [ "rsync" ; rsync_arg;
            "--exclude"; ".git";
            "--exclude"; "_darcs";
            "--exclude"; ".hg";
            "--delete";
            src; dst; ]
        ) in
      match OpamMisc.rsync_trim lines with
      | []    -> Up_to_date []
      | lines -> Result lines
    ) else
      Up_to_date []
  ) else
    Not_available src

let rsync_dirs src dst =
  let src_s = Filename.concat (OpamFilename.Dir.to_string src) "" in
  let dst_s = OpamFilename.Dir.to_string dst in
  match rsync src_s dst_s with
  | Not_available s -> Not_available s
  | Result _        -> Result dst
  | Up_to_date _    -> Up_to_date dst

let rsync_file src dst =
  log "rsync_file src=%s dst=%s"
    (OpamFilename.to_string src) (OpamFilename.to_string dst);
  if OpamFilename.exists src then (
    let lines = OpamSystem.read_command_output [
        "rsync"; rsync_arg; OpamFilename.to_string src; OpamFilename.to_string dst;
      ] in
    match OpamMisc.rsync_trim lines with
    | []  -> Up_to_date dst
    | [_] -> Result dst
    | l   ->
      OpamSystem.internal_error
        "unknown rsync output: {%s}"
        (String.concat ", " l)
  ) else
    Not_available (OpamFilename.to_string src)

module B = struct

  let pull_file_quiet local_dirname remote_filename =
    let local_filename =
      OpamFilename.create local_dirname (OpamFilename.basename remote_filename) in
    rsync_file remote_filename local_filename

  let pull_dir_quiet local_dirname remote_dirname =
    rsync_dirs remote_dirname local_dirname

  let pull_repo repo =
    log "pull-repo";
    OpamGlobals.msg "%-10s Synchronizing with %s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (string_of_address repo.repo_address);
    ignore (pull_file_quiet repo.repo_root (OpamPath.Repository.version repo));
    List.iter
      (fun (local, remote) -> ignore (pull_dir_quiet (local repo) (remote repo)))
      [
        (OpamPath.Repository.packages_dir , OpamPath.Repository.remote_packages_dir);
        (OpamPath.Repository.compilers_dir, OpamPath.Repository.remote_compilers_dir);
      ];
    let archives = OpamFilename.files (OpamPath.Repository.archives_dir repo) in
    log "archives: %s" (OpamMisc.string_of_list OpamFilename.to_string archives);
    List.iter (fun archive ->
        match OpamPackage.of_archive archive with
        | None    ->
          OpamGlobals.msg "Removing %s\n." (OpamFilename.to_string archive);
          OpamFilename.remove archive
        | Some nv ->
        let remote_filename = OpamPath.Repository.remote_archive repo nv in
        match rsync_file remote_filename archive with
        | Not_available _ -> OpamFilename.remove archive
        | _               -> ()
      ) archives

  let pull_file package local_dirname remote_filename =
    if OpamFilename.exists remote_filename then
      OpamGlobals.msg "%-10s Synchronizing with %s\n"
        (OpamPackage.to_string package)
        (OpamFilename.to_string remote_filename);
    pull_file_quiet local_dirname remote_filename

  let pull_dir package local_dirname remote_dirname =
    OpamGlobals.msg "%-10s Synchronizing with %s\n"
      (OpamPackage.to_string package)
      (OpamFilename.Dir.to_string remote_dirname);
    pull_dir_quiet local_dirname remote_dirname

  let pull_url package local_dirname remote_url =
    let remote_url = string_of_address remote_url in
    if Sys.file_exists remote_url && Sys.is_directory remote_url then
      download_dir
        (pull_dir package local_dirname (OpamFilename.Dir.of_string remote_url))
    else if Sys.file_exists remote_url then
      download_file
        (pull_file package local_dirname (OpamFilename.of_string remote_url))
    else
      Not_available remote_url

  let pull_archive repo filename =
    if OpamFilename.exists filename then
      OpamGlobals.msg "%-10s Synchronizing with %s\n"
        (OpamRepositoryName.to_string repo.repo_name)
        (OpamFilename.to_string filename);
    pull_file_quiet (OpamPath.Repository.archives_dir repo) filename

  let revision _ =
    None

end

let register () =
  OpamRepository.register_backend `local (module B: OpamRepository.BACKEND)
