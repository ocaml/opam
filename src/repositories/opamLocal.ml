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

let rsync src dst =
  log "rsync: src=%s dst=%s" src dst;
  if Sys.file_exists src then (
    if src <> dst then (
      OpamSystem.mkdir src;
      OpamSystem.mkdir dst;
      let lines = OpamSystem.read_command_output (
          [ "rsync" ; "-arv";
            "--exclude"; ".git/*";
            "--exclude"; "_darcs/*";
            "--delete";
            src; dst; ]
        ) in
      match OpamMisc.rsync_trim lines with
      | []    -> Up_to_date []
      | lines -> Result lines
    ) else
      Up_to_date []
  ) else
    Not_available

let rsync_dirs src dst =
  let src_s = Filename.concat (OpamFilename.Dir.to_string src) "" in
  let dst_s = OpamFilename.Dir.to_string dst in
  match rsync src_s dst_s with
  | Not_available -> Not_available
  | Result _      -> Result dst
  | Up_to_date _  -> Up_to_date dst

let rsync_file src dst =
  log "rsync_file src=%s dst=%s"
    (OpamFilename.to_string src) (OpamFilename.to_string dst);
  if OpamFilename.exists src then (
    let lines = OpamSystem.read_command_output [
        "rsync"; "-av"; OpamFilename.to_string src; OpamFilename.to_string dst;
      ] in
    match OpamMisc.rsync_trim lines with
    | []  -> Up_to_date dst
    | [_] -> Result dst
    | l   ->
      OpamSystem.internal_error
        "unknown rsync output: {%s}"
        (String.concat ", " l)
  ) else
    Not_available

module B = struct

  let pull_file local_dirname remote_filename =
    log "pull-file";
    let local_filename =
      OpamFilename.create local_dirname (OpamFilename.basename remote_filename) in
    rsync_file remote_filename local_filename

  let pull_dir local_dirname remote_dirname =
    log "pull-dir";
    rsync_dirs remote_dirname local_dirname

  let pull_repo repo =
    log "pull-repo";
    OpamGlobals.msg "%-10s Synchronizing with %s\n"
      (OpamRepositoryName.to_string repo.repo_name)
      (OpamFilename.prettify_dir repo.repo_address);
    ignore (pull_file repo.repo_root (OpamPath.Repository.version repo));
    List.iter
      (fun (local, remote) -> ignore (pull_dir (local repo) (remote repo)))
      [
        (OpamPath.Repository.packages_dir , OpamPath.Repository.remote_packages_dir);
        (OpamPath.Repository.compilers_dir, OpamPath.Repository.remote_compilers_dir);
      ]

  let pull_archive repo filename =
    log "pull-archive";
    pull_file (OpamPath.Repository.archives_dir repo) filename

end

let register () =
  OpamRepository.register_backend `local (module B: OpamRepository.BACKEND)
