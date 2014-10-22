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
open OpamTypesBase

let log fmt = OpamGlobals.log "RSYNC" fmt
let slog = OpamGlobals.slog

let rsync_arg = "-rLptgoDrvc"

let call_rsync args result unavail =
  try
    result (OpamSystem.read_command_output ("rsync"::args))
  with OpamSystem.Process_error r as e ->
    match r.OpamProcess.r_code with
    | 3 | 5 | 10 | 11 | 12 -> (* protocol or file errors *)
      unavail
    | 20 -> (* signal *)
      raise Sys.Break
    | 23 | 24 -> (* partial, mostly mode, link or perm errors *)
      OpamGlobals.warning "Rsync partially failed:\n  %s"
        (String.concat "\n  " r.OpamProcess.r_stderr);
      OpamProcess.cleanup r;
      result r.OpamProcess.r_stdout
    | 30 | 35 -> (* timeouts *)
      unavail
    | _ -> raise e

let rsync src dst =
  log "rsync: src=%s dst=%s" src dst;
  let remote = String.contains src ':' in
  if not(remote || Sys.file_exists src) then
    Not_available src
  else if src = dst then
    Up_to_date []
  else
  let result lines =
    match OpamMisc.rsync_trim lines with
    | []    -> Up_to_date []
    | lines -> Result lines in
  OpamSystem.mkdir dst;
  call_rsync [ rsync_arg;
               "--exclude"; ".git";
               "--exclude"; "_darcs";
               "--exclude"; ".hg";
               "--exclude"; ".#*";
               "--delete";
               src; dst; ]
    result (Not_available src)

let rsync_dirs src dst =
  let src_s =
    (* ensure trailing '/' *)
    Filename.concat (OpamFilename.Dir.to_string src) ""
  in
  let dst_s = OpamFilename.Dir.to_string dst in
  let remote = String.contains src_s ':' in
  if not remote then OpamFilename.mkdir src;
  match rsync src_s dst_s with
  | Not_available s -> Not_available s
  | Result _        -> Result dst
  | Up_to_date _    -> Up_to_date dst

let rsync_file src dst =
  let src_s = OpamFilename.to_string src in
  let dst_s = OpamFilename.to_string dst in
  log "rsync_file src=%s dst=%s" src_s dst_s;
  let remote = String.contains src_s ':' in
  if not (remote || OpamFilename.exists src) then
    Not_available src_s
  else if src_s = dst_s then
    Up_to_date src
  else
  let result lines =
    match OpamMisc.rsync_trim lines with
    | []  -> Up_to_date dst
    | [_] -> Result dst
    | l   ->
      OpamSystem.internal_error
        "unknown rsync output: {%s}"
        (String.concat ", " l)
  in
  call_rsync [ rsync_arg; src_s; dst_s ]
    result (Not_available src_s)

module B = struct

  let pull_file_quiet local_dirname remote_filename =
    let local_filename =
      OpamFilename.create local_dirname (OpamFilename.basename remote_filename) in
    rsync_file remote_filename local_filename

  let pull_dir_quiet local_dirname remote_dirname =
    rsync_dirs remote_dirname local_dirname

  let pull_repo repo =
    log "pull-repo";
    OpamGlobals.msg "[%s] Synchronizing with %s\n"
      (OpamGlobals.colorise `blue
         (OpamRepositoryName.to_string repo.repo_name))
      (string_of_address repo.repo_address);
    ignore (pull_file_quiet repo.repo_root (OpamPath.Repository.remote_repo repo));
    List.iter
      (fun (local, remote) -> ignore (pull_dir_quiet (local repo) (remote repo)))
      [
        (OpamPath.Repository.packages_dir , OpamPath.Repository.remote_packages_dir);
        (OpamPath.Repository.compilers_dir, OpamPath.Repository.remote_compilers_dir);
      ];
    let archives = OpamFilename.files (OpamPath.Repository.archives_dir repo) in
    log "archives: %a"
      (slog (OpamMisc.string_of_list OpamFilename.to_string)) archives;
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

  let pull_url package local_dirname checksum remote_url =
    let remote_url = string_of_address remote_url in
    OpamFilename.mkdir local_dirname;
    OpamGlobals.msg "[%s] Synchronizing with %s\n"
      (OpamGlobals.colorise `green
         (OpamPackage.to_string package))
      remote_url;
    let dir = OpamFilename.Dir.to_string local_dirname in
    let remote_url =
      if Sys.file_exists remote_url && Sys.is_directory remote_url
      (* ensure that rsync doesn't recreate a subdir: add trailing '/' *)
      then Filename.concat remote_url ""
      else remote_url in
    match rsync remote_url dir with
    | Result [f] | Up_to_date [f] as r
      when not (Sys.is_directory (Filename.concat dir f)) ->
      let filename = OpamFilename.OP.(local_dirname // f) in
      OpamRepository.check_digest filename checksum;
      (match r with
       | Result _ -> Result (F filename)
       | Up_to_date _ -> Up_to_date (F filename)
       | _ -> assert false)
    | Result _ -> Result (D local_dirname)
    | Up_to_date _ -> Up_to_date (D local_dirname)
    | Not_available d -> Not_available d

  let pull_archive repo filename =
    if OpamFilename.exists filename then
      OpamGlobals.msg "[%s] Synchronizing with %s\n"
        (OpamGlobals.colorise `blue
           (OpamRepositoryName.to_string repo.repo_name))
        (OpamFilename.to_string filename);
    let local_dir = OpamPath.Repository.archives_dir repo in
    OpamFilename.mkdir local_dir;
    pull_file_quiet local_dir filename

  let revision _ =
    None

end

let register () = assert false
  (* OpamRepository.register_backend `local (module B: OpamRepository.BACKEND) *)
