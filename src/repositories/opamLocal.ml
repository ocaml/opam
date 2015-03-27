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
open OpamProcess.Job.Op

let log fmt = OpamGlobals.log "RSYNC" fmt
let slog = OpamGlobals.slog

let rsync_arg = "-rLptgoDrvc"

let call_rsync check args =
  OpamSystem.make_command "rsync" args
  @@> fun r ->
  match r.OpamProcess.r_code with
  | 0 -> Done (Some (OpamMisc.rsync_trim r.OpamProcess.r_stdout))
  | 3 | 5 | 10 | 11 | 12 -> (* protocol or file errors *)
    Done None
  | 20 -> (* signal *)
    raise Sys.Break
  | 23 | 24 ->
    (* partial, mostly mode, link or perm errors. But may also be a
       complete error so we do an additional check *)
    if check () then
      (OpamGlobals.warning "Rsync partially failed:\n%s"
         (OpamMisc.itemize ~bullet:"" (fun x -> x) r.OpamProcess.r_stderr);
       Done (Some (OpamMisc.rsync_trim r.OpamProcess.r_stdout)))
    else Done None
  | 30 | 35 -> (* timeouts *)
    Done None
  | _ -> OpamSystem.process_error r

let rsync ?(args=[]) src dst =
  log "rsync: src=%s dst=%s" src dst;
  let remote = String.contains src ':' in
  let norm d = Filename.concat d "" in
  if not(remote || Sys.file_exists src) then
    Done (Not_available src)
  else if src = dst then
    Done (Up_to_date [])
  else if OpamMisc.starts_with ~prefix:(norm src) (norm dst) ||
          OpamMisc.starts_with ~prefix:(norm dst) (norm src)
  then
    (OpamGlobals.error "Cannot sync %s into %s: they overlap" src dst;
     Done (Not_available src))
  else (
    OpamSystem.mkdir dst;
    call_rsync (fun () -> not (OpamSystem.dir_is_empty dst))
      ( rsync_arg :: args @ [
            "--exclude"; ".git";
            "--exclude"; "_darcs";
            "--exclude"; ".hg";
            "--exclude"; ".#*";
            "--delete";
            src; dst; ])
    @@| function
    | None -> Not_available src
    | Some [] -> Up_to_date []
    | Some lines -> Result lines
  )

let rsync_dirs ?args src dst =
  let src_s =
    (* ensure trailing '/' *)
    Filename.concat (OpamFilename.Dir.to_string src) ""
  in
  let dst_s = OpamFilename.Dir.to_string dst in
  let remote = String.contains src_s ':' in
  if not remote then OpamFilename.mkdir src;
  rsync ?args src_s dst_s @@| function
  | Not_available s -> Not_available s
  | Result _        ->
    if OpamFilename.exists_dir dst then Result dst
    else Not_available dst_s
  | Up_to_date _    -> Up_to_date dst

let rsync_file ?(args=[]) src dst =
  let src_s = OpamFilename.to_string src in
  let dst_s = OpamFilename.to_string dst in
  log "rsync_file src=%s dst=%s" src_s dst_s;
  let remote = String.contains src_s ':' in
  if not (remote || OpamFilename.exists src) then
    Done (Not_available src_s)
  else if src_s = dst_s then
    Done (Up_to_date src)
  else
  call_rsync (fun () -> Sys.file_exists dst_s)
    ( rsync_arg :: args @ [ src_s; dst_s ])
  @@| function
  | None -> Not_available src_s
  | Some [] -> Up_to_date dst
  | Some [_] ->
    if OpamFilename.exists dst then Result dst
    else Not_available src_s
  | Some l ->
    OpamSystem.internal_error
      "unknown rsync output: {%s}"
      (String.concat ", " l)

module B = struct

  let pull_file_quiet local_dirname remote_filename =
    let local_filename =
      OpamFilename.create local_dirname (OpamFilename.basename remote_filename) in
    rsync_file remote_filename local_filename

  let pull_dir_quiet local_dirname remote_dirname =
    rsync_dirs remote_dirname local_dirname

  let pull_repo repo =
    log "pull-repo";
    pull_file_quiet repo.repo_root (OpamPath.Repository.remote_repo repo)
    @@+ fun _ ->
    pull_dir_quiet
      (OpamPath.Repository.packages_dir repo)
      (OpamPath.Repository.remote_packages_dir repo)
    @@+ fun _ ->
    pull_dir_quiet
      (OpamPath.Repository.compilers_dir repo)
      (OpamPath.Repository.remote_compilers_dir repo)
    @@+ fun _ ->
    let archives = OpamFilename.files (OpamPath.Repository.archives_dir repo) in
    log "archives: %a"
      (slog (OpamMisc.string_of_list OpamFilename.to_string)) archives;
    let rec dl_archives = function
      | [] -> Done ()
      | archive::archives ->
        match OpamPackage.of_archive archive with
        | None ->
          OpamGlobals.msg "Removing %s\n." (OpamFilename.to_string archive);
          OpamFilename.remove archive;
          dl_archives archives
        | Some nv ->
          let remote_filename = OpamPath.Repository.remote_archive repo nv in
          rsync_file remote_filename archive @@+ function
          | Not_available _ -> OpamFilename.remove archive; dl_archives archives
          | _ -> dl_archives archives
    in
    dl_archives archives @@| fun () ->
    OpamGlobals.msg "[%s] %s synchronized\n"
      (OpamGlobals.colorise `blue
         (OpamRepositoryName.to_string repo.repo_name))
      (string_of_address repo.repo_address)

  let pull_url package local_dirname checksum remote_url =
    let remote_url = string_of_address remote_url in
    OpamFilename.mkdir local_dirname;
    let dir = OpamFilename.Dir.to_string local_dirname in
    let remote_url =
      if Sys.file_exists remote_url && Sys.is_directory remote_url
      (* ensure that rsync doesn't recreate a subdir: add trailing '/' *)
      then Filename.concat remote_url ""
      else remote_url in
    rsync remote_url dir
    @@| fun r ->
    let r = match r with
      | Not_available d -> Not_available d
      | Result _ | Up_to_date _ ->
        let res x = match r with
          | Result _ -> Result x
          | Up_to_date _ -> Up_to_date x
          | _ -> assert false
        in
        if OpamMisc.ends_with ~suffix:"/" remote_url then
          res (D local_dirname)
        else match Sys.readdir dir with
          | [|f|] when not (Sys.is_directory (Filename.concat dir f)) ->
            let filename = OpamFilename.OP.(local_dirname // f) in
            if OpamRepository.check_digest filename checksum
            then res (F filename)
            else (OpamFilename.remove filename; Not_available remote_url)
          | _ -> res (D local_dirname)
    in
    OpamGlobals.msg "[%s] %s %s\n"
      (OpamGlobals.colorise `green (OpamPackage.to_string package))
      remote_url
      (string_of_download r);
    r

  let pull_archive repo filename =
    let local_dir = OpamPath.Repository.archives_dir repo in
    OpamFilename.mkdir local_dir;
    pull_file_quiet local_dir filename @@| function
    | Not_available _ as r when !OpamGlobals.verbose_level < 2 -> r
    | r ->
      OpamGlobals.msg "[%s] %s %s\n"
        (OpamGlobals.colorise `blue
           (OpamRepositoryName.to_string repo.repo_name))
        (OpamFilename.to_string filename)
        (string_of_download r);
      r

  let revision _ =
    Done None

end

let register () =
  OpamRepository.register_backend `local (module B: OpamRepository.BACKEND)
