(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "RSYNC" fmt
let slog = OpamConsole.slog

let rsync_arg = "-rLptgoDrvc"

(* if rsync -arv return 4 lines, this means that no files have changed *)
let rsync_trim = function
  | [] -> []
  | _ :: t ->
    match List.rev t with
    | _ :: _ :: _ :: l -> List.filter ((<>) "./") l
    | _ -> []

let call_rsync check args =
  OpamSystem.make_command "rsync" args
  @@> fun r ->
  match r.OpamProcess.r_code with
  | 0 -> Done (Some (rsync_trim r.OpamProcess.r_stdout))
  | 3 | 5 | 10 | 11 | 12 -> (* protocol or file errors *)
    Done None
  | 20 -> (* signal *)
    raise Sys.Break
  | 23 | 24 ->
    (* partial, mostly mode, link or perm errors. But may also be a
       complete error so we do an additional check *)
    if check () then
      (OpamConsole.warning "Rsync partially failed:\n%s"
         (OpamStd.Format.itemize ~bullet:"" (fun x -> x) r.OpamProcess.r_stderr);
       Done (Some (rsync_trim r.OpamProcess.r_stdout)))
    else Done None
  | 30 | 35 -> (* timeouts *)
    Done None
  | _ -> OpamSystem.process_error r

let rsync ?(args=[]) ?(exclude_vcdirs=true) src dst =
  log "rsync: src=%s dst=%s" src dst;
  let remote = String.contains src ':' in
  let overlap src dst =
    let norm d = Filename.concat d "" in
    OpamStd.String.starts_with ~prefix:(norm src) (norm dst) &&
    not (OpamStd.String.starts_with
           ~prefix:(norm (Filename.concat src OpamSwitch.external_dirname))
           (norm dst)) ||
    OpamStd.String.starts_with ~prefix:(norm dst) (norm src)
  in
  let exclude_args =
    if exclude_vcdirs then [
      "--exclude"; ".git";
      "--exclude"; "_darcs";
      "--exclude"; ".hg";
      "--exclude"; ".#*";
      "--exclude"; OpamSwitch.external_dirname;
    ]
    else [
      "--exclude"; ".#*";
      "--exclude"; OpamSwitch.external_dirname;
    ]
  in
  if not(remote || Sys.file_exists src) then
    Done (Not_available src)
  else if src = dst then
    Done (Up_to_date [])
  else if overlap src dst then
    (OpamConsole.error "Cannot sync %s into %s: they overlap" src dst;
     Done (Not_available src))
  else (
    OpamSystem.mkdir dst;
    call_rsync (fun () -> not (OpamSystem.dir_is_empty dst))
      ( rsync_arg :: args @ exclude_args @
        [ "--delete"; "--delete-excluded"; src; dst; ])
    @@| function
    | None -> Not_available src
    | Some [] -> Up_to_date []
    | Some lines -> Result lines
  )

let is_remote url = url.OpamUrl.transport <> "file"

let rsync_dirs ?args ?exclude_vcdirs url dst =
  let src_s = OpamUrl.(Op.(url / "").path) in (* Ensure trailing '/' *)
  let dst_s = OpamFilename.Dir.to_string dst in
  if not (is_remote url) &&
     not (OpamFilename.exists_dir (OpamFilename.Dir.of_string src_s))
  then
    Done (Not_available (Printf.sprintf "Directory %s does not exist" src_s))
  else
  rsync ?args ?exclude_vcdirs src_s dst_s @@| function
  | Not_available s -> Not_available s
  | Result _ ->
    if OpamFilename.exists_dir dst then Result dst
    else Not_available dst_s
  | Up_to_date _ -> Up_to_date dst

let rsync_file ?(args=[]) url dst =
  let src_s = url.OpamUrl.path in
  let dst_s = OpamFilename.to_string dst in
  log "rsync_file src=%s dst=%s" src_s dst_s;
  if not (is_remote url || OpamFilename.(exists (of_string src_s))) then
    Done (Not_available src_s)
  else if src_s = dst_s then
    Done (Up_to_date dst)
  else
    (OpamFilename.mkdir (OpamFilename.dirname dst);
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
         (String.concat ", " l))

module B = struct

  let name = `rsync

  let pull_file_quiet local_dirname url =
    let basename = OpamUrl.basename url in
    let local_filename =
      OpamFilename.create local_dirname (OpamFilename.Base.of_string basename)
    in
    rsync_file url local_filename

  let pull_dir_quiet local_dirname url =
    rsync_dirs url local_dirname

  let pull_repo repo_name repo_root repo_url =
    log "pull-repo";
    pull_file_quiet repo_root (OpamRepositoryPath.Remote.repo repo_url)
    @@+ fun res_repo ->
    pull_dir_quiet
      (OpamRepositoryPath.packages_dir repo_root)
      (OpamRepositoryPath.Remote.packages_url repo_url)
    @@+ fun res_pkgs ->
    match res_repo, res_pkgs with
    | Not_available _, Not_available _ ->
      OpamConsole.error "Could not synchronize %s from %S"
        (OpamRepositoryName.to_string repo_name)
        (OpamUrl.to_string repo_url);
      OpamConsole.msg "[%s] %s %s\n"
        (OpamConsole.colorise `blue
           (OpamRepositoryName.to_string repo_name))
        (OpamUrl.to_string repo_url)
        (OpamConsole.colorise `red "unavailable");
      Done ()
    | _ ->
    let archives =
      OpamFilename.files (OpamRepositoryPath.archives_dir repo_root)
    in
    log "archives: %a"
      (slog (OpamStd.List.to_string OpamFilename.to_string)) archives;
    let rec dl_archives = function
      | [] -> Done ()
      | archive::archives ->
        match OpamPackage.of_archive archive with
        | None ->
          OpamConsole.msg "Removing %s\n." (OpamFilename.to_string archive);
          OpamFilename.remove archive;
          dl_archives archives
        | Some nv ->
          let remote_url = OpamRepositoryPath.Remote.archive repo_url nv in
          rsync_file remote_url archive @@+ function
          | Not_available _ -> OpamFilename.remove archive; dl_archives archives
          | _ -> dl_archives archives
    in
    dl_archives archives @@| fun () ->
    OpamConsole.msg "[%s] %s synchronized\n"
      (OpamConsole.colorise `blue
         (OpamRepositoryName.to_string repo_name))
      (OpamUrl.to_string repo_url)

  let pull_url local_dirname checksum remote_url =
    OpamFilename.mkdir local_dirname;
    let dir = OpamFilename.Dir.to_string local_dirname in
    let remote_url =
      match OpamUrl.local_dir remote_url with
      | Some _ ->
        (* ensure that rsync doesn't recreate a subdir: add trailing '/' *)
        OpamUrl.Op.(remote_url / "")
      | None -> remote_url
    in
    rsync remote_url.OpamUrl.path dir
    @@| function
    | Not_available d -> Not_available d
    | (Result _ | Up_to_date _) as r ->
      let res x = match r with
        | Result _ -> Result x
        | Up_to_date _ -> Up_to_date x
        | _ -> assert false
      in
      if OpamUrl.has_trailing_slash remote_url then res (D local_dirname)
      else match Sys.readdir dir with
        | [|f|] when not (Sys.is_directory (Filename.concat dir f)) ->
          let filename = OpamFilename.Op.(local_dirname // f) in
          if OpamRepositoryBackend.check_digest filename checksum
          then
            res (F filename)
          else
            (OpamFilename.remove filename;
             Not_available (OpamUrl.to_string remote_url))
        | _ -> res (D local_dirname)

  let revision _ =
    Done None

end
