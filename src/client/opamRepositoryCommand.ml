(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
open OpamStateTypes
open OpamStd.Op

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog


(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config (gt: [< rw ] global_state) repos =
  log "update-config %a"
    (slog @@ OpamStd.List.concat_map ", " OpamRepositoryName.to_string)
    repos;
  let new_config = OpamFile.Config.with_repositories repos gt.config in
  OpamStateConfig.write gt.root new_config;
  { gt with config = new_config }

let find_repository rt repo_name =
  try OpamRepositoryName.Map.find repo_name rt.repositories
  with Not_found ->
    OpamConsole.error_and_exit
      "%s is not a valid repository name."
      (OpamRepositoryName.to_string repo_name)

let priority gt repo_name ~priority =
  log "repository-priority";

  let rt = OpamRepositoryState.load ~lock:`Lock_write gt in
  let repo = find_repository rt repo_name in
  let config_f = OpamRepositoryPath.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_priority = priority } in
  OpamFile.Repo_config.write config_f config;
  rt

let add gt name url ~priority:prio =
  log "repository-add";
  let rt = OpamRepositoryState.load ~lock:`Lock_write gt in
  if OpamRepositoryName.Map.mem name rt.repositories then
    OpamConsole.error_and_exit "%s is already a remote repository"
      (OpamRepositoryName.to_string name);
  let prio = match prio with
    | Some p -> p
    | None ->
      OpamRepositoryName.Map.fold
        (fun _ { repo_priority; _ } m -> max (repo_priority + 10) m)
        rt.repositories 0
  in
  let repo = {
    repo_name     = name;
    repo_url      = url;
    repo_priority = prio;
    repo_root     = OpamRepositoryPath.create rt.repos_global.root name;
  } in
  if OpamUrl.local_dir url <> None &&
     OpamUrl.local_dir (OpamRepositoryPath.Remote.packages_url repo) = None &&
     not (OpamConsole.confirm
            "%S doesn't contain a \"packages\" nor a \"compilers\" directory.\n\
             Is it really the directory of your repo ?"
            (OpamUrl.to_string url))
    then OpamStd.Sys.exit 1;
  OpamProcess.Job.run (OpamRepository.init repo);
  log "Adding %a" (slog OpamRepositoryBackend.to_string) repo;
  let repositories = OpamRepositoryName.Map.add name repo rt.repositories in
  let gt1 =
    update_config gt (OpamRepositoryName.Map.keys repositories)
  in
  let rt = { rt with repos_global = gt1; repositories } in
  try OpamUpdate.repositories rt [repo] with
  | e ->
    let _gt = update_config gt (OpamFile.Config.repositories gt.config) in
    OpamFilename.rmdir repo.repo_root;
    OpamStd.Exn.fatal e;
    OpamConsole.error_and_exit "Could not fetch repo: %s"
      (Printexc.to_string e)

let remove gt name =
  log "repository-remove";
  OpamFilename.with_flock `Lock_write (OpamPath.repos_lock gt.root) @@ fun () ->
  let repo_root =
    (OpamFile.Repo_config.read (OpamRepositoryPath.raw_config gt.root name))
    .repo_root
  in
  let repos =
    List.filter ((<>) name) (OpamFile.Config.repositories gt.config)
  in
  let gt = update_config gt repos in
  OpamRepositoryState.Cache.remove ();
  OpamFilename.rmdir repo_root;
  gt

let set_url gt name url =
  log "repository-remove";
  let rt = OpamRepositoryState.load ~lock:`Lock_write gt in
  let repo = find_repository rt name in
  let config_f = OpamRepositoryPath.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_url = url }
  in
  OpamFile.Repo_config.write config_f config;
  OpamUpdate.repositories rt [repo]

let list gt ~short =
  log "repository-list";
  let rt = OpamRepositoryState.load ~lock:`Lock_none gt in
  if short then
    List.iter
      (fun r ->
         OpamConsole.msg "%s\n" (OpamRepositoryName.to_string r))
      (OpamRepositoryState.repos_list rt)
  else
    let pretty_print name =
      let r = OpamRepositoryName.Map.find name rt.repositories in
      OpamConsole.msg "%4d %-7s %10s     %s\n"
        r.repo_priority
        (Printf.sprintf "[%s]"
           (OpamUrl.string_of_backend r.repo_url.OpamUrl.backend))
        (OpamRepositoryName.to_string r.repo_name)
        (OpamUrl.to_string r.repo_url) in
    let repos = OpamRepositoryState.repos_list rt in
    List.iter pretty_print repos
