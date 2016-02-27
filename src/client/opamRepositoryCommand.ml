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
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog

let eval_redirect gt repo =
  if repo.repo_url.OpamUrl.backend <> `http then None else
  let redirect =
    repo
    |> OpamRepositoryPath.repo
    |> OpamFile.Repo.safe_read
    |> OpamFile.Repo.redirect
  in
  let redirect = List.fold_left (fun acc (redirect, filter) ->
      if OpamFilter.opt_eval_to_bool (OpamPackageVar.resolve_global gt) filter
      then (redirect, filter) :: acc
      else acc
    ) [] redirect in
  match redirect with
  | []         -> None
  | (r,f) :: _ ->
    let config_f = OpamRepositoryPath.config repo in
    let config = OpamFile.Repo_config.read config_f in
    let repo_url = OpamUrl.of_string r in
    if repo_url <> config.repo_url then (
      let config = { config with repo_url } in
      OpamFile.Repo_config.write config_f config;
      Some (config, f)
    ) else
      None

let update t repo =
  let max_loop = 10 in
  (* Recursively traverse redirection links, but stop after 10 steps or if
     we start to cycle. *)
  let rec job r n =
    if n = 0 then
      (OpamConsole.warning "%s: Too many redirections, stopping."
         (OpamRepositoryName.to_string repo.repo_name);
       Done ())
    else
      let text =
        OpamProcess.make_command_text ~color:`blue
          (OpamRepositoryName.to_string repo.repo_name)
          OpamUrl.(string_of_backend repo.repo_url.backend)
      in
      OpamProcess.Job.with_text text @@
      OpamRepository.update r @@+ fun () ->
      if n <> max_loop && r = repo then
        (OpamConsole.warning "%s: Cyclic redirections, stopping."
           (OpamRepositoryName.to_string repo.repo_name);
         Done ())
      else match eval_redirect t r with
        | None -> Done ()
        | Some (new_repo, f) ->
          OpamFilename.rmdir repo.repo_root;
          OpamFile.Repo_config.write (OpamRepositoryPath.config repo) new_repo;
          let reason = match f with
            | None   -> ""
            | Some f -> Printf.sprintf " (%s)" (OpamFilter.to_string f) in
          OpamConsole.note
            "The repository '%s' will be *%s* redirected to %s%s"
            (OpamRepositoryName.to_string repo.repo_name)
            (OpamConsole.colorise `bold "permanently")
            (OpamUrl.to_string new_repo.repo_url)
            reason;
          job new_repo (n-1)
  in
  job repo max_loop @@+ fun () ->
  let repo =
    repo
    |> OpamRepositoryPath.config
    |> OpamFile.Repo_config.safe_read
  in
  OpamRepository.check_version repo @@+ fun () ->
  Done (
    fun t ->
      { t with
        repositories =
          OpamRepositoryName.Map.add repo.repo_name repo t.repositories }
  )

let update_package_index rt =
  let file = OpamPath.package_index rt.repos_global.root in
  log "Updating %a ...\n" (slog OpamFile.to_string) file;
  let package_index = OpamRepositoryState.package_index rt in
  OpamFile.Package_index.write file package_index;
  { rt with package_index }

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_config t repos =
  log "update-config %a"
    (slog @@ OpamStd.List.concat_map ", " OpamRepositoryName.to_string)
    repos;
  let new_config = OpamFile.Config.with_repositories t.config repos in
  OpamStateConfig.write t.root new_config

(* Remove any remaining of [repo] from OPAM state *)
let cleanup t repo =
   log "cleanup %a" (slog OpamRepositoryName.to_string) repo.repo_name;
  let repos = OpamRepositoryName.Map.keys t.repositories in
  update_config t.repos_global (List.filter ((<>) repo.repo_name) repos);
  OpamFilename.rmdir repo.repo_root;
  OpamRepositoryState.Cache.remove ()

let find_repository rt repo_name =
  try OpamRepositoryName.Map.find repo_name rt.repositories
  with Not_found ->
    OpamConsole.error_and_exit
      "%s is not a valid repository name."
      (OpamRepositoryName.to_string repo_name)

let priority repo_name ~priority =
  log "repository-priority";

  (* 1/ update the config file *)
  let rt =
    OpamRepositoryState.load ~save_cache:false (OpamGlobalState.load ())
  in
  let repo = find_repository rt repo_name in
  let config_f = OpamRepositoryPath.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_priority = priority } in
  OpamFile.Repo_config.write config_f config

let add name url ~priority:prio =
  log "repository-add";
  let rt = OpamRepositoryState.load ~save_cache:false (OpamGlobalState.load ()) in
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
  update_config rt.repos_global (OpamRepositoryName.Map.keys repositories);
  let rt = { rt with repositories } in
  OpamRepositoryState.Cache.remove ();
  try
    ignore (OpamProcess.Job.run (update rt.repos_global repo) rt)
  with
  | e ->
    cleanup rt repo;
    OpamStd.Exn.fatal e;
    OpamConsole.error_and_exit "Could not fetch repo: %s"
      (Printexc.to_string e)

let remove name =
  log "repository-remove";
  let rt = OpamRepositoryState.load (OpamGlobalState.load ()) in
  let repo = find_repository rt name in
  cleanup rt repo

let set_url name url =
  log "repository-remove";
  let rt = OpamRepositoryState.load (OpamGlobalState.load ()) in
  let repo = find_repository rt name in
  if repo.repo_url.OpamUrl.backend <> url.OpamUrl.backend then
    (remove name;
     add name url ~priority:(Some repo.repo_priority))
  else
  let config_f = OpamRepositoryPath.config repo in
  let config =
    let config = OpamFile.Repo_config.read config_f in
    { config with repo_url = url } in
  OpamFile.Repo_config.write config_f config;
  OpamRepositoryState.Cache.remove ()

let list ~short =
  log "repository-list";
  let rt = OpamRepositoryState.load (OpamGlobalState.load ()) in
  if short then
    List.iter
      (fun r ->
         OpamConsole.msg "%s\n" (OpamRepositoryName.to_string r.repo_name))
      (OpamRepository.sort rt.repositories)
  else
    let pretty_print r =
      OpamConsole.msg "%4d %-7s %10s     %s\n"
        r.repo_priority
        (Printf.sprintf "[%s]"
           (OpamUrl.string_of_backend r.repo_url.OpamUrl.backend))
        (OpamRepositoryName.to_string r.repo_name)
        (OpamUrl.to_string r.repo_url) in
    let repos = OpamRepository.sort rt.repositories in
    List.iter pretty_print repos
