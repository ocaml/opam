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
open OpamStateTypes
open OpamStd.Op

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_global_config gt repos =
  log "update-config %a"
    (slog @@ OpamStd.List.concat_map ", " OpamRepositoryName.to_string) repos;
  let config = OpamFile.Config.with_repositories repos gt.config in
  let gt = { gt with config } in
  OpamGlobalState.write gt;
  gt

let update_repos_config rt ?(gt=rt.repos_global) repositories =
  let repos_global = (gt :> unlocked global_state) in
  (* Remove cached opam files for changed or removed repos *)
  let repo_opams =
    OpamRepositoryName.Map.filter (fun name _ ->
        OpamRepositoryName.Map.find_opt name rt.repositories =
        OpamRepositoryName.Map.find_opt name repositories)
      rt.repo_opams
  in
  let rt = { rt with repos_global; repositories; repo_opams } in
  OpamRepositoryState.Cache.remove ();
  OpamRepositoryState.write_config rt;
  rt

(** If specified, also checks that [name] is a configured repo *)
let get_repos_list ?name gt =
  let repos = OpamGlobalState.repos_list gt in
  match name with
  | None -> repos
  | Some n ->
    if not (List.mem n repos) then
      OpamConsole.error_and_exit "No repository %s found"
        (OpamRepositoryName.to_string n);
    repos

let priority gt repo_name ~priority =
  log "repository-priority";
  let repos = get_repos_list ~name:repo_name gt in
  let repos = List.filter ((<>) repo_name) repos in
  let rank =
    if priority < 0 then List.length repos + priority + 1 else priority - 1
  in
  let repos = OpamStd.List.insert_at rank repo_name repos in
  update_global_config gt repos

let add gt rt name url ~priority:prio =
  log "repository-add";
  if OpamRepositoryName.Map.mem name rt.repositories then
    OpamConsole.error_and_exit "Repository %s is already set up to %s"
      (OpamRepositoryName.to_string name)
      (OpamUrl.to_string
         (OpamRepositoryName.Map.find name rt.repositories).repo_url);
  let repo = { repo_name = name; repo_url = url;
               repo_root = OpamRepositoryPath.create gt.root name;
               repo_priority = 0; }
  in
  if OpamUrl.local_dir url <> None &&
     OpamUrl.local_dir (OpamRepositoryPath.Remote.packages_url repo) = None &&
     not (OpamConsole.confirm
            "%S doesn't contain a \"packages\" directory.\n\
             Is it really the directory of your repo ?"
            (OpamUrl.to_string url))
  then OpamStd.Sys.exit 1;
  OpamProcess.Job.run (OpamRepository.init gt.root name);
  let repos_list = get_repos_list gt in
  let prio = OpamStd.Option.default 1 prio in
  let rank = if prio < 0 then List.length repos_list + prio + 1 else prio - 1 in
  let repos_list = OpamStd.List.insert_at rank name repos_list in
  log "Adding %a" (slog OpamRepositoryBackend.to_string) repo;
  let gt = update_global_config gt repos_list in
  let rt = { rt with repos_global = (gt :> unlocked global_state) } in
  let rt =
    update_repos_config rt
      (OpamRepositoryName.Map.add name repo rt.repositories)
  in
  gt, rt

let remove gt rt name =
  log "repository-remove";
  let repos = get_repos_list ~name gt in
  let repos = List.filter ((<>) name) repos in
  let gt = update_global_config gt repos in
  let rt =
    update_repos_config ~gt rt
      (OpamRepositoryName.Map.remove name rt.repositories)
  in
  OpamRepositoryState.Cache.save rt;
  OpamFilename.rmdir (OpamRepositoryPath.create gt.root name);
  gt, rt

let set_url rt name url =
  log "repository-set-url";
  if not (OpamRepositoryName.Map.mem name rt.repositories) then
    OpamConsole.error_and_exit "No repository %s found"
      (OpamRepositoryName.to_string name);
  OpamFilename.cleandir (OpamRepositoryPath.create rt.repos_global.root name);
  let repo = OpamRepositoryName.Map.find name rt.repositories in
  let repo = { repo with repo_url = url } in
  update_repos_config rt (OpamRepositoryName.Map.add name repo rt.repositories)

let list rt ~short =
  log "repository-list";
  if short then
    List.iter
      (fun r -> OpamConsole.msg "%s\n" (OpamRepositoryName.to_string r))
      (get_repos_list rt.repos_global)
  else
    get_repos_list rt.repos_global |>
    List.mapi (fun i name -> [
          Printf.sprintf "%2d" (i+1);
          OpamRepositoryName.to_string name |> OpamConsole.colorise `bold;
          try
            let r = OpamRepositoryName.Map.find name rt.repositories in
            if r.repo_url = OpamUrl.empty then "-" else
              OpamUrl.to_string r.repo_url |> OpamConsole.colorise `underline
          with Not_found -> "NOT FOUND" |> OpamConsole.colorise `red
        ]) |>
    OpamStd.Format.align_table |>
    OpamStd.Format.print_table stdout ~sep:" "
