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


let update_global_selection gt update_fun =
  let repos = OpamFile.Config.repositories gt.config in
  let config = OpamFile.Config.with_repositories (update_fun repos) gt.config in
  let gt = { gt with config } in
  OpamGlobalState.write gt;
  gt

let update_selection gt ~global ~switches update_fun =
  List.iter (OpamSwitchState.update_repositories gt update_fun) switches;
  if global then
    (* ensure all unselected switches aren't modified by changing the default *)
    (List.iter (fun sw ->
         if not (List.mem sw switches) then
           OpamSwitchState.update_repositories gt (fun r -> r) sw)
        (OpamFile.Config.installed_switches gt.config);
     OpamGlobalState.with_write_lock gt @@ fun gt ->
     update_global_selection gt update_fun)
  else gt

(* update the repository config file:
   ~/.opam/repo/<repo>/config *)
let update_repos_config rt repositories =
  (* Remove cached opam files for changed or removed repos *)
  let repo_opams =
    OpamRepositoryName.Map.filter (fun name _ ->
        OpamRepositoryName.Map.find_opt name rt.repositories =
        OpamRepositoryName.Map.find_opt name repositories)
      rt.repo_opams
  in
  let rt = { rt with repositories; repo_opams } in
  OpamRepositoryState.Cache.remove ();
  OpamRepositoryState.write_config rt;
  rt

let add rt name url =
  log "repository-add";
  let root = rt.repos_global.root in
  let repo_exists =
    OpamStd.Option.of_Not_found
      (OpamRepositoryName.Map.find name) rt.repositories
  in
  match repo_exists with
  | Some r when r.repo_url = url -> rt
  | Some r ->
    OpamConsole.error_and_exit
      "Repository %s is already set up and points to %s. Maybe you meant \
       'opam repository set-url' ?"
      (OpamRepositoryName.to_string name)
      (OpamUrl.to_string r.repo_url)
  | None ->
  let repo = { repo_name = name; repo_url = url;
               repo_root = OpamRepositoryPath.create root name;
               repo_priority = 0; }
  in
  if OpamFilename.exists OpamFilename.(of_string (Dir.to_string repo.repo_root))
  then
    OpamConsole.error_and_exit
      "Invalid repository name, %s exists"
      (OpamFilename.Dir.to_string repo.repo_root);
  if OpamUrl.local_dir url <> None &&
     OpamUrl.local_dir (OpamRepositoryPath.Remote.packages_url repo) = None &&
     not (OpamConsole.confirm
            "%S doesn't contain a \"packages\" directory.\n\
             Is it really the directory of your repo ?"
            (OpamUrl.to_string url))
  then OpamStd.Sys.exit 1;
  OpamProcess.Job.run (OpamRepository.init root name);
  update_repos_config rt
    (OpamRepositoryName.Map.add name repo rt.repositories)

let remove rt name =
  log "repository-remove";
  let rt =
    update_repos_config rt (OpamRepositoryName.Map.remove name rt.repositories)
  in
  OpamRepositoryState.Cache.save rt;
  OpamFilename.rmdir (OpamRepositoryPath.create rt.repos_global.root name);
  rt

let set_url rt name url =
  log "repository-set-url";
  if not (OpamRepositoryName.Map.mem name rt.repositories) then
    OpamConsole.error_and_exit "No repository %s found"
      (OpamRepositoryName.to_string name);
  OpamFilename.cleandir (OpamRepositoryPath.create rt.repos_global.root name);
  let repo = OpamRepositoryName.Map.find name rt.repositories in
  let repo = { repo with repo_url = url } in
  update_repos_config rt (OpamRepositoryName.Map.add name repo rt.repositories)

let print_selection rt ~short repos_list =
  if short then
    List.iter
      (fun r -> OpamConsole.msg "%s\n" (OpamRepositoryName.to_string r))
      repos_list
  else
    List.mapi (fun i name -> [
          Printf.sprintf "%2d" (i+1);
          OpamRepositoryName.to_string name |> OpamConsole.colorise `bold;
          try
            let r = OpamRepositoryName.Map.find name rt.repositories in
            if r.repo_url = OpamUrl.empty then "-" else
              OpamUrl.to_string r.repo_url |> OpamConsole.colorise `underline
          with Not_found -> "not found" |> OpamConsole.colorise `red
        ])
      repos_list |>
    OpamStd.Format.align_table |>
    OpamStd.Format.print_table stdout ~sep:" "

let switch_repos rt sw =
  let switch_config =
    OpamFile.Switch_config.safe_read
      (OpamPath.Switch.switch_config rt.repos_global.root sw)
  in
  match switch_config.OpamFile.Switch_config.repos with
  | None -> OpamGlobalState.repos_list rt.repos_global
  | Some rl -> rl

let list rt ~global ~switches ~short =
  if global then
    (let repos = OpamGlobalState.repos_list rt.repos_global in
     if not short then
       OpamConsole.header_msg
         "Default repository configuration (for newly created switches)";
     print_selection rt ~short repos);
  List.iter (fun sw ->
      if not short then
        OpamConsole.header_msg
          "Repository configuration for switch %s" (OpamSwitch.to_string sw);
      print_selection rt ~short (switch_repos rt sw))
    switches

let list_all rt ~short =
  log "repository-list";
  if short then
    OpamRepositoryName.Map.iter
      (fun r _ ->
         OpamConsole.msg "%s\n" (OpamRepositoryName.to_string r))
      rt.repositories
  else
  let default_repos = OpamGlobalState.repos_list rt.repos_global in
  let repos_switches =
    List.fold_left (fun acc sw ->
        let acc,_ =
          List.fold_left (fun (acc,i) repo ->
              OpamRepositoryName.Map.update repo (fun s -> (sw,i)::s) [] acc,
              i + 1)
            (acc,1) (switch_repos rt sw)
        in acc)
      OpamRepositoryName.Map.empty
      (OpamFile.Config.installed_switches rt.repos_global.config)
  in
  let cols =
    List.map (OpamConsole.colorise `blue)
      ["# Repository"; "# Default"; "# Switches"]
  in
  let lines =
    OpamRepositoryName.Map.mapi (fun name _ -> [
          OpamRepositoryName.to_string name |> OpamConsole.colorise `bold;
          if List.mem name default_repos then "yes" else "no";
          OpamStd.List.concat_map " "
            (fun (sw,i) ->
               OpamSwitch.to_string sw ^
               (Printf.sprintf "(%d)" i |> OpamConsole.colorise `yellow))
            (List.rev (try OpamRepositoryName.Map.find name repos_switches
                       with Not_found -> []));
        ])
      rt.repositories
  in
  cols :: OpamRepositoryName.Map.values lines |>
  OpamStd.Format.align_table |>
  OpamStd.Format.print_table stdout ~sep:" "
