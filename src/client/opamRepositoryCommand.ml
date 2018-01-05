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
     let (), gt =
       OpamGlobalState.with_write_lock gt @@ fun gt ->
       (), update_global_selection gt update_fun
     in
     gt)
  else gt

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

let add rt name url trust_anchors =
  log "repository-add";
  let root = rt.repos_global.root in
  let repo_exists =
    OpamStd.Option.of_Not_found
      (OpamRepositoryName.Map.find name) rt.repositories
  in
  match repo_exists with
  | Some r when r.repo_url = url &&
                (trust_anchors = r.repo_trust || trust_anchors = None)
    -> rt
  | Some r ->
    OpamConsole.error_and_exit `Bad_arguments
      "Repository %s is already set up %s. To change that, use 'opam \
       repository set-url'."
      (OpamRepositoryName.to_string name)
      (if r.repo_url <> url then
         "and points to "^OpamUrl.to_string r.repo_url
       else match r.repo_trust with
         | None -> "without trust anchors"
         | Some ta ->
           Printf.sprintf "with trust anchors %s and quorum %d"
             (OpamStd.List.concat_map ~nil:"()" "," String.escaped
                ta.fingerprints)
             ta.quorum)
  | None ->
    let repo = { repo_name = name; repo_url = url;
                 repo_root = OpamRepositoryPath.create root name;
                 repo_trust = trust_anchors; }
    in
    if OpamFilename.exists OpamFilename.(of_string (Dir.to_string repo.repo_root))
    then
      OpamConsole.error_and_exit `Bad_arguments
        "Invalid repository name, %s exists"
        (OpamFilename.Dir.to_string repo.repo_root);
    if url.OpamUrl.backend = `rsync &&
       OpamUrl.local_dir url <> None &&
       OpamUrl.local_dir (OpamRepositoryPath.Remote.packages_url repo.repo_url)
       = None &&
       not (OpamConsole.confirm
              "%S doesn't contain a \"packages\" directory.\n\
               Is it really the directory of your repo ?"
              (OpamUrl.to_string url))
    then OpamStd.Sys.exit_because `Aborted;
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

let set_url rt name url trust_anchors =
  log "repository-set-url";
  let repo =
    try OpamRepositoryName.Map.find name rt.repositories
    with Not_found ->
      OpamConsole.error_and_exit `Not_found "No repository %s found"
        (OpamRepositoryName.to_string name);
  in
  OpamFilename.cleandir (OpamRepositoryPath.create rt.repos_global.root name);
  let repo = { repo with repo_url = url; repo_trust = trust_anchors; } in
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
    OpamConsole.print_table stdout ~sep:" "

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
  let repos_switches, _ =
    List.fold_left (fun (acc,i) repo ->
        OpamRepositoryName.Map.add repo [None, i] acc,
        i + 1)
      (OpamRepositoryName.Map.empty, 1)
      (OpamGlobalState.repos_list rt.repos_global)
  in
  let repos_switches =
    List.fold_left (fun acc sw ->
        let acc,_ =
          List.fold_left (fun (acc,i) repo ->
              OpamRepositoryName.Map.update repo
                (fun s -> (Some sw, i)::s) [] acc,
              i + 1)
            (acc,1) (switch_repos rt sw)
        in acc)
      repos_switches
      (OpamFile.Config.installed_switches rt.repos_global.config)
  in
  let cols =
    List.map (OpamConsole.colorise `blue)
      ["# Repository"; "# Url"; "# Switches(rank)"]
  in
  let lines =
    OpamRepositoryName.Map.mapi (fun name repo -> [
          OpamRepositoryName.to_string name |> OpamConsole.colorise `bold;
          OpamUrl.to_string repo.repo_url;
          OpamStd.List.concat_map " "
            (fun (sw,i) ->
               OpamStd.Option.to_string ~none:"<default>"
                 OpamSwitch.to_string sw ^
               (Printf.sprintf "(%d)" i |> OpamConsole.colorise `yellow))
            (List.rev (try OpamRepositoryName.Map.find name repos_switches
                       with Not_found -> []));
        ])
      rt.repositories
  in
  cols :: OpamRepositoryName.Map.values lines |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" "

let update_with_auto_upgrade rt repo_names =
  let repos = List.map (OpamRepositoryState.get_repo rt) repo_names in
  let failed, rt = OpamUpdate.repositories rt repos in
  let failed = List.map (fun r -> r.repo_name) failed in
  if OpamFormatConfig.(!r.skip_version_checks) ||
     OpamClientConfig.(!r.no_auto_upgrade)
  then
    failed, rt
  else
  let rt, done_upgrade =
    List.fold_left (fun (rt, done_upgrade) r ->
        if List.mem r.repo_name failed then rt, done_upgrade else
        let def =
          OpamRepositoryName.Map.find r.repo_name rt.repos_definitions
        in
        let need_upgrade = match OpamFile.Repo.opam_version def with
          | None ->
            OpamConsole.note
              "Repository at %s doesn't define its version, assuming it's 1.2."
              (OpamUrl.to_string r.repo_url);
            true
          | Some v when
              OpamVersion.compare v OpamAdminRepoUpgrade.upgradeto_version < 0
            -> true
          | _ -> false
        in
        if need_upgrade then
          (if not done_upgrade then
             (OpamConsole.header_msg
                "Upgrading repositories from older opam format";
              OpamRepositoryState.Cache.remove ());
           OpamConsole.msg "Upgrading repository \"%s\"...\n"
             (OpamRepositoryName.to_string r.repo_name);
           OpamAdminRepoUpgrade.do_upgrade r.repo_root;
           let def =
             OpamFile.Repo.safe_read (OpamRepositoryPath.repo r.repo_root) |>
             OpamFile.Repo.with_root_url r.repo_url
           in
           let opams = OpamRepositoryState.load_repo_opams r in
           let rt = {
             rt with
             repos_definitions =
               OpamRepositoryName.Map.add r.repo_name def rt.repos_definitions;
             repo_opams =
               OpamRepositoryName.Map.add r.repo_name opams rt.repo_opams;
           } in
           rt, true)
        else rt, done_upgrade)
      (rt, false) repos
  in
  if done_upgrade then OpamRepositoryState.Cache.save rt;
  failed, rt
