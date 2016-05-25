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
open OpamStd.Op
open OpamFilename.Op

open OpamStateTypes

let log fmt = OpamConsole.log "GSTATE" fmt
let slog = OpamConsole.slog

let load_config global_lock root =
  let config = match OpamStateConfig.load root with
    | Some c -> c
    | None ->
      if OpamFilename.exists (root // "aliases") then
        OpamFile.Config.(with_opam_version (OpamVersion.of_string "1.1") empty)
      else if OpamFilename.exists_dir root then
        OpamConsole.error_and_exit `Configuration_error
          "%s exists, but does not appear to be a valid opam root. Please \
           remove it and use `opam init', or specify a different `--root' \
           argument"
          (OpamFilename.Dir.to_string root)
      else
        OpamConsole.error_and_exit `Configuration_error
          "Opam has not been initialised, please run `opam init'"
  in
  OpamFormatUpgrade.as_necessary global_lock root config;
  config

let load lock_kind =
  let root = OpamStateConfig.(!r.root_dir) in
  log "LOAD-GLOBAL-STATE @ %a" (slog OpamFilename.Dir.to_string) root;
  (* Always take a global read lock, this is only used to prevent concurrent
     ~/.opam format changes *)
  let global_lock =
    if OpamFilename.exists_dir root then
      OpamFilename.flock `Lock_read (OpamPath.lock root)
    else OpamSystem.lock_none
  in
  (* The global_state lock actually concerns the global config file only (and
     the consistence thereof with the repository and switch sets, and the
     currently installed shell init scripts) *)
  let config_lock = OpamFilename.flock lock_kind (OpamPath.config_lock root) in
  let config = load_config global_lock root in
  let switches =
    List.filter
      (fun sw -> not (OpamSwitch.is_external sw) ||
                 OpamFilename.exists_dir (OpamSwitch.get_root root sw))
      (OpamFile.Config.installed_switches config)
  in
  let config = OpamFile.Config.with_installed_switches switches config in
  let global_variables =
    List.fold_left (fun acc (v,value) ->
        OpamVariable.Map.add v
          (lazy (Some (OpamStd.Option.default (S "unknown") (Lazy.force value))),
           "Inferred from system")
          acc)
      OpamVariable.Map.empty
      (OpamSysPoll.variables)
  in
  let global_variables =
    List.fold_left (fun acc (v,value,doc) ->
        OpamVariable.Map.add v (lazy (Some value), doc) acc)
      global_variables
      (OpamFile.Config.global_variables config)
  in
  let eval_variables = OpamFile.Config.eval_variables config in
  let global_variables =
    let env = lazy (OpamEnv.get_pure () |> OpamTypesBase.env_array) in
    List.fold_left (fun acc (v, cmd, doc) ->
        OpamVariable.Map.update v
          (fun previous_value ->
             (lazy
               (try
                  let ret =
                    OpamSystem.read_command_output
                      ~env:(Lazy.force env)
                      ~allow_stdin:false
                      cmd
                  in
                  Some (S (OpamStd.String.strip (String.concat "\n" ret)))
                with e ->
                  OpamStd.Exn.fatal e;
                  log "Failed to evaluate global variable %a: %a"
                    (slog OpamVariable.to_string) v
                    (slog Printexc.to_string) e;
                  Lazy.force (fst previous_value))),
             doc)
          (lazy None, "")
          acc)
      global_variables eval_variables
  in
  { global_lock = config_lock;
    root;
    config;
    global_variables; }

let switches gt =
  OpamFile.Config.installed_switches gt.config

let fold_switches f gt acc =
  List.fold_left (fun acc switch ->
      f switch
        (OpamFile.SwitchSelections.safe_read
           (OpamPath.Switch.selections gt.root switch))
        acc
    ) acc (OpamFile.Config.installed_switches gt.config)

let all_installed gt =
  fold_switches (fun _ sel acc ->
      OpamPackage.Set.union acc sel.sel_installed)
    gt  OpamPackage.Set.empty

let installed_versions gt name =
  fold_switches (fun switch sel acc ->
      let installed =
        OpamPackage.packages_of_name sel.sel_installed name
      in
      try
        let nv = OpamPackage.Set.choose installed in
        try OpamPackage.Map.add nv (switch::OpamPackage.Map.find nv acc) acc
        with Not_found -> OpamPackage.Map.add nv [switch] acc
      with Not_found -> acc)
    gt OpamPackage.Map.empty

let repos_list gt = OpamFile.Config.repositories gt.config

let unlock gt =
  OpamSystem.funlock gt.global_lock;
  (gt :> unlocked global_state)

let with_write_lock ?dontblock gt f =
  let ret, gt =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock gt.global_lock
    @@ fun _ -> f ({ gt with global_lock = gt.global_lock } : rw global_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible*)
  in
  ret, { gt with global_lock = gt.global_lock }

let with_ lock f =
  let gt = load lock in
  try let r = f gt in ignore (unlock gt); r
  with e -> OpamStd.Exn.finalise e (fun () -> ignore (unlock gt))

let write gt =
  OpamFile.Config.write (OpamPath.config gt.root) gt.config
