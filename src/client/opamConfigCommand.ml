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

let log fmt = OpamConsole.log "CONFIG" fmt
let slog = OpamConsole.slog

open OpamTypes
open OpamStateTypes

let help t =
  let (%) s col = OpamConsole.colorise col s in
  OpamConsole.header_msg "Global opam variables";
  let all_global_vars =
    List.fold_left (fun acc (v,doc) ->
        OpamVariable.Map.add (OpamVariable.of_string v) doc acc)
      OpamVariable.Map.empty
      OpamPackageVar.global_variable_names
  in
  let all_global_vars =
    OpamVariable.Map.union (fun _ x -> x)
      all_global_vars
      (OpamVariable.Map.map snd t.switch_global.global_variables)
  in
  List.map (fun (var, doc) -> [
        OpamVariable.to_string var % `bold;
        OpamFilter.ident_string (OpamPackageVar.resolve t) ~default:""
          ([],var,None) % `blue;
        "#"; doc
      ])
    (OpamVariable.Map.bindings all_global_vars) |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" ";

  OpamConsole.header_msg "Configuration variables from the current switch";
  let global = t.switch_config in
  List.map (fun stdpath -> [
        OpamTypesBase.string_of_std_path stdpath % `bold;
        OpamPath.Switch.get_stdpath
          t.switch_global.root t.switch global stdpath |>
        OpamFilename.Dir.to_string |>
        OpamConsole.colorise `blue
      ])
    OpamTypesBase.all_std_paths @
  List.map (fun (var,value) -> [
        OpamVariable.to_string var % `bold;
        OpamVariable.string_of_variable_contents value % `blue;
      ])
    (global.OpamFile.Switch_config.variables) |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" ";

  OpamConsole.header_msg "Package variables ('opam config list PKG' to show)";
  List.map (fun (var, doc) -> [
        ("PKG:"^var) % `bold;
        "";
        "#";doc
      ])
    OpamPackageVar.package_variable_names |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" "

(* List all the available variables *)
let list gt ns =
  log "config-list";
  OpamSwitchState.with_ `Lock_none gt @@ fun t ->
  if ns = [] then help t else
  let list_vars name =
    if OpamPackage.Name.to_string name = "-" then
      let conf = t.switch_config in
      List.map (fun (v,c) ->
          OpamVariable.Full.global v,
          OpamVariable.string_of_variable_contents c,
          "")
        (conf.OpamFile.Switch_config.variables)
    else
    try
      let nv = OpamSwitchState.get_package t name in
      let opam = OpamSwitchState.opam t nv in
      let env = OpamPackageVar.resolve ~opam t in
      let conf = OpamSwitchState.package_config t name in
      let pkg_vars =
        OpamStd.List.filter_map (fun (vname, desc) ->
            let v = OpamVariable.(Full.create name (of_string vname)) in
            try
              let c = OpamFilter.ident_string env (OpamFilter.ident_of_var v) in
              Some (v, c, desc)
            with Failure _ -> None)
          OpamPackageVar.package_variable_names
      in
      let conf_vars =
        List.map (fun (v,c) ->
            OpamVariable.Full.create name v,
            OpamVariable.string_of_variable_contents c,
            "")
          (OpamFile.Dot_config.bindings conf)
      in
      pkg_vars @ conf_vars
    with Not_found -> []
  in
  let vars = List.flatten (List.map list_vars ns) in
  let (%) s col = OpamConsole.colorise col s in
  List.map (fun (variable, value, descr) -> [
        OpamVariable.Full.to_string variable % `bold;
        value % `blue;
        if descr = "" then "" else "# "^descr;
      ]) vars |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" "

let rec print_env = function
  | [] -> ()
  | (k, v, comment) :: r ->
    if OpamConsole.verbose () then
      OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
    if not (List.exists (fun (k1, _, _) -> k = k1) r) || OpamConsole.verbose ()
    then
      OpamConsole.msg "%s='%s'; export %s;\n"
        k (OpamStd.Env.escape_single_quotes v) k;
    print_env r

let rec print_csh_env = function
  | [] -> ()
  | (k, v, comment) :: r ->
    if OpamConsole.verbose () then
      OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
    if not (List.exists (fun (k1, _, _) -> k = k1) r) || OpamConsole.verbose ()
    then
      OpamConsole.msg "setenv %s '%s';\n"
        k (OpamStd.Env.escape_single_quotes v);
    print_csh_env r

let print_sexp_env env =
  let rec aux = function
    | [] -> ()
    | (k, v, _) :: r ->
      if not (List.exists (fun (k1, _, _) -> k = k1) r) then
        OpamConsole.msg "  (%S %S)\n" k v;
      aux r
  in
  OpamConsole.msg "(\n";
  aux env;
  OpamConsole.msg ")\n"

let rec print_fish_env env =
  let set_arr_cmd k v =
    let v = OpamStd.String.split v ':' in
    OpamConsole.msg "set -gx %s %s;\n" k
      (OpamStd.List.concat_map " "
         (fun v ->
            Printf.sprintf "'%s'"
              (OpamStd.Env.escape_single_quotes ~using_backslashes:true v))
         v)
  in
  match env with
  | [] -> ()
  | (k, v, _) :: r ->
    if not (List.exists (fun (k1, _, _) -> k = k1) r) then
      (match k with
       | "PATH" | "CDPATH" ->
         (* This function assumes that `v` does not include any variable
          * expansions and that the directory names are written in full. See the
          * opamState.ml for details *)
         set_arr_cmd k v
       | "MANPATH" ->
         if OpamStd.Env.getopt k <> None then
           set_arr_cmd k v
       | _ ->
         OpamConsole.msg "set -gx %s '%s';\n"
           k (OpamStd.Env.escape_single_quotes ~using_backslashes:true v));
    print_fish_env r

let print_eval_env ~csh ~sexp ~fish env =
  if sexp then
    print_sexp_env env
  else if csh then
    print_csh_env env
  else if fish then
    print_fish_env env
  else
    print_env env

let env gt switch ?(set_opamroot=false) ?(set_opamswitch=false)
    ~csh ~sexp ~fish ~inplace_path =
  log "config-env";
  let opamroot_not_current =
    let current = gt.root in
    let default = OpamStateConfig.(default.root_dir) in
    match OpamStd.Config.env_string "ROOT" with
    | None -> current <> default
    | Some r -> OpamFilename.Dir.of_string r <> current
  in
  let opamswitch_not_current =
    let default =
      OpamStd.Option.Op.(++)
        (OpamStateConfig.get_current_switch_from_cwd gt.root)
        (OpamFile.Config.switch gt.config)
    in
    match OpamStd.Config.env_string "SWITCH" with
    | None ->
      Some (OpamStateConfig.resolve_local_switch gt.root switch) <> default
    | Some s ->
      OpamStateConfig.resolve_local_switch gt.root (OpamSwitch.of_string s) <>
      OpamStateConfig.resolve_local_switch gt.root switch
  in
  if opamroot_not_current && not set_opamroot then
    OpamConsole.note
      "To make opam select %s as its root in the current shell, add %s or set \
       %s"
      (OpamFilename.Dir.to_string gt.root)
      (OpamConsole.colorise `bold "--set-root")
      (OpamConsole.colorise `bold "OPAMROOT");
  if opamswitch_not_current && not set_opamswitch then
    OpamConsole.note
      "To make opam select the switch %s in the current shell, add %s or set \
       %s"
      (OpamSwitch.to_string switch)
      (OpamConsole.colorise `bold "--set-switch")
      (OpamConsole.colorise `bold "OPAMSWITCH");
  let force_path = not inplace_path in
  let env =
    let env_file = OpamPath.Switch.environment gt.root switch in
    if not (OpamFile.exists env_file) then
      (OpamSwitchState.with_ `Lock_none gt @@ fun st ->
       let upd =
         OpamEnv.updates ~set_opamroot ~set_opamswitch ~force_path st
       in
       log "Missing environment file, regenerates it";
       if not (OpamCoreConfig.(!r.safe_mode)) then
         (let _st =
            OpamSwitchState.with_write_lock st @@ fun _st ->
            (OpamFile.Environment.write env_file upd), _st
          in ());
       OpamEnv.add [] upd)
    else
      OpamEnv.get_opam_raw
        ~set_opamroot ~set_opamswitch ~force_path
        gt.root switch
  in
  print_eval_env ~csh ~sexp ~fish env

let subst gt fs =
  log "config-substitute";
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  List.iter
    (OpamFilter.expand_interpolations_in_file (OpamPackageVar.resolve st))
    fs

let expand gt str =
  log "config-expand";
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  OpamConsole.msg "%s\n"
    (OpamFilter.expand_string ~default:(fun _ -> "")
       (OpamPackageVar.resolve st) str)

let set var value =
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit `Bad_arguments
      "Only global variables may be set using this command";
  let root = OpamStateConfig.(!r.root_dir) in
  let switch = OpamStateConfig.get_switch () in
  OpamFilename.with_flock `Lock_write (OpamPath.Switch.lock root switch)
  @@ fun _ ->
  let var = OpamVariable.Full.variable var in
  let config_f = OpamPath.Switch.switch_config root switch in
  let config = OpamFile.Switch_config.read config_f in
  let oldval = OpamFile.Switch_config.variable config var in
  let newval = OpamStd.Option.map (fun s -> S s) value in
  if oldval = newval then
    OpamConsole.note "No change for \"%s\"" (OpamVariable.to_string var)
  else
  let () = match oldval, newval with
    | Some old, Some _ ->
      OpamConsole.note "Overriding value of \"%s\": was \"%s\""
        (OpamVariable.to_string var)
        (OpamVariable.string_of_variable_contents old)
    | _ -> ()
  in
  let variables = config.OpamFile.Switch_config.variables in
  let variables =
    match newval with
    | None -> List.remove_assoc var variables
    | Some v -> OpamStd.List.update_assoc var v variables
  in
  OpamFile.Switch_config.write config_f
    {config with OpamFile.Switch_config.variables}

let set_global var value =
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit `Bad_arguments
      "Only global variables may be set using this command";
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  let var = OpamVariable.Full.variable var in
  let config =
    gt.config |>
    OpamFile.Config.with_global_variables
      (let vars =
         List.filter (fun (k,_,_) -> k <> var)
           (OpamFile.Config.global_variables gt.config)
       in
       match value with
       | Some v -> (var, S v, "Set through 'opam config set-global'") :: vars
       | None -> vars) |>
    OpamFile.Config.with_eval_variables
      (List.filter (fun (k,_,_) -> k <> var)
         (OpamFile.Config.eval_variables gt.config))
  in
  OpamGlobalState.write { gt with config }

let variable gt v =
  match OpamPackageVar.resolve_global gt v with
  | Some c ->
    OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
  | None ->
    match OpamStateConfig.get_switch_opt () with
    | None ->
      OpamConsole.error_and_exit `Not_found
        "Variable %s not found"
        (OpamVariable.Full.to_string v)
    | Some switch ->
      let switch_config =
        OpamFile.Switch_config.safe_read
          (OpamPath.Switch.switch_config gt.root switch)
      in
      match OpamPackageVar.resolve_switch_raw gt switch switch_config v with
      | Some c ->
        OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
      | None ->
        OpamSwitchState.with_ `Lock_none gt @@ fun st ->
        match OpamPackageVar.resolve st v with
        | Some c ->
          OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
        | None ->
          OpamConsole.error_and_exit `Not_found
            "Variable %s not found"
            (OpamVariable.Full.to_string v)

let exec gt ?set_opamroot ?set_opamswitch ~inplace_path command =
  log "config-exec command=%a" (slog (String.concat " ")) command;
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  let cmd, args =
    match
      List.map (OpamFilter.expand_string ~default:(fun _ -> "")
                  (OpamPackageVar.resolve st)) command
    with
    | []        -> OpamSystem.internal_error "Empty command"
    | h::_ as l -> h, Array.of_list l in
  let env =
    OpamTypesBase.env_array
      (OpamEnv.get_full
         ?set_opamroot ?set_opamswitch ~force_path:(not inplace_path) st)
  in
  match OpamSystem.resolve_command ~env cmd with
  | Some cmd -> raise (OpamStd.Sys.Exec (cmd, args, env))
  | None -> raise (OpamStd.Sys.Exit 127)
