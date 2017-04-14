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
  OpamStd.Format.print_table stdout ~sep:" ";

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
  OpamStd.Format.print_table stdout ~sep:" ";

  OpamConsole.header_msg "Package variables ('opam config list PKG' to show)";
  List.map (fun (var, doc) -> [
        ("PKG:"^var) % `bold;
        "";
        "#";doc
      ])
    OpamPackageVar.package_variable_names |>
  OpamStd.Format.align_table |>
  OpamStd.Format.print_table stdout ~sep:" "

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
      let feature_vars =
        List.map (fun (v, desc, filt) ->
            let v = OpamVariable.Full.create name v in
            v, OpamFilter.eval_to_string ~default:"#undefined" env filt, desc
          )
          (OpamFile.OPAM.features opam)
      in
      let conf_vars =
        List.map (fun (v,c) ->
            OpamVariable.Full.create name v,
            OpamVariable.string_of_variable_contents c,
            "")
          (OpamFile.Dot_config.bindings conf)
      in
      pkg_vars @ feature_vars @ conf_vars
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
  OpamStd.Format.print_table stdout ~sep:" "

let print_env env =
  List.iter (fun (k,v,comment) ->
      (* '#' comment are concatenated by shell expansion + eval and cause
         everything to be ignored, don't use them.
         ':' is not supported by fish, just ignore comments there. *)
      if OpamConsole.verbose () then
        OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
      OpamConsole.msg "%s='%s'; export %s;\n"
        k (OpamStd.Env.escape_single_quotes v) k;
  ) env

let print_csh_env env =
  List.iter (fun (k,v,comment) ->
      if OpamConsole.verbose () then
        OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
      OpamConsole.msg "setenv %s '%s';\n"
        k (OpamStd.Env.escape_single_quotes v);
  ) env

let print_sexp_env env =
  OpamConsole.msg "(\n";
  List.iter (fun (k,v,_) ->
    OpamConsole.msg "  (%S %S)\n" k v;
  ) env;
  OpamConsole.msg ")\n"

let print_fish_env env =
  List.iter (fun (k,v,_) ->
      match k with
      | "PATH" | "MANPATH" | "CDPATH" ->
        (* This function assumes that `v` does not include any variable expansions
         * and that the directory names are written in full. See the opamState.ml for details *)
        let v = OpamStd.String.split_delim v ':' in
        OpamConsole.msg "set -gx %s %s;\n" k
          (OpamStd.List.concat_map " "
             (fun v ->
               Printf.sprintf "'%s'"
                 (OpamStd.Env.escape_single_quotes ~using_backslashes:true v))
             v)
      | _ ->
        OpamConsole.msg "set -gx %s '%s';\n"
          k (OpamStd.Env.escape_single_quotes ~using_backslashes:true v)
    ) env

let print_eval_env ~csh ~sexp ~fish env =
  if sexp then
    print_sexp_env env
  else if csh then
    print_csh_env env
  else if fish then
    print_fish_env env
  else
    print_env env

let env st ~csh ~sexp ~fish ~inplace_path =
  log "config-env";
  let env = OpamEnv.get_opam ~force_path:(not inplace_path) st in
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
    OpamConsole.error_and_exit
      "Only global variables may be set using this command";
  let root = OpamStateConfig.(!r.root_dir) in
  let switch = OpamStateConfig.get_switch () in
  OpamFilename.with_flock `Lock_write (OpamPath.Switch.lock root switch)
  @@ fun () ->
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
    OpamConsole.error_and_exit
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
    OpamSwitchState.with_ `Lock_none gt @@ fun st ->
    match OpamPackageVar.resolve st v with
    | Some c ->
      OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
    | None ->
      OpamConsole.error_and_exit "Variable %s not found"
        (OpamVariable.Full.to_string v)

let setup gt ?dot_profile ~completion ~shell
  ~user ~global =
  log "config-setup";
  if user then
    OpamEnv.update_user_setup gt.root ?dot_profile shell;
  if global then (
    OpamEnv.write_static_init_scripts gt.root ~completion;
    match OpamFile.Config.switch gt.config with
    | Some sw ->
      OpamSwitchState.with_ `Lock_none gt ~switch:sw @@ fun st ->
      OpamEnv.write_dynamic_init_scripts st
    | None -> ()
  )

let setup_list shell dot_profile =
  log "config-setup-list";
  OpamEnv.display_setup OpamStateConfig.(!r.root_dir) ~dot_profile shell

let exec gt ~inplace_path command =
  log "config-exec command=%a" (slog (String.concat " ")) command;
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  let cmd, args =
    match
      List.map (OpamFilter.expand_string ~default:(fun _ -> "")
                  (OpamPackageVar.resolve st)) command
    with
    | []        -> OpamSystem.internal_error "Empty command"
    | h::_ as l -> h, Array.of_list l in
  let opamswitch = OpamStateConfig.(!r.switch_from <> `Default) in
  let env =
    OpamTypesBase.env_array
      (OpamEnv.get_full ~opamswitch ~force_path:(not inplace_path) st)
  in
  raise (OpamStd.Sys.Exec (cmd, args, env))
