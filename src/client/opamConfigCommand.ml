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

let log fmt = OpamConsole.log "CONFIG" fmt
let slog = OpamConsole.slog

open OpamTypes
open OpamStateTypes

let help t =
  OpamConsole.msg "# Global OPAM configuration variables\n\n";
  let global = t.switch_config in
  let global_vars = OpamFile.Dot_config.variables global in
  List.iter (fun var ->
      OpamConsole.msg "%-20s %s\n"
        (OpamVariable.to_string var)
        (match OpamFile.Dot_config.variable global var with
         | Some c -> OpamVariable.string_of_variable_contents c
         | None -> "")
    )
    global_vars;
  OpamConsole.msg "\n# Global variables from the environment\n\n";
  List.iter (fun (varname, doc) ->
      let var = OpamVariable.of_string varname in
      if not (List.mem var global_vars) then
        OpamConsole.msg "%-20s %-20s # %s\n"
          varname
          (OpamFilter.ident_string (OpamPackageVar.resolve t) ~default:""
             ([],var,None))
          doc)
    OpamPackageVar.global_variable_names;
  OpamConsole.msg "\n# Package variables ('opam config list PKG' to show)\n\n";
  List.iter (fun (var, doc) ->
      OpamConsole.msg "PKG:%-37s # %s\n" var doc)
    OpamPackageVar.package_variable_names

(* List all the available variables *)
let list ns =
  log "config-list";
  let t = OpamSwitchState.load_full_compat "config-list"
      OpamStateConfig.(!r.current_switch) in
  if ns = [] then help t else
  let list_vars name =
    if OpamPackage.Name.to_string name = "-" then
      let conf = t.switch_config in
      List.map (fun (v,c) ->
          OpamVariable.Full.global v,
          OpamVariable.string_of_variable_contents c,
          "")
        (OpamFile.Dot_config.bindings conf)
    else
    try
      let nv = OpamSwitchState.get_package t name in
      let opam = OpamSwitchState.opam t nv in
      let env = OpamPackageVar.resolve ~opam t in
      let conf =
        OpamFile.Dot_config.safe_read
          (OpamPath.Switch.config t.switch_global.root t.switch name)
      in
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
  List.iter (fun (variable, value, descr) ->
      OpamConsole.msg "%-20s %-40s %s\n"
        (OpamVariable.Full.to_string variable)
        value
        (if descr <> "" then "# "^descr else "")
    ) vars

let print_env env =
  List.iter (fun (k,v,comment) ->
      (* '#' comment are concatenated by shell expansion + eval and cause
         everything to be ignored, don't use them.
         ':' is not supported by fish, just ignore comments there. *)
      if OpamConsole.verbose () then
        OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
      OpamConsole.msg "%s=%S; export %s;\n" k v k;
  ) env

let print_csh_env env =
  List.iter (fun (k,v,comment) ->
      if OpamConsole.verbose () then
        OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
      OpamConsole.msg "setenv %s %S;\n" k v;
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
          (OpamStd.List.concat_map " " (Printf.sprintf "%S") v)
      | _ ->
        OpamConsole.msg "set -gx %s %S;\n" k v
    ) env

let env ~csh ~sexp ~fish ~inplace_path =
  log "config-env";
  let t = OpamSwitchState.load_full_compat "config-env"
      OpamStateConfig.(!r.current_switch) in
  let env = OpamEnv.get_opam ~force_path:(not inplace_path) t in
  if sexp then
    print_sexp_env env
  else if csh then
    print_csh_env env
  else if fish then
    print_fish_env env
  else
    print_env env

let subst fs =
  log "config-substitute";
  let t = OpamSwitchState.load_full_compat "config-substitute"
      OpamStateConfig.(!r.current_switch) in
  List.iter
    (OpamFilter.expand_interpolations_in_file (OpamPackageVar.resolve t))
    fs

let expand str =
  log "config-expand";
  let t = OpamSwitchState.load_full_compat "config-expand"
      OpamStateConfig.(!r.current_switch) in
  OpamConsole.msg "%s\n"
    (OpamFilter.expand_string (OpamPackageVar.resolve t) str)

let set var value =
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit
      "Only global variables may be set using this command";
  let var = OpamVariable.Full.variable var in
  let config_f =
    OpamPath.Switch.global_config
      OpamStateConfig.(!r.root_dir) OpamStateConfig.(!r.current_switch)
  in
  let config = OpamFile.Dot_config.read config_f in
  let oldval = OpamFile.Dot_config.variable config var in
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
    OpamFile.Dot_config.write config_f
      (OpamFile.Dot_config.set config var newval)

let variable v =
  let gt = OpamGlobalState.load () in
  match OpamPackageVar.resolve_global gt v with
  | Some c ->
    OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
  | None ->
    let t =
      OpamSwitchState.load_full_compat "config-variable"
        OpamStateConfig.(!r.current_switch)
    in
    match OpamPackageVar.resolve t v with
    | Some c ->
      OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
    | None ->
      OpamConsole.error_and_exit "Variable not found"

let setup user global =
  log "config-setup";
  let t = OpamSwitchState.load_full_compat "config-setup"
      OpamStateConfig.(!r.current_switch) in
  OpamEnv.update_setup t user global

let setup_list shell dot_profile =
  log "config-setup-list";
  let t = OpamGlobalState.load () in
  OpamEnv.display_setup t shell dot_profile

let exec ~inplace_path command =
  log "config-exec command=%a" (slog (String.concat " ")) command;
  let t = OpamSwitchState.load_full_compat "config-exec"
      OpamStateConfig.(!r.current_switch) in
  let cmd, args =
    match List.map (OpamFilter.expand_string (OpamPackageVar.resolve t)) command
    with
    | []        -> OpamSystem.internal_error "Empty command"
    | h::_ as l -> h, Array.of_list l in
  let env =
    OpamTypesBase.env_array
      (OpamEnv.get_full ~force_path:(not inplace_path) t)
  in
  raise (OpamStd.Sys.Exec (cmd, args, env))
