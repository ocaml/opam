(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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
(* open OpamState.Types *)

let need_globals ns =
  ns = []
  || List.mem OpamPackage.Name.global_config ns

(* Implicit variables *)
let implicits t ns =
  List.fold_left (fun acc name ->
      let vars =
        if name = OpamPackage.Name.global_config
        then OpamState.global_variable_names
        else
          OpamState.package_variable_names @
          try
            let nv =
              try OpamState.find_installed_package_by_name t name with
              | Not_found ->
                OpamPackage.Set.choose (OpamState.find_packages_by_name t name)
            in
            List.map (fun (v,desc,_) -> OpamVariable.to_string v,desc)
              (OpamFile.OPAM.features (OpamState.opam t nv))
          with Not_found -> []
      in
      List.rev_append
        (List.rev_map (fun (variable,desc) ->
             OpamVariable.Full.create
               name
               (OpamVariable.of_string variable),
             desc
           ) vars)
        acc)
    [] ns

let help t =
  OpamConsole.msg "# Global OPAM configuration variables\n\n";
  let global = OpamState.dot_config t OpamPackage.Name.global_config in
  List.iter (fun var ->
      OpamConsole.msg "%-20s %s\n"
        (OpamVariable.to_string var)
        (match OpamFile.Dot_config.variable global var with
         | Some c -> OpamVariable.string_of_variable_contents c
         | None -> "")
    )
    (OpamFile.Dot_config.variables global);
  OpamConsole.msg "\n# Global variables from the environment\n\n";
  List.iter (fun (varname, doc) ->
      let var = OpamVariable.of_string varname in
      OpamConsole.msg "%-20s %-20s # %s\n"
        varname
        (OpamFilter.ident_string (OpamState.filter_env t) ~default:""
           ([],var,None))
        doc)
    OpamState.global_variable_names;
  OpamConsole.msg "\n# Package variables ('opam config list PKG' to show)\n\n";
  List.iter (fun (var, doc) ->
      OpamConsole.msg "PKG:%-37s # %s\n" var doc)
    OpamState.package_variable_names

(* List all the available variables *)
let list ns =
  log "config-list";
  let t = OpamState.load_state "config-list"
      OpamClientConfig.(!r.current_switch) in
  if ns = [] then help t else
  let globals =
    if need_globals ns then
      [OpamPackage.Name.global_config,
       OpamState.dot_config t OpamPackage.Name.global_config]
    else
      [] in
  let configs =
    globals @
    OpamPackage.Set.fold (fun nv l ->
      let name = OpamPackage.name nv in
      let file = OpamState.dot_config t (OpamPackage.name nv) in
      (name, file) :: l
    ) t.OpamState.Types.installed [] in
  let variables =
    implicits t ns @
    List.fold_left (fun accu (name, config) ->
        (* add all the global variables *)
        List.fold_left (fun accu variable ->
          (OpamVariable.Full.create name variable, "") :: accu
        ) accu (OpamFile.Dot_config.variables config)
      ) [] configs in
  let contents =
    List.map
      (fun (v,descr) ->
         v, descr,
         (OpamFilter.ident_string (OpamState.filter_env t) ~default:"#undefined"
            (OpamFilter.ident_of_var v)))
      variables in
  List.iter (fun (variable, descr, value) ->
      OpamConsole.msg "%-20s %-40s %s\n"
        (OpamVariable.Full.to_string variable)
        value
        (if descr <> "" then "# "^descr else "")
    ) contents

let print_env env =
  List.iter (fun (k,v) ->
    OpamConsole.msg "%s=%S; export %s;\n" k v k;
  ) env

let print_csh_env env =
  List.iter (fun (k,v) ->
    OpamConsole.msg "setenv %s %S;\n" k v;
  ) env

let print_sexp_env env =
  OpamConsole.msg "(\n";
  List.iter (fun (k,v) ->
    OpamConsole.msg "  (%S %S)\n" k v;
  ) env;
  OpamConsole.msg ")\n"

let print_fish_env env =
  List.iter (fun (k,v) ->
      match k with
      | "PATH" | "MANPATH" ->
        let v = OpamStd.String.split_delim v ':' in
        OpamConsole.msg "set -gx %s %s;\n" k
          (OpamStd.List.concat_map " " (Printf.sprintf "%S") v)
      | _ ->
        OpamConsole.msg "set -gx %s %S;\n" k v
    ) env

let env ~csh ~sexp ~fish ~inplace_path =
  log "config-env";
  let t = OpamState.load_env_state "config-env"
      OpamClientConfig.(!r.current_switch) in
  let env = OpamState.get_opam_env ~force_path:(not inplace_path) t in
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
  let t = OpamState.load_state "config-substitute"
      OpamClientConfig.(!r.current_switch) in
  List.iter
    (OpamFilter.expand_interpolations_in_file (OpamState.filter_env t))
    fs

let quick_lookup v =
  let name = OpamVariable.Full.package v in
  let var = OpamVariable.Full.variable v in
  if name = OpamPackage.Name.global_config then (
    let root = OpamPath.root () in
    let switch = OpamClientConfig.(!r.current_switch) in
    let config = OpamPath.Switch.global_config root switch in
    let config = OpamFile.Dot_config.read config in
    match OpamState.get_env_var v with
    | Some _ as c -> c
    | None ->
      if OpamVariable.to_string var = "switch" then
        Some (S (OpamSwitch.to_string switch))
      else
        OpamFile.Dot_config.variable config var
  ) else
    None

let variable v =
  log "config-variable";
  let contents =
    match quick_lookup v with
    | Some c -> c
    | None   ->
      let t = OpamState.load_state "config-variable"
          OpamClientConfig.(!r.current_switch) in
      OpamFilter.ident_value (OpamState.filter_env t) ~default:(S "#undefined")
        (OpamFilter.ident_of_var v)
  in
  OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents contents)

let setup user global =
  log "config-setup";
  let t = OpamState.load_state "config-setup"
      OpamClientConfig.(!r.current_switch) in
  OpamState.update_setup t user global

let setup_list shell dot_profile =
  log "config-setup-list";
  let t = OpamState.load_state "config-setup-list"
      OpamClientConfig.(!r.current_switch) in
  OpamState.display_setup t shell dot_profile

let exec ~inplace_path command =
  log "config-exec command=%a" (slog (String.concat " ")) command;
  let t = OpamState.load_state "config-exec"
      OpamClientConfig.(!r.current_switch) in
  let cmd, args =
    match command with
    | []        -> OpamSystem.internal_error "Empty command"
    | h::_ as l -> h, Array.of_list l in
  let env =
    let env = OpamState.get_full_env ~force_path:(not inplace_path) t in
    let env = List.rev_map (fun (k,v) -> k^"="^v) env in
    Array.of_list env in
  raise (OpamStd.Sys.Exec (cmd, args, env))
