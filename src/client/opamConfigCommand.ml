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

let log fmt = OpamGlobals.log "CONFIG" fmt
let slog = OpamGlobals.slog

open OpamTypes
open OpamState.Types

let need_globals ns =
  ns = []
  || List.mem OpamPackage.Name.global_config ns

(* Implicit variables *)
let implicits ns =
  List.fold_left (fun acc name ->
      let vars =
        if name = OpamPackage.Name.global_config
        then OpamState.global_variable_names
        else OpamState.package_variable_names
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
  OpamGlobals.msg "# Global OPAM configuration variables\n\n";
  let global = OpamState.dot_config t OpamPackage.Name.global_config in
  List.iter (fun var ->
      OpamGlobals.msg "%-20s %s\n"
        (OpamVariable.to_string var)
        (match OpamFile.Dot_config.variable global var with
         | Some c -> OpamVariable.string_of_variable_contents c
         | None -> "")
    )
    (OpamFile.Dot_config.variables global);
  OpamGlobals.msg "\n# Global variables from the environment\n\n";
  List.iter (fun (varname, doc) ->
      let var = OpamVariable.of_string varname in
      let fullvar =
        OpamVariable.Full.create OpamPackage.Name.global_config var in
      OpamGlobals.msg "%-20s %-20s # %s\n"
        varname
        (match
           OpamState.contents_of_variable t OpamVariable.Map.empty fullvar
         with
         | Some c -> OpamVariable.string_of_variable_contents c
         | None -> "")
        doc)
    OpamState.global_variable_names;
  OpamGlobals.msg "\n# Package variables\n\n";
  List.iter (fun (var, doc) ->
      OpamGlobals.msg "PKG:%-37s # %s\n" var doc)
    OpamState.package_variable_names

(* List all the available variables *)
let list ns =
  log "config-list";
  let t = OpamState.load_state "config-list" in
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
    ) t.installed [] in
  let variables =
    implicits ns @
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
         OpamState.contents_of_variable_exn t OpamVariable.Map.empty v)
      variables in
  List.iter (fun (variable, descr, contents) ->
      OpamGlobals.msg "%-20s %-40s %s\n"
        (OpamVariable.Full.to_string variable)
        (OpamVariable.string_of_variable_contents contents)
        (if descr <> "" then "# "^descr else "")
    ) contents

let print_env env =
  List.iter (fun (k,v) ->
    OpamGlobals.msg "%s=%S; export %s;\n" k v k;
  ) env

let print_csh_env env =
  List.iter (fun (k,v) ->
    OpamGlobals.msg "setenv %s %S;\n" k v;
  ) env

let print_sexp_env env =
  OpamGlobals.msg "(\n";
  List.iter (fun (k,v) ->
    OpamGlobals.msg "  (%S %S)\n" k v;
  ) env;
  OpamGlobals.msg ")\n"

let print_fish_env env =
  List.iter (fun (k,v) ->
    OpamGlobals.msg "set -x %s %s;\n" k (String.concat " " (OpamMisc.split v ':'));
  ) env

let env ~csh ~sexp ~fish=
  log "config-env";
  let t = OpamState.load_env_state "config-env" in
  let env = OpamState.get_opam_env t in
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
  let t = OpamState.load_state "config-substitute" in
  List.iter (OpamState.substitute_file t OpamVariable.Map.empty) fs

let quick_lookup v =
  let name = OpamVariable.Full.package v in
  let var = OpamVariable.Full.variable v in
  if name = OpamPackage.Name.global_config then (
    let root = OpamPath.root () in
    let switch = match !OpamGlobals.switch with
      | `Command_line s
      | `Env s   -> OpamSwitch.of_string s
      | `Not_set ->
	let config = OpamPath.config root in
	OpamFile.Config.switch (OpamFile.Config.read config) in
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
      let t = OpamState.load_state "config-variable" in
      OpamState.contents_of_variable_exn t OpamVariable.Map.empty v in
  OpamGlobals.msg "%s\n" (OpamVariable.string_of_variable_contents contents)

let setup user global =
  log "config-setup";
  let t = OpamState.load_state "config-setup" in
  OpamState.update_setup t user global

let setup_list shell dot_profile =
  log "config-setup-list";
  let t = OpamState.load_state "config-setup-list" in
  OpamState.display_setup t shell dot_profile

let exec command =
  log "config-exex command=%S" (String.concat " " command);
  let t = OpamState.load_state "config-exec" in
  let cmd, args =
    match command with
    | []        -> OpamSystem.internal_error "Empty command"
    | h::_ as l -> h, Array.of_list l in
  let env =
    let env = OpamState.get_full_env t in
    let env = List.rev_map (fun (k,v) -> k^"="^v) env in
    Array.of_list env in
  let lock =
    OpamSystem.flock ~read:true
      (OpamFilename.to_string (OpamPath.lock (OpamPath.root ()))) in
  OpamSystem.funlock lock;
  Unix.execvpe cmd args env
