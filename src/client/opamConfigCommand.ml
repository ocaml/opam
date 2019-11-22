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
  let env = OpamPackageVar.resolve t in
  List.map (fun (var, doc) ->
      let content =
        OpamFilter.ident_string env ~default:"" ([],var,None)
      in
      let doc =
        if doc = OpamGlobalState.inferred_from_system then
          match OpamStd.Option.Op.(
              OpamVariable.Map.find_opt var t.switch_global.global_variables
              >>| fst
              >>= Lazy.force) with
          | Some c when (OpamVariable.string_of_variable_contents c) <> content ->
            "Set through local opam config or env"
          | _ -> doc
        else doc
      in
      [
        OpamVariable.to_string var % `bold;
        content % `blue;
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
  let set_arr_cmd ?(modf=fun x -> x) k v =
    let v = modf @@ OpamStd.String.split v ':' in
    OpamConsole.msg "set -gx %s %s;\n" k
      (OpamStd.List.concat_map " "
         (fun v ->
            Printf.sprintf "'%s'"
              (OpamStd.Env.escape_single_quotes ~using_backslashes:true v))
         v)
  in
  (* set manpath if and only if fish version >= 2.7 *)
  let manpath_cmd v =
    OpamConsole.msg "%s" (
      (* test for existence of `argparse` builtin, introduced in fish 2.7 .
       * use `grep' instead of `builtin string match' so that old fish versions do not
       *     produce unwanted error messages on stderr.
       * use `grep' inside a `/bin/sh' fragment so that nothing is written to stdout or
       *     stderr if `grep' does not exist. *)
      "builtin -n | /bin/sh -c 'grep -q \\'^argparse$\\'' 1>/dev/null 2>/dev/null; and "
    ) ;
    let modf = function | x::v' -> (":"^x)::v' | v -> v in
    set_arr_cmd ~modf "MANPATH" v in
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
         manpath_cmd v
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

let ensure_env_aux ?(set_opamroot=false) ?(set_opamswitch=false) ?(force_path=true) gt switch =
  let env_file = OpamPath.Switch.environment gt.root switch in
  if not (OpamFile.exists env_file) then
    Some (OpamSwitchState.with_ `Lock_none gt @@ fun st ->
          let upd =
            OpamEnv.updates ~set_opamroot ~set_opamswitch ~force_path st
          in
          log "Missing environment file, regenerate it";
          if not (OpamCoreConfig.(!r.safe_mode)) then
            (let _, st =
               OpamSwitchState.with_write_lock st @@ fun st ->
               (OpamFile.Environment.write env_file upd), st
             in OpamSwitchState.drop st);
          OpamEnv.add [] upd)
  else
    None

let ensure_env gt switch = ignore (ensure_env_aux gt switch)

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
    match ensure_env_aux ~set_opamroot ~set_opamswitch ~force_path gt switch with
    | Some env -> env
    | None ->
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

let _set_var_switch st var value =
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit `Bad_arguments
      "Only global variables may be set using this command";
  let root = st.switch_global.root in
  let config_f = OpamPath.Switch.switch_config root st.switch in
  let var = OpamVariable.Full.variable var in
  let oldval = OpamFile.Switch_config.variable st.switch_config var in
  let newval = OpamStd.Option.map (fun s -> S s) value in
  if oldval = newval then
    (OpamConsole.note "No change for \"%s\"" (OpamVariable.to_string var);
     st)
  else
  let () = match oldval, newval with
    | Some old, Some _ ->
      OpamConsole.note "Overriding value of \"%s\": was \"%s\""
        (OpamVariable.to_string var)
        (OpamVariable.string_of_variable_contents old)
    | _ -> ()
  in
  let variables = st.switch_config.OpamFile.Switch_config.variables in
  let variables =
    match newval with
    | None -> List.remove_assoc var variables
    | Some v -> OpamStd.List.update_assoc var v variables
  in
  let switch_config = { st.switch_config with OpamFile.Switch_config.variables} in
  OpamFile.Switch_config.write config_f switch_config;
  { st with switch_config }

let _set_var_global gt var value =
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit `Bad_arguments
      "Only global variables may be set using this command";
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
  let gt = { gt with config } in
  OpamGlobalState.write gt;
  gt

let variable gt v =
  let raw_switch_content =
    match OpamStateConfig.get_switch_opt () with
    | Some switch ->
      let switch_config =
        OpamFile.Switch_config.safe_read
          (OpamPath.Switch.switch_config gt.root switch)
      in
      OpamPackageVar.resolve_switch_raw gt switch switch_config v
    | None -> None
  in
  let switch_content =
    match raw_switch_content with
    | None when not (OpamVariable.Full.is_global v) ->
      OpamSwitchState.with_ `Lock_none gt @@ fun st ->
      OpamPackageVar.resolve st v
    | rsc -> rsc
  in
  let content =
    match switch_content with
    | Some _ as some -> some
    | None -> OpamPackageVar.resolve_global gt v
  in
  match content with
  | Some c -> OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
  | None ->
    OpamConsole.error_and_exit `Not_found
      "Variable %s not found"
      (OpamVariable.Full.to_string v)

(* For function that takes two config and update (add or remove) elements in a
   field. Used for appending or deleting element in config file fields *)
type 'config fld_updater =  ('config -> 'config -> 'config)
(* Only some field can be modifiable. [Modifiable] is for user modifiable
   field,  [InModifiable] for fields that can only be modified from inner opam
   code (see [set_var_global]).
   First argument is the addition function, the second the remove one. *)
type 'config fld_policy =
  | Fixed
  | Modifiable of 'config fld_updater * 'config fld_updater
  | InModifiable of 'config fld_updater * 'config fld_updater

(* "Configuration" of the [set_opt] function. As modification can be on global
   or config switch, on normal fields and sections, adding, removing, or
   overwritng values, this record type permits to aggregate all needed inputs.
   See [set_opt_global] and [set_opt_switch]. *)
type 'config confset =
  {
    stg_fields: (string * ('config, value) OpamPp.field_parser) list;
    stg_allwd_fields:
      (string * 'config fld_policy * ('config -> 'config)) list;
    stg_sections:
      (string * ('config, (string option * opamfile_item list) list)
         OpamPp.field_parser) list;
    stg_allwd_sections:
      ((string * 'config fld_policy * ('config -> 'config)) list);
    stg_config: 'config;
    stg_write_config: 'config -> unit;
    stg_file: filename;
  }

let kind_req value =
  OpamStd.Option.Op.(
    (value >>| (fun value ->
         match String.get value 0, String.get value 1 with
         | '+', '=' ->
           `add (OpamStd.String.remove_prefix ~prefix:"+=" value)
         | '_', '=' ->
           `rem (OpamStd.String.remove_prefix ~prefix:"_=" value)
         | '=', _ -> `over (OpamStd.String.remove_prefix ~prefix:"=" value)
         | _,_ -> `over value
         | exception Invalid_argument _ -> `over value))
    +! `revert)

let set_opt ?(inner=false) field value conf =
  let open OpamStd.Op in
  let wrap allowed all parse =
    List.map (fun (field, pp) ->
        match OpamStd.List.find_opt (fun (x,_,_) -> x = field) allowed with
        | None -> field, None
        | Some (_, modd, default) ->
          let parse elem config =
            OpamPp.parse ~pos:OpamTypesBase.pos_null pp
              (config, Some (parse elem))
          in
          field,
          Some (parse, modd, default)
      ) all
  in
  let fields =
    (wrap conf.stg_allwd_fields conf.stg_fields
      (fun str_value ->
         OpamParser.value_from_string str_value "<command-line>"))
    @ (wrap conf.stg_allwd_sections conf.stg_sections
      (fun str_value ->
         [None, (OpamParser.string str_value "<command-line>").file_contents]))
  in
  let requested_value = kind_req value in
  let new_config =
    match OpamStd.List.find_opt ((=) field @* fst) fields, requested_value with
    | None, _ ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s doesn't exist" (OpamConsole.colorise `underline field)
    | Some (_, None), _ ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s is not modifiable" (OpamConsole.colorise `underline field)
    | Some (_, Some (_, Fixed, _)), ((`add _ | `rem _) as ar) ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s can't be %s" (OpamConsole.colorise `underline field)
        (match ar with `add _ -> "appended" | `rem _ -> "substracted")
    | Some (_, Some (_, InModifiable (_,_), _)), ((`add _ | `rem _) as ar)
      when not inner ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s can't be directly %s, use instead `opam config set-var`"
        (OpamConsole.colorise `underline field)
        (match ar with `add _ -> "appended" | `rem _ -> "substracted")
    | Some (_, Some (_, _, set_default)), `revert ->
      if OpamConsole.confirm ~default:false
          "Are you sure you want to revert %s value"
          (OpamConsole.colorise `underline field)
      then
        set_default conf.stg_config
      else conf.stg_config
   | Some (_, Some (parse, fix_app, _)),
      ((`add v | `rem v | `over v) as req_value) ->
      (try
         let updf v = parse v conf.stg_config in
         match req_value, fix_app with
         | `add value, (Modifiable (add, _) | InModifiable (add, _))  ->
           add (updf value) conf.stg_config
         | `rem value, (Modifiable (_, rem) | InModifiable (_, rem)) ->
           rem (updf value) conf.stg_config
         | `over value, _ -> (updf value)
         | _,_ -> assert false
       with
       | (OpamPp.Bad_format (_,_) | Parsing.Parse_error) as e ->
         OpamConsole.error_and_exit `Bad_arguments
           "Parse error on %s value '%s': %s"
           (OpamConsole.colorise `underline field) v
           (OpamPp.string_of_bad_format e))
  in
  conf.stg_write_config new_config;
  OpamConsole.msg "%s field %s in %s.\n"
    (match requested_value with
     | `add value ->  Printf.sprintf "Added %s to" value
     | `rem value ->  Printf.sprintf "Removed %s from" value
     | `over value -> Printf.sprintf "Overwritted %s from" value
     | `revert -> "Reverted")
    (OpamConsole.colorise `underline field)
    (OpamFilename.to_string conf.stg_file);
  new_config

let allwd_wrappers wdef wrappers with_wrappers =
  let open OpamFile in
  List.map (fun (n, set, get) ->
      n,
      Modifiable (
        (fun nc c ->
           let w = wrappers c in
           let nw = wrappers nc in
           with_wrappers (set (get nw @ get w) w) c),
        (fun nc c ->
           let w = wrappers c in
           let nw = wrappers nc in
           let n_cmd =
             List.filter (fun cmd ->
                 None = OpamStd.List.find_opt (fun cmd' -> cmd = cmd') (get nw))
               (get w)
           in
           with_wrappers (set n_cmd w) c)
      ),
      fun c -> with_wrappers (set (get wdef) (wrappers c)) c)
    [
      "pre-build-commands",
      Wrappers.with_pre_build, Wrappers.pre_build;
      "pre-install-commands",
      Wrappers.with_pre_install, Wrappers.pre_install;
      "pre-remove-commands",
      Wrappers.with_pre_remove, Wrappers.pre_remove;
      "pre-session-commands",
      Wrappers.with_pre_session, Wrappers.pre_session;
      "wrap-build-commands",
      Wrappers.with_wrap_build, Wrappers.wrap_build;
      "wrap-install-commands",
      Wrappers.with_wrap_install, Wrappers.wrap_install;
      "wrap-remove-commands",
      Wrappers.with_pre_remove, Wrappers.pre_remove;
      "post-build-commands",
      Wrappers.with_post_build, Wrappers.post_build;
      "post-install-commands",
      Wrappers.with_post_install, Wrappers.post_install;
      "post-remove-commands",
      Wrappers.with_post_remove, Wrappers.post_remove;
      "post-session-commands",
      Wrappers.with_post_session, Wrappers.post_session;
    ]

let set_opt_switch_t ?inner st field value =
  let allowed_fields =
    OpamFile.Switch_config.(
      [
        ("synopsis", Fixed,
         fun t -> { t with synopsis = empty.synopsis });
      ] @ allwd_wrappers empty.wrappers wrappers
        (fun wrappers t -> { t with wrappers }))
  in
  let allowed_sections =
    let rem_elem nelems elems =
      List.filter (fun (n,p) ->
          None = OpamStd.List.find_opt (fun (n',p') -> n = n' && p = p')
            nelems)
        elems
    in
    OpamFile.Switch_config.([
        ("paths", Modifiable (
            (fun nc c -> { c with paths = nc.paths @ c.paths }),
            (fun nc c -> { c with paths = rem_elem nc.paths c.paths })),
         (fun c -> { c with paths = empty.paths }));
        ("variables", Modifiable (
            (fun nc c -> { c with variables = nc.variables @ c.variables }),
            (fun nc c ->
               { c with variables = rem_elem nc.variables c.variables })),
         (fun c -> { c with variables = empty.variables }));
      ])
  in
  let root = st.switch_global.root in
  let config_f = OpamPath.Switch.switch_config root st.switch in
  let write new_config = OpamFile.Switch_config.write config_f new_config in
  let switch_config =
    set_opt ?inner field value
      { stg_fields = OpamFile.Switch_config.fields;
        stg_allwd_fields = allowed_fields;
        stg_sections = OpamFile.Switch_config.sections;
        stg_allwd_sections = allowed_sections;
        stg_config = st.switch_config;
        stg_write_config = write;
        stg_file = OpamFile.filename config_f;
      }
  in
  { st with switch_config }

let set_opt_switch = set_opt_switch_t ~inner:false

let set_opt_global_t ?inner gt field value =
  let allowed_fields =
    let open OpamStd.Option.Op in
    let open OpamFile in
    let in_config = OpamInitDefaults.init_config () in
    let wrapper_init = InitConfig.wrappers in_config in
    let upd_vars get set =
      (fun nc c ->  set (get nc @ get c) c),
      (fun nc c ->
         let gv = get nc in
         set (List.filter (fun (k,v,_) ->
             None = OpamStd.List.find_opt (fun (k',v',_) -> k = k' && v = v') gv)
             (get c)) c)
    in
    [ "download-command", Fixed,
      Config.with_dl_tool_opt
        (InitConfig.dl_tool in_config ++ Config.dl_tool Config.empty);
      "download-jobs", Fixed,
      Config.with_dl_jobs
        (InitConfig.dl_jobs in_config +! Config.dl_jobs Config.empty);
      "jobs", Fixed,
      Config.with_jobs_opt
        (InitConfig.jobs in_config ++ Config.jobs Config.empty);
      "best-effort-prefix-criteria", Fixed,
      Config.with_best_effort_prefix_opt
        (Config.best_effort_prefix Config.empty);
      "solver", Fixed,
      Config.with_solver_opt
        (InitConfig.solver in_config ++ Config.solver Config.empty);
      "global-variables",
      (let add, rem =
         upd_vars Config.global_variables Config.with_global_variables
       in
       InModifiable (add, rem)),
      Config.with_global_variables (InitConfig.global_variables in_config);
      "eval-variables",
      (let add, rem =
         upd_vars Config.eval_variables Config.with_eval_variables
       in
       InModifiable (add, rem)),
      Config.with_eval_variables (InitConfig.eval_variables in_config);
      "repository-validation-command", Fixed,
      Config.with_validation_hook_opt (Config.validation_hook Config.empty);
    ] @ List.map (fun f ->
        f, Fixed, Config.with_criteria
          (Config.criteria Config.empty))
      [ "solver-criteria";
        "solver-upgrade-criteria";
        "solver-fixup-criteria" ]
    @ allwd_wrappers wrapper_init Config.wrappers Config.with_wrappers
  in
  let write new_config = OpamGlobalState.write {gt with config = new_config} in
  let config =
    set_opt ?inner field value
      { stg_fields = OpamFile.Config.fields;
        stg_allwd_fields = allowed_fields;
        stg_sections = [];
        stg_allwd_sections = [];
        stg_config = gt.config;
        stg_write_config = write;
        stg_file = OpamFile.filename (OpamPath.config gt.root);
      }
  in
  { gt with config }

let set_opt_global = set_opt_global_t ~inner:false

type ('var,'t) var_confset =
  {
    stv_vars: 'var list;
    stv_find: 'var -> bool;
    stv_state: 't;
    stv_varstr: string -> string;
    stv_set_opt: string -> 't;
    stv_remove_elem: 'var list -> 't -> 't;
    stv_revert: 't -> 't;
  }

let set_var var value conf =
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit `Bad_arguments
      "Only global variables may be set using this command";
  let global_vars = conf.stv_vars in
  let found_var, rest =
    List.fold_left (fun (var,rest) v ->
        match var with
        | Some _ -> var, v::rest
        | None -> if conf.stv_find v then (Some v), rest else var, v::rest)
      (None, []) global_vars
  in
  let t = conf.stv_state in
  let t = if found_var = None then t else conf.stv_remove_elem rest t in
  match kind_req value with
  | `add _ | `rem _ ->
    OpamConsole.error_and_exit `Bad_arguments "Variables are not appendable"
  | `over value -> conf.stv_set_opt ("+=" ^ conf.stv_varstr value)
  | `revert -> conf.stv_revert t

let set_var_global gt var value =
  set_var var value
    (let var = OpamVariable.Full.variable var in
     let global_vars = OpamFile.Config.global_variables gt.config in
     { stv_vars = global_vars;
       stv_find = (fun (k,_,_) -> k = var);
       stv_state = gt;
       stv_varstr = (fun v ->
           Printf.sprintf
             "[%s \"%s\" \"Set through 'opam config set-var global'\"]"
             (OpamVariable.to_string var) v);
       stv_set_opt = (fun s ->
           set_opt_global_t ~inner:true gt "global-variables" (Some s));
       stv_remove_elem = (fun rest gt ->
           let config =
             OpamFile.Config.with_global_variables rest gt.config
             |> OpamFile.Config.with_eval_variables
               (List.filter (fun (k,_,_) -> k <> var)
                  (OpamFile.Config.eval_variables gt.config))
           in
           { gt with config });
       stv_revert = (fun gt -> OpamGlobalState.write gt; gt);
     })

let set_var_switch st var value =
  set_var var value
    (let var = OpamVariable.Full.variable var in
     let switch_vars = st.switch_config.OpamFile.Switch_config.variables in
     { stv_vars = switch_vars;
       stv_find = (fun (k,_) -> k = var);
       stv_state = st;
       stv_varstr = (fun v ->
           Printf.sprintf "%s: \"%s\"" (OpamVariable.to_string var) v);
       stv_set_opt = (fun s ->
           set_opt_switch_t ~inner:true st "variables" (Some s));
       stv_remove_elem = (fun rest st ->
           { st with switch_config = { st.switch_config with variables = rest }});
       stv_revert = (fun st ->
           OpamFile.Switch_config.write
             (OpamPath.Switch.switch_config st.switch_global.root st.switch)
             st.switch_config;
           st);
     })

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
