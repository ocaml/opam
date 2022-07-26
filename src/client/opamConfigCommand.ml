(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let log fmt = OpamConsole.log "CONFIG" fmt
let slog = OpamConsole.slog

open OpamParserTypes.FullPos
open OpamTypesBase
open OpamStateTypes

(* List all the available variables *)
let list t ns =
  log "config-list";
  if ns = [] then () else
  let list_vars name =
    if OpamPackage.Name.to_string name = "-" then
      let conf = t.switch_config in
      List.map (fun (v,c) ->
          OpamVariable.Full.global v,
          OpamVariable.string_of_variable_contents c,
          "")
        (conf.OpamFile.Switch_config.variables)
    else
    let nv = OpamSwitchState.get_package t name in
    let pkg_vars =
      try
        let opam = OpamSwitchState.opam t nv in
        let env = OpamPackageVar.resolve ~opam t in
        OpamStd.List.filter_map (fun (vname, desc) ->
            let v = OpamVariable.(Full.create name (of_string vname)) in
            try
              let c = OpamFilter.ident_string env (OpamFilter.ident_of_var v) in
              Some (v, c, desc)
            with Failure _ -> None)
          OpamPackageVar.package_variable_names
      with Not_found -> []
    in
    let conf_vars =
      try
        let conf = OpamSwitchState.package_config t name in
        List.map (fun (v,c) ->
            OpamVariable.Full.create name v,
            OpamVariable.string_of_variable_contents c,
            "")
          (OpamFile.Dot_config.bindings conf)
      with Not_found -> []
    in
    pkg_vars @ conf_vars
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

let possibly_unix_path_env_value k v =
  if k = "PATH" then (Lazy.force OpamSystem.get_cygpath_path_transform) v
  else v

let rec print_env = function
  | [] -> ()
  | (k, v, comment) :: r ->
    if OpamConsole.verbose () then
      OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
    if not (List.exists (fun (k1, _, _) -> k = k1) r) || OpamConsole.verbose ()
    then (
      let v' = possibly_unix_path_env_value k v in
      OpamConsole.msg "%s='%s'; export %s;\n"
        k (OpamStd.Env.escape_single_quotes v') k);
    print_env r

let rec print_csh_env = function
  | [] -> ()
  | (k, v, comment) :: r ->
    if OpamConsole.verbose () then
      OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
    if not (List.exists (fun (k1, _, _) -> k = k1) r) || OpamConsole.verbose ()
    then (
      let v' = possibly_unix_path_env_value k v in
      OpamConsole.msg "setenv %s '%s';\n"
        k (OpamStd.Env.escape_single_quotes v'));
    print_csh_env r

let rec print_pwsh_env = function
  | [] -> ()
  | (k, v, comment) :: r ->
    if OpamConsole.verbose () then
      OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
    if not (List.exists (fun (k1, _, _) -> k = k1) r) || OpamConsole.verbose ()
    then
      OpamConsole.msg "$env:%s = '%s'\n"
        k (OpamStd.Env.escape_powershell v);
    print_pwsh_env r

let print_cmd_env env =
  let rec aux = function
    | [] -> ()
    | (k, v, comment) :: r ->
      if OpamConsole.verbose () then
        OpamStd.Option.iter (OpamConsole.msg ": %s;\n") comment;
      if not (List.exists (fun (k1, _, _) -> k = k1) r) || OpamConsole.verbose ()
      then begin
        let is_special = function
        | '(' | ')' | '!' | '^' | '%' | '"' | '<' | '>' | '|' -> true
        | _ -> false
        in
        if OpamCompat.String.(exists is_special v || exists is_special k) then
          OpamConsole.msg "SET \"%s=%s\"\n" k v
        else
          OpamConsole.msg "SET %s=%s\n" k v
      end;
      aux r
  in
  aux env

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
         let v' = possibly_unix_path_env_value k v in
         set_arr_cmd k v'
       | "MANPATH" ->
         manpath_cmd v
       | _ ->
         OpamConsole.msg "set -gx %s '%s';\n"
           k (OpamStd.Env.escape_single_quotes ~using_backslashes:true v));
    print_fish_env r

let print_eval_env ~csh ~sexp ~fish ~pwsh ~cmd env =
  if sexp then
    print_sexp_env env
  else if csh then
    print_csh_env env
  else if fish then
    print_fish_env env
  else if pwsh then
    print_pwsh_env env
  else if cmd then
    print_cmd_env env
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
    ~csh ~sexp ~fish ~pwsh ~cmd ~inplace_path =
  log "config-env";
  let opamroot_not_current =
    let current = gt.root in
    let default = OpamStateConfig.(default.root_dir) in
    match OpamStateConfig.E.root () with
    | None -> current <> default
    | Some r -> OpamFilename.Dir.of_string r <> current
  in
  let opamswitch_not_current =
    let default =
      OpamStd.Option.Op.(++)
        (OpamStateConfig.get_current_switch_from_cwd gt.root)
        (OpamFile.Config.switch gt.config)
    in
    match OpamStateConfig.E.switch () with
    | None | Some "" ->
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
  print_eval_env ~csh ~sexp ~fish ~pwsh ~cmd env
[@@ocaml.warning "-16"]

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

let exec gt ~set_opamroot ~set_opamswitch ~inplace_path ~no_switch command =
  log "config-exec command=%a" (slog (String.concat " ")) command;
  let switch = OpamStateConfig.get_switch () in
  let st_lazy = lazy (
    let rt = OpamRepositoryState.load `Lock_none gt in
    OpamSwitchState.load `Lock_none gt rt switch
  ) in
  let env_file = OpamPath.Switch.environment gt.root switch in
  let env =
    let base = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
    if no_switch then
      let revert = OpamEnv.add [] [] in
      List.map (fun ((var, _, _) as base) ->
          match List.find_opt (fun (v,_,_) -> v = var) revert with
          | Some reverted -> reverted
          | None -> base) base
    else if OpamFile.exists env_file then
      OpamEnv.get_opam_raw ~base
        ~set_opamroot ~set_opamswitch ~force_path:(not inplace_path)
        gt.root switch
    else
      OpamEnv.get_full ~set_opamroot ~set_opamswitch
        ~force_path:(not inplace_path)
        (Lazy.force st_lazy)
  in
  let env = OpamTypesBase.env_array env in
  let resolve var =
    OpamPackageVar.resolve (Lazy.force st_lazy) var
  in
  let cmd, args =
    match
      List.map (OpamFilter.expand_string ~default:(fun _ -> "") resolve) command
    with
    | []        -> OpamSystem.internal_error "Empty command"
    | h::_ as l -> h, Array.of_list l
  in
  (* it's OK not to release [st_lazy] since we are certain everything will be
     cleaned up anyway *)
  match OpamSystem.resolve_command ~env cmd with
  | Some cmd -> raise (OpamStd.Sys.Exec (cmd, args, env))
  | None ->
    OpamConsole.error "Command not found '%s'" cmd;
    raise (OpamStd.Sys.Exit 127)


(** Options and Variables settings *)

(** Option settings *)

(* For function that takes two config and update (add or remove) elements in a
   field. Used for appending or deleting element in config file fields *)
type 'config fld_updater =  ('config -> 'config -> 'config)

(* Only some field can be modifiied. [Modifiable] is for user modifiable
   field, [InModifiable] for fields that can only be modified from inner opam
   code (see [set_var_global]).
   First argument is the addition function, the second the remove one. *)
type 'config fld_policy =
  | Atomic
  | Modifiable of 'config fld_updater * 'config fld_updater
  | InModifiable of 'config fld_updater * 'config fld_updater

(* "Configuration" of the [set_opt] function. As modification can be on global
   or config switch, on normal fields and sections, adding, removing, or
   overwritng values, this record type permits to aggregate all needed inputs.
   See [set_opt_global] and [set_opt_switch]. *)
type 'config confset =
  {
    stg_fields: (string * ('config, value) OpamPp.field_parser) list;
    (* Config file fields: field name and parser *)
    stg_allwd_fields:
      (string * 'config fld_policy * ('config -> 'config)) list;
    (* Config file updatable fields: field name, update policy, and function to
       revert the given field in config file *)
    stg_sections:
      (string * ('config, (string option * opamfile_item list) list)
         OpamPp.field_parser) list;
    (* Same as [stg_field] but for sections *)
    stg_allwd_sections:
      ((string * 'config fld_policy * ('config -> 'config)) list);
    (* Same as [stg_allwd_fields] but for sections *)
    stg_config: 'config;
    (* The config *)
    stg_write_config: 'config -> unit;
    (* Function to write the config file *)
    stg_doc: string;
    (* Global or switch specification, used to print final user message *)
  }

type whole_op =
  [ `Overwrite of string
  | `Revert ]

type append_op =
  [ `Add of string
  | `Remove of string ]

type update_op =
  [ append_op  | whole_op ]

let parse_update fv =
  let reg =
    Re.(compile @@ seq [
        group @@ seq [
          wordc;
          opt @@ (seq [ rep @@ alt [ wordc ; char '-' ]; wordc ])
        ];
        (opt @@ seq [
            (group @@ (alt [
                 str "+=";
                 str "-=";
                 str "==";
                 char '=';
               ]));
            opt @@ (group @@ rep1 any)
          ]);
      ])
  in
  let grs = Re.exec reg fv in
  let var = Re.Group.get grs 1 in
  let value =
    try
      let value =
        OpamStd.Option.of_Not_found (fun () -> Re.Group.get grs 3) ()
      in
      match Re.Group.get grs 2, value with
      | "+=", Some value -> `Add value
      | "-=", Some value -> `Remove value
      | ("=" | "=="), Some value -> `Overwrite value
      | ("=" | "=="), None -> `Revert
      | ("+=" | "-="), None -> raise (Invalid_argument "parse_update: rhs needed")
      | _, _ -> raise (Invalid_argument "parse_update: illegal operator")
    with Not_found ->  raise (Invalid_argument "parse_update: operator needed")
  in
  var, value

let whole_of_update_op = function
  | #whole_op as w -> w
  | _ -> raise Not_found

let parse_whole fv =
  let v, upd = parse_update fv in
  try v, (whole_of_update_op upd)
  with Not_found -> raise (Invalid_argument "parse_whole: append operator")

let global_doc = "global configuration"
let switch_doc switch =
  Printf.sprintf "switch %s"
    (OpamConsole.colorise `bold (OpamSwitch.to_string switch))

module OpamParser = OpamParser.FullPos
module OpamPrinter = OpamPrinter.FullPos

(* General setting option function. Takes the [field] to update, the [value]
   operation, [conf] the configuration according the config file (['config
   confest]). If [inner] is set, it allows the modification of [InModifiable]
   fields *)
let set_opt ?(inner=false) field value conf =
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
            [None,
             (OpamParser.string str_value "<command-line>").file_contents]))
  in
  let new_config =
    match OpamStd.List.assoc_opt field fields, value with
    | None, _ ->
      OpamConsole.error
        "There is no option named '%s'. The allowed options are:"
        (OpamConsole.colorise `underline field);
      OpamConsole.print_table stderr ~sep:" "
        (OpamStd.Format.as_aligned_table
           (OpamStd.List.filter_map
              (function fl, Some _ -> Some fl | _ -> None)
              fields));
      OpamStd.Sys.exit_because `Bad_arguments
    | Some None, _ ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s is not modifiable" (OpamConsole.colorise `underline field)
    | Some (Some (_, Atomic, _)), (#append_op as ar) ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s can't be %s" (OpamConsole.colorise `underline field)
        (match ar with `Add _ -> "appended" | `Remove _ -> "substracted")
    | Some (Some (_, InModifiable (_,_), _)), (#append_op as ar) when not inner ->
      OpamConsole.error_and_exit `Bad_arguments
        "Field %s can't be directly %s, use `opam var` instead"
        (OpamConsole.colorise `underline field)
        (match ar with `Add _ -> "appended to"
                     | `Remove _ -> "substracted from")
    | Some (Some (_, _, set_default)), `Revert ->
      set_default conf.stg_config
    | Some (Some (parse, fix_app, _)),
      ((`Add v | `Remove v | `Overwrite v) as req_value) ->
      (try
         let updf v = parse v conf.stg_config in
         match req_value, fix_app with
         | `Add value, (Modifiable (add, _) | InModifiable (add, _))  ->
           add (updf value) conf.stg_config
         | `Remove value, (Modifiable (_, rem) | InModifiable (_, rem)) ->
           rem (updf value) conf.stg_config
         | `Overwrite value, _ -> (updf value)
         | _, Atomic -> assert false
       with
       | (OpamPp.Bad_format (_,_) | Parsing.Parse_error) as e ->
         OpamConsole.error_and_exit `Bad_arguments
           "Parse error on the value of %s '%s': %s"
           (OpamConsole.colorise `underline field) v
           (OpamPp.string_of_bad_format e))
  in
  if conf.stg_config = new_config then
    OpamConsole.msg "No modification in %s\n" conf.stg_doc
  else
    (conf.stg_write_config new_config;
     OpamConsole.msg "%s field %s in %s\n"
       (match value with
        | `Add value ->  Printf.sprintf "Added '%s' to" value
        | `Remove value ->  Printf.sprintf "Removed '%s' from" value
        | `Overwrite value -> Printf.sprintf "Set to '%s' the" value
        | `Revert -> "Reverted")
       (OpamConsole.colorise `underline field)
       conf.stg_doc);
  new_config

let allwd_wrappers wdef wrappers with_wrappers  =
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

let switch_allowed_fields, switch_allowed_sections =
  let allowed_fields =
    lazy (
      OpamFile.Switch_config.(
        [
          ("synopsis", Atomic,
           fun t -> { t with synopsis = empty.synopsis });
          ("setenv", Modifiable (
              (fun nc c -> { c with env = nc.env @ c.env }),
              (fun nc c ->
                 let env =
                   List.filter (fun (vr,op,vl,_) ->
                       None = OpamStd.List.find_opt (fun (vr',op',vl',_) ->
                           vr = vr' && op = op' && vl = vl') nc.env) c.env
                 in
                 { c with env })),
           fun t -> { t with env = empty.env });
          "depext-bypass", OpamSysPkg.Set.Op.(Modifiable (
              (fun nc c ->
                 { c with depext_bypass = nc.depext_bypass ++ c.depext_bypass }),
              (fun nc c ->
                 { c with depext_bypass = c.depext_bypass -- nc.depext_bypass })
            )),
          (fun t -> { t with depext_bypass = empty.depext_bypass });
        ] @ allwd_wrappers empty.wrappers wrappers
          (fun wrappers t -> { t with wrappers })))
  in
  let allowed_sections =
    let rem_elem new_elems elems =
      List.filter (fun n -> not (List.mem n new_elems)) elems
    in
    lazy (
      OpamFile.Switch_config.([
          ("variables", InModifiable (
              (fun nc c -> { c with variables = nc.variables @ c.variables }),
              (fun nc c ->
                 { c with variables = rem_elem nc.variables c.variables })),
           (fun c -> { c with variables = empty.variables }));
        ]))
  in
  (fun () -> Lazy.force allowed_fields),
  fun () -> Lazy.force allowed_sections

let confset_switch gt switch switch_config =
  let config_f = OpamPath.Switch.switch_config gt.root switch in
  let write new_config = OpamFile.Switch_config.write config_f new_config in
  { stg_fields = OpamFile.Switch_config.fields;
    stg_allwd_fields = switch_allowed_fields ();
    stg_sections = OpamFile.Switch_config.sections;
    stg_allwd_sections = switch_allowed_sections ();
    stg_config = switch_config;
    stg_write_config = write;
    stg_doc = switch_doc switch
  }

let with_switch ~display gt lock_kind st k =
  match st with
  | Some st -> k st.switch st.switch_config
  | None ->
    match OpamStateConfig.get_switch_opt () with
    | None ->
      OpamConsole.error_and_exit `Configuration_error "No switch selected"
    | Some switch ->
      let switch_config =
        if lock_kind = `Lock_write then
          match OpamStateConfig.Switch.read_opt ~lock_kind gt switch with
          | Some c -> c
          | exception (OpamPp.Bad_version _ as e) ->
            OpamFormatUpgrade.hard_upgrade_from_2_1_intermediates gt.root;
            raise e
          | None -> OpamFile.Switch_config.empty
        else
          OpamStateConfig.Switch.safe_load ~lock_kind gt switch
      in
      let lock_file = OpamPath.Switch.lock gt.root switch in
      if switch_config = OpamFile.Switch_config.empty then
        if display then
          OpamConsole.error "switch %s not found, display default values"
            (OpamSwitch.to_string switch)
        else
          OpamConsole.error_and_exit `Bad_arguments
            "The selected switch %s is not installed"
            (OpamSwitch.to_string switch);
      OpamFilename.with_flock lock_kind lock_file @@ fun _ ->
      k switch switch_config

let set_opt_switch_t ?inner gt switch switch_config field value =
  set_opt ?inner field value (confset_switch gt switch switch_config)

let set_opt_switch gt ?st field value =
  with_switch ~display:false gt `Lock_write st @@ fun sw swc ->
  let switch_config = set_opt_switch_t ~inner:false gt sw swc field value in
  OpamStd.Option.map (fun st -> { st with switch_config }) st

let global_allowed_fields, global_allowed_sections =
  let allowed_fields =
    lazy (
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
      [
        "download-command", Atomic,
        Config.with_dl_tool_opt
          (InitConfig.dl_tool in_config ++ Config.dl_tool Config.empty);
        "download-jobs", Atomic,
        Config.with_dl_jobs
          (InitConfig.dl_jobs in_config +! Config.dl_jobs Config.empty);
        "jobs", Atomic,
        Config.with_jobs_opt
          (InitConfig.jobs in_config ++ Config.jobs Config.empty);
        "best-effort-prefix-criteria", Atomic,
        Config.with_best_effort_prefix_opt
          (Config.best_effort_prefix Config.empty);
        "solver", Atomic,
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
        "repository-validation-command", Atomic,
        Config.with_validation_hook_opt (Config.validation_hook Config.empty);
        "depext", Atomic,
        Config.with_depext (Config.depext Config.empty);
        "depext-run-installs", Atomic,
        Config.with_depext_run_installs
          (Config.depext_run_installs Config.empty);
        "depext-cannot-install", Atomic,
        Config.with_depext_cannot_install
          (Config.depext_cannot_install Config.empty);
        "depext-bypass", OpamSysPkg.Set.Op.(Modifiable (
            (fun nc c -> Config.with_depext_bypass
                (Config.depext_bypass nc ++ Config.depext_bypass c) c),
            (fun nc c -> Config.with_depext_bypass
                (Config.depext_bypass c -- Config.depext_bypass nc) c)
          )),
        Config.with_depext_bypass (Config.depext_bypass Config.empty);
      ] @ List.map (fun f ->
        f, Atomic, Config.with_criteria
          (Config.criteria Config.empty))
        [ "solver-criteria";
          "solver-upgrade-criteria";
          "solver-fixup-criteria" ]
        @ allwd_wrappers wrapper_init Config.wrappers Config.with_wrappers
      )
  in
  (fun () -> Lazy.force allowed_fields),
  fun () -> []

let confset_global gt =
  let write new_config = OpamGlobalState.write {gt with config = new_config} in
  { stg_fields = OpamFile.Config.fields;
    stg_allwd_fields = global_allowed_fields ();
    stg_sections = [];
    stg_allwd_sections = global_allowed_sections ();
    stg_config = gt.config;
    stg_write_config = write;
    stg_doc = global_doc;
  }

let set_opt_global_t ?inner gt field value =
  let config =
    set_opt ?inner field value (confset_global gt)
  in
  { gt with config }

let set_opt_global = set_opt_global_t ~inner:false

(** Variable settings *)

(* "Configuration" of the [set_var] function. As these modification can be on
   global and switch config, this record aggregates all needed inputs. *)
type ('var,'config) var_confset =
  {
    stv_vars: 'var list;
    (* Variables list *)
    stv_find: 'var -> bool;
    (* Find function embedding a wanted var *)
    stv_config: 'config;
    (* State to use *)
    stv_varstr: string -> string;
    (* [stv_vars value] returns the string of the variable with the new value.
       It is used to give the overall value to [set_opt] functions. *)
    stv_set_opt: 'config -> update_op -> 'config;
    (* The [set_opt] function call [stv_set_opt state var_value] *)
    stv_remove_elem: 'var list -> 'config -> 'config;
    (* As variable can't be duplicated, a function to remove it from the list *)
    stv_write: 'config -> unit;
    (* Write the config file *)
    stv_doc: string;
    (* Global or switch specification, used to print final user message *)
  }

let set_var svar value conf =
  let var = OpamVariable.Full.of_string svar in
  let conf = conf (OpamVariable.Full.variable var) in
  if not (OpamVariable.Full.is_global var) then
    OpamConsole.error_and_exit `Bad_arguments
      "Only global variables may be set using this command";
  let global_vars = conf.stv_vars in
  let rest = List.filter (fun v -> not (conf.stv_find v)) global_vars in
  let config = conf.stv_remove_elem rest conf.stv_config in
  match value with
  | `Overwrite value -> conf.stv_set_opt config (`Add (conf.stv_varstr value))
  | `Revert ->
    (* only write, as the var is already removed *)
    if config = conf.stv_config then
      OpamConsole.msg "No modification in %s\n" conf.stv_doc
    else
      (conf.stv_write config;
       OpamConsole.msg "Removed variable %s in %s\n"
         (OpamConsole.colorise `underline svar)
         conf.stv_doc);
    config

let set_var_global gt var value =
  let config =
    set_var var value @@
    fun var ->
    let global_vars = OpamFile.Config.global_variables gt.config in
    { stv_vars = global_vars;
      stv_find = (fun (k,_,_) -> k = var);
      stv_config = gt.config;
      stv_varstr = (fun v ->
          OpamPrinter.Normalise.value (nullify_pos @@ List (nullify_pos @@ [
              nullify_pos @@ Ident (OpamVariable.to_string var);
              nullify_pos @@ String v;
              nullify_pos @@ String "Set through 'opam var'"
            ])));
      stv_set_opt = (fun config value ->
          let gt =
            set_opt_global_t ~inner:true { gt with config }
              "global-variables" value
          in gt.config);
      stv_remove_elem = (fun rest config ->
          OpamFile.Config.with_global_variables rest config
          |> OpamFile.Config.with_eval_variables
            (List.filter (fun (k,_,_) -> k <> var)
               (OpamFile.Config.eval_variables config)));
      stv_write = (fun config -> OpamGlobalState.write { gt with config });
      stv_doc = global_doc;
    } in
  { gt with config }

let set_var_switch gt ?st var value =
  let var_confset switch switch_config var =
    let switch_vars = switch_config.OpamFile.Switch_config.variables in
    { stv_vars = switch_vars;
      stv_find = (fun (k,_) -> k = var);
      stv_config = switch_config;
      stv_varstr = (fun v ->
          OpamStd.String.remove_suffix ~suffix:"\n" @@
          OpamPrinter.Normalise.items
            [ nullify_pos @@ Variable
                (nullify_pos @@ OpamVariable.to_string var,
                 nullify_pos @@ String v)]);
      stv_set_opt = (fun swc value ->
          set_opt_switch_t ~inner:true gt switch swc "variables" value);
      stv_remove_elem = (fun rest switch_config ->
          { switch_config with variables = rest });
      stv_write = (fun swc ->
          OpamFile.Switch_config.write
            (OpamPath.Switch.switch_config gt.root switch) swc);
      stv_doc = switch_doc switch;
    } in
  let switch_config =
    with_switch ~display:false gt `Lock_write st @@ fun sw swc ->
    set_var var value (var_confset sw swc)
  in
  OpamStd.Option.map (fun st -> { st with switch_config }) st

(** Option and var list display *)

let print_fields fields =
  let fields =
    List.sort (fun (x,_) (x',_) -> compare x x') fields
    |> List.map (fun (name, value) ->
        let value = match value with
          | None -> "{}"
          | Some value -> (OpamPrinter.Normalise.value value)
        in
        [
          OpamConsole.colorise `bold name ;
          OpamConsole.colorise `blue value
        ])
  in
  OpamConsole.print_table stdout ~sep:"  "
    (OpamStd.Format.align_table fields)

let find_field field name_value =
  match OpamStd.List.find_opt (fun (name, _) -> name = field) name_value with
  | None -> (field, None)
  | Some (name, value) -> (name, Some value)

let find_section section name_value =
  let sections =
    List.find_all (fun (name, _) ->
        match OpamStd.String.cut_at name '.' with
        | None -> false
        | Some (name,_) -> name = section)
      name_value
  in
  match sections with
  | [] -> [section, None]
  | section -> List.map (fun (n,v) -> n, Some v) section

let options_list_t to_list conf =
  let name_value = to_list conf.stg_config in
  let fields =
    OpamStd.List.filter_map (fun (field, policy, _) ->
        match policy with
        | InModifiable _ -> None
        | _ -> Some (find_field field name_value))
      conf.stg_allwd_fields
  in
  let sections =
    OpamStd.List.filter_map (fun (field, policy, _) ->
        match policy with
        | InModifiable _ -> None
        | _ -> Some (find_section field name_value))
      conf.stg_allwd_sections
    |> List.flatten
  in
  print_fields (fields @ sections)

let options_list_switch ?st gt =
  with_switch ~display:true gt `Lock_none st @@ fun sw swc ->
  options_list_t OpamFile.Switch_config.to_list (confset_switch gt sw swc)

let options_list_global gt =
  options_list_t OpamFile.Config.to_list (confset_global gt)

let options_list ?st gt =
  OpamConsole.header_msg "Global configuration";
  options_list_global gt;
  let switch_header = OpamConsole.header_msg "Switch configuration%s" in
  match OpamStateConfig.get_switch_opt () with
  | None ->
    switch_header "";
    OpamConsole.msg "No switch installed\n"
  | Some switch ->
    switch_header (Printf.sprintf " (%s)" (OpamSwitch.to_string switch));
    options_list_switch ?st gt

let vars_list_global gt =
  let (%) s col = OpamConsole.colorise col s in
  let all_global_vars =
    List.fold_left (fun acc (v,doc) ->
        OpamVariable.Map.add (OpamVariable.of_string v) doc acc)
      OpamVariable.Map.empty
      OpamPackageVar.global_variable_names
  in
  let all_global_vars =
    OpamVariable.Map.union (fun _ x -> x)
      all_global_vars
      (OpamVariable.Map.map snd gt.global_variables)
  in
  let env = OpamPackageVar.resolve_global gt in
  List.map (fun (var, doc) ->
      let content =
        OpamFilter.ident_string env ~default:"" ([],var,None)
      in
      let doc =
        if doc = OpamGlobalState.inferred_from_system then
          match OpamStd.Option.Op.(
              OpamVariable.Map.find_opt var gt.global_variables
              >>| fst
              >>= Lazy.force) with
          | Some c
            when (OpamVariable.string_of_variable_contents c) <> content ->
            "Set through local opam config or env"
          | _ -> doc
        else doc
      in
      [
        OpamVariable.to_string var % `bold;
        content % `blue;
        "#"; doc
      ])
    (List.sort (fun (x,_) (x',_) -> compare x x')
       (OpamVariable.Map.bindings all_global_vars)) |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" "

let vars_list_switch ?st gt =
  let (%) s col = OpamConsole.colorise col s in
  let switch, config =
    match st with
    | Some st -> st.switch, st.switch_config
    | None ->
      let switch = OpamStateConfig.get_switch () in
      switch,
      OpamStateConfig.Switch.safe_load ~lock_kind:`Lock_read gt switch
  in
  List.map (fun stdpath -> [
        OpamTypesBase.string_of_std_path stdpath % `bold;
        OpamPath.Switch.get_stdpath gt.root switch config stdpath |>
        OpamFilename.Dir.to_string |>
        OpamConsole.colorise `blue
      ])
    OpamTypesBase.all_std_paths @
  List.map (fun (var,value) -> [
        OpamVariable.to_string var % `bold;
        OpamVariable.string_of_variable_contents value % `blue;
      ])
    (List.sort (fun (x,_) (x',_) -> compare x' x)
       config.OpamFile.Switch_config.variables) |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" "

let vars_list ?st gt =
  let (%) s col = OpamConsole.colorise col s in
  OpamConsole.header_msg "Global opam variables";
  vars_list_global gt;
  OpamConsole.header_msg "Configuration variables from the current switch";
  (match OpamStateConfig.get_switch_opt () with
  | None -> OpamConsole.msg "No switch installed\n"
  | Some _ -> vars_list_switch ?st gt);
  OpamConsole.header_msg "Package variables ('opam var --package PKG' to show)";
  List.map (fun (var, doc) -> [
        ("PKG:"^var) % `bold;
        "";
        "#";doc
      ])
    OpamPackageVar.package_variable_names |>
  OpamStd.Format.align_table |>
  OpamConsole.print_table stdout ~sep:" "

(* Specified option/var display *)

let option_show to_list conf field =
  match OpamStd.List.assoc_opt field conf.stg_fields with
  | Some pp ->
    (match OpamPp.print pp conf.stg_config with
     | _, Some value ->
       OpamConsole.msg "%s\n" (OpamPrinter.Normalise.value value)
     | _, None -> ())
  | None ->
    if List.mem_assoc field conf.stg_sections then
      let name_value = to_list conf.stg_config in
      let sections =
        OpamStd.List.filter_map (fun (name, v) ->
            match OpamStd.String.cut_at name '.' with
            | Some (name,elem) when name = field ->
              Some [ elem; OpamPrinter.Normalise.value v ]
            | _ -> None
          ) name_value
      in
      OpamConsole.print_table stdout ~sep:"  "
        (OpamStd.Format.align_table sections)
    else
      OpamConsole.error_and_exit `Not_found
        "Field or section %s not found" field

let option_show_switch gt ?st field =
  with_switch ~display:true gt `Lock_none st @@ fun sw swc ->
  option_show OpamFile.Switch_config.to_list (confset_switch gt sw swc) field

let option_show_global gt field =
  option_show OpamFile.Config.to_list (confset_global gt) field

let var_show_t resolve ?switch v =
  match resolve (OpamVariable.Full.of_string v) with
  | Some c ->
    OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c)
  | None ->
    OpamConsole.error_and_exit `Not_found "Variable %s not found in %s" v
      (match switch with
           | None -> "global config"
           | Some switch -> "switch " ^ (OpamSwitch.to_string switch))

let is_switch_defined_var switch_config v =
  OpamFile.Switch_config.variable switch_config
    (OpamVariable.of_string v) <> None
  || (try let _path = OpamTypesBase.std_path_of_string v in true
      with Failure _ -> false)
  || OpamStd.String.contains_char v ':'

let var_switch_raw gt v =
  match OpamStateConfig.get_switch_opt () with
  | Some switch ->
    let switch_config =
      OpamStateConfig.Switch.safe_load ~lock_kind:`Lock_read gt switch
    in
    let rsc =
      if is_switch_defined_var switch_config v then
        OpamPackageVar.resolve_switch_raw gt switch switch_config
          (OpamVariable.Full.of_string v)
      else None
    in
    (match rsc with
     | Some c ->
       OpamConsole.msg "%s\n" (OpamVariable.string_of_variable_contents c);
       rsc
     | None -> None)
  | None -> None

let var_show_switch gt ?st v =
  if var_switch_raw gt v = None then
    let resolve_switch st =
      if is_switch_defined_var st.switch_config v then
        var_show_t (OpamPackageVar.resolve st) ~switch:st.switch v
      else
        OpamConsole.error_and_exit `Not_found
          "Variable %s not found in switch %s"
          v (OpamSwitch.to_string st.switch)
    in
    match st with
    | Some st -> resolve_switch st
    | None -> OpamSwitchState.with_ `Lock_none gt resolve_switch

let var_show_global gt f = var_show_t (OpamPackageVar.resolve_global gt) f

let var_show gt v =
  if var_switch_raw gt v = None then
    let resolve, switch =
      match OpamStateConfig.get_switch_opt () with
      | None -> OpamPackageVar.resolve_global gt, None
      | Some switch ->
        OpamSwitchState.with_ `Lock_none ~switch gt @@ fun st ->
        let resolve = (OpamPackageVar.resolve st ?opam:None ?local:None) in
        resolve,
        if is_switch_defined_var st.switch_config v then Some st.switch else None
    in
    var_show_t resolve ?switch v

(* detect scope *)
let get_scope field =
  let field =
    try fst (parse_update field)
    with Invalid_argument _ -> field
  in
  let find l = OpamStd.List.find_opt (fun (f,_) -> f = field) l in
  if OpamStateConfig.get_switch_opt () <> None
  && (find OpamFile.Switch_config.fields <> None
      || find OpamFile.Switch_config.sections <> None) then
    `Switch
  else if find OpamFile.Config.fields <> None then
    `Global
  else `None field
