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
open OpamTypesBase
open OpamStd.Op
open OpamFilename.Op

let log fmt = OpamConsole.log "ENV" fmt
let slog = OpamConsole.slog

(* - Environment and updates handling - *)

let split_var v = OpamStd.Sys.split_path_variable ~clean:false v

let join_var l =
  String.concat (String.make 1 OpamStd.Sys.path_sep) l

(* To allow in-place updates, we store intermediate values of path-like as a
   pair of list [(rl1, l2)] such that the value is [List.rev_append rl1 l2] and
   the place where the new value should be inserted is in front of [l2] *)
let unzip_to elt =
  let rec aux acc = function
    | [] -> None
    | x::r ->
      if x = elt then Some (acc, r)
      else aux (x::acc) r
  in
  aux []

let rezip ?insert (l1, l2) =
  List.rev_append l1 (match insert with None -> l2 | Some i -> i::l2)

let rezip_to_string ?insert z =
  join_var (rezip ?insert z)

let apply_op_zip op arg (rl1,l2 as zip) =
  let colon_eq ?(eqcol=false) = function (* prepend a, but keep ":"s *)
    | [] | [""] -> [], [arg; ""]
    | "" :: l ->
      (* keep surrounding colons *)
      if eqcol then l@[""], [arg] else l, [""; arg]
    | l -> l, [arg]
  in
  match op with
  | Eq -> [],[arg]
  | PlusEq -> [], arg :: rezip zip
  | EqPlus -> List.rev_append l2 rl1, [arg]
  | EqPlusEq -> rl1, arg::l2
  | ColonEq ->
    let l, add = colon_eq (rezip zip) in [], add @ l
  | EqColon ->
    let l, add = colon_eq ~eqcol:true (List.rev_append l2 rl1) in
    l, List.rev add

(** Undoes previous updates done by opam, useful for not duplicating already
    done updates; this is obviously not perfect, as all operators are not
    reversible.

    [cur_value] is provided as a list split at path_sep.

    None is returned if the revert doesn't match. Otherwise, a zip (pair of lists
    [(preceding_elements_reverted, following_elements)]) is returned, to keep the
    position of the matching element and allow [=+=] to be applied later. A pair
    or empty lists is returned if the variable should be unset or has an unknown
    previous value. *)
let reverse_env_update op arg cur_value =
  match op with
  | Eq ->
    if arg = join_var cur_value
    then Some ([],[]) else None
  | PlusEq | EqPlusEq -> unzip_to arg cur_value
  | EqPlus ->
    (match unzip_to arg (List.rev cur_value) with
     | None -> None
     | Some (rl1, l2) -> Some (List.rev l2, List.rev rl1))
  | ColonEq ->
    (match unzip_to arg cur_value with
     | Some ([], [""]) -> Some ([], [])
     | r -> r)
  | EqColon ->
    (match unzip_to arg (List.rev cur_value) with
     | Some ([], [""]) -> Some ([], [])
     | Some (rl1, l2) -> Some (List.rev l2, List.rev rl1)
     | None -> None)

let updates_from_previous_instance = lazy (
  match OpamStd.Env.getopt "OPAM_SWITCH_PREFIX" with
  | None -> None
  | Some pfx ->
    let env_file =
      OpamPath.Switch.env_relative_to_prefix (OpamFilename.Dir.of_string pfx)
    in
    try OpamFile.Environment.read_opt env_file
    with e -> OpamStd.Exn.fatal e; None
)

let expand (updates: env_update list) : env =
  (* Reverse all previous updates, in reverse order, on current environment *)
  let reverts =
    match Lazy.force updates_from_previous_instance with
    | None -> []
    | Some updates ->
      List.fold_right (fun (var, op, arg, _) defs0 ->
          let v_opt, defs = OpamStd.List.pick_assoc var defs0 in
          let v =
            OpamStd.Option.Op.((v_opt >>| rezip >>+ fun () ->
                                OpamStd.Env.getopt var >>| split_var) +! [])
          in
          match reverse_env_update op arg v with
          | Some v -> (var, v)::defs
          | None -> defs0)
        updates []
  in
  (* And apply the new ones *)
  let rec apply_updates reverts acc = function
    | (var, op, arg, doc) :: updates ->
      let zip, reverts =
        let f, var =
          if Sys.win32 then
            String.uppercase_ascii, String.uppercase_ascii var
          else (fun x -> x), var
        in
        match OpamStd.List.find_opt (fun (v, _, _) -> f v = var) acc with
        | Some (_, z, _doc) -> z, reverts
        | None ->
          match OpamStd.List.pick_assoc var reverts with
          | Some z, reverts -> z, reverts
          | None, _ ->
            match OpamStd.Env.getopt var with
            | Some s -> ([], split_var s), reverts
            | None -> ([], []), reverts
      in
      apply_updates
        reverts
        ((var, apply_op_zip op arg zip, doc) :: acc)
        updates
    | [] ->
      List.rev @@
      List.rev_append
        (List.rev_map (fun (var, z, doc) -> var, rezip_to_string z, doc) acc) @@
      List.rev_map (fun (var, z) ->
          var, rezip_to_string z, Some "Reverting previous opam update")
        reverts
  in
  apply_updates reverts [] updates

let add (env: env) (updates: env_update list) =
  let env =
    if Sys.win32 then
      (*
       * Environment variable names are case insensitive on Windows
       *)
      let updates = List.rev_map (fun (u,_,_,_) -> (String.uppercase_ascii u, "", "", None)) updates in
      List.filter (fun (k,_,_) -> let k = String.uppercase_ascii k in List.for_all (fun (u,_,_,_) -> u <> k) updates) env
    else
      List.filter (fun (k,_,_) -> List.for_all (fun (u,_,_,_) -> u <> k) updates)
        env
  in
  env @ expand updates

let compute_updates ?(force_path=false) st =
  (* Todo: put these back into their packages!
  let perl5 = OpamPackage.Name.of_string "perl5" in
  let add_to_perl5lib =  OpamPath.Switch.lib t.root t.switch t.switch_config perl5 in
  let new_perl5lib = "PERL5LIB", "+=", OpamFilename.Dir.to_string add_to_perl5lib in
*)
  let fenv ?opam v =
    try OpamPackageVar.resolve st ?opam v
    with Not_found ->
      log "Undefined variable: %s" (OpamVariable.Full.to_string v);
      None
  in
  let bindir =
    OpamPath.Switch.bin st.switch_global.root st.switch st.switch_config
  in
  let path =
    "PATH",
    (if force_path then PlusEq else EqPlusEq),
    OpamFilename.Dir.to_string bindir,
    Some ("Binary dir for opam switch "^OpamSwitch.to_string st.switch)
  in
  let man_path =
    let open OpamStd.Sys in
    match os () with
    | OpenBSD | NetBSD | FreeBSD | Darwin | DragonFly ->
      [] (* MANPATH is a global override on those, so disabled for now *)
    | _ ->
      ["MANPATH", EqColon,
       OpamFilename.Dir.to_string
         (OpamPath.Switch.man_dir
            st.switch_global.root st.switch st.switch_config),
      Some "Current opam switch man dir"]
  in
  let env_expansion ?opam (name,op,str,cmt) =
    let s = OpamFilter.expand_string ~default:(fun _ -> "") (fenv ?opam) str in
    name, op, s, cmt
  in
  let switch_env =
    ("OPAM_SWITCH_PREFIX", Eq,
     OpamFilename.Dir.to_string
       (OpamPath.Switch.root st.switch_global.root st.switch),
     Some "Prefix of the current opam switch") ::
    List.map env_expansion st.switch_config.OpamFile.Switch_config.env
  in
  let pkg_env = (* XXX: Does this need a (costly) topological sort? *)
    OpamPackage.Set.fold (fun nv acc ->
        match OpamPackage.Map.find_opt nv st.opams with
        | Some opam -> List.map (env_expansion ~opam) (OpamFile.OPAM.env opam) @ acc
        | None -> acc)
      st.installed []
  in
  switch_env @ pkg_env @ man_path @ [path]

let updates_common ~set_opamroot ~set_opamswitch root switch =
  let root =
    if set_opamroot then
      [ "OPAMROOT", Eq, OpamFilename.Dir.to_string root,
        Some "Opam root in use" ]
    else []
  in
  let switch =
    if set_opamswitch then
      [ "OPAMSWITCH", Eq, OpamSwitch.to_string switch, None ]
    else [] in
  root @ switch

let updates ?(set_opamroot=false) ?(set_opamswitch=false) ?force_path st =
  updates_common ~set_opamroot ~set_opamswitch st.switch_global.root st.switch @
  compute_updates ?force_path st

let get_pure ?(updates=[]) () =
  let env = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add env updates

let get_opam ?(set_opamroot=false) ?(set_opamswitch=false) ~force_path st =
  add [] (updates ~set_opamroot ~set_opamswitch ~force_path st)

let get_opam_raw ?(set_opamroot=false) ?(set_opamswitch=false) ~force_path
    root switch =
  let env_file = OpamPath.Switch.environment root switch in
  let upd = OpamFile.Environment.safe_read env_file in
  let upd =
    ("OPAM_SWITCH_PREFIX", Eq,
     OpamFilename.Dir.to_string (OpamPath.Switch.root root switch),
     Some "Prefix of the current opam switch") ::
    List.filter (function ("OPAM_SWITCH_PREFIX", Eq, _, _) -> false | _ -> true)
      upd
  in
  let upd =
    if force_path then
      List.map (function
          | "PATH", EqPlusEq, v, doc -> "PATH", PlusEq, v, doc
          | e -> e)
        upd
    else
      List.map (function
          | "PATH", PlusEq, v, doc -> "PATH", EqPlusEq, v, doc
          | e -> e)
        upd

  in
  add []
    (updates_common ~set_opamroot ~set_opamswitch root switch @
     upd)

let get_full
    ?(set_opamroot=false) ?(set_opamswitch=false) ~force_path ?updates:(u=[])
    st =
  let env0 = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  let updates = u @ updates ~set_opamroot ~set_opamswitch ~force_path st in
  add env0 updates

let is_up_to_date_raw updates =
  OpamStateConfig.(!r.no_env_notice) ||
  let not_utd =
    List.fold_left (fun notutd (var, op, arg, _doc as upd) ->
        match OpamStd.Env.getopt var with
        | None -> upd::notutd
        | Some v ->
          if reverse_env_update op arg (split_var v) = None then upd::notutd
          else List.filter (fun (v, _, _, _) -> v <> var) notutd)
      []
      updates
  in
  let r = not_utd = [] in
  if not r then
    log "Not up-to-date env variables: [%a]"
      (slog @@ String.concat " " @* List.map (fun (v, _, _, _) -> v)) not_utd
  else log "Environment is up-to-date";
  r

let is_up_to_date_switch root switch =
  let env_file = OpamPath.Switch.environment root switch in
  try
    match OpamFile.Environment.read_opt env_file with
    | Some upd -> is_up_to_date_raw upd
    | None -> true
  with e -> OpamStd.Exn.fatal e; true

let switch_path_update ~force_path root switch =
  let bindir =
    OpamPath.Switch.bin root switch
      (OpamFile.Switch_config.safe_read
         (OpamPath.Switch.switch_config root switch))
  in
  [
    "PATH",
    (if force_path then PlusEq else EqPlusEq),
    OpamFilename.Dir.to_string bindir,
    Some "Current opam switch binary dir"
  ]

let path ~force_path root switch =
  let env = expand (switch_path_update ~force_path root switch) in
  let (_, path_value, _) = List.find (fun (v, _, _) -> v = "PATH") env in
  path_value

let full_with_path ~force_path ?(updates=[]) root switch =
  let env0 = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add env0 (switch_path_update ~force_path root switch @ updates)

let is_up_to_date st =
  is_up_to_date_raw
    (updates ~set_opamroot:false ~set_opamswitch:false ~force_path:false st)

let eval_string gt ?(set_opamswitch=false) switch =
  let root =
    let opamroot_cur = OpamFilename.Dir.to_string gt.root in
    let opamroot_env =
      OpamStd.Option.Op.(
        OpamStd.Env.getopt "OPAMROOT" +!
        OpamFilename.Dir.to_string OpamStateConfig.(default.root_dir)
      ) in
    if opamroot_cur <> opamroot_env then
      Printf.sprintf " --root=%s" opamroot_cur
    else
      "" in
  let switch =
    match switch with
    | None -> ""
    | Some sw ->
      let sw_cur = OpamSwitch.to_string sw in
      let sw_env =
        OpamStd.Option.Op.(
          OpamStd.Env.getopt "OPAMSWITCH" ++
          (OpamStateConfig.get_current_switch_from_cwd gt.root >>|
           OpamSwitch.to_string) ++
          (OpamFile.Config.switch gt.config >>| OpamSwitch.to_string)
        )
      in
      if Some sw_cur <> sw_env then Printf.sprintf " --switch=%s" sw_cur
      else ""
  in
  let setswitch = if set_opamswitch then " --set-switch" else "" in
  match OpamStd.Sys.guess_shell_compat () with
  | SH_fish ->
    Printf.sprintf "eval (opam env%s%s%s)" root switch setswitch
  | SH_csh ->
    Printf.sprintf "eval `opam env%s%s%s`" root switch setswitch
  | _ ->
    Printf.sprintf "eval $(opam env%s%s%s)" root switch setswitch



(* -- Shell and init scripts handling -- *)

(** The shells for which we generate init scripts (bash and sh are the same
    entry) *)
let shells_list = [ SH_sh; SH_zsh; SH_csh; SH_fish ]

let complete_file = function
  | SH_sh | SH_bash -> Some "complete.sh"
  | SH_zsh -> Some "complete.zsh"
  | SH_csh | SH_fish -> None

let env_hook_file = function
  | SH_sh | SH_bash -> Some "env_hook.sh"
  | SH_zsh -> Some "env_hook.zsh"
  | SH_csh -> Some "env_hook.csh"
  | SH_fish -> Some "env_hook.fish"

let variables_file = function
  | SH_sh | SH_bash | SH_zsh -> "variables.sh"
  | SH_csh -> "variables.csh"
  | SH_fish -> "variables.fish"

let init_file = function
  | SH_sh | SH_bash -> "init.sh"
  | SH_zsh -> "init.zsh"
  | SH_csh -> "init.csh"
  | SH_fish -> "init.fish"

let complete_script = function
  | SH_sh | SH_bash -> Some OpamScript.complete
  | SH_zsh -> Some OpamScript.complete_zsh
  | SH_csh | SH_fish -> None

let env_hook_script_base = function
  | SH_sh | SH_bash -> Some OpamScript.env_hook
  | SH_zsh -> Some OpamScript.env_hook_zsh
  | SH_csh -> Some OpamScript.env_hook_csh
  | SH_fish -> Some OpamScript.env_hook_fish

let export_in_shell shell =
  let make_comment comment_opt =
    OpamStd.Option.to_string (Printf.sprintf "# %s\n") comment_opt
  in
  let sh   (k,v,comment) =
    Printf.sprintf "%s%s=%s; export %s;\n"
      (make_comment comment) k v k in
  let csh  (k,v,comment) =
    Printf.sprintf "%sif ( ! ${?%s} ) setenv %s \"\"\nsetenv %s %s\n"
      (make_comment comment) k k k v in
  let fish (k,v,comment) =
    (* Fish converts some colon-separated vars to arrays, which have to be
       treated differently. MANPATH is handled automatically, so better not to
       set it at all when not already defined *)
    let to_arr_string v =
      OpamStd.List.concat_map " "
        (fun v ->
           if v = Printf.sprintf "\"$%s\"" k then
             "$"^k (* remove quotes *)
           else v)
        (OpamStd.String.split v ':')
    in
    match k with
    | "PATH" ->
      Printf.sprintf "%sset -gx %s %s;\n"
        (make_comment comment) k (to_arr_string v)
    | "MANPATH" ->
      Printf.sprintf "%sif [ (count $%s) -gt 0 ]; set -gx %s %s; end;\n"
        (make_comment comment) k k (to_arr_string v)
    | _ ->
      (* Regular string variables *)
      Printf.sprintf "%sset -gx %s %s;\n"
        (make_comment comment) k v
  in
  match shell with
  | SH_zsh | SH_bash | SH_sh -> sh
  | SH_fish -> fish
  | SH_csh -> csh

let env_hook_script shell =
  OpamStd.Option.map (fun script ->
      export_in_shell shell ("OPAMNOENVNOTICE", "true", None)
      ^ script)
    (env_hook_script_base shell)

let source root shell f =
  let file f = OpamFilename.to_string (OpamPath.init root // f) in
  match shell with
  | SH_csh ->
    Printf.sprintf "source %s >& /dev/null || true\n" (file f)
  | SH_fish ->
    Printf.sprintf "source %s > /dev/null 2> /dev/null; or true\n" (file f)
  | SH_sh | SH_bash | SH_zsh ->
    Printf.sprintf "test -r %s && . %s > /dev/null 2> /dev/null || true\n"
      (file f) (file f)

let if_interactive_script shell t e =
  let ielse else_opt = match else_opt with
    |  None -> ""
    | Some e -> Printf.sprintf "else\n  %s" e
  in
  match shell with
  | SH_sh | SH_zsh | SH_bash ->
    Printf.sprintf "if [ -t 0 ]; then\n  %s%sfi\n" t @@ ielse e
  | SH_csh ->
    Printf.sprintf "if ( $?prompt ) then\n  %s%sendif\n" t @@ ielse e
  | SH_fish ->
    Printf.sprintf "if isatty\n  %s%send\n" t @@ ielse e

let init_script root shell =
  let interactive =
    List.map (source root shell) @@
    OpamStd.List.filter_some [complete_file shell; env_hook_file shell]
  in
  String.concat "\n" @@
  (if interactive <> [] then
     [if_interactive_script shell (String.concat "\n  " interactive) None]
   else []) @
  [source root shell (variables_file shell)]

let string_of_update st shell updates =
  let fenv = OpamPackageVar.resolve st in
  let aux (ident, symbol, string, comment) =
    let string =
      OpamFilter.expand_string ~default:(fun _ -> "") fenv string |>
      OpamStd.Env.escape_single_quotes ~using_backslashes:(shell = SH_fish)
    in
    let key, value =
      ident, match symbol with
      | Eq  -> Printf.sprintf "'%s'" string
      | PlusEq | ColonEq | EqPlusEq ->
        Printf.sprintf "'%s':\"$%s\"" string ident
      | EqColon | EqPlus ->
        Printf.sprintf "\"$%s\":'%s'" ident string
    in
    export_in_shell shell (key, value, comment) in
  OpamStd.List.concat_map "" aux updates

let write_script dir (name, body) =
  let file = dir // name in
  try OpamFilename.write file body
  with e ->
    OpamStd.Exn.fatal e;
    OpamConsole.error "Could not write %s" (OpamFilename.to_string file)

let write_init_shell_scripts root =
  let scripts =
    List.map (fun shell -> init_file shell, init_script root shell) shells_list
  in
  List.iter (write_script (OpamPath.init root)) scripts

let write_static_init_scripts root ?completion ?env_hook () =
  write_init_shell_scripts root;
  let update_scripts filef scriptf enable =
    let scripts =
      OpamStd.List.filter_map (fun shell ->
          match filef shell, scriptf shell with
          | Some f, Some s -> Some (f, s)
          | _ -> None)
        shells_list
    in
    match enable with
    | Some true ->
      List.iter (write_script (OpamPath.init root)) scripts
    | Some false ->
      List.iter (fun (f,_) -> OpamFilename.remove (OpamPath.init root // f))
        scripts
    | None -> ()
  in
  update_scripts complete_file complete_script completion;
  update_scripts env_hook_file env_hook_script env_hook

let write_custom_init_scripts root custom =
  List.iter (fun (name, script) ->
      write_script (OpamPath.hooks_dir root) (name, script);
      OpamFilename.chmod (OpamPath.hooks_dir root // name) 0o777
    ) custom

let write_dynamic_init_scripts st =
  let updates = updates ~set_opamroot:false ~set_opamswitch:false st in
  try
    OpamFilename.with_flock_upgrade `Lock_write ~dontblock:true
      st.switch_global.global_lock
    @@ fun _ ->
    List.iter
      (fun shell ->
         write_script (OpamPath.init st.switch_global.root)
           (variables_file shell, string_of_update st shell updates))
      [SH_sh; SH_csh; SH_fish]
  with OpamSystem.Locked ->
    OpamConsole.warning
      "Global shell init scripts not installed (could not acquire lock)"

let clear_dynamic_init_scripts gt =
  List.iter (fun shell ->
      OpamFilename.remove (OpamPath.init gt.root // variables_file shell))
    [SH_sh; SH_csh; SH_fish]

let dot_profile_needs_update root dot_profile =
  if not (OpamFilename.exists dot_profile) then `yes else
  let body = OpamFilename.read dot_profile in
  let pattern1 = "opam config env" in
  let pattern1b = "opam env" in
  let pattern2 = OpamFilename.to_string (OpamPath.init root // "init") in
  let pattern3 =
    OpamStd.String.remove_prefix ~prefix:(OpamFilename.Dir.to_string root)
      pattern2
  in
  let uncommented_re patts =
    Re.(compile (seq [bol; rep (diff any (set "#:"));
                      alt (List.map str patts)]))
  in
  if Re.execp (uncommented_re [pattern1; pattern1b; pattern2]) body then `no
  else if Re.execp (uncommented_re [pattern3]) body then `otherroot
  else `yes

let update_dot_profile root dot_profile shell =
  let pretty_dot_profile = OpamFilename.prettify dot_profile in
  let bash_src () =
    if shell = SH_bash || shell = SH_sh then
      OpamConsole.note "Make sure that %s is well %s in your ~/.bashrc.\n"
        pretty_dot_profile
        (OpamConsole.colorise `underline "sourced")
  in
  match dot_profile_needs_update root dot_profile with
  | `no        -> OpamConsole.msg "  %s is already up-to-date.\n" pretty_dot_profile; bash_src()
  | `otherroot ->
    OpamConsole.msg
      "  %s is already configured for another opam root.\n"
      pretty_dot_profile
  | `yes       ->
    let init_file = init_file shell in
    let body =
      if OpamFilename.exists dot_profile then
        OpamFilename.read dot_profile
      else
        "" in
    OpamConsole.msg "  Updating %s.\n" pretty_dot_profile;
    bash_src();
    let body =
      Printf.sprintf
        "%s\n\n\
         # opam configuration\n\
         %s"
        (OpamStd.String.strip body) (source root shell init_file) in
    OpamFilename.write dot_profile body


let update_user_setup root ?dot_profile shell =
  if dot_profile <> None then (
    OpamConsole.msg "\nUser configuration:\n";
    OpamStd.Option.iter (fun f -> update_dot_profile root f shell) dot_profile
  )

let check_and_print_env_warning st =
  if not (is_up_to_date st) &&
     (OpamFile.Config.switch st.switch_global.config = Some st.switch ||
      OpamStateConfig.(!r.switch_from <> `Command_line))
  then
    OpamConsole.formatted_msg
      "# Run %s to update the current shell environment\n"
      (OpamConsole.colorise `bold (eval_string st.switch_global
                                     (Some st.switch)))

let setup
    root ~interactive ?dot_profile ?update_config ?env_hook ?completion
    shell =
  let update_dot_profile =
    match update_config, dot_profile, interactive with
    | Some false, _, _ -> None
    | _, None, _ -> invalid_arg "OpamEnv.setup"
    | Some true, Some dot_profile, _ -> Some dot_profile
    | None, _, false -> None
    | None, Some dot_profile, true ->
      OpamConsole.header_msg "Required setup - please read";

      OpamConsole.msg
        "\n\
        \  In normal operation, opam only alters files within ~/.opam.\n\
         \n\
        \  However, to best integrate with your system, some environment variables\n\
        \  should be set. If you allow it to, this initialisation step will update\n\
        \  your %s configuration by adding the following line to %s:\n\
         \n\
        \    %s\
         \n\
        \  Otherwise, every time you want to access your opam installation, you will\n\
        \  need to run:\n\
         \n\
        \    %s\n\
         \n\
        \  You can always re-run this setup with 'opam init' later.\n\n"
        (OpamConsole.colorise `bold @@ string_of_shell shell)
        (OpamConsole.colorise `cyan @@ OpamFilename.prettify dot_profile)
        (OpamConsole.colorise `bold @@ source root shell (init_file shell))
        (OpamConsole.colorise `bold @@ "eval $(opam env)");
      match
        OpamConsole.read
          "Do you want opam to modify %s? [N/y/f]\n\
           (default is 'no', use 'f' to choose a different file)"
          (OpamFilename.prettify dot_profile)
      with
      | Some ("y" | "Y" | "yes"  | "YES" ) -> Some dot_profile
      | Some ("f" | "F" | "file" | "FILE") ->
        begin
          match OpamConsole.read "  Enter the name of the file to update:"
          with
          | None   ->
            OpamConsole.msg "Alright, assuming you changed your mind, not \
                             performing any changes.\n";
            None
          | Some f -> Some (OpamFilename.of_string f)
        end
      | _ -> None
  in
  let env_hook = match env_hook, interactive with
    | Some b, _ -> Some b
    | None, false -> None
    | None, true ->
      Some
        (OpamConsole.confirm ~default:false
           "A hook can be added to opam's init scripts to ensure that the \
            shell remains in sync with the opam environment when they are \
            loaded. Set that up?")
  in
  update_user_setup root ?dot_profile:update_dot_profile shell;
  write_static_init_scripts root ?completion ?env_hook ()
