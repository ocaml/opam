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

open OpamTypes
open OpamStateTypes
open OpamTypesBase
open OpamFilename.Op

let log fmt = OpamConsole.log "ENV" fmt

(* - Environment and updates handling - *)

let expand_update (ident, op, string, comment) =
  let prefix = OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir) in
  let read_env () =
    try OpamStd.Env.reset_value ~prefix (OpamStd.Sys.path_sep ())
          (OpamStd.Env.get ident)
    with Not_found -> [] in
  let update_env a =
    let before, after =
      OpamStd.Env.cut_value
        ~prefix (OpamStd.Sys.path_sep ()) (OpamStd.Env.get ident)
    in
    List.rev_append before (a::after)
  in
  let cons ~head a b =
    let c = List.filter ((<>)"") b in
    match b with
    | []      -> if head then [ ""; a ] else [ a; "" ]
    | "" :: _ -> "" :: a :: c
    | _       ->
      match List.rev b with
      | "" :: _ -> (a :: c) @ [""]
      | _       -> a :: c in
  let c = String.make 1 (OpamStd.Sys.path_sep ()) in
  match op with
  | Eq  -> ident, string, comment
  | PlusEq -> ident, String.concat c (string :: read_env ()), comment
  | EqPlus -> ident, String.concat c (read_env () @ [string]), comment
  | EqPlusEq -> ident, String.concat c (update_env string), comment
  | ColonEq ->
    ident, String.concat c (cons ~head:true string (read_env())), comment
  | EqColon ->
    ident, String.concat c (cons ~head:false string (read_env())), comment

let expand (env: env_update list) : env =
  List.map expand_update env

let add (env: env) (updates: env_update list) =
  let env =
    List.filter (fun (k,_,_) -> List.for_all (fun (u,_,_,_) -> u <> k) updates)
      env
  in
  env @ expand updates

let compute_updates st =
  (* Todo: put these back into their packages !
  let perl5 = OpamPackage.Name.of_string "perl5" in
  let add_to_perl5lib =  OpamPath.Switch.lib t.root t.switch t.switch_config perl5 in
  let new_perl5lib = "PERL5LIB", "+=", OpamFilename.Dir.to_string add_to_perl5lib in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=",
    OpamFilename.Dir.to_string (OpamPath.Switch.toplevel t.root t.switch t.switch_config) in
*)
  let fenv ?opam v =
    try OpamPackageVar.resolve st ?opam v
    with Not_found ->
      log "Undefined variable: %s" (OpamVariable.Full.to_string v);
      None
  in
  let man_path =
    let open OpamStd.Sys in
    match os () with
    | OpenBSD | NetBSD | FreeBSD ->
      [] (* MANPATH is a global override on those, so disabled for now *)
    | _ ->
      ["MANPATH", EqColon,
       OpamFilename.Dir.to_string
         (OpamPath.Switch.man_dir
            st.switch_global.root st.switch st.switch_config),
      Some "Current opam switch man dir"]
  in
  let pkg_env = (* XXX: Does this need a (costly) topological sort ? *)
    OpamPackage.Set.fold (fun nv acc ->
        let opam = OpamSwitchState.opam st nv in
        List.map (fun (name,op,str,cmt) ->
            let s = OpamFilter.expand_string (fenv ~opam) str in
            name, op, s, cmt)
          (OpamFile.OPAM.env opam)
        @ acc)
      st.installed []
  in
  let root =
    let current = st.switch_global.root in
    let default = OpamStateConfig.(default.root_dir) in
    let current_string = OpamFilename.Dir.to_string current in
    let env = OpamStd.Env.getopt "OPAMROOT" in
    if current <> default || (env <> None && env <> Some current_string)
    then [ "OPAMROOT", Eq, current_string, None ]
    else []
  in
  man_path @ root @ pkg_env

let updates ~opamswitch ?(force_path=false) st =
  let root = st.switch_global.root in
  let update =
    let fn = OpamPath.Switch.environment root st.switch in
    match OpamFile.Environment.read_opt fn with
    | Some env -> env
    | None -> compute_updates st
  in
  let add_to_path = OpamPath.Switch.bin root st.switch st.switch_config in
  let new_path =
    "PATH",
    (if force_path then PlusEq else EqPlusEq),
    OpamFilename.Dir.to_string add_to_path,
    Some "Current opam switch binary dir" in
  let switch =
    if opamswitch then
      [ "OPAMSWITCH", Eq, OpamSwitch.to_string st.switch, None ]
    else [] in
  new_path :: switch @ update

(* This function is used by 'opam config env' and 'opam switch' to
   display the environment variables. We have to make sure that
   OPAMSWITCH is always the one being reported in '~/.opam/config'
   otherwise we can have very weird results (as the inability to switch
   between compilers).

   Note: when we do the later command with --switch=SWITCH, this mean
   we really want to get the environment for this switch. *)
let get_opam ~force_path st =
  let opamswitch = OpamStateConfig.(!r.switch_from <> `Default) in
  add [] (updates ~opamswitch ~force_path st)

let get_full ?(opamswitch=true) ~force_path st =
  let env0 = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add env0 (updates ~opamswitch ~force_path st)

let path ~force_path root switch =
  let bindir =
    OpamPath.Switch.bin root switch
      (OpamFile.Dot_config.safe_read
         (OpamPath.Switch.global_config root switch))
  in
  let _, path, _ =
    expand_update (
      "PATH",
      (if force_path then PlusEq else EqPlusEq),
      OpamFilename.Dir.to_string bindir,
      Some "Current opam switch binary dir"
    )
  in
  path

let full_with_path ~force_path root switch =
  let env0 = List.map (fun (v,va) -> v,va,None) (OpamStd.Env.list ()) in
  add env0 [
    "PATH",
    (if force_path then PlusEq else EqPlusEq),
    path ~force_path root switch,
    None
  ]

let is_up_to_date st =
  let changes =
    List.filter
      (fun (s, v, _) -> Some v <>
                        try Some (OpamStd.Env.get s) with Not_found -> None)
      (get_opam ~force_path:false st) in
  log "Not up-to-date env variables: [%s]"
    (String.concat " " (List.map (fun (v, _, _) -> v) changes));
  changes = []

let eval_string gt switch =
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
          (OpamFile.Config.switch gt.config >>| OpamSwitch.to_string)
        )
      in
      if Some sw_cur <> sw_env then Printf.sprintf " --switch=%s" sw_cur
      else ""
  in
  match OpamStd.Sys.guess_shell_compat () with
  | `fish ->
    Printf.sprintf "eval (opam config env%s%s)" root switch
  | _ ->
    Printf.sprintf "eval `opam config env%s%s`" root switch



(* -- Shell and init scripts handling -- *)

let switch_eval_sh = "switch_eval.sh"
let complete_sh    = "complete.sh"
let complete_zsh   = "complete.zsh"
let variables_sh   = "variables.sh"
let variables_csh  = "variables.csh"
let variables_fish = "variables.fish"
let init_sh        = "init.sh"
let init_zsh       = "init.zsh"
let init_csh       = "init.csh"
let init_fish      = "init.fish"
let init_file = function
  | `sh   -> init_sh
  | `csh  -> init_csh
  | `zsh  -> init_zsh
  | `bash -> init_sh
  | `fish -> init_fish

let source root ~shell ?(interactive_only=false) f =
  let file f = OpamFilename.to_string (OpamPath.init root // f) in
  let s =
    match shell with
    | `csh ->
      Printf.sprintf "source %s >& /dev/null || true\n" (file f)
    | `fish ->
      Printf.sprintf "source %s > /dev/null 2> /dev/null; or true\n" (file f)
    | _ ->
      Printf.sprintf "test -x %s && . %s > /dev/null 2> /dev/null || true\n"
        (file f) (file f)
  in
  if interactive_only then
    match shell with
    | `csh ->
      Printf.sprintf "if (tty -s >&/dev/null) then\n  %sendif\n" s
    | `fish ->
      Printf.sprintf "if tty -s >/dev/null 2>&1\n %send\n" s
    | _ ->
      Printf.sprintf "if tty -s >/dev/null 2>&1; then\n  %sfi\n" s
  else s

let string_of_update st shell updates =
  let fenv = OpamPackageVar.resolve st in
  let make_comment comment_opt =
    OpamStd.Option.to_string (Printf.sprintf "# %s\n") comment_opt
  in
  let sh   (k,v,comment) =
    Printf.sprintf "%s%s=%S; export %s;\n"
      (make_comment comment) k v k in
  let csh  (k,v,comment) =
    Printf.sprintf "%sif ( ! ${?%s} ) setenv %s \"\"\nsetenv %s %S\n"
      (make_comment comment) k k k v in
  let fish (k,v,comment) =
    (* Fish converts some colon-separated vars to arrays, which have to be treated differently.
     * Opam only changes PATH and MANPATH but we handle CDPATH for completeness. *)
    let fish_array_vars = ["PATH"; "MANPATH"; "CDPATH"] in
    let fish_array_derefs = List.map (fun s -> "$" ^ s) fish_array_vars in
    if not (List.mem k fish_array_vars) then
      (* Regular string variables *)
      Printf.sprintf "%sset -gx %s %S;\n"
        (make_comment comment) k v
    else
      (* The MANPATH and CDPATH have default "values" if they are unset and we
       * must be sure that we preserve these defaults when "appending" to them.
       * This because Fish has trouble dealing with the case where we want to
       * have a colon at the start or at the end of the string that gets exported.
       *  - MANPATH: ""  (default system manpages)
       *  - CDPATH:  "." (current directory) *)
      let init_array = match k with
        | "PATH"    -> "" (* PATH is always set *)
        | "MANPATH" -> "if [ 0 -eq (count $MANPATH) ]; set -gx MANPATH \"\"; end;\n"
        | "CDPATH"  -> "if [ 0 -eq (count $CDPATH) ]; set -gx CDPATH \".\"; end;\n"
        | _         -> assert false in
      (* Opam assumes that `v` is a string with colons in the middle so we have
       * to convert that to an array assignment that fish understands.
       * We also have to pay attention so we don't quote array expansions - that
       * would replace some colons by spaces in the exported string *)
      let vs = OpamStd.String.split_delim v ':' in
      let to_arr_element v =
        if List.mem v fish_array_derefs then v else Printf.sprintf "%S" v in
      let set_array =
        Printf.sprintf "%sset -gx %s %s;\n"
          (make_comment comment)
          k (OpamStd.List.concat_map " " to_arr_element vs) in
      (init_array ^ set_array) in
  let export = match shell with
    | `zsh | `sh  -> sh
    | `fish -> fish
    | `csh -> csh in
  let aux (ident, symbol, string, comment) =
    let string = OpamFilter.expand_string fenv string in
    let key, value = match symbol with
      | Eq  -> ident, string
      | PlusEq | ColonEq -> ident, Printf.sprintf "%s:$%s" string ident
      | EqColon | EqPlus ->
        ident, (match shell with `csh -> Printf.sprintf "${%s}:%s" ident string
                               | _ -> Printf.sprintf "$%s:%s" ident string)
      | EqPlusEq -> ident, Printf.sprintf "%s:$%s" string ident
    in
    export (key, value, comment) in
  OpamStd.List.concat_map "" aux updates

let init_script root ~switch_eval ~completion ~shell
    (variables_sh, switch_eval_sh, complete_sh) =
  let variables =
    Some (source root ~shell variables_sh) in
  let switch_eval =
    if switch_eval then
      OpamStd.Option.map (source root ~shell ~interactive_only:true)
        switch_eval_sh
    else
      None in
  let complete =
    if completion then
      OpamStd.Option.map (source root ~shell ~interactive_only:true) complete_sh
    else
      None in
  let buf = Buffer.create 128 in
  let append name = function
    | None   -> ()
    | Some c ->
      Printf.bprintf buf "# %s\n%s\n" name c in
  append "Load the environment variables" variables;
  append "Load the auto-complete scripts" complete;
  append "Load the opam-switch-eval script" switch_eval;
  Buffer.contents buf

let write_script root (name, body) =
  let file = OpamPath.init root // name in
  try OpamFilename.write file body
  with e ->
    OpamStd.Exn.fatal e;
    OpamConsole.error "Could not write %s" (OpamFilename.to_string file)

let write_static_init_scripts root ~switch_eval ~completion =
  let scripts =
    List.map (fun (shell, init, scripts) ->
        init, init_script root ~shell ~switch_eval ~completion scripts) [
      `sh, init_sh, (variables_sh, Some switch_eval_sh, Some complete_sh);
      `zsh, init_zsh, (variables_sh, Some switch_eval_sh, Some complete_zsh);
      `csh, init_csh, (variables_csh, None, None);
      `fish, init_fish, (variables_fish, None, None);
    ] @ [
      complete_sh, OpamScript.complete;
      complete_zsh, OpamScript.complete_zsh;
      switch_eval_sh, OpamScript.switch_eval;
    ]
  in
  List.iter (write_script root) scripts

let write_dynamic_init_scripts st =
  let updates = updates ~opamswitch:false st in
  try
    OpamFilename.with_flock_upgrade `Lock_write ~dontblock:true
      st.switch_global.global_lock
    @@ fun () ->
    List.iter (write_script st.switch_global.root) [
      variables_sh, string_of_update st `sh updates;
      variables_csh, string_of_update st `csh updates;
      variables_fish, string_of_update st `fish updates;
    ]
  with OpamSystem.Locked ->
    OpamConsole.warning
      "Global shell init scripts not installed (could not acquire lock)"

let status_of_init_file root init_sh =
  let init_sh = OpamPath.init root // init_sh in
  if OpamFilename.exists init_sh then (
    let init = OpamFilename.read init_sh in
    if OpamFilename.exists init_sh then
      let complete_sh = OpamStd.String.contains ~sub:complete_sh init in
      let complete_zsh = OpamStd.String.contains ~sub:complete_zsh init in
      let switch_eval_sh = OpamStd.String.contains ~sub:switch_eval_sh init in
      Some (complete_sh, complete_zsh, switch_eval_sh)
    else
      None
  ) else
    None

let dot_profile_needs_update root dot_profile =
  if not (OpamFilename.exists dot_profile) then `yes else
  let body = OpamFilename.read dot_profile in
  let pattern1 = "opam config env" in
  let pattern2 = OpamFilename.to_string (OpamPath.init root // "init") in
  let pattern3 =
    OpamStd.String.remove_prefix ~prefix:(OpamFilename.Dir.to_string root)
      pattern2
  in
  let uncommented_re patts =
    Re.(compile (seq [bol; rep (diff any (set "#:"));
                      alt (List.map str patts)]))
  in
  if Re.execp (uncommented_re [pattern1; pattern2]) body then `no
  else if Re.execp (uncommented_re [pattern3]) body then `otherroot
  else `yes

let update_dot_profile root dot_profile shell =
  let pretty_dot_profile = OpamFilename.prettify dot_profile in
  match dot_profile_needs_update root dot_profile with
  | `no        -> OpamConsole.msg "  %s is already up-to-date.\n" pretty_dot_profile
  | `otherroot ->
    OpamConsole.msg
      "  %s is already configured for another OPAM root.\n"
      pretty_dot_profile
  | `yes       ->
    let init_file = init_file shell in
    let body =
      if OpamFilename.exists dot_profile then
        OpamFilename.read dot_profile
      else
        "" in
    OpamConsole.msg "  Updating %s.\n" pretty_dot_profile;
    let body =
      Printf.sprintf
        "%s\n\n\
         # OPAM configuration\n\
         %s"
        (OpamStd.String.strip body) (source root ~shell init_file) in
    OpamFilename.write dot_profile body

(* A little bit of remaining OCaml specific stuff. Can we find another way ? *)
let ocamlinit () =
  try
    let file = Filename.concat (OpamStd.Env.get "HOME") ".ocamlinit" in
    Some (OpamFilename.of_string file)
  with Not_found ->
    None

let ocamlinit_needs_update () =
  match ocamlinit () with
  | None      -> true
  | Some file ->
    if OpamFilename.exists file then (
      let body = OpamFilename.read file in
      let sub = "OCAML_TOPLEVEL_PATH" in
      not (OpamStd.String.contains ~sub body)
    ) else
      true

let update_ocamlinit () =
  if ocamlinit_needs_update () then (
    match ocamlinit () with
    | None      -> ()
    | Some file ->
      let body =
        if not (OpamFilename.exists file) then ""
        else OpamFilename.read file in
      if body = "" then
        OpamConsole.msg "  Generating ~/.ocamlinit.\n"
      else
        OpamConsole.msg "  Updating ~/.ocamlinit.\n";
      try
        let header =
          "(* Added by OPAM. *)\n\
           let () =\n\
          \  try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")\n\
          \  with Not_found -> ()\n\
           ;;\n\n" in
        let oc = open_out_bin (OpamFilename.to_string file) in
        output_string oc (header ^ body);
        close_out oc;
      with e ->
        OpamStd.Exn.fatal e;
        OpamSystem.internal_error "Cannot write ~/.ocamlinit."
  ) else
    OpamConsole.msg "  ~/.ocamlinit is already up-to-date.\n"

let update_user_setup root ~ocamlinit ?dot_profile shell =
  if ocamlinit || dot_profile <> None then (
    OpamConsole.msg "User configuration:\n";
    if ocamlinit then update_ocamlinit ();
    OpamStd.Option.iter (fun f -> update_dot_profile root f shell) dot_profile
  )

let display_setup root ~dot_profile shell =
  let print (k,v) = OpamConsole.msg "  %-25s - %s\n" k v in
  let not_set = "not set" in
  let ok      = "string is already present so file unchanged" in
  let error   = "error" in
  let user_setup =
    let ocamlinit_status =
      if ocamlinit_needs_update () then not_set else ok in
    let dot_profile_status =
      match dot_profile_needs_update root dot_profile with
      | `no        -> ok
      | `yes       -> not_set
      | `otherroot -> error in
    [ ("~/.ocamlinit"                   , ocamlinit_status);
      (OpamFilename.prettify dot_profile, dot_profile_status); ]
  in
  let init_file = init_file shell in
  let pretty_init_file = OpamFilename.prettify (OpamPath.init root // init_file) in
  let global_setup =
    match status_of_init_file root init_file with
    | None -> [pretty_init_file, not_set ]
    | Some(complete_sh, complete_zsh, switch_eval_sh) ->
      let completion =
        if not complete_sh
        && not complete_zsh then
          not_set
        else ok in
      let switch_eval =
        if switch_eval_sh then
          ok
        else
          not_set in
      [ ("init-script"     , Printf.sprintf "%s" pretty_init_file);
        ("auto-completion" , completion);
        ("opam-switch-eval", switch_eval);
      ]
  in
  OpamConsole.msg "User configuration:\n";
  List.iter print user_setup;
  OpamConsole.msg "Global configuration:\n";
  List.iter print global_setup

let print_env_warning_at_init gt ~ocamlinit ?dot_profile shell =
  let profile_string = match dot_profile with
    | None -> ""
    | Some f ->
      Printf.sprintf
        "%s To correctly configure OPAM for subsequent use, add the following\n\
        \   line to your profile file (for instance %s):\n\
         \n\
        \      %s\n"
        (OpamConsole.colorise `yellow "2.")
        (OpamFilename.prettify f)
        (source gt.root ~shell (init_file shell))
  in
  let ocamlinit_string =
    if not ocamlinit then "" else
      OpamConsole.colorise `yellow "3." ^
      " To avoid issues related to non-system installations of `ocamlfind`\n\
      \   add the following lines to ~/.ocamlinit (create it if necessary):\n\
       \n\
      \      let () =\n\
      \        try Topdirs.dir_directory (Sys.getenv \"OCAML_TOPLEVEL_PATH\")\n\
      \        with Not_found -> ()\n\
      \      ;;\n\n"
  in
  let line =
    OpamConsole.colorise `cyan
      "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\
       =-=-="
  in
  OpamConsole.msg
    "\n%s\n\n\
     %s To configure OPAM in the current shell session, you need to run:\n\
     \n\
    \      %s\n\
     \n\
     %s%s%s\n\n"
    line
    (OpamConsole.colorise `yellow "1.")
    (eval_string gt None)
    profile_string ocamlinit_string
    line

let check_and_print_env_warning st =
  if (OpamSwitchState.is_switch_globally_set st ||
      OpamStateConfig.(!r.switch_from <> `Command_line)) &&
     not (is_up_to_date st) then
    OpamConsole.formatted_msg
      "# Run %s to update the current shell environment\n"
      (OpamConsole.colorise `bold (eval_string st.switch_global
                                     (Some st.switch)))

let setup_interactive root ~dot_profile shell =
  let update dot_profile =
    OpamConsole.msg "\n";
    update_user_setup root ~ocamlinit:(dot_profile <> None) ?dot_profile shell;
    write_static_init_scripts root ~switch_eval:true ~completion:true;
    dot_profile <> None in

  OpamConsole.msg "\n";

  match OpamConsole.read
      "In normal operation, OPAM only alters files within ~/.opam.\n\
       \n\
       During this initialisation, you can allow OPAM to add information to two\n\
       other files for best results. You can also make these additions manually\n\
       if you wish.\n\
       \n\
       If you agree, OPAM will modify:\n\n\
      \  - %s (or a file you specify) to set the right environment\n\
      \    variables and to load the auto-completion scripts for your shell (%s)\n\
      \    on startup. Specifically, it checks for and appends the following line:\n\
      \n\
      \    %s\n\
      \n\
      \  - %s to ensure that non-system installations of `ocamlfind`\n\
      \    (i.e. those installed by OPAM) will work correctly when running the\n\
      \    OCaml toplevel. It does this by adding $OCAML_TOPLEVEL_PATH to the list\n\
      \    of include directories.\n\
      \n\
       If you choose to not configure your system now, you can either configure\n\
       OPAM manually (instructions will be displayed) or launch the automatic setup\n\
       later by running:\n\
      \n\
       \   opam config setup -a\n\
       \n\
      \n\
       Do you want OPAM to modify %s and ~/.ocamlinit?\n\
       (default is 'no', use 'f' to name a file other than %s)\n\
      \    [N/y/f]"
      (OpamConsole.colorise `cyan @@ OpamFilename.prettify dot_profile)
      (OpamConsole.colorise `bold @@ string_of_shell shell)
      (source root ~shell (init_file shell))
      (OpamConsole.colorise `cyan @@ "~/.ocamlinit")
      (OpamFilename.prettify dot_profile)
      (OpamFilename.prettify dot_profile)
  with
  | Some ("y" | "Y" | "yes"  | "YES" ) -> update (Some dot_profile)
  | Some ("f" | "F" | "file" | "FILE") ->
    begin match OpamConsole.read "  Enter the name of the file to update:" with
      | None   ->
        OpamConsole.msg "-- No filename: skipping the auto-configuration step --\n";
        false
      | Some f -> update (Some (OpamFilename.of_string f))
    end
  | _ -> update None
