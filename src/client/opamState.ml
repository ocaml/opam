(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open OpamTypes
open OpamFilename.OP
open OpamMisc.OP

let log fmt =
  OpamGlobals.log "STATE" fmt

let () =
  OpamHTTP.register ();
  OpamGit.register ();
  OpamDarcs.register();
  OpamLocal.register ()

let confirm fmt =
  Printf.ksprintf (fun msg ->
    OpamGlobals.msg "%s [Y/n] %!" msg;
    if not !OpamGlobals.yes then
      match read_line () with
      | "y" | "Y"
      | "" -> true
      | _  -> false
    else
      true
  ) fmt

let check f =
  let root = OpamPath.default () in
  let with_switch_lock a f =
    OpamFilename.with_flock (OpamPath.Switch.lock root a) f in
  let error () =
    OpamGlobals.error_and_exit
      "Cannot find %s. Have you run 'opam init' first ?"
      (OpamFilename.Dir.to_string root) in

  if not (OpamFilename.exists_dir root) then
    error ()

  else match f with

    | Global_lock f ->
      (* Take the global lock *)
      OpamFilename.with_flock (OpamPath.lock root) (fun () ->
        (* Take all the switch locks *)
        let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
        let f =
          OpamSwitch.Map.fold (fun a _ f ->
            if OpamFilename.exists_dir (OpamPath.Switch.root root a)
            then with_switch_lock a (fun () -> f ())
            else f
          ) aliases f in
        f ()
      ) ()

    | Read_lock f ->
      (* Simply check that OPAM is correctly initialized *)
      if OpamFilename.exists_dir (OpamPath.root root) then
        f ()
      else
       error ()

    | Switch_lock f ->
      (* Take a switch lock (and check that the global lock is free). *)
      let switch =
        OpamFilename.with_flock
          (OpamPath.lock root)
          (fun () -> match !OpamGlobals.switch with
          | None   -> OpamFile.Config.switch (OpamFile.Config.read (OpamPath.config root))
          | Some a -> OpamSwitch.of_string a)
          () in
      (* XXX: We can have a small race just here ... *)
      with_switch_lock switch f ()

type state = {
  root: OpamPath.t;
  switch: switch;
  compiler: compiler;
  compiler_version: compiler_version;
  opams: OpamFile.OPAM.t package_map;
  repositories: OpamFile.Repo_config.t repository_name_map;
  packages: package_set;
  available_packages: package_set Lazy.t;
  aliases: OpamFile.Aliases.t;
  pinned: OpamFile.Pinned.t;
  installed: OpamFile.Installed.t;
  reinstall: OpamFile.Reinstall.t;
  config: OpamFile.Config.t;
  repo_index: OpamFile.Repo_index.t;
}

let universe t action = {
  u_action    = action;
  u_installed = t.installed;
  u_available = Lazy.force t.available_packages;
  u_depends   = OpamPackage.Map.map OpamFile.OPAM.depends t.opams;
  u_depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts t.opams;
  u_conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts t.opams;
}

let string_of_repositories r =
  OpamMisc.string_of_list
    OpamRepositoryName.to_string
    (OpamRepositoryName.Map.keys r)

let print_state t =
  log "ROOT      : %s" (OpamFilename.Dir.to_string (OpamPath.root t.root));
  log "SWITCH     : %s" (OpamSwitch.to_string t.switch);
  log "COMPILER  : %s" (OpamCompiler.to_string t.compiler);
  log "REPOS     : %s" (string_of_repositories t.repositories);
  if !OpamGlobals.debug then
    log "AVAILABLE : %s" (OpamPackage.Set.to_string (Lazy.force t.available_packages))
  else
    log "PACKAGES  : %s" (OpamPackage.Set.to_string t.packages);
  log "INSTALLED : %s" (OpamPackage.Set.to_string t.installed);
  log "REINSTALL : %s" (OpamPackage.Set.to_string t.reinstall)

let compiler_of_switch t switch =
  try Some (OpamSwitch.Map.find switch t.aliases)
  with Not_found -> None

let config t =
  OpamFile.Config.read (OpamPath.config t.root)

let compilers t =
  OpamCompiler.list (OpamPath.compilers_dir t.root)

let repositories t =
  OpamFile.Config.repositories t.config

let opam t nv =
  OpamPackage.Map.find nv t.opams

let archives t =
  OpamFilename.Set.of_list (OpamFilename.list_files (OpamPath.archives_dir t.root))

let compiler t c =
  OpamFile.Comp.safe_read (OpamPath.compiler t.root c)

let mem_installed_package_by_name_aux installed name =
  let set = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) installed in
  not (OpamPackage.Set.is_empty set)

let mem_installed_package_by_name t name =
  mem_installed_package_by_name_aux t.installed name

let find_installed_package_by_name_aux installed name =
  try OpamPackage.Set.find (fun nv -> OpamPackage.name nv = name) installed
  with Not_found ->
    OpamGlobals.error_and_exit "Package %s is not installed" (OpamPackage.Name.to_string name)

let find_installed_package_by_name t name =
  find_installed_package_by_name_aux t.installed name

let find_packages_by_name t name =
  let r = OpamPackage.Set.filter (fun nv -> OpamPackage.name nv = name) t.packages in
  if OpamPackage.Set.is_empty r then
    None
  else
    Some r

let installed_map t =
  OpamPackage.Name.Map.map OpamPackage.Version.Set.choose_one (OpamPackage.to_map t.installed)

let dot_config t nv =
  OpamFile.Dot_config.safe_read (OpamPath.Switch.config t.root t.switch nv)

let reinstall t =
  OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall t.root t.switch)

let mem_repository_name t name =
  OpamRepositoryName.Map.exists (fun n _ -> n = name) t.repositories

let find_repository_name t name =
  OpamRepositoryName.Map.find name t.repositories

let find_repository_aux repositories root repo_index nv =
  log "find_repository %s" (OpamPackage.to_string nv);
  let name = OpamPackage.name nv in
  let rec aux = function
    | []          -> None
    | r :: repo_s ->
        let repo = OpamRepositoryName.Map.find r repositories in
        let repo_p = OpamPath.Repository.create root r in
        let opam_f = OpamPath.Repository.opam repo_p nv in
        if OpamFilename.exists opam_f then (
          Some (repo_p, repo)
        ) else
          aux repo_s in
  if OpamPackage.Name.Map.mem name repo_index then
    aux (OpamPackage.Name.Map.find name repo_index)
  else
    None

let find_repository t nv =
  find_repository_aux t.repositories t.root t.repo_index nv

let mem_repository t nv =
  find_repository t nv <> None

let with_repository t nv fn =
  match find_repository t nv with
  | None ->
    OpamGlobals.error_and_exit
      "Unable to find a repository containing %s"
      (OpamPackage.to_string nv)
  | Some (repo_p, repo) -> fn repo_p repo

  (* List the packages which does fullfil the compiler constraints *)
let available_packages root opams repositories repo_index compiler_version config pinned packages =
  let filter nv =
    let opam = OpamPackage.Map.find nv opams in
    let available () =
      find_repository_aux repositories root repo_index nv <> None in
    let consistent_ocaml_version () =
      let atom (r,v) = OpamCompiler.Version.compare compiler_version r v in
      match OpamFile.OPAM.ocaml_version opam with
      | None   -> true
      | Some c -> OpamFormula.eval atom c in
    let consistent_os () =
      match OpamFile.OPAM.os opam with
      | [] -> true
      | l  ->
        List.fold_left (fun accu (b,os) ->
          let ($) = if b then (=) else (<>) in
          accu || (os $ Lazy.force OpamGlobals.os_string)
        ) false l in
    let consistent_pinned_version () =
      not (OpamPackage.Name.Map.mem (OpamPackage.name nv) pinned) ||
        match OpamPackage.Name.Map.find (OpamPackage.name nv) pinned with
        | Version v -> v = OpamPackage.version nv
        | _         -> true (* any version is fine, as this will be overloaded on install *) in
    available ()
    && consistent_ocaml_version ()
    && consistent_pinned_version ()
    && consistent_os () in
  OpamPackage.Set.filter filter packages

let base_packages =
  List.map OpamPackage.Name.of_string [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_system_compiler_description root = function
  | None         -> ()
  | Some version ->
    let comp = OpamPath.compiler root OpamCompiler.default in
    OpamFilename.remove comp;
    let f =
      OpamFile.Comp.create_preinstalled
        OpamCompiler.default version
        (if not !OpamGlobals.no_base_packages then base_packages else [])
        [ ("CAML_LD_LIBRARY_PATH", "=",
           "%{lib}%/stublibs"
           ^ ":" ^
             (match Lazy.force OpamSystem.system_ocamlc_where with
             | Some d -> Filename.concat d "stublibs"
             | None   -> assert false))
        ] in
    OpamFile.Comp.write comp f

let system_needs_upgrade t =
  t.compiler = OpamCompiler.default
  && match OpamCompiler.Version.system () with
  | None   -> OpamGlobals.error_and_exit "No OCaml compiler found in path"
  | Some v -> t.compiler_version <> v

let upgrade_system_compiler =
  ref (fun _ -> assert false)

let load_state () =
  let root = OpamPath.default () in
  log "load_state root=%s" (OpamFilename.Dir.to_string root);

  let config_p = OpamPath.config root in
  let config =
    let config = OpamFile.Config.read config_p in
    if OpamFile.Config.opam_version config <> OpamVersion.current then
      (* opam has been updated, so refresh the configuration file *)
      let config = OpamFile.Config.with_current_opam_version config in
      OpamFile.Config.write config_p config;
      config
    else
      config in

  let switch = match !OpamGlobals.switch with
    | None   -> OpamFile.Config.switch config
    | Some a -> OpamSwitch.of_string a in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch, compiler =
    try switch, OpamSwitch.Map.find switch aliases
    with Not_found ->
      log "%S does not contain the compiler name associated to the switch %s"
        (OpamFilename.to_string (OpamPath.aliases root))
        (OpamSwitch.to_string switch);
      if OpamSwitch.Map.cardinal aliases > 0 then (
        let new_switch, new_compiler = OpamSwitch.Map.choose aliases in
        OpamGlobals.error "The current switch (%s) is an unknown compiler switch. Switching back to %s ..."
          (OpamSwitch.to_string switch)
          (OpamSwitch.to_string new_switch);
        let config = OpamFile.Config.with_switch config new_switch in
        OpamFile.Config.write config_p config;
        new_switch, new_compiler;
      ) else
        OpamGlobals.error_and_exit
          "The current switch (%s) is an unknown compiler switch."
          (OpamSwitch.to_string switch) in

  let compiler_version =
    let comp = OpamFile.Comp.read (OpamPath.compiler root compiler) in
    OpamFile.Comp.version comp in
  let opams =
    OpamPackage.Set.fold (fun nv map ->
      try
        let opam = OpamFile.OPAM.read (OpamPath.opam root nv) in
        OpamPackage.Map.add nv opam map
      with _ ->
        map
    ) (OpamPackage.list (OpamPath.opam_dir root)) OpamPackage.Map.empty in
  let repositories =
    List.fold_left (fun map repo ->
      let repo_p = OpamPath.Repository.create root repo in
      let config = OpamFile.Repo_config.read (OpamPath.Repository.config repo_p) in
      OpamRepositoryName.Map.add repo config map
    ) OpamRepositoryName.Map.empty (OpamFile.Config.repositories config) in
  let repo_index = OpamFile.Repo_index.safe_read (OpamPath.repo_index root) in
  let pinned = OpamFile.Pinned.safe_read (OpamPath.Switch.pinned root switch) in
  let installed = OpamFile.Installed.safe_read (OpamPath.Switch.installed root switch) in
  let reinstall = OpamFile.Reinstall.safe_read (OpamPath.Switch.reinstall root switch) in
  let packages = OpamPackage.list (OpamPath.opam_dir root) in
  let available_packages =
    lazy (available_packages root opams repositories repo_index compiler_version config pinned packages) in
  let t = {
    root; switch; compiler; compiler_version; repositories; opams;
    packages; available_packages; installed; reinstall;
    repo_index; config; aliases; pinned;
  } in
  print_state t;
  (* update from opam 0.7 to 0.8: Remove spurious conf-ocaml packages *)
  if mem_installed_package_by_name t OpamPackage.Name.default then
    OpamFile.Installed.write (OpamPath.Switch.installed t.root t.switch)
      (OpamPackage.Set.filter (fun nv -> OpamPackage.name nv <> OpamPackage.Name.default) t.installed);
  (match find_packages_by_name t OpamPackage.Name.default with
  | None          -> ()
  | Some packages ->
    OpamPackage.Set.iter (fun nv ->
      OpamFilename.remove (OpamPath.opam t.root nv);
      OpamFilename.remove (OpamPath.descr t.root nv);
    ) packages);
  (* Check whether the system compiler has been updated *)
  if system_needs_upgrade t then (
    !upgrade_system_compiler t;
    OpamGlobals.exit 0
  ) else
    t

(* Return the contents of a fully qualified variable *)
let contents_of_variable t v =
  let name = OpamVariable.Full.package v in
  let var = OpamVariable.Full.variable v in
  let var_str = OpamVariable.to_string var in
  let read_var name =
    let c = dot_config t name in
    try match OpamVariable.Full.section v with
      | None   -> OpamFile.Dot_config.variable c var
      | Some s -> OpamFile.Dot_config.Section.variable c s var
    with Not_found ->
      OpamGlobals.error_and_exit "%s is not defined" (OpamVariable.Full.to_string v) in
  if name = OpamPackage.Name.default then (
    try S (OpamMisc.getenv var_str)
    with Not_found ->
      if var_str = "ocaml-version" then
        S (OpamCompiler.Version.to_string t.compiler_version)
      else if var_str = "preinstalled" then
        B (OpamFile.Comp.preinstalled (compiler t t.compiler))
      else
        read_var name
  ) else (
    let process_one name =
      let name_str = OpamPackage.Name.to_string name in
      try Some (S (OpamMisc.getenv (name_str ^"_"^ var_str)))
      with Not_found ->
        let installed = mem_installed_package_by_name t name in
        if var = OpamVariable.enable && installed then
          Some (S "enable")
        else if var = OpamVariable.enable && not installed then
          Some (S "disable")
        else if var = OpamVariable.installed then
          Some (B installed)
        else if not installed then
          None
        else
          Some (read_var name) in
    match process_one name with
    | Some r -> r
    | None   ->
      let name_str = OpamPackage.Name.to_string name in
      let names = OpamMisc.split name_str '+' in
      if List.length names = 1 then
        OpamGlobals.error_and_exit "Package %s is not installed" name_str;
      let names = List.map OpamPackage.Name.of_string names in
      let results =
        List.map (fun name ->
          match process_one name with
          | None   -> OpamGlobals.error_and_exit "Package %s is not installed" (OpamPackage.Name.to_string name)
          | Some r -> r
        ) names in
      let rec compose x y = match x,y with
        | S "enable" , S "enable"  -> S "enable"
        | S "disable", S "enable"
        | S "enable" , S "disable"
        | S "disable", S "disable" -> S "disable"
        | B b1       , B b2        -> B (b1 && b2)
        | S b, r     | r, S b      ->
          if b = "true" then compose (B true) r
          else if b = "false" then compose (B false) r
          else
            OpamGlobals.error_and_exit
              "Cannot compose %s and %s"
              (OpamVariable.string_of_variable_contents x)
              (OpamVariable.string_of_variable_contents y) in
      match results with
      | [] | [_] -> assert false
      | h::t     -> List.fold_left compose h t
  )

let substitute_ident t i =
  let v = OpamVariable.Full.of_string i in
  let c = contents_of_variable t v in
  OpamVariable.string_of_variable_contents c

(* Substitute the file contents *)
let substitute_file t f =
  let f = OpamFilename.of_basename f in
  let src = OpamFilename.add_extension f "in" in
  let contents = OpamFile.Subst.read src in
  let newcontents = OpamFile.Subst.replace contents (contents_of_variable t) in
  OpamFile.Subst.write f newcontents

(* Substitue the string contents *)
let substitute_string t s =
  OpamFile.Subst.replace_string s (contents_of_variable t)

let rec substitute_filter t = function
  | FBool b    -> FBool b
  | FString s  -> FString (substitute_string t s)
  | FIdent s   -> FString (substitute_ident t s)
  | FOp(e,s,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    FOp(e, s, f)
  | FAnd (e,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    FAnd(e,f)
  | FOr(e,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    FOr(e,f)

let substitute_arg t (a, f) =
  let a = match a with
    | CString s -> CString (substitute_string t s)
    | CIdent  i -> CString (substitute_ident t i) in
  let f = match f with
    | None   -> None
    | Some f -> Some (substitute_filter t f) in
  (a, f)

let substitute_command t (l, f) =
  let l = List.map (substitute_arg t) l in
  let f = match f with
    | None   -> None
    | Some f -> Some (substitute_filter t f) in
  (l, f)

let substitute_commands t c =
  List.map (substitute_command t) c

let rec eval_filter t = function
  | FBool b    -> string_of_bool b
  | FString s  -> substitute_string t s
  | FIdent s   -> substitute_string t s
  | FOp(e,s,f) ->
    (* We are supposed to compare version strings *)
    let s = match s with
      | Eq  -> (=)
      | Neq -> (<>)
      | Ge  -> (fun a b -> Debian.Version.compare a b >= 0)
      | Le  -> (fun a b -> Debian.Version.compare a b <= 0)
      | Gt  -> (fun a b -> Debian.Version.compare a b >  0)
      | Lt  -> (fun a b -> Debian.Version.compare a b <  0) in
    let e = eval_filter t e in
    let f = eval_filter t f in
    if s e f then "true" else "false"
  | FOr(e,f)  ->
    if eval_filter t e = "true"
    || eval_filter t f = "true"
    then "true" else "false"
  | FAnd(e,f) ->
    if eval_filter t e = "true"
    && eval_filter t f = "true"
    then "true" else "false"

let eval_filter t = function
  | None   -> true
  | Some f -> eval_filter t f = "true"

let filter_arg t (a,f) =
  if eval_filter t f then
    match a with
    | CString s -> Some (substitute_string t s)
    | CIdent i  -> Some (substitute_ident t i)
  else
    None

let filter_command t (l, f) =
  if eval_filter t f then
    match OpamMisc.filter_map (filter_arg t) l with
    | [] -> None
    | l  -> Some l
  else
    None

let filter_commands t l =
  OpamMisc.filter_map (filter_command t) l

let expand_env t env =
  List.map (fun (ident, symbol, string) ->
    let string = substitute_string t string in
    let read_env () =
      let prefix = OpamFilename.Dir.to_string t.root in
      try OpamMisc.reset_env_value ~prefix (OpamMisc.getenv ident)
      with _ -> [] in
    match symbol with
    | "="  -> (ident, string)
    | "+=" -> (ident, String.concat ":" (string :: read_env ()))
    | "=+" -> (ident, String.concat ":" (read_env () @ [string]))
    | ":=" -> (ident, string ^":"^ (String.concat ":" (read_env())))
    | "=:" -> (ident, (String.concat ":" (read_env())) ^":"^ string)
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let update_env t env e =
  let expanded = expand_env t e in
  { env with
    add_to_env = expanded @ env.add_to_env;
    new_env    = expanded @ env.new_env }

let get_env t =
  let comp = compiler t t.compiler in

  let add_to_path = OpamPath.Switch.bin t.root t.switch in
  let new_path = "PATH", "+=", OpamFilename.Dir.to_string add_to_path in

  let add_to_env = OpamFile.Comp.env comp in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=", OpamFilename.Dir.to_string (OpamPath.Switch.toplevel t.root t.switch) in
  let man_path =
    "MANPATH", ":=", OpamFilename.Dir.to_string (OpamPath.Switch.man_dir t.root t.switch) in
  let new_env = new_path :: man_path :: toplevel_dir :: add_to_env in

  let add_to_env = expand_env t add_to_env in
  let new_env = expand_env t new_env in

  (* if --root <dir> is passed on the command line, or if OPAMROOT is set. *)
  let new_env =
    if !OpamGlobals.root_dir <> OpamGlobals.default_opam_dir then
      ("OPAMROOT", !OpamGlobals.root_dir) :: new_env
    else
      new_env in

  { add_to_env; add_to_path; new_env }

let print_env_warning ?(add_profile = false) t =
  match
    List.filter
      (fun (s, v) ->
        Some v <> try Some (OpamMisc.getenv s) with _ -> None)
      (get_env t).new_env
  with
    | [] -> () (* every variables are correctly set *)
    | l ->
      let which_opam =
        if add_profile then
          "which opam && "
        else
          "" in
      let add_profile =
        if add_profile then
          "\nand add this in your ~/.profile"
        else
          "" in
      let opam_root =
        (if !OpamGlobals.root_dir = OpamGlobals.default_opam_dir then
            ""
         else
            Printf.sprintf " --root %s" !OpamGlobals.root_dir) in
      let variables = String.concat ", " (List.map (fun (s, _) -> "$" ^ s) l) in
      OpamGlobals.msg "\nTo update %s; you can now run:
            \n\    $ %seval `opam%s config env`\n%s\n"
        variables
        which_opam
        opam_root
        add_profile

let get_compiler_packages t comp =
  let comp = compiler t comp in
  let available = OpamPackage.to_map (Lazy.force t.available_packages) in

  let pkg_available, pkg_not =
    List.partition
      (fun (n, _) -> OpamPackage.Name.Map.mem n available)
      (OpamFormula.atoms (OpamFile.Comp.packages comp)) in

  (* check that all packages in [comp] are in [available] except for
     "base-..."  (depending if "-no-base-packages" is set or not) *)
  let pkg_not = List.rev_map (function (n, _) -> n) pkg_not in
  let pkg_not =
    if not !OpamGlobals.no_base_packages then
      pkg_not
    else
      List.filter (fun n -> not (List.mem n base_packages)) pkg_not in
  if pkg_not <> [] then (
    List.iter (OpamPackage.Name.to_string |> OpamGlobals.error "Package %s not found") pkg_not;
    OpamGlobals.exit 2
  );

  pkg_available

let add_switch root switch compiler =
  log "add_switch switch=%s compiler=%s" (OpamSwitch.to_string switch) (OpamCompiler.to_string compiler);
  let aliases_f = OpamPath.aliases root in
  let aliases = OpamFile.Aliases.safe_read aliases_f in
  if not (OpamSwitch.Map.mem switch aliases) then begin
    OpamFile.Aliases.write aliases_f (OpamSwitch.Map.add switch compiler aliases);
  end

(* install ~/.opam/<switch>/config/conf-ocaml.config *)
let install_conf_ocaml_config root switch =
  log "install_conf_ocaml_config switch=%s" (OpamSwitch.to_string switch);
  (* .config *)
  let vars =
    let map f l = List.map (fun (s,p) -> OpamVariable.of_string s, S (f p)) l in
    let id x = x in

    map OpamFilename.Dir.to_string
      [
        ("prefix", OpamPath.Switch.root root switch);
        ("lib", OpamPath.Switch.lib_dir root switch);
        ("bin", OpamPath.Switch.bin root switch);
        ("doc", OpamPath.Switch.doc_dir root switch);
        ("stublibs", OpamPath.Switch.stublibs root switch);
        ("toplevel", OpamPath.Switch.toplevel root switch);
        ("man", OpamPath.Switch.man_dir root switch);
        ("share", OpamPath.Switch.share_dir root switch);
      ]
    @ map id [
      ("user" , try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with _ -> "user");
      ("group", try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name with _ -> "group");
      ("make" , Lazy.force !OpamGlobals.makecmd);
      ("os"   , Lazy.force OpamGlobals.os_string);
    ] in

  let config = OpamFile.Dot_config.create vars in
  OpamFile.Dot_config.write (OpamPath.Switch.config root switch OpamPackage.Name.default) config

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[switch]
   - update $opam/switch
   - update $opam/config *)
let install_compiler t ~quiet switch compiler =
  log "install_compiler switch=%s compiler=%s"
    (OpamSwitch.to_string switch)
    (OpamCompiler.to_string compiler);

  let comp_f = OpamPath.compiler t.root compiler in
  if not (OpamFilename.exists comp_f) then (
    OpamGlobals.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (OpamFilename.to_string comp_f)
      (OpamCompiler.to_string compiler);
    OpamGlobals.exit 0;
  );

  let switch_dir = OpamPath.Switch.root t.root switch in

  (* Do some clean-up if necessary *)
  if not (OpamSwitch.Map.mem switch t.aliases) && OpamFilename.exists_dir switch_dir then
    OpamFilename.rmdir switch_dir;

  if OpamFilename.exists_dir switch_dir then (
    OpamGlobals.msg "The compiler %s is already installed.\n" (OpamSwitch.to_string switch);
    OpamGlobals.exit 0;
  );

  (* Create base directories *)
  OpamFilename.mkdir switch_dir;
  OpamFilename.mkdir (OpamPath.Switch.lib_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.stublibs t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.toplevel t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.build_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.bin t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.doc_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.man_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.install_dir t.root switch);
  OpamFilename.mkdir (OpamPath.Switch.config_dir t.root switch);
  List.iter (fun num ->
    OpamFilename.mkdir (OpamPath.Switch.man_dir ~num t.root switch)
  ) ["1";"1M";"2";"3";"4";"5";"6";"7";"9"];

  install_conf_ocaml_config t.root switch;

  let comp = OpamFile.Comp.read comp_f in
  begin try
    if not (OpamFile.Comp.preinstalled comp) then begin

      OpamGlobals.verbose := not quiet;

      (* Install the compiler *)
      let comp_src = match OpamFile.Comp.src comp with
        | Some f -> f
        | None   ->
          OpamGlobals.error_and_exit
            "No source for compiler %s"
            (OpamCompiler.to_string compiler) in
      let build_dir = OpamPath.Switch.build_ocaml t.root switch in
      let comp_src_raw = OpamFilename.to_string comp_src in
      if Sys.file_exists comp_src_raw && Sys.is_directory comp_src_raw then
        OpamFilename.link_dir (OpamFilename.Dir.of_string comp_src_raw) build_dir
      else if Sys.file_exists comp_src_raw then
        OpamFilename.extract comp_src build_dir
      else OpamFilename.with_tmp_dir (fun download_dir ->
        let file = OpamFilename.download ~overwrite:true comp_src download_dir in
        OpamFilename.extract file build_dir;
      );
      let patches = OpamFile.Comp.patches comp in
      let patches = List.map (fun f -> OpamFilename.download ~overwrite:true f build_dir) patches in
      List.iter (fun f -> OpamFilename.patch f build_dir) patches;
      if OpamFile.Comp.configure comp @ OpamFile.Comp.make comp <> [] then begin
        OpamFilename.exec build_dir
          [ ( "./configure" :: OpamFile.Comp.configure comp )
            @ [ "-prefix";  OpamFilename.Dir.to_string switch_dir ]
          (*-bindir %s/bin -libdir %s/lib -mandir %s/man*)
          (* NOTE In case it exists 2 '-prefix', in general the script
             ./configure will only consider the last one, others will be
             discarded. *)
          ; ( Lazy.force !OpamGlobals.makecmd :: OpamFile.Comp.make comp )
          ; [ Lazy.force !OpamGlobals.makecmd ; "install" ]
          ]
      end else begin
        let t = { t with switch } in
        let builds =
          List.map (List.map (substitute_string t)) (OpamFile.Comp.build comp) in
        OpamFilename.exec build_dir builds
      end;
    end;

    (* write the new version in the configuration file *)
    let config = OpamFile.Config.with_switch t.config switch in
    OpamFile.Config.write (OpamPath.config t.root) config;
    add_switch t.root switch compiler

  with e ->
    if not !OpamGlobals.debug then
      OpamFilename.rmdir switch_dir;
    raise e
  end

let update_pinned_package t nv pin =
  match kind_of_pin_option pin with
  | (`git|`darcs|`local as k) ->
    let path = OpamFilename.raw_dir (path_of_pin_option pin) in
    let module B = (val OpamRepository.find_backend k: OpamRepository.BACKEND) in
    let build = OpamPath.Switch.build t.root t.switch nv in
    B.download_dir nv ~dst:build path
  | _ ->
    OpamGlobals.error_and_exit
      "Cannot update the pinned package %s: wrong backend."
      (OpamPackage.to_string nv)

module Types = struct
  type t = state = {
    root: OpamPath.t;
    switch: switch;
    compiler: compiler;
    compiler_version: compiler_version;
    opams: OpamFile.OPAM.t package_map;
    repositories: OpamFile.Repo_config.t repository_name_map;
    packages: package_set;
    available_packages: package_set Lazy.t;
    aliases: OpamFile.Aliases.t;
    pinned: OpamFile.Pinned.t;
    installed: OpamFile.Installed.t;
    reinstall: OpamFile.Reinstall.t;
    config: OpamFile.Config.t;
    repo_index: OpamFile.Repo_index.t;
  }
end
