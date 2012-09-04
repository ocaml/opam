(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Types
open Utils

let log fmt =
  Globals.log "CLIENT" fmt

open Solver

type t = {
  (* ~/.opam/ *)
  global: Path.G.t;

  (* ~/.opam/$oversion/ *)
  compiler: Path.C.t;

  (* ~/.opam/repo/$repo/ *)
  repositories: (repository * Path.R.t) list;

  (* ~/.opam/opam/ files *)
  available: NV.Set.t;

  (* [available] but restricted to packages that are supported by the current version of the OCaml installed *)
  available_current: NV.Set.t option;
    (* WARNING 
       Instead of [option] we can put [NV.Set.t]. 
       However, at the time of writing, the semantic will change and "make tests" will fail. 
       This is because [load_state] calls "ocaml -version" as side effect for example. 

       Note that putting [option] gives a quite optimized data structure now. *)

  (* ~/.opam/aliases *)
  aliases: (Alias.t * OCaml_V.t) list;

  (* ~/.opam/$oversion/pinned *)
  pinned: pin_option N.Map.t;

  (* ~/.opam/$oversion/installed contents *)
  installed: NV.Set.t;

  (* ~/.opam/$oversion/reinstall contents *)
  reinstall: NV.Set.t;

  (* ~/.opam/config contents *)
  config: File.Config.t;

  (* ~/.opam/repo/index contents *)
  repo_index: string list N.Map.t;
}

let print_state t =
  let string_of_repos r =
    let s (r,p) =
      Printf.sprintf "%s:%s"
        (Repository.to_string r)
        (Dirname.to_string (Path.R.root p)) in
    String.concat ", " (List.map s r) in
  log "GLOBAL    : %s" (Dirname.to_string (Path.G.root t.global));
  log "COMPILER  : %s" (Dirname.to_string (Path.C.root t.compiler));
  log "REPO      : %s" (string_of_repos t.repositories);
  log "AVAILABLE : %s" (NV.Set.to_string t.available);
  log "AVAILABLE_CURRENT : %s" (match t.available_current with None -> "<none>" | Some set -> NV.Set.to_string set);
  log "INSTALLED : %s" (NV.Set.to_string t.installed);
  log "REINSTALL : %s" (NV.Set.to_string t.reinstall);
  log "REPO_INDEX: %s" (N.Map.to_string (String.concat ",") t.repo_index)

let current_ocaml_version t =
  let alias = File.Config.ocaml_version t.config in
  let aliases = File.Aliases.read (Path.G.aliases t.global) in
  List.assoc alias aliases

let confirm fmt =
  Printf.kprintf (fun msg ->
    Globals.msg "%s [Y/n] " msg;
    if not !Globals.yes then
      match read_line () with
      | "y" | "Y"
      | "" -> true
      | _  -> false
    else
      true
  ) fmt

let update_available_current t = 
  { t with available_current =     
    (* Remove the package which does not fullfil the compiler constraints *)
    let ocaml_version =
      if File.Config.ocaml_version t.config = Alias.of_string Globals.default_compiler_version then (
        let current = OCaml_V.current () in
        let system = File.Config.system_ocaml_version t.config in
        match current, system  with
        | None  , None   -> Globals.error_and_exit "No OCaml compiler installed."
        | None  , Some s ->
            if not (confirm "No OCaml compiler found. Continue ?") then 
              Globals.exit 1
            else
              s
        | Some c, Some s ->
            if s <> c
            && not (confirm "The version of OCaml in your path (%S) \
                             is not the same as the one OPAM has been \
                             initialized with (%S). Continue ?"
                      (OCaml_V.to_string c)
                      (OCaml_V.to_string s)) then
              Globals.exit 1
            else
              s
        | Some c, None   -> c
      ) else
        current_ocaml_version t in
    let filter nv =
      let opam = File.OPAM.read (Path.G.opam t.global nv) in
      let consistent_ocaml_version =
        match File.OPAM.ocaml_version opam with
        | None       -> true
        | Some (r,v) -> OCaml_V.compare ocaml_version r v in
      let consistent_pinned_version =
        not (N.Map.mem (NV.name nv) t.pinned) ||
        match N.Map.find (NV.name nv) t.pinned with
        | Version v -> v = NV.version nv
        | _         -> true (* any version is fine, as this will be overloaded on install *) in
      consistent_ocaml_version && consistent_pinned_version in
    Some (NV.Set.filter filter t.available) }

let get_available_current t = 
  match t.available_current with
    | None -> assert false
    | Some v -> v

(* - Look into the content of ~/.opam/config to build the client
   state
   - Also try to check package consistency as much as possible *)
let load_state () =
  log "root path is %s" !Globals.root_path;
  let global = Path.G.create () in
  let config = File.Config.read (Path.G.config global) in
  let ocaml_version = File.Config.ocaml_version config in
  let aliases = File.Aliases.safe_read (Path.G.aliases global) in
  let compiler = Path.C.create ocaml_version in
  let repositories = File.Config.repositories config in
  let repositories = List.map (fun r -> r, Path.R.create r) repositories in
  let repo_index = 
    let repo_index = File.Repo_index.safe_read (Path.G.repo_index global) in
    let l_wrong = 
      List.fold_left (fun accu (n, repo_s) ->
        List.fold_left (fun accu repo ->
          if List.for_all (fun (r, _) -> Repository.name r <> repo) repositories then
            (n, repo) :: accu
          else
            accu
        ) accu repo_s
      ) [] (N.Map.bindings repo_index) in
    let () = List.iter 
      (fun (n, repo) ->
        Globals.error "File %S: unbound repository %S associated to name %S" 
          (Filename.to_string (Path.G.repo_index global)) repo (N.to_string n))
      l_wrong in
    if l_wrong = [] then
      repo_index
    else
      Globals.exit 66 in
  let pinned = File.Pinned.safe_read (Path.C.pinned compiler) in
  let installed = File.Installed.safe_read (Path.C.installed compiler) in
  let reinstall = File.Reinstall.safe_read (Path.C.reinstall compiler) in
  let available = Path.G.available_packages global in
  let available_current = None in
  let t = {
    global; compiler; repositories;
    available; available_current; installed; reinstall;
    repo_index; config; aliases; pinned;
  } in
  print_state t;
  t

type main_function = 
  | Read_only of (unit -> unit)
  | Write_lock of (unit -> unit)

let check f =
  if Dirname.exists (Dirname.of_string !Globals.root_path) then
    match f with
    | Write_lock f -> Run.with_flock f
    | Read_only f ->
      let warn msg e = Globals.warning "%s: %s" msg (Printexc.to_string e) in
      try f () with e ->
        if
          None = try Some (current_ocaml_version (load_state ())) with e -> let () = warn "check" e in None
        then
          let () = warn "main" e in
          Globals.warning "initialization is not yet finished (or the state %s is inconsistent)" !Globals.root_path
          (* NOTE it is feasible to determine here if initialization is finished or not *)
        else
          raise e
  else
    Globals.error_and_exit
      "Cannot find %s. Have you run 'opam init first ?"
      !Globals.root_path

let find_repository_path t name =
  let _, r = List.find (fun (r,_) -> Repository.name r = name) t.repositories in
  r

let mem_repository, find_repository = 
  let f name (r,_) = Repository.name r = name in
  (fun t name -> List.exists (f name) t.repositories),
  fun t name ->
    let r, _ = List.find (f name) t.repositories in
    r

let string_of_repositories t = 
  String.concat ", " (List.map (fun (r, _) -> Repository.name r) t.repositories)

let mem_installed_package_by_name t name =
  not (NV.Set.is_empty (NV.Set.filter (fun nv -> NV.name nv = name) t.installed))

let find_installed_package_by_name t name =
  try NV.Set.choose (NV.Set.filter (fun nv -> NV.name nv = name) t.installed)
  with Not_found ->
    Globals.error_and_exit "Package %s is not installed" (N.to_string name)

let find_available_package_by_name t name =
  let s = NV.Set.filter (fun nv -> NV.name nv = name) (get_available_current t) in
  if NV.Set.is_empty s then
    None
  else
    Some s

let print_updated t updated pinned_updated =
  if not (NV.Set.is_empty (NV.Set.union updated pinned_updated)) then
    Globals.msg "New packages available:\n";
  NV.Set.iter (fun nv ->
    Globals.msg " - %s%s\n"
      (NV.to_string nv)
      (if NV.Set.mem nv t.installed then " (*)" else "")
  ) updated;
  NV.Set.iter (fun nv ->
    Globals.msg " - %s(+)\n" (N.to_string (NV.name nv))
  ) pinned_updated

let print_compilers compilers repo =
  let repo_compilers = Path.R.available_compilers repo in
  let new_compilers = OCaml_V.Set.diff repo_compilers compilers in
  if not (OCaml_V.Set.is_empty new_compilers) then
    Globals.msg "New compiler descriptions available:\n";
  OCaml_V.Set.iter (fun v ->
    Globals.msg " - %s\n" (OCaml_V.to_string v)
  ) new_compilers

let install_conf_ocaml_config t =
  let name = N.of_string Globals.default_package in
  (* .config *)
  let vars = 
    let map f l = List.map (fun (s,p) -> Variable.of_string s, S (f p)) l in

    map Dirname.to_string
      [
        ("prefix", Path.C.root t.compiler);
        ("lib", Path.C.lib_dir t.compiler);
        ("bin", Path.C.bin t.compiler);
        ("doc", Path.C.doc_dir t.compiler);
        ("stublibs", Path.C.stublibs t.compiler);
        ("toplevel", Path.C.toplevel t.compiler);
      ]
    @ 
    map (fun x -> x)
      [
        ("user", (Unix.getpwuid (Unix.getuid ())).Unix.pw_name);
        ("group", (Unix.getgrgid (Unix.getgid ())).Unix.gr_name);
      ] in

  let config = File.Dot_config.create vars in
  File.Dot_config.write (Path.C.config t.compiler name) config

let install_conf_ocaml () =
  log "installing conf-ocaml";
  let t = load_state () in
  let name = N.of_string Globals.default_package in
  let version = V.of_string (Alias.to_string (File.Config.ocaml_version t.config)) in
  let nv = NV.create name version in
  (* .opam *)
  let opam = File.OPAM.create nv in
  File.OPAM.write (Path.G.opam t.global nv) opam;
  (* description *)
  let descr = File.Descr.create "Compiler configuration flags" in
  File.Descr.write (Path.G.descr t.global nv) descr;
  install_conf_ocaml_config t;
  (* installed *)
  let installed_p = Path.C.installed t.compiler in
  let installed = File.Installed.safe_read installed_p in
  let installed = NV.Set.add nv installed in
  File.Installed.write installed_p installed;
  (* stublibs *)
  let stublibs = Path.C.stublibs t.compiler in
  Dirname.mkdir stublibs;
  (* toplevel dir *)
  let toplevel = Path.C.toplevel t.compiler in
  Dirname.mkdir toplevel

let update_repo_index t =

  (* Update repo_index *)
  let repo_index =
    List.fold_left (fun repo_index (r,p) ->
      let available = Path.R.available_packages p in
      log "repo=%s packages=%s" (Repository.name r) (NV.Set.to_string available);
      NV.Set.fold (fun nv repo_index ->
        let name = NV.name nv in
        let repo = Repository.name r in
        if not (N.Map.mem name repo_index) then
          N.Map.add name [repo] repo_index
        else
          let repo_s = N.Map.find name repo_index in
          if not (List.mem repo repo_s) then
            let repo_index = N.Map.remove name repo_index in
            N.Map.add name (repo_s @ [repo]) repo_index
          else
            repo_index
      ) available repo_index
    ) t.repo_index t.repositories in
  File.Repo_index.write (Path.G.repo_index t.global) repo_index;

  (* suppress previous links *)
  Dirname.rmdir (Path.G.opam_dir t.global);
  Dirname.mkdir (Path.G.opam_dir t.global);
  Dirname.rmdir (Path.G.descr_dir t.global);
  Dirname.mkdir (Path.G.descr_dir t.global);
  install_conf_ocaml ();
  
  (* Create symbolic links from $repo dirs to main dir *)
  N.Map.iter (fun n repo_s ->
    let all_versions = ref V.Set.empty in
    List.iter (fun r ->
      let repo_p = find_repository_path t r in
      let available_versions = Path.R.available_versions repo_p n in
      V.Set.iter (fun v ->
        if not (V.Set.mem v !all_versions) then (
          let nv = NV.create n v in
          let opam_g = Path.G.opam t.global nv in
          let opam_r = Path.R.opam repo_p nv in
          let descr_g = Path.G.descr t.global nv in
          let descr_r = Path.R.descr repo_p nv in
          Filename.link opam_r opam_g;
          if Filename.exists descr_r then
            Filename.link descr_r descr_g
          else
           Globals.msg "WARNING: %s does not exist\n" (Filename.to_string descr_r)
        )
      ) available_versions
    ) repo_s
  ) repo_index

let update_repo ~show_compilers  =
  log "update_repo";
  let t = load_state () in
  let compilers = Path.G.available_compilers t.global in

  (* first update all the repo *)
  List.iter (fun (r,_) -> Repositories.update r) t.repositories;

  (* Display the new compilers available *)
  List.iter (fun (_, r) -> if show_compilers then print_compilers compilers r) t.repositories;

  (* XXX: we could have a special index for compiler descriptions as
  well, but that's become a bit too heavy *)
  List.iter (fun (r,p) ->
    let comps = Path.R.available_compilers p in
    let comp_dir = Path.G.compilers_dir t.global in
    OCaml_V.Set.iter (fun o ->
      let comp_f = Path.R.compiler p o in
      Filename.link_in comp_f comp_dir
    ) comps
  ) t.repositories

let update_package ~show_packages =
  log "update_packages";
  let t = load_state () in
  (* Update the pinned packages *)
  let pinned_updated =
    NV.Set.of_list (
      Utils.filter_map
        (function
          | n, Path p ->
              if mem_installed_package_by_name t n then
                let nv = find_installed_package_by_name t n in
                let build = Path.C.build t.compiler nv in
                Globals.msg "Synchronizing %s with %s ...\n" (NV.to_string nv) (Dirname.to_string p);
                (* XXX: make it more generic *)
                if Dirname.exists build then
                  match
                    Run.read_command_output
                      [ "rsync"; "-arv"; Dirname.to_string p ^ "/"; Dirname.to_string build ]
                  with
                  | None -> None
                  | Some lines ->
                      match Utils.rsync_trim lines with
                      | [] -> None
                      | l  -> Some nv
                else
                  None
              else
                None
          | _ -> None)
        (N.Map.bindings t.pinned)) in

  (* then update $opam/repo/index *)
  update_repo_index t;

  let updated =
    N.Map.fold (fun n repo_s accu ->
      (* we do not try to upgrade pinned packages *)
      if N.Map.mem n t.pinned then
        accu
      else (
        let all_versions = ref V.Set.empty in
        List.fold_left (fun accu repo ->
          let p = find_repository_path t repo in
          let available_versions = Path.R.available_versions p n in
          let new_versions = V.Set.diff available_versions !all_versions in
          log "new_versions: %s" (V.Set.to_string new_versions);
          if not (V.Set.is_empty new_versions) then (
            all_versions := V.Set.union !all_versions new_versions;
            let all_updated = File.Updated.safe_read (Path.R.updated p) in
            let updated =
              NV.Set.filter (fun nv ->
                NV.name nv = n && V.Set.mem (NV.version nv) new_versions
              ) all_updated in
            NV.Set.union updated accu
          ) else
            accu
        ) accu repo_s
      )
    ) t.repo_index NV.Set.empty in  
  if show_packages then
    print_updated t updated pinned_updated;

  let updated = NV.Set.union pinned_updated updated in
  (* update $opam/$oversion/reinstall *)
  Alias.Set.iter (fun alias ->
    let t = Path.C.create alias in
    let installed = File.Installed.safe_read (Path.C.installed t) in
    let reinstall = File.Reinstall.safe_read (Path.C.reinstall t) in
    let reinstall = 
      NV.Set.fold (fun nv reinstall ->
        if NV.Set.mem nv installed then
          NV.Set.add nv reinstall
        else
          reinstall
      ) updated reinstall in
    if not (NV.Set.is_empty reinstall) then
      File.Reinstall.write (Path.C.reinstall t) reinstall
  ) (Path.G.available_aliases t.global);

  (* Check all the dependencies exist *)
  let t = update_available_current (load_state ()) in
  let has_error = ref false in
  NV.Set.iter (fun nv ->
    let opam = File.OPAM.read (Path.G.opam t.global nv) in
    let name = File.OPAM.name opam in
    let version = File.OPAM.version opam in
    if nv <> NV.create name version then
      (Globals.error
        "The file %s is not consistent with the package %s (%s)"
        (Filename.to_string (Path.G.opam t.global nv))
        (N.to_string name)
        (V.to_string version);
      has_error := true);
    let depends = File.OPAM.depends opam in
    let depopts = File.OPAM.depopts opam in
    List.iter (List.iter (fun ((d,_),_) ->
      match find_available_package_by_name t (N.of_string d) with
        | None   ->
            let _ = Globals.error
              "Package %s depends on the unknown package %s"
              (N.to_string (NV.name nv)) d in
            has_error := true
        | Some _ -> ()
    )) (depends @ depopts)
  ) (get_available_current t);
  if !has_error then
    Globals.exit 1

let update () =
  log "update";
  update_repo ~show_compilers:true;
  update_package ~show_packages:true

(* Return the contents of a fully qualified variable *)
let contents_of_variable t v =
  let name = Full_variable.package v in
  let var = Full_variable.variable v in
  let installed = mem_installed_package_by_name t name in
  if var = Variable.enable && installed then
    S "enable"
  else if var = Variable.enable && not installed then
    S "disable"
  else if var = Variable.installed then
    B installed
  else if not installed then
    Globals.error_and_exit "Package %s is not installed" (N.to_string name)
  else begin
    let c = File.Dot_config.safe_read (Path.C.config t.compiler name) in
    try match Full_variable.section v with
      | None   -> File.Dot_config.variable c var
      | Some s -> File.Dot_config.Section.variable c s var
    with Not_found ->
      Globals.error_and_exit "%s is not defined" (Full_variable.to_string v)
  end

(* Substitute the file contents *)
let substitute_file t f =
  let f = Filename.of_basename f in
  let src = Filename.add_extension f "in" in
  let contents = File.Subst.read src in
  let newcontents = File.Subst.replace contents (contents_of_variable t) in
  File.Subst.write f newcontents

(* Substitue the string contents *)
let substitute_string t s =
  File.Subst.replace_string s (contents_of_variable t)

let base_packages = [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_default_compiler_description t =
  let ocaml_version = OCaml_V.of_string Globals.default_compiler_version in
  let mk name = ((name,None),None) in
  let f =
    File.Comp.create_preinstalled
      ocaml_version
      (List.map mk (if !Globals.base_packages then base_packages else []))
      [ ("CAML_LD_LIBRARY_PATH", "=",
           (Dirname.to_string (Path.C.stublibs t.compiler))
           ^ ":" ^
           (match Run.ocamlc_where () with
           | Some d -> Stdlib_filename.concat d "stublibs"
           | None   -> assert false))
      ] in
  let comp = Path.G.compiler t.global ocaml_version in
  File.Comp.write comp f

let add_alias alias ocaml_version =
  log "adding alias %s %s" (Alias.to_string alias) (OCaml_V.to_string ocaml_version);
  let t = load_state () in
  if ocaml_version = OCaml_V.of_string Globals.default_compiler_version then
    (* we create a dummy compiler description file the the system-wide
       OCaml configuration *)
    create_default_compiler_description t;
  let aliases_f = Path.G.aliases t.global in
  let aliases = File.Aliases.safe_read aliases_f in
  if not (List.mem_assoc alias aliases) then begin
    (* Install the initial package and reload the global state *)
    install_conf_ocaml ();
    (* Update the list of aliases *)
    File.Aliases.write aliases_f ((alias, ocaml_version) :: aliases);
  end

(* We assume that we have the right ocaml-version in $opam/config. Then we:
   - create $opam/$alias
   - compiles and install $opam/compiler/$descr.comp
   Some explanations:
   - [f_exists] is called if alias already exists
   - if [default_allowed] is set, then 'system' is an allowed alias argument
*)
let init_ocaml f_exists alias default_allowed ocaml_version =
  log "init_ocaml";
  let t = load_state () in

  let default = OCaml_V.of_string Globals.default_compiler_version in
  let system_ocaml_version, ocaml_version = 
    let current () =
      match OCaml_V.current () with
        | None -> Globals.error_and_exit "No OCaml compiler found in path"
        | Some system_ocaml -> Some system_ocaml, default in

    match ocaml_version with
      | None -> current ()
      | Some ocaml_version when default_allowed && ocaml_version = default -> current ()
      | Some ocaml_version -> 
          let comp_f = Path.G.compiler t.global ocaml_version in
          if not default_allowed && ocaml_version = default || not (Filename.exists comp_f) then 
            begin
              Globals.msg
                "  %S is not a valid compiler description%s. The available compilers descriptions are:\n"
                (OCaml_V.to_string ocaml_version)
                (if ocaml_version = default then " (because it is a reserved name)" else "");
              OCaml_V.Set.iter
                (fun v -> Globals.msg "    - %s\n" (OCaml_V.to_string v))
                (Path.G.available_compilers t.global);
              Globals.exit 2
            end 
          else
            None, ocaml_version in

  let alias = 
    match alias with 
      | None -> Alias.of_string (OCaml_V.to_string ocaml_version)
      | Some alias -> alias in

  log "init_ocaml (alias=%s, ocaml_version=%s)" (Alias.to_string alias) (OCaml_V.to_string ocaml_version);

  let alias_p = Path.C.create alias in
  let alias_p_dir = Path.C.root alias_p in
  if Dirname.exists alias_p_dir then
    f_exists alias_p_dir
  else
  begin
    Dirname.mkdir alias_p_dir;
    (if ocaml_version <> default then 
    try
      let comp_f = Path.G.compiler t.global ocaml_version in
      let comp = File.Comp.read comp_f in
      if not (File.Comp.preinstalled comp) then begin

        (* Install the compiler *)
        let comp_src = File.Comp.src comp in
        let build_dir = Path.C.build_ocaml alias_p in
        Dirname.with_tmp_dir (fun download_dir ->
          begin match Filename.download comp_src download_dir with
          | None   -> Globals.error_and_exit "Cannot download %s" (Filename.to_string comp_src)
          | Some f -> Filename.extract f build_dir
          end;
          let patches = File.Comp.patches comp in
              let patches =
                Utils.filter_map (fun f ->
                  match Filename.download f build_dir with
                  | None   -> Globals.error_and_exit "Cannot download %s" (Filename.to_string f) 
                  | Some f -> Some f
                ) patches in
              List.iter (fun f ->
                if not (Filename.patch f build_dir) then
                  Globals.error_and_exit "Cannot apply %s" (Filename.to_string f)
              ) patches;
          let err =
            let t = 
              { t with
                compiler = alias_p;
                installed = 
                  let name = N.of_string Globals.default_package in
                  let version = V.of_string (Alias.to_string alias) in
                  let nv = NV.create name version in
                  NV.Set.add nv NV.Set.empty } in
            install_conf_ocaml_config t;
            if File.Comp.configure comp @ File.Comp.make comp <> [] then begin
              Dirname.exec build_dir
                [ ( "./configure" :: File.Comp.configure comp )
                  @ [ "-prefix";  Dirname.to_string alias_p_dir ]
                (*-bindir %s/bin -libdir %s/lib -mandir %s/man*)
                (* NOTE In case it exists 2 '-prefix', in general the script
                   ./configure will only consider the last one, others will be
                   discarded. *)
                ; ( "make" :: File.Comp.make comp )
                ; [ "make" ; "install" ]
                ]
            end else begin
              let builds =
                List.map (List.map (substitute_string t)) (File.Comp.build comp) in
              Dirname.exec build_dir builds
            end in
          if err <> 0 then
            Globals.error_and_exit
              "The compilation of compiler version %s failed"
              (OCaml_V.to_string ocaml_version)
        )
      end
    with e -> 
      if not !Globals.debug then
      Dirname.rmdir alias_p_dir;
      raise e);
  end;

  (* write the new version in the configuration file *)
  let config = File.Config.with_ocaml_version t.config alias in
  let config = File.Config.with_system_ocaml_version config system_ocaml_version in
  File.Config.write (Path.G.config t.global) config;
  add_alias alias ocaml_version;
  ocaml_version

let indent_left s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    s ^ String.make nb ' '

let indent_right s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    String.make nb ' ' ^ s

let s_not_installed = "--"

let list print_short installed_only pkg_str =
  log "list";
  let t = load_state () in
  let re = Re_perl.compile_pat ~opts:[`Caseless] pkg_str in
  (* Get all the installed packages *)
  let installed = File.Installed.read (Path.C.installed t.compiler) in
  let map, max_n, max_v =
    NV.Set.fold
      (fun nv (map, max_n, max_v) ->
        let name = NV.name nv in
        let version = NV.version nv in
        if
          N.Map.mem name map (* If the packet has been processed yet *)
          &&
          fst (N.Map.find name map) <> None
            (* If moreover the version processed was the version that is installed.
               NB at the time of writing there is at most only 1 [version]
               installed for a given [name]. *)
        then
          map, max_n, max_v
        else
          let is_installed = NV.Set.mem nv installed in
          let descr_f = File.Descr.safe_read (Path.G.descr t.global nv) in
          let synopsis = File.Descr.synopsis descr_f in
          let map = N.Map.add name ((if is_installed then Some version else None), synopsis) map in
          let max_n = max max_n (String.length (N.to_string name)) in
          let max_v = if is_installed then max max_v (String.length (V.to_string version)) else max_v in
          map, max_n, max_v)
      t.available
      (N.Map.empty, min_int, String.length s_not_installed)
  in
  N.Map.iter (
    if print_short then
      fun name (version, _) -> 
        let version = match version with
          | None   -> s_not_installed
          | Some v -> V.to_string v in
        let name = N.to_string name in
        (if Re.execp re name && (not installed_only || version <> s_not_installed)
         then Globals.msg "%s " name)
    else
      fun name (version, description) ->
        let name = N.to_string name in
        let version = match version with
          | None   -> s_not_installed
          | Some v -> V.to_string v in
        (if (Re.execp re name || Re.execp re description)
            && (not installed_only || version <> s_not_installed) then
            Globals.msg "%s  %s  %s\n"
              (indent_left name max_n)
              (indent_right version max_v)
              description)
  ) map

let info package =
  log "info %s" (N.to_string package);
  let t = load_state () in

  let o_v =
    let installed = File.Installed.read (Path.C.installed t.compiler) in
    try Some (V.Set.choose_one (N.Map.find package (NV.to_map installed)))
    with Not_found -> None in

  let v_set =
    let v_set =
      try Path.G.available_versions t.global package
      with Not_found ->
        Globals.error_and_exit "unknown package %s" (N.to_string package) in
    match o_v with
      | None   -> v_set
      | Some v -> V.Set.remove v v_set in

  let installed_version = match o_v with
    | None   -> []
    | Some v -> [ "installed-version", V.to_string v ] in

  let available_versions =
    match List.map V.to_string (V.Set.elements v_set) with
    | []  -> []
    | [v] -> [ "available-version" , v ]
    | l   -> [ "available-versions", String.concat ", " l ] in

  let libraries, syntax = match o_v with
    | None   -> [], []
    | Some v ->
        let opam = File.OPAM.read (Path.G.opam t.global (NV.create package v)) in
        let libraries = match File.OPAM.libraries opam with
          | [] -> []
          | l  -> [ "libraries", String.concat ", " (List.map Section.to_string l) ] in
        let syntax = match File.OPAM.syntax opam with
          | [] -> []
          | l  -> [ "syntax", String.concat ", " (List.map Section.to_string l) ] in
        libraries, syntax in

  List.iter
    (fun (tit, desc) -> Globals.msg "%20s: %s\n" tit desc)
    ( [ "package", N.to_string package ]
     @ installed_version
     @ available_versions
     @ libraries
     @ syntax
     @ let latest = match o_v with
         | Some v -> Some v
         | None   ->
             try Some (V.Set.max_elt v_set)
             with Not_found -> None in
       let descr =
         match latest with
         | None   -> File.Descr.empty
         | Some v ->
             File.Descr.safe_read (Path.G.descr t.global (NV.create package v)) in
       [ "description", File.Descr.full descr ]
    )

let proceed_toinstall t nv =
  Globals.msg "Installing %s ...\n" (NV.to_string nv);

  let t = load_state () in
  let name = NV.name nv in
  let opam_f = Path.G.opam t.global nv in
  let opam = File.OPAM.read opam_f in
  let config_f = Path.C.build_config t.compiler nv in
  let config = File.Dot_config.safe_read config_f in
  let install_f = Path.C.build_install t.compiler nv in
  let install = File.Dot_install.safe_read install_f in

  Dirname.chdir (Path.C.build t.compiler nv);

  (* check that libraries and syntax extensions specified in .opam and
     .config are in sync *)
  let check kind config_sections opam_sections =
    List.iter (fun cs ->
      if not (List.mem cs opam_sections) then
        Globals.error_and_exit "The %s %s does not appear in %s"
          kind (Section.to_string cs) (Filename.to_string opam_f)
    ) config_sections;
    List.iter (fun os ->
      if not (List.mem os config_sections) then
        Globals.error_and_exit "The %s %s does not appear in %s"
          kind (Section.to_string os) (Filename.to_string config_f)
    ) opam_sections in
  if not (Filename.exists config_f) &&
    (File.OPAM.libraries opam <> [] || File.OPAM.syntax opam <> []) then
    Globals.error_and_exit
      "%s does not exists but %s defines some libraries and syntax extensions"
      (Filename.to_string config_f)
      (Filename.to_string opam_f);
  check "library"
    (File.Dot_config.Library.available config)
    (File.OPAM.libraries opam);
  check "syntax"
    (File.Dot_config.Syntax.available config)
    (File.OPAM.syntax opam);

  (* check that depends (in .opam) and requires (in .config) fields
     are in almost in sync *)
  (* NOTES: the check is partial as we don't know which clause is valid
     in depends (XXX there is surely a way to get it from the solver) *)
  let local_sections = File.Dot_config.Section.available config in
  let libraries_in_opam =
    List.fold_left (fun accu l ->
      List.fold_left (fun accu ((n,_),_) ->
        let n = N.of_string n in
        let nv = find_installed_package_by_name t n in
        let opam = File.OPAM.read (Path.G.opam t.global nv) in
        let libs = File.OPAM.libraries opam in
        let syntax = File.OPAM.syntax opam in
        List.fold_right Section.Set.add (libs @ syntax) accu
      ) accu l
    ) Section.Set.empty (File.OPAM.depends opam) in
  let libraries_in_config =
    List.fold_left (fun accu s ->
      List.fold_left (fun accu r ->
        Section.Set.add r accu
      ) accu (File.Dot_config.Section.requires config s)
    ) Section.Set.empty local_sections in
  Section.Set.iter (fun s ->
    if not (List.mem s local_sections)
    && not (Section.Set.mem s libraries_in_opam) then
      let config_f = Filename.to_string (Path.C.build_config t.compiler nv) in
      let opam_f = Filename.to_string (Path.G.opam t.global nv) in
      let local_sections = List.map Section.to_string local_sections in
      let opam_sections = List.map Section.to_string (Section.Set.elements libraries_in_opam) in
      Globals.error_and_exit
        "%s appears as a library dependency in %s, but:\n\
             - %s defines the libraries {%s}\n\
             - Packages in %s defines the libraries {%s}"
        (Section.to_string s) config_f
        config_f (String.concat ", " local_sections)
        opam_f (String.concat ", " opam_sections)
  ) libraries_in_config;

  (* .install *)
  File.Dot_install.write (Path.C.install t.compiler name) install;

  (* .config *)
  File.Dot_config.write (Path.C.config t.compiler name) config;

  (* lib *)
  let lib = Path.C.lib t.compiler name in
  List.iter (fun f -> Filename.copy_in f lib) (File.Dot_install.lib install);

  (* toplevel *)
  let toplevel = Path.C.toplevel t.compiler in
  List.iter (fun f -> Filename.copy_in f toplevel) (File.Dot_install.toplevel install);

  (* bin *)
  List.iter (fun (src, dst) ->
    let dst = Path.C.bin t.compiler // (Basename.to_string dst) in
    Filename.copy src dst
  ) (File.Dot_install.bin install);

  (* misc *)
  List.iter
    (fun (src, dst) ->
      if Filename.exists dst && confirm "Overwriting %s ?" (Filename.to_string dst) then
        Filename.copy src dst
      else begin
        Globals.msg "Installing %s to %s.\n" (Filename.to_string src) (Filename.to_string dst);
        if confirm "Continue ?" then
          Filename.copy src dst
      end
    ) (File.Dot_install.misc install)

let pinned_path t nv =
  let name = NV.name nv in
  if N.Map.mem name t.pinned then
    match N.Map.find name t.pinned with
    | Path p -> Some p
    | _      -> None
  else
    None

let get_archive t nv =
  log "get_archive %s" (NV.to_string nv);
  let name = NV.name nv in
  let rec aux = function
    | [] ->
        Globals.error_and_exit
          "Unable to find a repository containing %s"
          (NV.to_string nv)
    | repo :: repo_s ->
        let repo_p = find_repository_path t repo in
        let repo = find_repository t repo in
        let opam_f = Path.R.opam repo_p nv in
        if Filename.exists opam_f then (
          Repositories.download repo nv;
          let src = Path.R.archive repo_p nv in
          let dst = Path.G.archive t.global nv in
          Filename.link src dst;
          dst
        ) else
          aux repo_s in
  aux (N.Map.find name t.repo_index)

let extract_package t nv =
  log "extract_package: %s" (NV.to_string nv);
  let p_build = Path.C.build t.compiler nv in
  Dirname.rmdir p_build;
  match pinned_path t nv with
  | None   ->
      let archive = get_archive t nv in
      Globals.msg "Extracting ...\n";
      Filename.extract archive p_build;
      p_build
  | Some p ->
      (* XXX: make it a bit more generic ... *)
      Globals.msg "Synchronizing %s with %s ...\n" (NV.to_string nv) (Dirname.to_string p);
      let err = Run.command [ "rsync"; "-arv"; Dirname.to_string p ^ "/"; Dirname.to_string p_build ] in
      if err <> 0 then
        Globals.error_and_exit "Cannot synchronize %s with %s." (Dirname.to_string p) (Dirname.to_string p_build);
      p_build

let proceed_todelete t nv =
  log "deleting %s" (NV.to_string nv);
  Globals.msg "Uninstalling %s ...\n" (NV.to_string nv);
  let name = NV.name nv in

  (* Run the remove script *)
  let opam = File.OPAM.read (Path.G.opam t.global nv) in
  let remove = List.map (List.map (substitute_string t)) (File.OPAM.remove opam) in
  let p_build = Path.C.build t.compiler nv in
  if not (Dirname.exists p_build) then
    ignore (extract_package t nv);
  (* We try to run the remove scripts in the folder where it was extracted
     If it does not exist, we don't really care. *)
  let err = Dirname.exec ~add_to_path:[Path.C.bin t.compiler] p_build remove in
  if err <> 0 then
    Globals.error_and_exit "Cannot uninstall %s" (NV.to_string nv);

  (* Remove the libraries *)
  Dirname.rmdir (Path.C.lib t.compiler name);
  Dirname.rmdir (Path.C.build t.compiler nv);

  (* Clean-up the repositories *)
  let repos =
    try N.Map.find (NV.name nv) t.repo_index
    with _ -> [] in
  List.iter (fun r ->
    let p = find_repository_path t r in
    let archive = Path.R.archive p nv in
    let tmp_dir = Path.R.tmp_dir p nv in
    Filename.remove archive;
    Dirname.rmdir tmp_dir
  ) repos;
    
  (* Remove the binaries *)
  let install = File.Dot_install.safe_read (Path.C.install t.compiler name) in
  List.iter (fun (_,dst) ->
    let dst = Path.C.bin t.compiler // (Basename.to_string dst) in
    Filename.remove dst
  ) (File.Dot_install.bin install);

  (* Remove the misc files *)
  List.iter (fun (_,dst) ->
    if Filename.exists dst then begin
      Globals.msg "Removing %s." (Filename.to_string dst);
      if confirm "Continue ?" then
        Filename.remove dst
    end
  ) (File.Dot_install.misc install);

  (* Remove .config and .install *)
  Filename.remove (Path.C.install t.compiler name);
  Filename.remove (Path.C.config t.compiler name)

type env = {
  add_to_env : (string * string) list;
  add_to_path: dirname;
  new_env    : (string * string) list;
}

let expand_env t env =
  List.map (fun (ident, symbol, string) ->
    let string = substitute_string t string in
    let read_env () =
      let prefix = Dirname.to_string (Path.G.root t.global) in
      try Utils.reset_env_value ~prefix (Sys.getenv ident)
      with _ -> [] in
    match symbol with
    | "="  -> (ident, string)
    | "+=" -> (ident, String.concat ":" (string :: read_env ()))
    | "=+" -> (ident, String.concat ":" (read_env () @ [string]))
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let update_env t env e =
  let expanded = expand_env t e in
  { env with
    add_to_env = expanded @ env.add_to_env;
    new_env    = expanded @ env.new_env }

let get_env t =
  let ocaml_version = current_ocaml_version t in
  let comp_f = Path.G.compiler t.global ocaml_version in
  let comp = File.Comp.read comp_f in

  let add_to_path = Path.C.bin t.compiler in
  let new_path = "PATH", "+=", Dirname.to_string add_to_path in

  let add_to_env = File.Comp.env comp in
  let toplevel_dir =
    "OCAML_TOPLEVEL_PATH", "=", Dirname.to_string (Path.C.toplevel t.compiler) in
  let new_env = new_path :: toplevel_dir :: add_to_env in

  let add_to_env = expand_env t add_to_env in
  let new_env = expand_env t new_env in

  { add_to_env; add_to_path; new_env }

let print_env env =
  List.iter (fun (k,v) ->
    Globals.msg "%s=%s; export %s;\n" k v k;
  ) env.new_env

let print_env_warning () =
  Globals.msg "\nTo update your environment variables, you can now run:
            \n\    $ eval `opam config -env`\n\n"

let rec proceed_tochange t nv_old nv =
  Globals.msg "==== %s ====\n" (NV.to_string nv);

  (* First, uninstall any previous version *)
  (match nv_old with
  | Some nv_old -> proceed_todelete t nv_old
  | None        -> ());

  (* Then, untar the archive *)
  let p_build = extract_package t nv in
  let opam = File.OPAM.read (Path.G.opam t.global nv) in

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the substitution
     files (see [substitute_file] and [Filename.of_basename]. *)
  Dirname.chdir (Path.C.build t.compiler nv);
  List.iter (substitute_file t) (File.OPAM.substs opam);

  (* Get the env variables set up in the compiler description file *)
  let env0 = get_env t in
  let env = update_env t env0 (File.OPAM.build_env opam) in

  (* Generate an environnement file *)
  let env_f = Path.C.build_env t.compiler nv in
  File.Env.write env_f env.new_env;

  (* Call the build script and copy the output files *)
  let commands = List.map (List.map (substitute_string t))
    (File.OPAM.build opam) in
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  Globals.msg "Build commands:\n  %s\n" (String.concat "\n  " commands_s);
  let err =
    Dirname.exec
      ~add_to_env:env.add_to_env
      ~add_to_path:[env.add_to_path]
      p_build
      commands in
  if err = 0 then
    try proceed_toinstall t nv
    with e ->
      Globals.error "while copying some files of %S" (NV.to_string nv);
      proceed_todelete t nv;
      let _build = extract_package t nv in
      raise e
  else (
    proceed_todelete t nv;
    let p_build = extract_package t nv in
    match nv_old with 
    | None        ->
        Globals.error_and_exit "Compilation failed in %s." (Dirname.to_string p_build)
    | Some nv_old -> 
        if nv_old = nv then
          Globals.error_and_exit "Recompilation failed in %s."  (Dirname.to_string p_build)
        else
          (* try to restore the previous erased [nv_old] version *)
          try
            Globals.error "Compilation of %s failed in %s. Restoring previous working version (%s) ..."
              (NV.to_string nv) (Dirname.to_string p_build) (NV.to_string nv_old);
            proceed_tochange t None nv_old
          with _ ->
            (* XXX: determine if it is because some dependencies have been deleted or not... *)
            Globals.error_and_exit "Restoring %s failed" (NV.to_string nv_old) 
  )

(* We need to clean-up things before recompiling. *)
let proceed_torecompile t nv =
  proceed_tochange t (Some nv) nv

let debpkg_of_nv action t nv =
  let opam = File.OPAM.read (Path.G.opam t.global nv) in
  let installed =
    (action <> `upgrade || not (NV.Set.mem nv t.reinstall))
    && NV.Set.mem nv t.installed in
  File.OPAM.to_package opam installed

type version_constraint = 
  | V_any of name * V.Set.t (* versions available *) * version option (* version installed *)
  | V_eq of name * version

let string_of_version_constraint = function
  | V_any (n,s,i) ->
      Printf.sprintf
        "{name=%s available=%s installed=%s}"
        (N.to_string n)
        (V.Set.to_string s)
        (match i with None -> "<none>" | Some v -> V.to_string v)
  | V_eq (n,v) ->
      Printf.sprintf "{name=%s version=%s}" (N.to_string n) (V.to_string v)

module Heuristic = struct

  let vpkg_of_n op name = (N.to_string name, None), op

  let vpkg_of_n_op op name v = vpkg_of_n (Some (op, V.to_string v)) name

  let vpkg_eq = "="
  let vpkg_of_nv_eq = vpkg_of_n_op vpkg_eq
  let vpkg_of_nv_ge = vpkg_of_n_op ">="
  let vpkg_of_nv_le = vpkg_of_n_op "<="
  let vpkg_of_nv_any = vpkg_of_n None

  let v_any _ _ = vpkg_of_nv_any
  let v_eq _ set n = vpkg_of_nv_eq n (V.Set.max_elt set)
  let v_ge _ set n = vpkg_of_nv_ge n (V.Set.max_elt set)
  let v_eq_opt v set n = vpkg_of_nv_eq n (match v with None -> V.Set.max_elt set | Some v -> v)

  let get_installed t f_h =
    let available = NV.to_map (get_available_current t) in
    N.Map.mapi
      (fun n v -> f_h (Some (V.Set.choose_one v)) (N.Map.find n available) n)
      (NV.to_map t.installed)

  let get_packages t ocaml_version f_h = 
    let comp_f = Path.G.compiler t.global ocaml_version in
    let comp = File.Comp.read comp_f in
    let available = NV.to_map (get_available_current t) in

    let pkg_available, pkg_not = 
      List.partition
        (function (name, _), _ -> 
          N.Map.mem (N.of_string name) available)
        (File.Comp.packages comp) in

    let () = (* check that all packages in [comp] are in [available]
                except for "base-..." 
                (depending if "-no-base-packages" is set or not) *)
      match 
        let pkg_not = List.rev_map (function (n, _), _ -> n) pkg_not in
        if !Globals.base_packages then
          pkg_not
        else
          List.filter (fun n -> not (List.mem n base_packages)) pkg_not
      with
        | [] -> ()
        | l -> 
            let () = List.iter (Globals.error "Package %s not found") l in
            Globals.exit 66 in

    List.rev_map 
      (function 
        | (name, _), None ->
            let name = N.of_string name in
            f_h None (N.Map.find name available) name
        | n, v -> n, v)
      pkg_available

  let apply f_heuristic = 
    List.map
      (function 
        | V_any (n, set, v) -> f_heuristic v set n
        | V_eq (n, v) -> vpkg_of_nv_eq n v)

  let unknown_package name =
    Globals.error_and_exit "Unable to locate package %S\n" (N.to_string name)

  (* transform a name into:
     - <name, installed version> package
     - <$n,$v> package when name = $n.$v *)
  let nv_of_names t =
    let available = NV.to_map (get_available_current t) in
    let installed = NV.to_map t.installed in
    List.map
      (fun name -> 
        if N.Map.mem name available then begin
          let set = N.Map.find name available in
          if N.Map.mem name installed then
            let version = V.Set.choose_one (N.Map.find name installed) in
            V_any (name, set, Some version)
          else 
            V_any (name, set, None)
        end else
          (* consider 'name' to be 'name.version' *)
          let nv =
            try NV.of_string (N.to_string name)
            with Not_found -> unknown_package name in
          let sname = NV.name nv in
          let sversion = NV.version nv in
          Globals.msg
            "The raw name %S not found, looking for package %s version %s\n"
            (N.to_string name) (N.to_string sname) (V.to_string sversion);
          if N.Map.mem sname available
            && V.Set.mem sversion (N.Map.find sname available) then
            V_eq (sname, sversion)
          else
            unknown_package sname)

  let apply_solution t sol = 
    if Solver.solution_is_empty sol then
      true
    else (
      Globals.msg "The following actions will be performed:\n";      
      print_solution sol;
      let to_install, to_reinstall =
        PA_graph.fold_vertex
          (fun pkg (to_install, to_reinstall) ->
            match action pkg with
              | To_change (None, _) -> succ to_install, to_reinstall
              | To_change (Some _, _)
              | To_recompile _ -> to_install, succ to_reinstall
              | To_delete _ -> assert false) 
          sol.to_add
          (0, 0) in
      let to_remove = List.length sol.to_remove in
      Globals.msg "%d to install | %d to reinstall | %d to remove\n"
        to_install
        to_reinstall
        to_remove;

      let continue = 
        (* if only one package to install and none to remove, 
           or one package to remove and none to install
           or at most one package to reinstall
           then no need to confirm *)
        if to_install + to_reinstall + to_remove <= 1 then
          true
        else
          confirm "Do you want to continue ?" in
      
      if continue then (

        let installed = ref t.installed in
        let write_installed () =
          File.Installed.write (Path.C.installed t.compiler) !installed in

        (* Delete some packages *)
        (* In case of errors, we try to keep the list of installed packages up-to-date *)
        List.iter
          (fun nv ->
            if NV.Set.mem nv !installed then begin
              proceed_todelete t nv;
              installed := NV.Set.remove nv !installed;
              write_installed ()
            end)
          sol.to_remove;

        (* Install or recompile some packages on the child process *)
        let child n =
          let t = load_state () in
          match action n with
          | To_change (o, nv) -> proceed_tochange t o nv
          | To_recompile nv   -> proceed_torecompile t nv
          | To_delete _       -> assert false in

        let pre _ = () in

        (* Update the installed file in the parent process *)
        let post n = match action n with
          | To_delete _    -> assert false
          | To_recompile _ -> ()
          | To_change (None, nv) ->
              installed := NV.Set.add nv !installed;
              write_installed ()
          | To_change (Some o, nv)   ->
              installed := NV.Set.add nv (NV.Set.remove o !installed);
              write_installed () in

        let error n =
          let f msg nv =
            Globals.error_and_exit "Command failed while %s %s" msg (NV.to_string nv) in
          match action n with
          | To_change (Some _, nv) -> f "upgrading/downgrading" nv
          | To_change (None, nv)   -> f "installing" nv
          | To_recompile nv        -> f "recompiling" nv
          | To_delete _            -> assert false in

        let cores = File.Config.cores t.config in
        try PA_graph.Parallel.iter cores sol.to_add ~pre ~child ~post
        with PA_graph.Parallel.Errors n -> List.iter error n
      );
      continue
    )

  let apply_solutions t = 
    let rec aux = function
      | x :: xs when not (apply_solution t x) -> aux xs
      | _ -> () in
    aux

  let resolve action_k t l_request =
    let available = get_available_current t in

    let l_pkg = NV.Set.fold (fun nv l -> debpkg_of_nv action_k t nv :: l) available [] in

    match
      List.fold_left 
        (function 
          | None -> fun request -> 
            (match 
                Solver.resolve
                  (Solver.U l_pkg) 
                  request
                  (if action_k = `upgrade then t.reinstall else NV.Set.empty) 
             with
               | []  -> let _ = log "heuristic with no solution" in None
               | sol -> Some (apply_solutions t sol))
          | Some acc -> fun _ -> Some acc) None l_request
    with
      | None -> Globals.msg "No solution has been found.\n"
      | Some sol -> sol
end

let init repo alias ocaml_version cores =
  log "init %s" (Repository.to_string repo);
  let root = Path.G.create () in
  let config_f = Path.G.config root in
  if Filename.exists config_f then
    Globals.error_and_exit "%s already exist" (Filename.to_string config_f)
  else try
    let repo_p = Path.R.create repo in
    (* Create (possibly empty) configuration files *)
    let opam_version = OPAM_V.of_string Globals.opam_version in
    File.Config.write config_f (File.Config.create opam_version [repo] cores);
    File.Repo_index.write (Path.G.repo_index root) N.Map.empty;
    File.Repo_config.write (Path.R.config repo_p) repo;
    Repositories.init repo;
    Dirname.mkdir (Path.G.opam_dir root);
    Dirname.mkdir (Path.G.descr_dir root);
    Dirname.mkdir (Path.G.archives_dir root);
    Dirname.mkdir (Path.G.compilers_dir root);
    update_repo ~show_compilers:false;
    let ocaml_version = init_ocaml
      (fun alias_p -> 
        Globals.error_and_exit "%s does not exist whereas %s already exists" 
          (Filename.to_string config_f)
          (Dirname.to_string alias_p)) 
      alias
      false
      ocaml_version in
    update_package ~show_packages:false;
    let t = update_available_current (load_state ()) in
    let wish_install = Heuristic.get_packages t ocaml_version Heuristic.v_any in
    Heuristic.resolve `init t
      [ { wish_install
        ; wish_remove = [] 
        ; wish_upgrade = [] } ];

    print_env_warning ()

  with e ->
    if not !Globals.debug then
      Dirname.rmdir (Path.G.root root);
    raise e

let install names =
  log "install %s" (N.Set.to_string names);
  let t = update_available_current (load_state ()) in
  let map_installed = NV.to_map t.installed in

  let pkg_skip, pkg_new = N.Set.partition (fun name -> N.Map.mem name map_installed) names in

  (* Display a message if at least one package is already installed *)
  N.Set.iter 
    (fun name ->
      Globals.msg
        "Package %s is already installed (current version is %s)\n"
        (N.to_string name)
        (V.to_string (V.Set.choose_one (N.Map.find name map_installed))))
    pkg_skip;

  let pkg_new = Heuristic.nv_of_names t (N.Set.elements pkg_new) in

  (* Display a warning if at least one package contains
     dependencies to some unknown packages *)
  List.iter 
    (let available = NV.to_map (get_available_current t) in
     fun v_cstr ->
       match
         match v_cstr with
           | V_any (_, _, Some _) -> None (* We skip. An already installed package satisfies the dependency property. *)
           | V_any (n, s, None) -> Some (n, V.Set.choose s)
           | V_eq (n, v) -> Some (n, v)
       with
         | None -> ()
         | Some (n, v) -> 
           let opam = File.OPAM.read (Path.G.opam t.global (NV.create n v)) in
           let f_warn = 
             List.iter
               (fun ((n, _), _) -> 
                 if not (N.Map.mem (N.of_string n) available) then
                   Globals.warning "unknown package %S" n) in
           List.iter (List.iter f_warn)
             [ File.OPAM.depends opam
             ; File.OPAM.depopts opam ];
           f_warn (File.OPAM.conflicts opam))
    pkg_new;

  let pkg_installed = 
    N.Map.values (Heuristic.get_installed t (fun v set name -> V_any (name, set, v))) in

  Heuristic.resolve `install t
    (List.map 
       (fun (f_new, f_installed) -> 
         { wish_install = Heuristic.apply f_new pkg_new @ Heuristic.apply f_installed pkg_installed
         ; wish_remove  = []
         ; wish_upgrade = [] })
       (let open Heuristic in
        [ v_eq, v_eq_opt
        ; v_any, v_eq_opt
        ; v_eq, v_eq
        ; v_any, v_eq
        ; v_any, v_any ]))

let remove names =
  log "remove %s" (N.Set.to_string names);
  if N.Set.mem (N.of_string Globals.default_package) names then
    Globals.error_and_exit "Package %s can not be removed" Globals.default_package;
  let names = N.Set.elements names in
  let t = update_available_current (load_state ()) in
  let universe = Solver.U (NV.Set.fold (fun nv l -> (debpkg_of_nv `remove t nv) :: l) (get_available_current t) []) in
  let choose_any_v nv = 
    let n, v = match nv with 
      | V_any (n, set, None) -> n, V.Set.choose set
      | V_any (n, _, Some v)
      | V_eq (n, v) -> n, v in 
    NV.create n v in
  let wish_remove = Heuristic.nv_of_names t names in
  log "wish_remove=%s" (String.concat " " (List.map string_of_version_constraint wish_remove));
  let depends =
    Solver.filter_forward_dependencies ~depopts:true universe
      (Solver.P (List.rev_map
                   (fun nv -> debpkg_of_nv `remove t (choose_any_v nv)) 
                   wish_remove)) in
  let depends = NV.Set.of_list (List.rev_map NV.of_dpkg depends) in
  log "depends=%s" (NV.Set.to_string depends);
  let heuristic_apply = 
    let installed = List.filter (fun nv -> not (NV.Set.mem nv depends)) (NV.Set.elements t.installed) in
    fun f_heuristic ->
      List.rev_map (fun nv -> f_heuristic (NV.name nv) (NV.version nv)) installed in

  Heuristic.resolve `remove t
    (List.map 
       (let wish_remove = Heuristic.apply Heuristic.v_any wish_remove in
        fun f_h -> 
          { wish_install = heuristic_apply f_h
          ; wish_remove
          ; wish_upgrade = [] })
       [ Heuristic.vpkg_of_nv_eq
       ; fun n _ -> Heuristic.vpkg_of_nv_any n ])

let upgrade () =
  log "upgrade";
  let t = update_available_current (load_state ()) in
  Heuristic.resolve `upgrade t
    [ { wish_install = []
      ; wish_remove  = []
      ; wish_upgrade = N.Map.values (Heuristic.get_installed t Heuristic.v_ge) } ];
  Filename.remove (Path.C.reinstall t.compiler)

let upload upload repo =
  log "upload %s" (string_of_upload upload);
  let t = load_state () in
  let opam = File.OPAM.read upload.opam in
  let name = File.OPAM.name opam in
  let version = File.OPAM.version opam in
  let nv = NV.create name version in
  let repo = match repo with
  | None ->
      if N.Map.mem name t.repo_index then
        (* We upload the package to the first available repository. *)
        find_repository t (List.hd (N.Map.find name t.repo_index))
      else
        Globals.error_and_exit "No repository found to upload %s" (NV.to_string nv)
  | Some repo -> 
      if mem_repository t repo then
        find_repository t repo
      else
        Globals.error_and_exit "Unbound repository %S (available = %s)" 
          repo (string_of_repositories t) in
  let repo_p = List.assoc repo t.repositories in
  let upload_repo = Path.R.of_dirname (Path.R.upload_dir repo_p) in
  let upload_opam = Path.R.opam upload_repo nv in
  let upload_descr = Path.R.descr upload_repo nv in
  let upload_archives = Path.R.archive upload_repo nv in
  Filename.copy upload.opam upload_opam;
  Filename.copy upload.descr upload_descr;
  Filename.copy upload.archive upload_archives;
  Repositories.upload repo;
  Dirname.rmdir (Path.R.package upload_repo nv);
  Filename.remove (Path.R.archive upload_repo nv)

(* Return the transitive closure of dependencies *)
let get_transitive_dependencies ?(depopts = false) t names =
  let universe =
    Solver.U (List.map (debpkg_of_nv `config t) (NV.Set.elements t.installed)) in
  (* Compute the transitive closure of dependencies *)
  let pkg_of_name n = debpkg_of_nv `config t (find_installed_package_by_name t n) in
  let request = Solver.P (List.map pkg_of_name names) in
  let depends = Solver.filter_backward_dependencies ~depopts universe request in
  List.map NV.of_dpkg depends

let config request =
  log "config %s" (string_of_config request);
  let t = load_state () in

  match request with
  (* Display the compiler environment variables *)
  | Env -> print_env (get_env t)

  (* List all the available variables *)
  | List_vars ->
      let configs =
        NV.Set.fold (fun nv l ->
          let file = Path.C.config t.compiler (NV.name nv) in
          (nv, File.Dot_config.safe_read file) :: l
        ) t.installed [] in
      let variables =
        List.fold_left (fun accu (nv, c) ->
          let name = NV.name nv in
          (* add all the global variables *)
          let globals =
            List.fold_left (fun accu v ->
              (Full_variable.create_global name v, File.Dot_config.variable c v) :: accu
            ) accu (File.Dot_config.variables c) in
          (* then add the local variables *)
          List.fold_left
            (fun accu n ->
              let variables = File.Dot_config.Section.variables c n in
              List.fold_left (fun accu v ->
                (Full_variable.create_local name n v,
                 File.Dot_config.Section.variable c n v) :: accu
              ) accu variables
            ) globals (File.Dot_config.Section.available c)
        ) [] configs in
      List.iter (fun (fv, contents) ->
        Globals.msg "%-20s : %s\n"
          (Full_variable.to_string fv)
          (string_of_variable_contents contents)
      ) (List.rev variables)

  | Variable v ->
      let contents = contents_of_variable t v in
      Globals.msg "%s\n" (string_of_variable_contents contents)

  | Subst fs -> List.iter (substitute_file t) fs

  | Includes (is_rec, names) ->
      let deps =
        if is_rec then
          List.map NV.name (get_transitive_dependencies ~depopts:true t names)
        else
          names in
      let includes =
        List.fold_left (fun accu n ->
          "-I" :: Dirname.to_string (Path.C.lib t.compiler n) :: accu
        ) [] deps in
      Globals.msg "%s\n" (String.concat " " includes)

  | Compil c ->
      let oversion = current_ocaml_version t in
      let comp = File.Comp.read (Path.G.compiler t.global oversion) in
      let names =
        List.filter
          (fun n -> NV.Set.exists (fun nv -> NV.name nv = n) t.installed)
          (List.map (function (n, _), _ -> N.of_string n) (File.Comp.packages comp))
        @ List.map Full_section.package c.options in
      (* Compute the transitive closure of package dependencies *)
      let package_deps =
        if c.is_rec then
          List.map NV.name (get_transitive_dependencies ~depopts:true t names)
        else
          names in
      (* Map from libraries to package *)
      (* NOTES: we check that the set of packages/libraries given on
         the command line is consistent, ie. there isn't two libraries
         with the same name in the transitive closure of
         depedencies *)
      let library_map =
        List.fold_left (fun accu n ->
          let nv = find_installed_package_by_name t n in
          let opam = File.OPAM.read (Path.G.opam t.global nv) in
          let sections = (File.OPAM.libraries opam) @ (File.OPAM.syntax opam) in
          List.iter (fun s ->
            if Section.Map.mem s accu then
              Globals.error_and_exit "Conflict: the library %s appears in %s and %s"
                (Section.to_string s)
                (N.to_string n)
                (N.to_string (Section.Map.find s accu))
          ) sections;
          List.fold_left (fun accu s -> Section.Map.add s n accu) accu sections
        ) Section.Map.empty package_deps in
      (* Compute the transitive closure of libraries dependencies *)
      let library_deps =
        let graph = Section.G.create () in
        let todo = ref Section.Set.empty in
        let add_todo s =
          if Section.Map.mem s library_map then
            todo := Section.Set.add s !todo
          else
            Globals.error_and_exit "Unbound section %S" (Section.to_string s) in
        let seen = ref Section.Set.empty in
        (* Init the graph with vertices from the command-line *)
        (* NOTES: we check that [todo] is initialized before the [loop] *)
        List.iter (fun s ->
          let name = Full_section.package s in
          let sections = match Full_section.section s with
            | None   ->
                let config = File.Dot_config.safe_read (Path.C.config t.compiler name) in
                File.Dot_config.Section.available config
            | Some s -> [s] in
          List.iter (fun s ->
            Section.G.add_vertex graph s;
            add_todo s;
          ) sections
        ) c.options;
        (* Also add the [requires] field of the compiler description *)
        List.iter (fun s ->
          Section.G.add_vertex graph s;
          add_todo s
        ) (File.Comp.requires comp);
        (* Least fix-point to add edges and missing vertices *)
        let rec loop () =
          if not (Section.Set.is_empty !todo) then
            let s = Section.Set.choose !todo in
            todo := Section.Set.remove s !todo;
            seen := Section.Set.add s !seen;
            let name = Section.Map.find s library_map in
            let config = File.Dot_config.safe_read (Path.C.config t.compiler name) in
            let childs = File.Dot_config.Section.requires config s in
            (* keep only the build reqs which are in the package dependency list
               and the ones we haven't already seen *)
            let childs =
              List.filter (fun s ->
                Section.Map.mem s library_map && not (Section.Set.mem s !seen)
              ) childs in
            List.iter (fun child ->
                Section.G.add_vertex graph child;
                Section.G.add_edge graph child s;
                todo := Section.Set.add child !todo;
            ) childs;
            loop ()
        in
        loop ();
        let nodes = ref [] in
        Section.graph_iter (fun n -> nodes := n :: !nodes) graph;
        !nodes in
      let fn_comp = match c.is_byte, c.is_link with
        | true , true  -> File.Comp.bytelink
        | true , false -> File.Comp.bytecomp
        | false, true  -> File.Comp.asmlink
        | false, false -> File.Comp.asmcomp in
      let fn = match c.is_byte, c.is_link with
        | true , true  -> File.Dot_config.Section.bytelink
        | true , false -> File.Dot_config.Section.bytecomp
        | false, true  -> File.Dot_config.Section.asmlink
        | false, false -> File.Dot_config.Section.asmcomp in
      let strs =
        fn_comp comp ::
        List.fold_left (fun accu s ->
          let name = Section.Map.find s library_map in
          let config = File.Dot_config.read (Path.C.config t.compiler name) in
          fn config s :: accu
        ) [] library_deps in
      let output = String.concat " " (List.flatten strs) in
      log "OUTPUT: %S" output;
      Globals.msg "%s\n" output

let remote action =
  log "remote %s" (string_of_remote action);
  let t = load_state () in
  let repos = File.Config.repositories t.config in
  let update_config repos =
    let new_config = File.Config.with_repositories t.config repos in
    File.Config.write (Path.G.config t.global) new_config in
  match action with
  | List  ->
      let pretty_print r =
        Globals.msg "| %-10s| %-40s| %-10s |\n"
          (Repository.name r)
          (Dirname.to_string (Repository.address r))
          (Repository.kind r) in
      let line = String.make 68 '-' in
      line.[0] <- '|'; line.[12] <- '|'; line.[54] <- '|'; line.[67] <- '|';
      Globals.msg "%s\n| %-10s| %-40s| %-10s |\n%s\n"
        line "NAME" "ADDRESS" "KIND" line;
      List.iter pretty_print repos;
      Globals.msg "%s\n" line
  | Add repo ->
      let name = Repository.name repo in
      if List.exists (fun r -> Repository.name r = name) repos then
        Globals.error_and_exit "%s is already a remote repository" name
      else (
        (try Repositories.init repo with Repositories.Unknown_backend ->
          Globals.error_and_exit "\"%s\" is not a supported backend" (Repository.kind repo));
        log "Adding %s" (Repository.to_string repo);
        update_config (repo :: repos)
      );
      update ()
  | Rm n  ->
      let repo =
        try List.find (fun r -> Repository.name r = n) repos
        with Not_found ->
          Globals.error_and_exit "%s is not a remote index" n in
      update_config (List.filter ((!=) repo) repos);
      let repo_index =
        N.Map.fold (fun n repo_s repo_index ->
          let repo_s = List.filter (fun r -> r <> Repository.name repo) repo_s in
          match repo_s with
          | [] -> repo_index
          | _  -> N.Map.add n repo_s repo_index
        ) t.repo_index N.Map.empty in
        File.Repo_index.write (Path.G.repo_index t.global) repo_index;
      Dirname.rmdir (Path.R.root (Path.R.create repo))

let pin action =
  log "pin %s" (string_of_pin action);
  let t = load_state () in
  let pin_f = Path.C.pinned t.compiler in
  let pins = File.Pinned.safe_read pin_f in
  let update_config pins = File.Pinned.write pin_f pins in
  let name = action.pin_package in
  if mem_installed_package_by_name t name then (
    let reinstall_f = Path.C.reinstall t.compiler in
    let reinstall = File.Reinstall.safe_read reinstall_f in
    let nv = find_installed_package_by_name t name in
    File.Reinstall.write reinstall_f (NV.Set.add nv reinstall)
  );
  match action.pin_arg with
  | Unpin -> update_config (N.Map.remove name pins)
  | _     ->
      if N.Map.mem name pins then (
        let current = N.Map.find name pins in
        Globals.error_and_exit "Cannot pin %s to %s, it is already associated to %s."
          (N.to_string name)
          (string_of_pin_option current)
          (string_of_pin_option action.pin_arg);
      );
      log "Adding %s => %s" (string_of_pin_option action.pin_arg) (N.to_string name);
      update_config (N.Map.add name action.pin_arg pins)

let pin_list () =
  log "pin_list";
  let t = load_state () in
  let pins = File.Pinned.safe_read (Path.C.pinned t.compiler) in
  let print n a = Globals.msg "%-20s %s\n" (N.to_string n) (string_of_pin_option a) in
  N.Map.iter print pins

let compiler_list () =
  log "compiler_list";
  let t = load_state () in
  let descrs = Path.G.available_compilers t.global in
  let aliases = File.Aliases.read (Path.G.aliases t.global) in
  Globals.msg "--- Compilers installed ---\n";
  List.iter (fun (n,c) ->
    let current = if n = File.Config.ocaml_version t.config then "*" else " " in
    Globals.msg "%s %s (%s)\n" current (Alias.to_string n) (OCaml_V.to_string c)
  ) aliases;
  Globals.msg "\n--- Compilers available ---\n";
  OCaml_V.Set.iter (fun c ->
    let comp = File.Comp.read (Path.G.compiler t.global c) in
    let preinstalled = if File.Comp.preinstalled comp then "~" else " " in
    Globals.msg "%s  %s\n" preinstalled (OCaml_V.to_string c)
  ) descrs
  
let switch clone alias ocaml_version =
  log "switch %B %s %s" clone
    (Alias.to_string alias)
    (OCaml_V.to_string ocaml_version);
  let t = load_state () in

  (* install the new OCaml version *)
  let ocaml_version, exists = 
    let exists = ref false in
    let ocaml_version = 
      init_ocaml (fun _ -> exists := true) (Some alias) true (Some ocaml_version) in
    ocaml_version, !exists in

  (* install new package
     - the packages specified in the compiler description file if
       the compiler was not previously installed
     - also attempt to replicate the previous state, if required
       with -clone *)
  let t_new = update_available_current (load_state ()) in

  let comp_constraints =
    if exists then
      Heuristic.get_installed t_new Heuristic.v_eq
    else
      N.Map.of_list
        (List.rev_map
           (function (name, _), _ as nv -> N.of_string name, nv)
           (Heuristic.get_packages t_new ocaml_version Heuristic.v_eq)) in

  let clone_constraints =
    if clone then 
      (* we filter from "futur requested packages", 
         packages that are present in the OLD version of OCaml
         and absent in the NEW version of OCaml *)
      let available = NV.to_map (get_available_current t_new) in
      let new_installed = NV.to_map t_new.installed in
      N.Map.mapi
        (fun n _ -> 
          Heuristic.v_eq_opt
            (if N.Map.mem n new_installed then
                Some (V.Set.choose_one (N.Map.find n new_installed)) 
             else
                None)
            (N.Map.find n available) 
            n)
        (NV.to_map t.installed)
    else N.Map.empty in

  let all_constraints = 
    N.Map.merge_max
      (fun pkg p_clone p_comp ->
        (* NOTE 
           - both [p_clone] and [p_comp] constraints are valid
           - the intersection of these 2 constraints should not be empty *)
        if p_clone = p_comp then 
          Some p_comp
        else
          let () = Globals.warning "package %s : we reject the constraint to clone %s and we take the constraint from compiler %s" 
            (N.to_string pkg) 
            (string_of_atom_formula p_clone)
            (string_of_atom_formula p_comp) in
          (* we arbitrarily take the constraint from the compiler *)
          Some p_comp)
      clone_constraints
      comp_constraints in

  Heuristic.resolve `switch t_new
    [ { wish_install = N.Map.values all_constraints
      ; wish_remove = [] 
      ; wish_upgrade = [] } ];

  print_env_warning ()

(** We protect each main functions with a lock depending on its access
on some read/write data. *)

let list print_short installed pkg_str =
  check (Read_only (fun () -> list print_short installed pkg_str))

let info package =
  check (Read_only (fun () -> info package))

let config request =
  check (Read_only (fun () -> config request))

let install name =
  check (Write_lock (fun () -> install name))

let update () =
  check (Write_lock update)

let upgrade () =
  check (Write_lock upgrade)

let upload u r =
  check (Write_lock (fun () -> upload u r))

let remove name =
  check (Write_lock (fun () -> remove name))

let remote action =
  check (Write_lock (fun () -> remote action))

let switch clone alias ocaml_version =
  check (Write_lock (fun () -> switch clone alias ocaml_version))

let compiler_list () =
  check (Read_only compiler_list)

let pin action =
  check (Write_lock (fun () -> pin action))

let pin_list () =
  check (Read_only pin_list)
