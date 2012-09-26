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
  repositories: repository list;

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
    let s r =
      let p = Path.R.create r in
      Printf.sprintf "%s:%s"
        (Repository.to_string r)
        (Dirname.to_string (Path.R.root p)) in
    String.concat ", " (List.map s r) in
  log "GLOBAL    : %s" (Dirname.to_string (Path.G.root t.global));
  log "COMPILER  : %s" (Dirname.to_string (Path.C.root t.compiler));
  log "REPO      : %s" (string_of_repos t.repositories);
  log "AVAILABLE : %s" (NV.Set.to_string t.available);
  log "AV_CURRENT: %s" (match t.available_current with None -> "<none>" | Some set -> NV.Set.to_string set);
  log "INSTALLED : %s" (NV.Set.to_string t.installed);
  log "REINSTALL : %s" (NV.Set.to_string t.reinstall);
  log "REPO_INDEX: %s" (N.Map.to_string (String.concat ",") t.repo_index)

let current_ocaml_version t =
  let alias = File.Config.ocaml_version t.config in
  let aliases = File.Aliases.safe_read (Path.G.aliases t.global) in
  log "current_ocaml_version %s" (Alias.to_string alias);
  try Some (List.assoc alias aliases)
  with Not_found -> None

let confirm fmt =
  Printf.ksprintf (fun msg ->
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
    (* Remove the packages which does not fullfil the compiler constraints *)
    let ocaml_version =
      let opam_version = File.Config.ocaml_version t.config in
      let default_version = Alias.of_string Globals.default_compiler_version in
      if opam_version = default_version then (
        let current = OCaml_V.current () in
        let system = File.Config.system_ocaml_version t.config in
        match current, system  with
        | None  , None   -> Globals.error_and_exit "No OCaml compiler installed."
        | None  , Some s ->
            if not (confirm "No OCaml compiler found. Continue ?") then
              Globals.exit 0
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
        match current_ocaml_version t with
        | Some v -> v
        | None   -> Globals.error_and_exit "No OCaml compiler defined."in
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
    | None   -> assert false
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
  let repo_index =
    let repo_index = File.Repo_index.safe_read (Path.G.repo_index global) in
    let l_wrong =
      List.fold_left (fun accu (n, repo_s) ->
        List.fold_left (fun accu repo ->
          if List.for_all (fun r -> Repository.name r <> repo) repositories then
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

let mem_repository t name =
  List.exists (fun r -> Repository.name r = name) t.repositories

let find_repository t name =
  List.find (fun r -> Repository.name r = name) t.repositories

let string_of_repositories t =
  String.concat ", " (List.map (fun r -> Repository.name r) t.repositories)

let mem_installed_package_by_name t name =
  not (NV.Set.is_empty (NV.Set.filter (fun nv -> NV.name nv = name) t.installed))

let find_installed_package_by_name t name =
  try NV.Set.find (fun nv -> NV.name nv = name) t.installed
  with Not_found ->
    Globals.error_and_exit "Package %s is not installed" (N.to_string name)

let find_available_package_by_name t name =
  let s = NV.Set.filter (fun nv -> NV.name nv = name) (get_available_current t) in
  if NV.Set.is_empty s then
    None
  else
    Some s

let print_updated t updated pinned_updated =
  let new_packages =
    NV.Set.filter (fun nv -> not (NV.Set.mem nv t.installed)) updated in
  let updated_packages =
    let aux set = NV.Set.filter (fun nv -> NV.Set.mem nv t.installed) set in
    NV.Set.union (aux updated) (aux pinned_updated) in
  if not (NV.Set.is_empty new_packages) then (
    if NV.Set.cardinal new_packages = 1 then
      Globals.msg "The following NEW package is available:\n"
    else
      Globals.msg "The following NEW packages are available:\n";
    NV.Set.iter (fun nv ->
      Globals.msg " - %s\n" (NV.to_string nv)
    ) new_packages;
  );
  if not (NV.Set.is_empty updated_packages) then (
    if NV.Set.cardinal updated_packages = 1 then
      Globals.msg "The following package needs to be upgraded:\n"
    else
      Globals.msg "The following packages need to be upgraded:\n";
    NV.Set.iter (fun nv ->
      if NV.Set.mem nv pinned_updated then
        Globals.msg " - %s\n" (N.to_string (NV.name nv))
      else
        Globals.msg " - %s\n" (NV.to_string nv)
    ) updated_packages
  )

let print_compilers t compilers repo =
  let repo_p = Path.R.create repo in
  let repo_compilers = Path.R.available_compilers repo_p in
  let (--) = OCaml_V.Set.diff in
  let new_compilers = repo_compilers -- compilers in
  if not (OCaml_V.Set.is_empty new_compilers) then
    Globals.msg "New compiler descriptions available:\n";
  OCaml_V.Set.iter (fun v ->
    Globals.msg " - %s\n" (OCaml_V.to_string v)
  ) new_compilers;
  let all_compilers =
    List.fold_left (fun set repo ->
      let repo_p = Path.R.create repo in
      let repo_compilers = Path.R.available_compilers repo_p in
      OCaml_V.Set.union repo_compilers set;
    ) OCaml_V.Set.empty t.repositories in
  let default = OCaml_V.of_string Globals.default_compiler_version in
  let del_compilers = compilers -- all_compilers -- (OCaml_V.Set.singleton default) in
  if not (OCaml_V.Set.is_empty del_compilers) then
    Globals.msg "Some compilers are not available anymore:\n";
  OCaml_V.Set.iter (fun v ->
    Globals.msg " - %s\n" (OCaml_V.to_string v)
  ) del_compilers

let install_conf_ocaml_config t =
  let name = N.of_string Globals.default_package in
  (* .config *)
  let vars =
    let map f l = List.map (fun (s,p) -> Variable.of_string s, S (f p)) l in
    let id x = x in

    map Dirname.to_string
      [
        ("prefix", Path.C.root t.compiler);
        ("lib", Path.C.lib_dir t.compiler);
        ("bin", Path.C.bin t.compiler);
        ("doc", Path.C.doc_dir t.compiler);
        ("stublibs", Path.C.stublibs t.compiler);
        ("toplevel", Path.C.toplevel t.compiler);
        ("man", Path.C.man_dir t.compiler);
      ]
    @ map id [
      ("user" , try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name with _ -> "user");
      ("group", try (Unix.getgrgid (Unix.getgid ())).Unix.gr_name with _ -> "group");
      ("make" , !Globals.makecmd);
      ("os"   , Globals.os_string);
      ("enable", File.Subst.enable);
      ("disable", File.Subst.disable);
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

let uninstall_conf_ocaml () =
  let t = load_state () in
  let name = N.of_string Globals.default_package in
  let version = V.of_string (Alias.to_string (File.Config.ocaml_version t.config)) in
  let nv = NV.create name version in
  Filename.remove (Path.G.opam t.global nv);
  Filename.remove (Path.G.descr t.global nv)

let reinstall_conf_ocaml () =
  uninstall_conf_ocaml ();
  install_conf_ocaml ()

let update_repo_index t =

  (* Update repo_index *)
  let repo_index =
    List.fold_left (fun repo_index r ->
      let p = Path.R.create r in
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

  (* suppress previous links, but keep metadata of installed packages
     (because you need them to uninstall the package) *)
  let all_installed =
    Alias.Set.fold (fun alias accu ->
      let installed_f = Path.C.installed (Path.C.create alias) in
      let installed = File.Installed.safe_read installed_f in
      NV.Set.union installed accu
    ) (Path.G.available_aliases t.global) NV.Set.empty in
  NV.Set.iter (fun nv ->
    if not (NV.Set.mem nv all_installed) then (
      let opam_g = Path.G.opam t.global nv in
      let descr_g = Path.G.descr t.global nv in
      let archive_g = Path.G.archive t.global nv in
      Filename.remove opam_g;
      Filename.remove descr_g;
      Filename.remove archive_g;
    );
  ) (Path.G.available_packages t.global);

  reinstall_conf_ocaml ();

  (* Create symbolic links from $repo dirs to main dir *)
  N.Map.iter (fun n repo_s ->
    let all_versions = ref V.Set.empty in
    List.iter (fun r ->
      let repo = find_repository t r in
      let repo_p = Path.R.create repo in
      let available_versions = Path.R.available_versions repo_p n in
      V.Set.iter (fun v ->
        if not (V.Set.mem v !all_versions) then (
          all_versions := V.Set.add v !all_versions;
          let nv = NV.create n v in
          let opam_g = Path.G.opam t.global nv in
          let descr_g = Path.G.descr t.global nv in
          let archive_g = Path.G.archive t.global nv in
          let opam_r = Path.R.opam repo_p nv in
          let descr_r = Path.R.descr repo_p nv in
          let archive_r = Path.R.archive repo_p nv in
          (* clean-up previous versions *)
          Filename.remove opam_g;
          Filename.remove descr_g;
          Filename.remove archive_g;
          (* update global files *)
          if Filename.exists opam_r then (
            Filename.link opam_r opam_g;
            if Filename.exists descr_r then
              Filename.link descr_r descr_g;
            if Filename.exists archive_r then
              Filename.link archive_r archive_g;
          )
        )
      ) available_versions
    ) repo_s
  ) repo_index

let base_packages = [ "base-unix"; "base-bigarray"; "base-threads" ]

let create_default_compiler_description t =
  let ocaml_version = OCaml_V.of_string Globals.default_compiler_version in
  let mk name = ((name,None),None) in
  let f =
    File.Comp.create_preinstalled
      ocaml_version
      (List.map mk (if !Globals.base_packages then base_packages else []))
      [ ("CAML_LD_LIBRARY_PATH", "=",
           "%{lib}%/stublibs"
           ^ ":" ^
           (match Run.system_ocamlc_where () with
           | Some d -> Stdlib_filename.concat d "stublibs"
           | None   -> assert false))
      ] in
  let comp = Path.G.compiler t.global ocaml_version in
  File.Comp.write comp f

let update_repositories t ~show_compilers repos =
  log "update_repositories";
  let compilers = Path.G.available_compilers t.global in

  (* first update all the given repositories *)
  List.iter (fun r -> Repositories.update r) repos;

  (* Display the new compilers available *)
  List.iter (fun r -> if show_compilers then print_compilers t compilers r) repos;

  (* XXX: we could have a special index for compiler descriptions as
  well, but that's become a bit too heavy *)
  OCaml_V.Set.iter (fun comp ->
    if OCaml_V.to_string comp <> Globals.default_compiler_version
    && not (List.exists (fun (_,c) -> comp = c) t.aliases) then (
      let comp_f = Path.G.compiler t.global comp in
      Filename.remove comp_f;
    )
  ) (Path.G.available_compilers t.global);
  List.iter (fun repo ->
    let repo_p = Path.R.create repo in
    let comps = Path.R.available_compilers repo_p in
    let comp_dir = Path.G.compilers_dir t.global in
    OCaml_V.Set.iter (fun o ->
      let comp_g = Path.G.compiler t.global o in
      let comp_f = Path.R.compiler repo_p o in
      if not (Filename.exists comp_g) && Filename.exists comp_f then
        Filename.link_in comp_f comp_dir
    ) comps
  ) t.repositories;
  (* If system.comp has been deleted, create it *)
  let default_compiler =
    Path.G.compiler t.global (OCaml_V.of_string Globals.default_compiler_version) in
  if not (Filename.exists default_compiler) then
    create_default_compiler_description t

let update_packages t ~show_packages repos =
  log "update_packages";
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
                  try
                    let lines = Run.read_command_output
                      [ "rsync"; "-arv"; "--exclude"; "'.git/*'"; Dirname.to_string p ^ "/"; Dirname.to_string build ] in
                    match Utils.rsync_trim lines with
                    | [] -> None
                    | l  -> Some nv
                  with _ ->
                    None
                else
                  None
              else
                None
          | _ -> None)
        (N.Map.bindings t.pinned)) in

  (* then update $opam/repo/index *)
  update_repo_index t;
  let t = load_state () in
  let repos = Repository.Set.of_list repos in
  let updated =
    N.Map.fold (fun n repo_s accu ->
      (* we do not try to upgrade pinned packages *)
      if N.Map.mem n t.pinned then
        accu
      else (
        let all_versions = ref V.Set.empty in
        List.fold_left (fun accu r ->
          let repo = find_repository t r in
          let repo_p = Path.R.create repo in
          let available_versions = Path.R.available_versions repo_p n in
          let new_versions = V.Set.diff available_versions !all_versions in
          log "repo=%s n=%s new_versions= %s" r (N.to_string n) (V.Set.to_string new_versions);
          if not (V.Set.is_empty new_versions) then (
            all_versions := V.Set.union !all_versions new_versions;
            let all_updated = File.Updated.safe_read (Path.R.updated repo_p) in
            let updated =
              NV.Set.filter (fun nv ->
                NV.name nv = n && V.Set.mem (NV.version nv) new_versions
              ) all_updated in
            if Repository.Set.mem repo repos then
              NV.Set.union updated accu
            else
              accu
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
    let map_b b = List.map (List.map (fun s -> b, s)) in
    let depends = map_b true (File.OPAM.depends opam) in
    let depopts = map_b false (File.OPAM.depopts opam) in
    List.iter (List.iter (fun (raise_err, ((d,_),_)) ->
      match find_available_package_by_name t (N.of_string d) with
        | None   ->
            if raise_err then
              let _ = Globals.error
                "Package %s depends on the unknown package %s"
                (NV.to_string nv) d in
              has_error := true
            else
              Globals.warning
                "Package %s depends optionally on the unknown package %s"
                (NV.to_string nv) d
        | Some _ -> ()
    )) (depends @ depopts)
  ) (get_available_current t);
  if !has_error then
    Globals.exit 1

(* Return the contents of a fully qualified variable *)
let contents_of_variable t v =
  let name = Full_variable.package v in
  let var = Full_variable.variable v in
  let name_str = N.to_string name in
  let var_str = Variable.to_string var in
  let read_var () =
    let c = File.Dot_config.safe_read (Path.C.config t.compiler name) in
    try match Full_variable.section v with
      | None   -> File.Dot_config.variable c var
      | Some s -> File.Dot_config.Section.variable c s var
    with Not_found ->
      Globals.error_and_exit "%s is not defined" (Full_variable.to_string v) in
  if N.to_string name = Globals.default_package then (
    try S (Sys.getenv var_str)
    with Not_found ->
      match current_ocaml_version t with
      | None -> read_var ()
      | Some ocaml_version ->
        if var_str = "ocaml-version" then (
          let ocaml_version_str = OCaml_V.to_string ocaml_version in
          if ocaml_version_str = Globals.default_compiler_version then
            match File.Config.system_ocaml_version t.config with
            | None   -> S "<none>"
            | Some v -> S (OCaml_V.to_string v)
          else
            S ocaml_version_str
        ) else if var_str = "preinstalled" then (
          let comp = File.Comp.read (Path.G.compiler t.global ocaml_version) in
          B (File.Comp.preinstalled comp)
        ) else
          read_var ()
) else (
    try S (Sys.getenv (name_str ^"_"^ var_str))
    with Not_found ->
      let installed = mem_installed_package_by_name t name in
      if var = Variable.enable && installed then
        S File.Subst.enable
      else if var = Variable.enable && not installed then
        S File.Subst.disable
      else if var = Variable.installed then
        B installed
      else if not installed then
        Globals.error_and_exit "Package %s is not installed" (N.to_string name)
      else
        read_var ()
  )

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

let rec substitute_filter t = function
  | Bool b    -> Bool b
  | String s  -> String (substitute_string t s)
  | Op(e,s,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    Op(e, s, f)
  | And (e,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    And(e,f)
  | Or(e,f) ->
    let e = substitute_filter t e in
    let f = substitute_filter t f in
    Or(e,f)

let substitute_arg t (a, f) =
  let a = substitute_string t a in
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
  | Bool b -> string_of_bool b
  | String s -> s
  | Op(e,s,f) ->
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
  | Or(e,f)  ->
    if eval_filter t e = "true"
    || eval_filter t f = "true"
    then "true" else "false"
  | And(e,f) ->
    if eval_filter t e = "true"
    && eval_filter t f = "true"
    then "true" else "false"

let eval_filter t = function
  | None   -> true
  | Some f -> eval_filter t f = "true"

let filter_arg t (a,f) =
  if eval_filter t f then
    Some a
  else
    None

let filter_command t (l, f) =
  if eval_filter t f then
    match Utils.filter_map (filter_arg t) l with
    | [] -> None
    | l  -> Some l
  else
    None

let filter_commands t l =
  Utils.filter_map (filter_command t) l

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

(* - compiles and install $opam/compiler/[ocaml_version].comp in $opam/[alias]
   - update $opam/alias
   - update $opam/config *)
let init_ocaml t quiet alias ocaml_version =
  log "init_ocaml alias=%s ocaml_version=%s"
    (Alias.to_string alias)
    (OCaml_V.to_string ocaml_version);

  if ocaml_version = OCaml_V.of_string Globals.default_compiler_version then
    create_default_compiler_description t;

  let comp_f = Path.G.compiler t.global ocaml_version in
  if not (Filename.exists comp_f) then (
    Globals.msg "Cannot find %s: %s is not a valid compiler name.\n"
      (Filename.to_string comp_f)
      (OCaml_V.to_string ocaml_version);
    Globals.exit 0;
  );

  let alias_p = Path.C.create alias in
  let alias_p_dir = Path.C.root alias_p in
  if Dirname.exists alias_p_dir then (
    Globals.msg "The compiler %s is already installed.\n" (Alias.to_string alias);
    Globals.exit 0;
  );
  Dirname.mkdir alias_p_dir;

  let comp = File.Comp.read comp_f in
  begin try
    if not (File.Comp.preinstalled comp) then begin

      Globals.verbose := not quiet;

      (* Install the compiler *)
      let comp_src = match File.Comp.src comp with
        | Some f -> f
        | None   ->
          Globals.error_and_exit
            "No source for compiler %s"
            (OCaml_V.to_string ocaml_version) in
      let build_dir = Path.C.build_ocaml alias_p in
      let comp_src_raw = Filename.to_string comp_src in
      if Sys.file_exists comp_src_raw && Sys.is_directory comp_src_raw then
        Dirname.link (Dirname.of_string comp_src_raw) build_dir
      else if Sys.file_exists comp_src_raw then
        Filename.extract comp_src build_dir
      else Dirname.with_tmp_dir (fun download_dir ->
        let file = Filename.download comp_src download_dir in
        Filename.extract file build_dir;
      );
      let patches = File.Comp.patches comp in
      let patches = List.map (fun f -> Filename.download f build_dir) patches in
      List.iter (fun f -> Filename.patch f build_dir) patches;
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
      end;
    end;

    (* write the new version in the configuration file *)
    let config = File.Config.with_ocaml_version t.config alias in
    File.Config.write (Path.G.config t.global) config;
    add_alias alias ocaml_version

  with e ->
    if not !Globals.debug then
      Dirname.rmdir alias_p_dir;
    raise e
  end

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

let unknown_package name version =
  match version with
  | None   -> Globals.error_and_exit "%S is not a valid package.\n" (N.to_string name)
  | Some v -> Globals.error_and_exit "The package %S has no version %s." (N.to_string name) (V.to_string v)

let unavailable_package name version =
  match version with
  | None   -> Globals.error_and_exit "%S is not available for your compiler.\n" (N.to_string name)
  | Some v -> Globals.error_and_exit "Version %s of %S is incompatible with your compiler." (V.to_string v) (N.to_string name)

let list ~print_short ~installed_only ?(name_only = true) ?(case_sensitive = false) res =
  log "list";
  let t = load_state () in
  let res =
    Utils.filter_map (fun re ->
      try Some (Re.compile (let re = Re_glob.globx re in
                            if case_sensitive then re else Re.no_case re))
      with Re_glob.Parse_error ->
        Globals.error "\"%s\" is not a valid package descriptor" re;
        None
    ) res in
  let exact_match str =
    List.exists (fun re -> Utils.exact_match re str) res in
  let partial_match str =
    List.exists (fun re -> Re.execp re str) res in
  (* Get all the installed packages *)
  let installed = File.Installed.read (Path.C.installed t.compiler) in
  let map, max_n, max_v =
    NV.Set.fold
      (fun nv (map, max_n, max_v) ->
        let name = NV.name nv in
        let version = NV.version nv in
        if N.Map.mem name map (* If the packet has been processed yet *)
        (* And the version processed was the installed version. *)
        && fst (N.Map.find name map) <> None
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
      (N.Map.empty, min_int, String.length s_not_installed) in
  let map =
    N.Map.filter (fun name (version, descr) ->
      (* installp *) (not installed_only || version <> None)
      (* allp     *) && (res = []
      (* namep    *)  || name_only && exact_match (N.to_string name)
      (* descrp   *)  || not name_only && (partial_match (N.to_string name) || partial_match descr))
    ) map in
  let alias = File.Config.ocaml_version t.config in
  if not print_short then (
    let kind = if installed_only then "Installed" else "Available" in
    Globals.msg "%s packages for %s:\n" kind (Alias.to_string alias);
  );
  N.Map.iter (
    if print_short then
      fun name _ -> Globals.msg "%s " (N.to_string name)
    else
      fun name (version, description) ->
        let name = N.to_string name in
        let version = match version with
          | None   -> s_not_installed
          | Some v -> V.to_string v in
        Globals.msg "%s  %s  %s\n"
          (indent_left name max_n)
          (indent_right version max_v)
          description
  ) map

let info package =
  log "info %s" (N.to_string package);
  let t = load_state () in

  let o_v =
    let installed = File.Installed.read (Path.C.installed t.compiler) in
    try Some (V.Set.choose_one (N.Map.find package (NV.to_map installed)))
    with Not_found -> None in

  let v_set =
    let v_set = Path.G.available_versions t.global package in
    if V.Set.is_empty v_set then
      unknown_package package None
    else
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
  let build_dir = Path.C.build t.compiler nv in
  if Dirname.exists build_dir then Dirname.in_dir build_dir (fun () ->

    Globals.msg "Installing %s ...\n" (NV.to_string nv);
    let t = load_state () in
    let name = NV.name nv in
    let opam_f = Path.G.opam t.global nv in
    let opam = File.OPAM.read opam_f in
    let config_f = Path.C.build_config t.compiler nv in
    let config = File.Dot_config.safe_read config_f in
    let install_f = Path.C.build_install t.compiler nv in
    let install = File.Dot_install.safe_read install_f in

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
    if not (Filename.exists config_f)
    && (File.OPAM.libraries opam <> [] || File.OPAM.syntax opam <> []) then
      Globals.error_and_exit
        "%s does not exist but %s defines some libraries and syntax extensions"
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
    let warnings = ref [] in
    let check f dst =
      if not f.optional && not (Filename.exists f.c) then (
        warnings := (f.c, dst) :: !warnings
      );
      Filename.exists f.c in
    let lib = Path.C.lib t.compiler name in
    List.iter (fun f ->
      if check f lib then
        Filename.copy_in f.c lib
    ) (File.Dot_install.lib install);

    (* toplevel *)
    let toplevel = Path.C.toplevel t.compiler in
    List.iter (fun f ->
      if check f toplevel then
        Filename.copy_in f.c toplevel
    ) (File.Dot_install.toplevel install);

    (* bin *)
    List.iter (fun (src, dst) ->
      let dst = Path.C.bin t.compiler // (Basename.to_string dst) in
      (* WARNING [dst] could be a symbolic link (in this case, it will be removed). *)
      if check src  (Path.C.bin t.compiler) then
        Filename.copy src.c dst;
    ) (File.Dot_install.bin install);

    (* misc *)
    List.iter
      (fun (src, dst) ->
        if Filename.exists dst && confirm "Overwriting %s ?" (Filename.to_string dst) then
          Filename.copy src.c dst
        else begin
          Globals.msg "Installing %s to %s.\n" (Filename.to_string src.c) (Filename.to_string dst);
          if confirm "Continue ?" then
            Filename.copy src.c dst
        end
      ) (File.Dot_install.misc install);

    if !warnings <> [] then (
      let print (f, dst) = Printf.sprintf " - %s in %s" (Filename.to_string f) (Dirname.to_string dst) in
      Globals.error
        "Error while installing the following files:\n%s"
        (String.concat "\n" (List.map print !warnings));
      Globals.exit 2;
    )
  )

let pinned_path t nv =
  let name = NV.name nv in
  if N.Map.mem name t.pinned then
    match N.Map.find name t.pinned with
    | Path p -> Some p
    | _      -> None
  else
    None

let find_repo t nv =
  log "find_repo %s" (NV.to_string nv);
  let name = NV.name nv in
  let rec aux = function
    | []          -> None
    | r :: repo_s ->
        let repo = find_repository t r in
        let repo_p = Path.R.create repo in
        let opam_f = Path.R.opam repo_p nv in
        if Filename.exists opam_f then (
          Some (repo_p, repo)
        ) else
          aux repo_s in
  if N.Map.mem name t.repo_index then
    aux (N.Map.find name t.repo_index)
  else
    None

let mem_repo t nv =
  find_repo t nv <> None

let with_repo t nv fn =
  match find_repo t nv with
  | None ->
    Globals.error_and_exit
      "Unable to find a repository containing %s"
      (NV.to_string nv)
  | Some (repo_p, repo) -> fn repo_p repo

let get_archive t nv =
  let aux repo_p repo =
    Repositories.download repo nv;
    let src = Path.R.archive repo_p nv in
    let dst = Path.G.archive t.global nv in
    if Filename.exists src then (
      Filename.link src dst;
      Some dst
    ) else
      None in
  with_repo t nv aux

let get_files t nv =
  let aux repo_p _ =
    Path.R.available_files repo_p nv in
  with_repo t nv aux

let extract_package t nv =
  log "extract_package: %s" (NV.to_string nv);
  let p_build = Path.C.build t.compiler nv in
  Dirname.rmdir p_build;
  match pinned_path t nv with
  | None   ->
      (match get_archive t nv with
      | None         -> None
      | Some archive ->
        Globals.msg "Extracting %s ...\n" (Filename.to_string archive);
        Filename.extract archive p_build;
        Some p_build)
  | Some p ->
      (* XXX: make it a bit more generic ... *)
      Globals.msg "Synchronizing %s with %s ...\n" (NV.to_string nv) (Dirname.to_string p);
      Run.command [ "rsync"; "-arv"; "--exclude"; "'.git/*'"; Dirname.to_string p ^ "/"; Dirname.to_string p_build ];
      let files = get_files t nv in
      List.iter (fun f -> Filename.copy_in f p_build) files;
      Some p_build

let proceed_todelete t nv =
  log "deleting %s" (NV.to_string nv);
  Globals.msg "Uninstalling %s ...\n" (NV.to_string nv);
  let name = NV.name nv in

  (* Run the remove script *)
  let opam_f = Path.G.opam t.global nv in
  if Filename.exists opam_f then (
    let opam = File.OPAM.read opam_f in
    let remove = substitute_commands t (File.OPAM.remove opam) in
    let remove = filter_commands t remove in
    let p_build = Path.C.build t.compiler nv in
    (* We try to run the remove scripts in the folder where it was extracted
       If it does not exist, we try to download and extract the archive again,
       if that fails, we don't really care. *)
    if not (Dirname.exists p_build) && mem_repo t nv then (
      try ignore (extract_package t nv)
      with _ -> Dirname.mkdir p_build;
    );
    begin
      try Dirname.exec ~add_to_path:[Path.C.bin t.compiler] p_build remove
      with _ -> ();
    end;
    Dirname.rmdir p_build;
  );

  (* Remove the libraries *)
  Dirname.rmdir (Path.C.lib t.compiler name);
  Dirname.rmdir (Path.C.build t.compiler nv);

  (* Clean-up the repositories *)
  log "Cleaning-up the repositories";
  let repos =
    try N.Map.find (NV.name nv) t.repo_index
    with _ -> [] in
  List.iter (fun r ->
    let repo = find_repository t r in
    let repo_p = Path.R.create repo in
    let tmp_dir = Path.R.tmp_dir repo_p nv in
    Dirname.rmdir tmp_dir
  ) repos;

  (* Remove the binaries *)
  log "Removing the binaries";
  let install = File.Dot_install.safe_read (Path.C.install t.compiler name) in
  List.iter (fun (_,dst) ->
    let dst = Path.C.bin t.compiler // (Basename.to_string dst) in
    Filename.remove dst
  ) (File.Dot_install.bin install);

  (* Remove the misc files *)
  log "Removing the misc files";
  List.iter (fun (_,dst) ->
    if Filename.exists dst then begin
      Globals.msg "Removing %s." (Filename.to_string dst);
      if confirm "Continue ?" then
        Filename.remove dst
    end
  ) (File.Dot_install.misc install);

  (* Remove .config and .install *)
  log "Removing config and install files";
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
    | ":=" -> (ident, string ^":"^ (String.concat ":" (read_env())))
    | "=:" -> (ident, (String.concat ":" (read_env())) ^":"^ string)
    | _    -> failwith (Printf.sprintf "expand_env: %s is an unknown symbol" symbol)
  ) env

let update_env t env e =
  let expanded = expand_env t e in
  { env with
    add_to_env = expanded @ env.add_to_env;
    new_env    = expanded @ env.new_env }

let empty_env = { add_to_env=[]; add_to_path=Dirname.raw ""; new_env=[] }
let get_env t =
  match current_ocaml_version t with
  | None               -> empty_env
  | Some ocaml_version ->
    let comp_f = Path.G.compiler t.global ocaml_version in
    let comp = File.Comp.read comp_f in

    let add_to_path = Path.C.bin t.compiler in
    let new_path = "PATH", "+=", Dirname.to_string add_to_path in

    let add_to_env = File.Comp.env comp in
    let toplevel_dir =
      "OCAML_TOPLEVEL_PATH", "=", Dirname.to_string (Path.C.toplevel t.compiler) in
    let man_path =
      "MANPATH", ":=", Dirname.to_string (Path.C.man_dir t.compiler) in
    let new_env = new_path :: man_path :: toplevel_dir :: add_to_env in

    let add_to_env = expand_env t add_to_env in
    let new_env = expand_env t new_env in

    { add_to_env; add_to_path; new_env }

let print_env env =
  if env <> empty_env then
    List.iter (fun (k,v) ->
      Globals.msg "%s=%s; export %s;\n" k v k;
    ) env.new_env

let print_env_warning ?(add_profile = false) t =
  match
    List.filter
      (fun (s, v) ->
        Some v <> try Some (Unix.getenv s) with _ -> None)
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
        (if !Globals.root_path = Globals.default_opam_path then
            ""
         else
            Printf.sprintf " --root %s" !Globals.root_path) in
      let variables = String.concat ", " (List.map (fun (s, _) -> "$" ^ s) l) in
      Globals.msg "\nTo update %s; you can now run:
            \n\    $ %seval `opam%s config -env`\n%s\n"
        variables
        which_opam
        opam_root
        add_profile

(* In case of error, simply return the error traces, and let the
   repo in a state that the user can explore.
   Do not try to recover yet. *)
let proceed_tochange t nv_old nv =
  Globals.msg "\n=-=-= %s =-=-=\n" (NV.to_string nv);

  (* First, uninstall any previous version *)
  (match nv_old with
  | Some nv_old -> proceed_todelete t nv_old
  | None        -> ());

  let opam = File.OPAM.read (Path.G.opam t.global nv) in

  (* Get the env variables set up in the compiler description file *)
  let env0 = get_env t in
  let env = update_env t env0 (File.OPAM.build_env opam) in

  (* Prepare the package for the build.
     This function is run before the build and after an error has
     occured, to help debugging. *)
  let prepare_package () =
    (* First, untar the archive *)
    match extract_package t nv with
    | None         -> None
    | Some p_build ->

      (* Substitute the configuration files. We should be in the right
         directory to get the correct absolute path for the substitution
         files (see [substitute_file] and [Filename.of_basename]. *)
      Dirname.in_dir (Path.C.build t.compiler nv) (fun () ->
        List.iter (substitute_file t) (File.OPAM.substs opam)
      );

      (* Generate an environnement file *)
      let env_f = Path.C.build_env t.compiler nv in
      File.Env.write env_f env.new_env;

      Some p_build in

  let p_build = match prepare_package () with
    | None         -> Path.C.root t.compiler
    | Some p_build -> p_build in

  (* Call the build script and copy the output files *)
  let commands = substitute_commands t (File.OPAM.build opam) in
  let commands = filter_commands t commands in
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  if commands_s <> [] then
    Globals.msg "Build commands:\n  %s\n" (String.concat "\n  " commands_s)
  else
    Globals.msg "Nothing to do.\n";
  try
    Dirname.exec
      ~add_to_env:env.add_to_env
      ~add_to_path:[env.add_to_path]
      p_build
      commands;
    proceed_toinstall t nv;
  with e ->
    proceed_todelete t nv;
    let p_build = match prepare_package () with
      | None         -> Path.C.root t.compiler
      | Some p_build -> p_build in
    begin match nv_old with
    | None        ->
      Globals.error
        "The compilation of %s failed in %s."
        (NV.to_string nv)
        (Dirname.to_string p_build)
    | Some nv_old ->
      Globals.error
        "The recompilation of %s failed in %s."
        (NV.to_string nv)
        (Dirname.to_string p_build)
    end;
    raise e

(* We need to clean-up things before recompiling. *)
let proceed_torecompile t nv =
  proceed_tochange t (Some nv) nv

let debpkg_of_nv action t nv =
  let opam = File.OPAM.read (Path.G.opam t.global nv) in
  let installed =
    NV.Set.mem nv t.installed &&
    match action with
    | `upgrade reinstall -> not (NV.Set.mem nv reinstall)
    | _                  -> true in
  File.OPAM.to_package opam installed

type version_constraint =
  | V_any of name * V.Set.t (* available versions *) * version option (* installed version *)
  | V_eq  of name * version

let string_of_version_constraint = function
  | V_any (n,s,i) ->
      Printf.sprintf
        "{name=%s available=%s installed=%s}"
        (N.to_string n)
        (V.Set.to_string s)
        (match i with None -> "<none>" | Some v -> V.to_string v)
  | V_eq (n,v) ->
      Printf.sprintf "{name=%s version=%s}" (N.to_string n) (V.to_string v)

let name_of_version_constraint = function
  | V_any (n,_,_) -> n
  | V_eq (n,_)    -> n

let nv_of_version_constraint = function
  | V_eq (n, v)
  | V_any (n, _, Some v) -> NV.create n v
  | V_any (n, vs, None)  -> NV.create n (V.Set.choose vs)

type solver_result =
  | Nothing_to_do
  | OK
  | Aborted
  | No_solution

let get_stats sol =
  let s_install, s_reinstall, s_upgrade, s_downgrade =
    PA_graph.fold_vertex
      (fun pkg (to_install, to_reinstall, to_upgrade, to_downgrade) ->
        match action pkg with
        | To_change (None, _)             -> succ to_install, to_reinstall, to_upgrade, to_downgrade
        | To_change (Some x, y) when x<>y ->
          if V.compare (NV.version x) (NV.version y) < 0 then
            to_install, to_reinstall, succ to_upgrade, to_downgrade
          else
            to_install, to_reinstall, to_upgrade, succ to_downgrade
        | To_change (Some _, _)
        | To_recompile _                  -> to_install, succ to_reinstall, to_upgrade, to_downgrade
        | To_delete _ -> assert false)
      sol.to_add
      (0, 0, 0, 0) in
  let s_remove = List.length sol.to_remove in
  { s_install; s_reinstall; s_upgrade; s_downgrade; s_remove }

let sum stats =
  stats.s_install + stats.s_reinstall + stats.s_remove + stats.s_upgrade + stats.s_downgrade

let print_stats stats =
  Globals.msg "%d to install | %d to reinstall | %d to upgrade | %d to downgrade | %d to remove\n"
    stats.s_install
    stats.s_reinstall
    stats.s_upgrade
    stats.s_downgrade
    stats.s_remove

module Heuristic = struct

  let vpkg_of_n op name =
    (N.to_string name, None), op

  let vpkg_of_n_op op name v =
    vpkg_of_n (Some (op, V.to_string v)) name

  let vpkg_of_nv_eq = vpkg_of_n_op "="
  let vpkg_of_nv_ge = vpkg_of_n_op ">="
  let vpkg_of_nv_le = vpkg_of_n_op "<="
  let vpkg_of_nv_any = vpkg_of_n None

  (* Choose any available version *)
  let v_any _ _ =
    vpkg_of_nv_any

  (* Choose the max version *)
  let v_max _ set n =
    vpkg_of_nv_eq n (V.Set.max_elt set)

  (* Choose the installed version (if any); if the package is not installed,
     pick the max version *)
  let v_eq v set n =
    match v with
    | None   -> vpkg_of_nv_eq n (V.Set.max_elt set)
    | Some v -> vpkg_of_nv_eq n v

  (* Choose at least the installed version (if any); if the package is not
     installed, pick the max version *)
  let v_ge v set n =
    match v with
    | None   -> vpkg_of_nv_eq n (V.Set.max_elt set)
    | Some v -> vpkg_of_nv_ge n v

  let get t packages f_h =
    let available = get_available_current t in
    let available_map = NV.to_map available in
    let packages =
      NV.Set.filter
        (fun nv -> NV.Set.mem nv available)
        packages in
    let map = NV.to_map packages in
    N.Map.mapi
      (fun n vs -> f_h (Some (V.Set.choose_one vs)) (N.Map.find n available_map) n)
      map

  let get_installed t f_h =
    get t t.installed f_h

  let get_comp_packages t ocaml_version f_h =
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

  (* Take a list of version constraints and an heuristic, and return a list of
     packages constraints satisfying the constraints *)
  let apply f_heuristic constraints =
    List.map
      (function
        | V_any (n, set, v) -> f_heuristic v set n
        | V_eq (n, v)       -> vpkg_of_nv_eq n v)
      constraints

  (* transform a name into:
     - <name, installed version> package
     - <$n,$v> package when name = $n.$v *)
  let nv_of_names t names =
    let available = NV.to_map (get_available_current t) in
    let installed = NV.to_map t.installed in
    List.map
      (fun name ->
        if N.Map.mem name installed && not (N.Map.mem name available) then
          V_eq (name, V.Set.choose_one (N.Map.find name installed))
        else if N.Map.mem name available then begin
          let set = N.Map.find name available in
          if N.Map.mem name installed then
            let version = V.Set.choose_one (N.Map.find name installed) in
            V_any (name, set, Some version)
          else
            V_any (name, set, None)
        end else
          (* perhaps the package is unavailable for this compiler *)
          let get_available = Path.G.available_versions t.global in
          let all_versions = get_available name in
          if not (V.Set.is_empty all_versions) then
            unavailable_package name None
          else
            (* consider 'name' to be 'name.version' *)
            let nv =
              try NV.of_string (N.to_string name)
              with Not_found -> unknown_package name None in
            let sname = NV.name nv in
            let sversion = NV.version nv in
            log "The raw name %S not found, looking for package %s version %s"
                (N.to_string name) (N.to_string sname) (V.to_string sversion);
            if N.Map.mem sname available
               && V.Set.mem sversion (N.Map.find sname available) then
              V_eq (sname, sversion)
            else
              let all_versions = get_available sname in
              if V.Set.mem sversion all_versions then
                unavailable_package sname (Some sversion)
              else
                unknown_package sname (Some sversion)
      )
      (N.Set.elements names)

  (* Apply a solution *)
  let apply_solution ?(force = false) t sol =
    if Solver.solution_is_empty sol then
      (* The current state satisfies the request contraints *)
      Nothing_to_do
    else (
      let stats = get_stats sol in
      Globals.msg "The following actions will be performed:\n";
      print_solution sol;
      print_stats stats;

      let continue =
        if force || sum stats <= 1 then
          true
        else
          confirm "Do you want to continue ?" in

      if continue then (

        let installed = ref t.installed in
        (* This function should be called by the parent process only, as it modifies
           the global state of OPAM *)
        let write_installed () =
          File.Installed.write (Path.C.installed t.compiler) !installed in

        (* Delete the requested packages in the parent process *)
        (* In case of errors, we try to keep the list of installed packages up-to-date *)
        List.iter
          (fun nv ->
            if NV.Set.mem nv !installed then begin
              try
                proceed_todelete t nv;
                installed := NV.Set.remove nv !installed;
                write_installed ()
              with _ ->
                ()
            end)
          sol.to_remove;

        (* Installation and recompilation are done by child processes *)
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

        (* Try to recover from errors.
           XXX: this is higly experimental. *)
        let can_try_to_recover_from_error l =
          List.exists (function (n,_) ->
            match action n with
            | To_change(Some _,_)
            | To_recompile _ -> true
            | _ -> false
          ) l in

        let recover_from_error (n, _) = match action n with
          | To_change (Some o, _) ->
              begin try
                proceed_toinstall t o;
                installed := NV.Set.add o !installed;
                write_installed ()
              with _ ->
                  ()
              end
          | To_change (None, _)   -> ()
          | To_recompile nv       ->
              (* this case is quite tricky. We have to remove all the packages
                 depending in nv, as they will be broken if nv is uninstalled. *)
              let universe =
                Solver.U
                  (NV.Set.fold
                     (fun nv l -> (debpkg_of_nv `remove t nv) :: l)
                     (get_available_current t) []) in
              let depends =
                Solver.get_forward_dependencies ~depopts:true universe
                  (Solver.P [debpkg_of_nv `remove t nv]) in
              let depends = NV.Set.of_list (List.rev_map NV.of_dpkg depends) in
              let depends = NV.Set.filter (fun nv -> NV.Set.mem nv t.installed) depends in
              NV.Set.iter (proceed_todelete t) depends;
              installed := NV.Set.diff !installed depends;
              write_installed ();
          | To_delete nv            -> assert false in

        let display_error (n, error) =
          let f action nv =
            Globals.error "[ERROR] while %s %s" action (NV.to_string nv);
            match error with
            | Parallel.Process_error r  -> Process.display_error_message r
            | Parallel.Internal_error s -> Globals.error "  %s" s in
          match action n with
          | To_change (Some o, nv) ->
              if V.compare (NV.version o) (NV.version nv) < 0 then
                f "upgrading to" nv
              else
                f "downgrading to" nv
          | To_change (None, nv)   -> f "installing" nv
          | To_recompile nv        -> f "recompiling" nv
          | To_delete nv           -> f "removing" nv in

        let string_of_errors errors =
          let actions = List.map action (List.map fst errors) in
          let packages =
            List.map (function
              | To_change (_,nv)
              | To_recompile nv
              | To_delete nv -> nv
            ) actions in
          match packages with
          | []  -> assert false
          | [h] -> NV.to_string h
          | l   -> NV.Set.to_string (NV.Set.of_list l) in

        let cores = File.Config.cores t.config in
        try
          PA_graph.Parallel.iter cores sol.to_add ~pre ~child ~post;
          OK
        with PA_graph.Parallel.Errors (errors, remaining) ->
          Globals.msg "\n";
          if remaining <> [] then (
            Globals.error
              "Due to some errors while processing %s, the following actions will NOT be proceeded:"
              (string_of_errors errors);
            List.iter (fun n -> Globals.error "%s" (string_of_action (action n))) remaining;
          );
          if can_try_to_recover_from_error errors then (
            Globals.msg "\nRecovering from errors:\n";
            List.iter recover_from_error errors;
          );
          List.iter display_error errors;
          Globals.exit 2
      ) else
        Aborted
    )

  let find_solution action_k t l_request =
    let available = get_available_current t in
    let l_pkg = NV.Set.fold (fun nv l -> debpkg_of_nv action_k t nv :: l) available [] in
    let rec aux = function
      | []                    -> None
      | request :: l_request ->
          match Solver.resolve (Solver.U l_pkg) request t.installed with
          | None  ->
              log "heuristic with no solution";
              aux l_request
          | Some sol -> Some sol in
    aux l_request

  let new_variables e =
    let open Utils in
    let e = List.filter (fun (_,s,_) -> s="=") e in
    let e = List.map (fun (v,_,_) -> v) e in
    List.fold_right StringSet.add e StringSet.empty

  let print_variable_warnings =
    let warnings = ref false in
    fun t ->
      let variables = ref [] in
      if not !warnings then (
        let warn w =
          let is_defined s =
            try let _ = Sys.getenv s in true
            with Not_found -> false in
          if is_defined w then
            variables := w :: !variables in

      (* 1. Warn about OCAMLFIND variables if it is installed *)
        let ocamlfind_vars = [
          "OCAMLFIND_DESTDIR";
          "OCAMLFIND_CONF";
          "OCAMLFIND_METADIR";
          "OCAMLFIND_COMMANDS";
          "OCAMLFIND_LDCONF";
        ] in
        if NV.Set.exists (fun nv -> N.to_string (NV.name nv) = "ocamlfind") t.installed then
          List.iter warn ocamlfind_vars;
      (* 2. Warn about variables possibly set by other compilers *)
        let new_variables version =
          let comp_f = Path.G.compiler t.global version in
          let env = File.Comp.env (File.Comp.read comp_f) in
          new_variables env in
        let vars = ref StringSet.empty in
        List.iter (fun (_,version) ->
          vars := StringSet.union !vars (new_variables version)
        ) t.aliases;
        begin match current_ocaml_version t with
        | None   -> ()
        | Some v -> vars := StringSet.diff !vars (new_variables v);
        end;
        StringSet.iter warn !vars;
        if !variables <> [] then (
          Globals.msg "The following variables are set in your environment, \
                     you should better unset it if you want OPAM to work \
                     correctly.\n";
          List.iter (Globals.msg " - %s\n") !variables;
          if not (confirm "Do you want to continue ?") then
            Globals.exit 0;
        );
        warnings := true;
      )

  let resolve ?(force=false) action_k t l_request =
    match find_solution action_k t l_request with
    | None ->
        Globals.msg "No solution has been found.\n";
        No_solution
    | Some sol ->
      print_variable_warnings t;
      apply_solution ~force t sol

end

let dry_upgrade () =
  log "dry-upgrade";
  let t = update_available_current (load_state ()) in
  let reinstall = NV.Set.inter t.reinstall t.installed in
  let solution = Heuristic.find_solution (`upgrade reinstall) t
    (List.map (fun to_upgrade ->
      { wish_install = [];
        wish_remove  = [];
        wish_upgrade = N.Map.values (Heuristic.get_installed t to_upgrade) })
       [ Heuristic.v_max; Heuristic.v_ge ]) in
  match solution with
  | None     -> None
  | Some sol -> Some (get_stats sol)

let update repos =
  log "update %s" (String.concat " " repos);
  let t = load_state () in
  let repos =
    if repos = [] then
      t.repositories
    else
      let aux r =
        if mem_repository t r then
          Some (find_repository t r)
        else (
          Globals.msg "%s is not a valid repository.\n" r;
          None
        ) in
      Utils.filter_map aux repos in
  if repos <> [] then (
    update_repositories t ~show_compilers:true repos;
    update_packages t ~show_packages:true repos;
  );
  match dry_upgrade () with
  | None -> Globals.msg "Already up-to-date.\n"
  | Some stats ->
    if sum stats > 0 then (
      print_stats stats;
      Globals.msg "You can now run 'opam upgrade' to upgrade your system.\n"
    ) else
      Globals.msg "Already up-to-date.\n"

let init repo ocaml_version cores =
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
    let t = load_state () in
    update_repositories t ~show_compilers:false t.repositories;
    let system_ocaml_version = OCaml_V.current () in
    begin match system_ocaml_version with
      | None   -> ()
      | Some v ->
          let config = File.Config.with_system_ocaml_version t.config v in
          File.Config.write (Path.G.config t.global) config
    end;
    let t = load_state () in
    let ocaml_version = match ocaml_version, system_ocaml_version with
      | None  , Some _ -> OCaml_V.of_string Globals.default_compiler_version
      | Some v, _      -> v
      | None  , None   ->
          Globals.msg "No compiler found.\n";
          Globals.exit 1 in
    let alias = Alias.of_string (OCaml_V.to_string ocaml_version) in
    let quiet = (system_ocaml_version = Some ocaml_version) in
    init_ocaml t quiet alias ocaml_version;
    update_packages t ~show_packages:false t.repositories;
    let t = update_available_current (load_state ()) in
    let wish_install = Heuristic.get_comp_packages t ocaml_version Heuristic.v_any in
    let _solution = Heuristic.resolve ~force:true `init t
      [ { wish_install
        ; wish_remove = []
        ; wish_upgrade = [] } ] in

    print_env_warning ~add_profile:true t

  with e ->
    if not !Globals.debug then
      Dirname.rmdir (Path.G.root root);
    raise e

let install names =
  log "install %s" (N.Set.to_string names);
  let t = update_available_current (load_state ()) in
  let names = Heuristic.nv_of_names t names in
  let nvs = List.map nv_of_version_constraint names in

  let pkg_skip, pkg_new =
    List.partition (fun nv ->
      let name = NV.name nv in
      NV.Set.exists (fun nv -> NV.name nv = name) t.installed
    ) nvs in

  (* Display a message if at least one package is already installed *)
  List.iter
    (fun nv ->
      Globals.msg
        "Package %s is already installed (current version is %s)\n"
        (N.to_string (NV.name nv))
        (V.to_string (NV.version nv)))
    pkg_skip;

  if pkg_new <> [] then (

    (* Display a warning if at least one package contains
       dependencies to some unknown packages *)
    let available = NV.to_map (get_available_current t) in
    List.iter
      (fun nv ->
        let opam = File.OPAM.read (Path.G.opam t.global nv) in
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

    let name_new = List.map NV.name pkg_new in
    List.iter (fun n -> log "new: %s" (N.to_string n)) name_new;

    let universe = Solver.U (NV.Set.fold (fun nv l -> (debpkg_of_nv `install t nv) :: l) (get_available_current t) []) in
    let depends =
      Solver.get_backward_dependencies ~depopts:true universe
        (Solver.P (List.rev_map (fun nv -> debpkg_of_nv `install t nv) pkg_new)) in
    let depends = NV.Set.of_list (List.rev_map NV.of_dpkg depends) in
    let depends =
      NV.Set.filter (fun nv ->
        let name = NV.name nv in
        NV.Set.exists (fun nv -> NV.name nv = name) t.installed
      ) depends in
    NV.Set.iter (fun nv -> log "might_change<2>: %s" (NV.to_string nv)) depends;

    let name_might_change = List.map NV.name (NV.Set.elements depends) in

    (* A gross approximation of the collection of packages which migh
       be upgraded/downloaded by the installation process *)
    let pkg_might_change f_h =
      let pkgs = Heuristic.get_installed t f_h in
      let pkgs = N.Map.filter (fun n _ -> List.mem n name_might_change) pkgs in
      N.Map.iter (fun n _ -> log "might_change: %s" (N.to_string n)) pkgs;
      N.Map.values pkgs in

    (* The collection of packages which should change very rarely (so the NOT is a bit misleading
       as it may happen if some packages indirectly List.map name_of_version_constraint pkg_new
       add new package constraints) *)
    let pkg_not_change f_h =
      let pkgs = Heuristic.get_installed t f_h in
      let pkgs = N.Map.filter (fun n _ -> not (List.mem n name_new)) pkgs in
      let pkgs = N.Map.filter (fun n _ -> not (List.mem n name_might_change)) pkgs in
      N.Map.iter (fun n _ -> log "not_change: %s" (N.to_string n)) pkgs;
      N.Map.values pkgs in

    let pkg_new =
      List.filter
        (fun v_cstr -> List.mem (name_of_version_constraint v_cstr) name_new)
        names in
    let _solution = Heuristic.resolve `install t
      (List.map
         (fun (f_new, f_might, f_not) ->
           { wish_install =
               Heuristic.apply f_new pkg_new
               @ (pkg_might_change f_might)
               @ (pkg_not_change f_not)
           ; wish_remove  = []
           ; wish_upgrade = [] })
         (let open Heuristic in
          [ v_max, v_eq , v_eq
          ; v_max, v_ge , v_eq
          ; v_max, v_any, v_eq
          ; v_any, v_eq , v_eq
          ; v_any, v_ge , v_eq
          ; v_any, v_any, v_eq
          ; v_any, v_any, v_any ])) in
    ()
  )

let remove names =
  log "remove %s" (N.Set.to_string names);
  let default_package = N.of_string Globals.default_package in
  if N.Set.mem default_package names then
    Globals.msg "Package %s can not be removed.\n" Globals.default_package;
  let names = N.Set.filter (fun n -> n <> default_package) names in
  let t = update_available_current (load_state ()) in
  let wish_remove = Heuristic.nv_of_names t names in
  log "wish_remove=%s" (String.concat " " (List.map string_of_version_constraint wish_remove));
  let whish_remove, not_installed, does_not_exist =
    let aux (whish_remove, not_installed, does_not_exist) c nv =
      let name = NV.name nv in
      if not (NV.Set.exists (fun nv -> NV.name nv = name) t.installed) then
        (whish_remove, N.Set.add name not_installed, does_not_exist)
      else if not (NV.Set.mem nv (get_available_current t)) then
        (whish_remove, not_installed, nv :: does_not_exist)
      else
        (c :: whish_remove, not_installed, does_not_exist) in
    List.fold_left
      (fun accu c ->
        match c with
        | V_eq (n, v)
        | V_any (n, _, Some v) -> aux accu c (NV.create n v)
        | V_any (n, _, None)   ->
            match find_available_package_by_name t n with
            | None    -> accu
            | Some vs ->  NV.Set.fold (fun v accu -> aux accu c v) vs accu
      ) ([], N.Set.empty, []) wish_remove in

  if does_not_exist <> [] then (
    List.iter (proceed_todelete t) does_not_exist;
    let installed_f = Path.C.installed t.compiler in
    let installed = File.Installed.read installed_f in
    let installed = NV.Set.filter (fun nv -> not (List.mem nv does_not_exist)) installed in
    File.Installed.write installed_f installed;
  );

  if not (N.Set.is_empty not_installed) then (
    if N.Set.cardinal not_installed = 1 then
      Globals.msg "%s is not installed.\n" (N.to_string (N.Set.choose not_installed))
    else
      Globals.msg "%s are not installed.\n" (N.Set.to_string not_installed)
  );

  if whish_remove <> [] then (
    let universe = Solver.U (NV.Set.fold (fun nv l -> (debpkg_of_nv `remove t nv) :: l) (get_available_current t) []) in
    let depends =
      Solver.get_forward_dependencies ~depopts:true universe
        (Solver.P (List.rev_map
                     (fun vc -> debpkg_of_nv `remove t (nv_of_version_constraint vc))
                     wish_remove)) in
    let depends = NV.Set.of_list (List.rev_map NV.of_dpkg depends) in
    let depends = NV.Set.filter (fun nv -> NV.Set.mem nv t.installed) depends in
    let wish_remove = Heuristic.apply Heuristic.v_eq wish_remove in
    let _solution = Heuristic.resolve `remove t
      (List.map
         (fun f_h ->
           let installed = Heuristic.get_installed t f_h in
           let installed =
             N.Map.filter
               (fun n _ -> not (NV.Set.exists (fun nv -> NV.name nv = n) depends))
               installed in
           { wish_install = N.Map.values installed
           ; wish_remove
           ; wish_upgrade = [] })
         [ Heuristic.v_eq
         ; Heuristic.v_any ]) in
    ())

let upgrade names =
  log "upgrade %s" (N.Set.to_string names);
  let t = update_available_current (load_state ()) in
  let reinstall = NV.Set.inter t.reinstall t.installed in
  let to_not_reinstall = ref NV.Set.empty in
  let solution_found = ref No_solution in
  if N.Set.is_empty names then (
    let solution = Heuristic.resolve (`upgrade reinstall) t
      (List.map (fun to_upgrade ->
        { wish_install = [];
          wish_remove  = [];
          wish_upgrade = N.Map.values (Heuristic.get_installed t to_upgrade) })
         [ Heuristic.v_max; Heuristic.v_ge ]) in
    solution_found := solution;
  ) else (
    let names = Heuristic.nv_of_names t names in
    let partial_reinstall = NV.Set.of_list (List.map nv_of_version_constraint names) in
    to_not_reinstall := NV.Set.diff reinstall partial_reinstall;
    let solution = Heuristic.resolve (`upgrade partial_reinstall)  t
      (List.map (fun (to_upgrade, to_keep) ->
        let wish_install = Heuristic.get_installed t to_keep in
        let wish_install =
          (* Remove the packages in [names] *)
          N.Map.filter
            (fun n _ -> List.for_all (fun vc -> name_of_version_constraint vc <> n) names)
            wish_install in
        let wish_install = N.Map.values wish_install in
        let wish_upgrade = Heuristic.apply to_upgrade names in
        { wish_install;
          wish_remove  = [];
          wish_upgrade })
         [ (Heuristic.v_max, Heuristic.v_eq);
           (Heuristic.v_max, Heuristic.v_ge);
           (Heuristic.v_max, Heuristic.v_any);
           (Heuristic.v_ge , Heuristic.v_eq);
           (Heuristic.v_ge , Heuristic.v_ge);
           (Heuristic.v_ge , Heuristic.v_any); ]
      ) in
    solution_found := solution;
  );
  let t = load_state () in
  begin match !solution_found with
    | OK            -> ()
    | Nothing_to_do -> Globals.msg "Already up-to-date.\n"
    | Aborted
    | No_solution   -> to_not_reinstall := reinstall
  end;
  let reinstall = NV.Set.inter t.installed !to_not_reinstall in
  let reinstall_f = Path.C.reinstall t.compiler in
  if NV.Set.is_empty reinstall then
    Filename.remove reinstall_f
  else
    File.Reinstall.write reinstall_f reinstall

let reinstall names =
  log "reinstall %s" (N.Set.to_string names);
  let t = update_available_current (load_state ()) in
  let packages = Heuristic.nv_of_names t names in
  let reinstall_new =
    Utils.filter_map (function
      | V_any (n, _, Some v)
      | V_eq (n, v) -> Some (NV.create n v)
      | V_any (n, _, _) ->
          Globals.msg "%s is not installed" (N.to_string n);
          None
    ) packages in
  let reinstall_new = NV.Set.of_list reinstall_new in
  let reinstall_f = Path.C.reinstall t.compiler in
  let reinstall_old = File.Reinstall.safe_read reinstall_f in
  File.Reinstall.write reinstall_f (NV.Set.union reinstall_new reinstall_old);
  upgrade names

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
  let repo_p = Path.R.create repo in
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

(* Return the transitive closure of dependencies sorted in topological order *)
let get_transitive_dependencies ?(depopts = false) t names =
  let universe =
    Solver.U (List.map (debpkg_of_nv `config t) (NV.Set.elements t.installed)) in
  (* Compute the transitive closure of dependencies *)
  let pkg_of_name n = debpkg_of_nv `config t (find_installed_package_by_name t n) in
  let request = Solver.P (List.map pkg_of_name names) in
  let depends = Solver.get_backward_dependencies ~depopts universe request in
  let topo = List.map NV.of_dpkg depends in
  topo

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
        ) [] (List.rev deps) in
      Globals.msg "%s\n" (String.concat " " includes)

  | Compil c ->
    begin match current_ocaml_version t with
    | None          -> ()
    | Some oversion ->
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
            List.iter (fun child ->
                Section.G.add_vertex graph child;
                Section.G.add_edge graph child s;
            ) childs;
            let new_childs =
              List.filter (fun s ->
                Section.Map.mem s library_map && not (Section.Set.mem s !seen)
              ) childs in
            todo := Section.Set.union (Section.Set.of_list new_childs) !todo;
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
    end

let remote action =
  log "remote %s" (string_of_remote action);
  let t = load_state () in
  let repos = File.Config.repositories t.config in
  let update_config repos =
    let new_config = File.Config.with_repositories t.config repos in
    File.Config.write (Path.G.config t.global) new_config in
  let cleanup_repo repo =
    update_config (List.filter ((!=) repo) repos);
    let repo_index =
      N.Map.fold (fun n repo_s repo_index ->
        let repo_s = List.filter (fun r -> r <> Repository.name repo) repo_s in
        match repo_s with
        | [] ->
            (* The package does not exist anymore in any remote repository,
               so we need to remove the associated meta-data if the package
               is not installed. *)
            let versions = Path.G.available_versions t.global n in
            V.Set.iter (fun v ->
              let nv = NV.create n v in
              if not (NV.Set.mem nv t.installed) then (
                Filename.remove (Path.G.opam t.global nv);
                Filename.remove (Path.G.descr t.global nv);
                Filename.remove (Path.G.archive t.global nv);
              )
            ) versions;
            repo_index
        | _  -> N.Map.add n repo_s repo_index
      ) t.repo_index N.Map.empty in
    File.Repo_index.write (Path.G.repo_index t.global) repo_index;
    Dirname.rmdir (Path.R.root (Path.R.create repo)) in
  match action with
  | List  ->
      let pretty_print r =
        Globals.msg "%-7s %10s     %s\n"
          (Printf.sprintf "[%s]" (Repository.kind r))
          (Repository.name r)
          (Dirname.to_string (Repository.address r)) in
      List.iter pretty_print repos
  | Add repo ->
      let name = Repository.name repo in
      if List.exists (fun r -> Repository.name r = name) repos then
        Globals.error_and_exit "%s is already a remote repository" name
      else (
        (try Repositories.init repo with
        | Repositories.Unknown_backend ->
            Globals.error_and_exit "\"%s\" is not a supported backend" (Repository.kind repo)
        | e ->
            cleanup_repo repo;
            raise e);
        log "Adding %s" (Repository.to_string repo);
        update_config (repo :: repos)
      );
      (try update [name]
       with e ->
         cleanup_repo repo;
         raise e)
  | Rm n  ->
      let repo =
        try List.find (fun r -> Repository.name r = n) repos
        with Not_found ->
          Globals.error_and_exit "%s is not a remote index" n in
      cleanup_repo repo

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
          (string_of_pin_option action.pin_arg)
          (string_of_pin_option current);
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
  Globals.msg "--- Installed compilers ---\n";
  List.iter (fun (n,c) ->
    let current = if n = File.Config.ocaml_version t.config then "*" else " " in
    let ocaml_version = OCaml_V.to_string c in
    let alias_name = Alias.to_string n in
    if alias_name = ocaml_version then
      Globals.msg " %s %s\n" current alias_name
    else
      Globals.msg " %s %s [%s]\n" current ocaml_version alias_name
  ) aliases;
  Globals.msg "\n--- Available compilers ---\n";
  OCaml_V.Set.iter (fun c ->
    let comp = File.Comp.read (Path.G.compiler t.global c) in
    let preinstalled = if File.Comp.preinstalled comp then "~" else " " in
    Globals.msg " %s %s\n" preinstalled (OCaml_V.to_string c)
  ) descrs

let compiler_install quiet alias ocaml_version =
  log "compiler_install %b %s %s" quiet
    (Alias.to_string alias)
    (OCaml_V.to_string ocaml_version);

  (* install the new OCaml version *)
  init_ocaml (load_state ()) quiet alias ocaml_version;

  (* install the compiler packages *)
  let t = update_available_current (load_state ()) in
  let packages =
    N.Map.of_list
      (List.rev_map
         (function (name, _), _ as nv -> N.of_string name, nv)
         (Heuristic.get_comp_packages t ocaml_version Heuristic.v_eq)) in

  let is_ok =
    N.Map.for_all (fun n c ->
      if mem_installed_package_by_name t n then (
        let nv = find_installed_package_by_name t n in
        c = Heuristic.vpkg_of_nv_eq n (NV.version nv)
      ) else (
        false
      )
    ) packages in
  if not is_ok then (
    let _solution = Heuristic.resolve ~force:true `switch t
      [ { wish_install = N.Map.values packages
        ; wish_remove = []
        ; wish_upgrade = [] } ] in
    ()
  );

  print_env_warning t

let compiler_switch quiet alias =
  log "compiler_switch alias=%s" (Alias.to_string alias);
  let t = load_state () in
  let comp_p = Path.C.create alias in
  let comp_dir = Path.C.root comp_p in
  let ocaml_version = OCaml_V.of_string (Alias.to_string alias) in
  let comp_f = Path.G.compiler t.global ocaml_version in
  if not (Dirname.exists comp_dir) && not (Filename.exists comp_f) then (
    Globals.error "The compiler's description for %s does not exist.\n" (Alias.to_string alias);
    Globals.exit 1;
  );
  if not (Dirname.exists comp_dir) then
    compiler_install quiet alias ocaml_version
  else
    let config = File.Config.with_ocaml_version t.config alias in
    File.Config.write (Path.G.config t.global) config;
    print_env_warning (load_state ())

let compiler_clone alias =
  log "compiler_clone alias=%s" (Alias.to_string alias);
  let t = update_available_current (load_state ()) in

  let installed_in_alias =
    let comp_p = Path.C.create alias in
    if not (Dirname.exists (Path.C.root comp_p)) then (
      Globals.msg "%s is not a valid compiler name.\n" (Alias.to_string alias);
      Globals.exit 1;
    );
    File.Installed.safe_read (Path.C.installed comp_p) in

  let new_packages = NV.Set.diff installed_in_alias t.installed in
  let installed =
    NV.Set.filter (fun nv ->
      let name = NV.name nv in
      not (NV.Set.exists (fun nv -> name = NV.name nv) new_packages)
    ) t.installed in

  let constraints f_h = Heuristic.get t (NV.Set.union new_packages installed) f_h in

  let _solution = Heuristic.resolve `switch t
    (List.map (fun f_h ->
      { wish_install = N.Map.values (constraints f_h);
        wish_remove  = [];
        wish_upgrade = [] })
     [ Heuristic.v_eq; Heuristic.v_ge; Heuristic.v_any ]
    ) in
  ()

let compiler_current () =
  let t = load_state () in
  let current = File.Config.ocaml_version t.config in
  Globals.msg "%s\n" (Alias.to_string current)

let compiler_remove alias =
  log "compiler_remove alias=%s" (Alias.to_string alias);
  let t = load_state () in
  let comp_p = Path.C.create alias in
  let comp_dir = Path.C.root comp_p in
  if not (Dirname.exists comp_dir) then (
    Globals.msg "The compiler alias %s does not exist.\n" (Alias.to_string alias);
    Globals.exit 1;
  );
  if File.Config.ocaml_version t.config = alias then (
    Globals.msg "Cannot remove %s as it is the current compiler.\n" (Alias.to_string alias);
    Globals.exit 1;
  );
  let aliases = List.filter (fun (a,_) -> a <> alias) t.aliases in
  File.Aliases.write (Path.G.aliases t.global) aliases;
  Dirname.rmdir comp_dir

let compiler_reinstall alias =
  log "compiler_reinstall alias=%s" (Alias.to_string alias);
  let t = load_state () in
  if not (List.mem_assoc alias t.aliases) then (
    Globals.msg "The compiler alias %s does not exist.\n" (Alias.to_string alias);
    Globals.exit 1;
  );
  let ocaml_version = List.assoc alias t.aliases in
  compiler_remove alias;
  compiler_install false alias ocaml_version

(** We protect each main functions with a lock depending on its access
on some read/write data. *)

let list ~print_short ~installed_only ?name_only ?case_sensitive pkg_str =
  check (Read_only (fun () -> list ~print_short ~installed_only ?name_only ?case_sensitive pkg_str))

let info package =
  check (Read_only (fun () -> info package))

let config request =
  check (Read_only (fun () -> config request))

let install name =
  check (Write_lock (fun () -> install name))

let reinstall name =
  check (Write_lock (fun () -> reinstall name))

let update repos =
  check (Write_lock (fun () -> update repos))

let upgrade names =
  check (Write_lock (fun () -> upgrade names))

let upload u r =
  check (Write_lock (fun () -> upload u r))

let remove name =
  check (Write_lock (fun () -> remove name))

let remote action =
  check (Write_lock (fun () -> remote action))

let compiler_install quiet alias ocaml_version =
  check (Write_lock (fun () -> compiler_install quiet alias ocaml_version))

let compiler_clone alias =
  check (Write_lock (fun () -> compiler_clone alias))

let compiler_remove alias =
  check (Write_lock (fun () -> compiler_remove alias))

let compiler_switch quiet alias =
  check (Write_lock (fun () -> compiler_switch quiet alias))

let compiler_reinstall alias =
  check (Write_lock (fun () -> compiler_reinstall alias))

let compiler_list () =
  check (Read_only compiler_list)

let compiler_current () =
  check (Read_only compiler_current)

let pin action =
  check (Write_lock (fun () -> pin action))

let pin_list () =
  check (Read_only pin_list)
