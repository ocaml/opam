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

  (* ~/.opam/$oversion/installed contents *)
  installed: NV.Set.t;

  (* ~/.opam/$oversion/reinstall contents *)
  reinstall: NV.Set.t;

  (* ~/.opam/config contents *)
  config: File.Config.t;

  (* ~/.opam/repo/index contents *)
  repo_index: string N.Map.t;
}

let print_state t =
  let string_of_repos r =
    let s (r,p) =
      Printf.sprintf "%s:%s"
        (Repository.to_string r)
        (Dirname.to_string (Path.R.root p)) in
    String.concat ", " (List.map s r) in
  let string_of_nmap m =
    let s (n,r) = Printf.sprintf "%s:%s" (N.to_string n) r in
    let l = N.Map.fold (fun n r l -> s (n,r)::l) m [] in
    String.concat ", " l in
  log "GLOBAL    : %s" (Dirname.to_string (Path.G.root t.global));
  log "COMPILER  : %s" (Dirname.to_string (Path.C.root t.compiler));
  log "REPO      : %s" (string_of_repos t.repositories);
  log "AVAILABLE : %s" (NV.string_of_set t.available);
  log "INSTALLED : %s" (NV.string_of_set t.installed);
  log "REINSTALL : %s" (NV.string_of_set t.reinstall);
  log "REPO_INDEX: %s" (string_of_nmap t.repo_index)

(* Look into the content of ~/.opam/config to build the client
   state *)
let load_state () =
  log "root path is %s" !Globals.root_path;
  let global = Path.G.create (Dirname.of_string !Globals.root_path) in
  let config = File.Config.read (Path.G.config global) in
  let ocaml_version = File.Config.ocaml_version config in
  let compiler = Path.C.create global ocaml_version in
  let repositories = File.Config.repositories config in
  let repositories = List.map (fun r -> r, Path.R.create global r) repositories in
  let repo_index = File.Repo_index.safe_read (Path.G.repo_index global) in
  let installed = File.Installed.safe_read (Path.C.installed compiler) in
  let reinstall = File.Reinstall.safe_read (Path.C.reinstall compiler) in
  let available = Path.G.available global in
  let t = {
    global; compiler; repositories;
    available; installed; reinstall;
    repo_index; config
  } in
  print_state t;
  t

let find_repository_path t name =
  let _, r = List.find (fun (r,_) -> Repository.name r = name) t.repositories in
  r

let find_repository t name =
  let r, _ = List.find (fun (r,_) -> Repository.name r = name) t.repositories in
  r

let update () =
  log "update";
  let t = load_state () in
  (* first update all the repo *)
  List.iter (fun (r,p) -> Repositories.update p r) t.repositories;
  (* then update $opam/repo/index *)
  let repo_index =
    List.fold_left (fun repo_index (r,p) ->
      let available = Path.R.available p in
      log "repo=%s packages=%s" (Repository.name r) (NV.string_of_set available);
      NV.Set.fold (fun nv repo_index ->
        let name = NV.name nv in
        if not (N.Map.mem name repo_index) then
          N.Map.add name (Repository.name r) repo_index
        else
          repo_index
      ) available repo_index
    ) t.repo_index t.repositories in
  File.Repo_index.write (Path.G.repo_index t.global) repo_index;
  (* update $opam/$oversion/reinstall *)
  let reinstall =
    List.fold_left (fun reinstall (r,p) ->
      let updated = File.Updated.safe_read (Path.R.updated p) in
      NV.Set.fold (fun nv reinstall ->
        if NV.Set.mem nv t.installed then
          NV.Set.add nv reinstall
        else
          reinstall
      ) updated reinstall
    ) t.reinstall t.repositories in
  if not (NV.Set.is_empty reinstall) then
    File.Reinstall.write (Path.C.reinstall t.compiler) reinstall;
  (* finally create symbolic links from $repo dirs to main dir *)
  N.Map.iter (fun n r ->
    let repo_p = find_repository_path t r in
    let available_versions = Path.R.available_versions repo_p n in
    V.Set.iter (fun v ->
      let nv = NV.create n v in
      let opam_dir = Path.G.opam_dir t.global in
      let opam = Path.R.opam repo_p nv in
      let descr_dir = Path.G.descr_dir t.global in
      let descr = Path.R.descr repo_p nv in
      Filename.link_in opam opam_dir;
      if Filename.exists descr then
        Filename.link_in descr descr_dir
      else
        Globals.msg "WARNING: %s does not exist\n" (Filename.to_string descr)
    ) available_versions
  ) repo_index

let init repo =
  log "init %s" (Repository.to_string repo);
  let root = Path.G.create (Dirname.of_string !Globals.root_path) in
  let config_f = Path.G.config root in
  if Filename.exists config_f then
    Globals.error_and_exit "%s already exist" (Filename.to_string config_f)
  else
    let opam_version = OPAM_V.of_string Globals.opam_version in
    let ocaml_version = OCaml_V.of_string Sys.ocaml_version in
    let config = File.Config.create opam_version [repo] ocaml_version in
    let compiler = Path.C.create root ocaml_version in
    let repo_p = Path.R.create root repo in
    (* Create (possibly empty) configuration files *)
    File.Config.write config_f config;
    File.Installed.write (Path.C.installed compiler) File.Installed.empty;
    File.Repo_index.write (Path.G.repo_index root) N.Map.empty;
    File.Repo_config.write (Path.R.config repo_p) repo;
    Repositories.init repo_p repo;
    Dirname.mkdir (Path.G.opam_dir root);
    Dirname.mkdir (Path.G.descr_dir root);
    Dirname.mkdir (Path.G.archive_dir root);
    (* Update the configuration files *)
    update ()

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

let find_package_by_name set name =
  NV.Set.choose (NV.Set.filter (fun nv -> NV.name nv = name) set)

let s_not_installed = "--"

let list () =
  log "list";
  let t = load_state () in
  (* Get all the installed packages *)
  let installed = File.Installed.read (Path.C.installed t.compiler) in
  let map, max_n, max_v = 
    NV.Set.fold
      (fun nv (map, max_n, max_v) ->
        let name = NV.name nv in
        let version = NV.version nv in
        if N.Map.mem name map then
          map, max_n, max_v
        else
          (* If the packet has not been processed yet or 
             if it has been processed but the version processed was not installed *)
          let is_installed = NV.Set.mem nv installed in
          let descr_f = File.Descr.read (Path.G.descr t.global nv) in
          let synopsis = File.Descr.synopsis descr_f in
          let map = N.Map.add name ((if is_installed then Some version else None), synopsis) map in
          let max_n = max max_n (String.length (N.to_string name)) in
          let max_v = if is_installed then max max_v (String.length (V.to_string version)) else max_v in
          map, max_n, max_v)
      (Path.G.available t.global)
      (N.Map.empty, min_int, String.length s_not_installed)
  in
  N.Map.iter (fun name (version, description) ->
    let version = match version with
    | None   -> s_not_installed
    | Some v -> V.to_string v in
    Globals.msg "%s  %s  %s\n" 
      (indent_left (N.to_string name) max_n)
      (indent_right version max_v)
      description) map

let info package =
  log "info %s" (N.to_string package);
  let t = load_state () in

  let o_v =
    let installed = File.Installed.read (Path.C.installed t.compiler) in
    try Some (V.Set.choose (N.Map.find package (NV.to_map installed)))
    with Not_found -> None in

  let v_set =
    let v_set = 
      try Path.G.available_versions t.global package
      with Not_found ->
        Globals.error_and_exit "unknown package %s" (N.to_string package) in
    match o_v with
      | None   -> v_set
      | Some v -> V.Set.remove v v_set in

  List.iter
    (fun (tit, desc) -> Globals.msg "%12s: %s\n" tit desc)
    (   ("package", N.to_string package)

        :: ("version",
            match o_v with
            | None   -> s_not_installed
            | Some v -> V.to_string v)

        :: ("versions",
            String.concat " " (List.map V.to_string (V.Set.elements v_set)))

       :: let latest = match o_v with
           | None   -> V.Set.max_elt v_set
           | Some v -> v in
          let descr = File.Descr.read (Path.G.descr t.global (NV.create package latest)) in
          [ "description", "\n  " ^ File.Descr.full descr ]
      )

let confirm fmt = 
  Printf.kprintf (fun msg ->
    Globals.msg "%s [Y/n] " msg;
    match read_line () with
    | "y" | "Y"
    | "" -> true
    | _  -> false
  ) fmt

let proceed_toinstall t nv = 

  let name = NV.name nv in

  Dirname.chdir (Path.C.build t.compiler nv);
  
  (* .install *)
  let to_install = File.Dot_install.read (Path.C.build_install t.compiler nv) in
  File.Dot_install.write (Path.C.install t.compiler name) to_install;

  (* .config *)
  let config = File.Dot_config.read (Path.C.build_config t.compiler nv) in
  File.Dot_config.write (Path.C.config t.compiler name) config;

  (* lib *) 
  let lib = Path.C.lib t.compiler name in
  List.iter (fun f -> Filename.copy_in f lib) (File.Dot_install.lib to_install);
  
  (* bin *) 
  List.iter (fun (src, dst) -> Filename.copy src dst) (File.Dot_install.bin to_install);
  
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
    ) (File.Dot_install.misc to_install)

let proceed_todelete t nv =
  log "deleting %s" (NV.to_string nv);
  let name = NV.name nv in

  (* Remove the libraries *)
  Dirname.rmdir (Path.C.lib t.compiler name);
  
  (* Remove the binaries *)
  let to_install = File.Dot_install.read (Path.C.install t.compiler name) in
  List.iter (fun (_,dst) -> Filename.remove dst) (File.Dot_install.bin to_install);

  (* Remove the misc files *)
  List.iter (fun (_,dst) ->
    if Filename.exists dst then begin
      Globals.msg "Removing %s." (Filename.to_string dst);
      if confirm "Continue ?" then
        Filename.remove dst
    end
  ) (File.Dot_install.misc to_install);

  (* Remove .config and .install *)
  Filename.remove (Path.C.install t.compiler name);
  Filename.remove (Path.C.config t.compiler name)

let get_archive t nv =
  log "get_archive %s" (NV.to_string nv);
  let name = NV.name nv in
  let repo = N.Map.find name t.repo_index in
  let repo_p = find_repository_path t repo in
  let repo = find_repository t repo in
  Repositories.download repo_p repo nv;
  let src = Path.R.archive repo_p nv in
  let dst = Path.G.archive t.global nv in
  Filename.link src dst;
  dst

(* Return the contents of a fully qualified variable *)
let contents_of_variable t v =
  let name = Full_variable.package v in
  let var = Full_variable.variable v in
  let _nv =
    try find_package_by_name t.installed name
    with Not_found ->
      Globals.error_and_exit "Package %s is not installed" (N.to_string name) in
  let c = File.Dot_config.safe_read (Path.C.config t.compiler name) in
  try match Full_variable.section v with
  | None   -> File.Dot_config.variable c var
  | Some s -> File.Dot_config.Sections.variable c s var
  with Not_found ->
    Globals.error_and_exit "%s is not defined" (Full_variable.to_string v)

(* Substitute the file contents *)
let substitute_file t f =
  let src = Filename.add_extension f "in" in
  let contents = File.Subst.read src in
  let newcontents = File.Subst.replace contents (contents_of_variable t) in
  File.Subst.write f newcontents

let proceed_tochange t nv_old nv =
  (* First, uninstall any previous version *)
  (match nv_old with 
  | Some nv_old -> proceed_todelete t nv_old
  | None        -> ());

  (* Then, untar the archive *)
  let p_build = Path.C.build t.compiler nv in
  Dirname.rmdir p_build;
  Filename.extract (get_archive t nv) p_build;

  (* OPAM files should be read in the right directory to get the
     correct absolute path for the substitution files *)
  Dirname.chdir (Path.C.build t.compiler nv);
  let opam = File.OPAM.read (Path.G.opam t.global nv) in

  (* Substitute the configuration files *)
  List.iter (substitute_file t) (File.OPAM.substs opam);

  (* Call the build script and copy the output files *)
  let commands =
    List.map
      (fun cmd -> String.concat " " (List.map (Printf.sprintf "'%s'") cmd))
      (File.OPAM.build opam) in
  let err = Dirname.exec p_build commands in
  if err = 0 then
    proceed_toinstall t nv
  else
    Globals.error_and_exit
      "Compilation failed with error %d" err

(* We need to clean-up things before recompiling. *)
let proceed_torecompile t nv =
  proceed_tochange t (Some nv) nv

let debpkg_of_nv t nv =
  let opam = File.OPAM.read (Path.G.opam t.global nv) in
  let installed = NV.Set.mem nv t.installed in
  File.OPAM.to_package opam installed

let resolve t request = 
  let l_pkg = NV.Set.fold (fun nv l -> debpkg_of_nv t nv :: l) t.available [] in

  match Solver.resolve_list (Solver.U l_pkg) request with
  | [] -> Globals.msg "No solution has been found.\n"
  | l -> 
      let nb_sol = List.length l in

      let rec aux pos = 
        Globals.msg "[%d/%d] The following solution has been found:\n" pos nb_sol;
        function
        | [x] ->
            (* Only 1 solution exists *)
            print_solution x;
            if Solver.delete_or_update x then
              if confirm "Continue ?" then
                Some x
              else
                None
            else
              Some x

        | x :: xs ->
            (* Multiple solution exist *)
            print_solution x;
            if Solver.delete_or_update x then
              if confirm "Continue ? (press [n] to try another solution)" then
                Some x
              else
                aux (succ pos) xs
            else
              Some x

        | [] -> assert false in

      match aux 1 l with
      | None -> ()
      | Some sol -> 

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
          let child n = match action n with
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
            | To_change (Some _, nv) -> f "upgrading" nv
            | To_change (None, nv)   -> f "installing" nv
            | To_recompile nv        -> f "recompiling" nv
            | To_delete _            -> assert false in

          try PA_graph.Parallel.iter Globals.cores sol.to_add ~pre ~child ~post
          with PA_graph.Parallel.Error n -> error n

let vpkg_of_nv nv =
  let name = NV.name nv in
  let version = NV.version nv in
  N.to_string name, Some ("=", V.to_string version)

let unknown_package name =
  Globals.error_and_exit "Unable to locate package %S\n" (N.to_string name)

let install name = 
  log "install %s" (N.to_string name);
  let t = load_state () in
  let available = NV.to_map (Path.G.available t.global) in
  let map_installed = NV.to_map t.installed in

  (* Fail if the package is already installed *)
  if N.Map.mem name map_installed then
    Globals.error_and_exit
      "Package %s is already installed (current version is %s)"
      (N.to_string name)
      (V.to_string (V.Set.choose (N.Map.find name map_installed)));

  let nv =
    try
      if N.Map.mem name available then
        NV.create name (V.Set.max_elt (N.Map.find name available))
      else
        unknown_package name
    with Not_found ->
      (* consider 'name' to be 'name.version' *)
      let nv = NV.of_string (N.to_string name) in
      let sname = NV.name nv in
      let sversion = NV.version nv in
      Globals.msg
        "Package %s not found, looking for package %s version %s\n"
        (N.to_string name) (N.to_string sname) (V.to_string sversion);
      if N.Map.mem sname map_installed
        && V.Set.mem sversion (N.Map.find sname map_installed) then
        nv
      else
        unknown_package name in

  (* remove any old packages from the list of packages to install *)
  let map_installed =
    List.map
      (fun (x,y) -> NV.create x (V.Set.choose y))
      (N.Map.bindings (N.Map.remove (NV.name nv) map_installed)) in

  resolve t
    [ { wish_install = List.map vpkg_of_nv (nv :: map_installed)
      ; wish_remove = [] 
      ; wish_upgrade = [] } ]

let remove name =
  log "remove %s" (N.to_string name);
  let t = load_state () in
  let map_installed = NV.to_map t.installed in
  if not (N.Map.mem name map_installed) then
    Globals.error_and_exit "Package %s is not installed" (N.to_string name);
  let nv = NV.create name (V.Set.choose (N.Map.find name map_installed)) in
  let universe = Solver.U (NV.Set.fold (fun nv l -> (debpkg_of_nv t nv) :: l) t.available []) in
  let depends = Solver.filter_forward_dependencies universe (Solver.P [debpkg_of_nv t nv]) in
  let depends =
    List.fold_left (fun set dpkg -> NV.Set.add (NV.of_dpkg dpkg) set) NV.Set.empty depends in

  (* XXX: do we really want to call the solver here ? *)
  let wish_install =
    List.fold_left
      (fun accu nv -> if NV.Set.mem nv depends then accu else (vpkg_of_nv nv)::accu)
      []
      (* XXX: do we need to remove nv here ? it should already be in depends *)
      (NV.Set.elements (NV.Set.remove nv t.installed)) in

  resolve t 
    [ { wish_install
      ; wish_remove  = [ N.to_string name, None ]
      ; wish_upgrade = [] } ]
      
let upgrade () =
  log "upgrade";
  let t = load_state () in
  let available = NV.to_map t.available in
  resolve t
    [ { wish_install = []
      ; wish_remove = []
      ; wish_upgrade =
        List.map
          (fun nv ->
            let name = NV.name nv in
            let versions = N.Map.find name available in
            let nv = NV.create name (V.Set.max_elt versions) in
            vpkg_of_nv nv)
          (NV.Set.elements t.installed) } ]

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
        find_repository t (N.Map.find name t.repo_index)
      else
        Globals.error_and_exit "No repository found to upload %s" (NV.to_string nv)
  | Some repo -> find_repository t repo in
  let repo_p = List.assoc repo t.repositories in
  let upload_opam = Path.R.upload_opam repo_p nv in
  let upload_descr = Path.R.upload_descr repo_p nv in
  let upload_archives = Path.R.upload_archives repo_p nv in
  Filename.copy upload.opam upload_opam;
  Filename.copy upload.descr upload_descr;
  Filename.copy upload.archive upload_archives;
  Repositories.upload repo_p repo;
  Filename.remove upload_opam;
  Filename.remove upload_descr;
  Filename.remove upload_archives

(* Return the transitive closure of dependencies *)
let get_transitive_dependencies t names =
  let universe =
    Solver.U (List.map (debpkg_of_nv t) (NV.Set.elements t.installed)) in
  (* Compute the transitive closure of dependencies *)
  let pkg_of_name n = debpkg_of_nv t (find_package_by_name t.installed n) in
  let request = Solver.P (List.map pkg_of_name names) in
  let depends = Solver.filter_backward_dependencies universe request in
  List.map NV.of_dpkg depends

let config request =
  log "config %s" (string_of_config request);
  let t = load_state () in

  match request with
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
              let variables = File.Dot_config.Sections.variables c n in
              List.fold_left (fun accu v ->
                (Full_variable.create_local name n v, File.Dot_config.Sections.variable c n v) :: accu
              ) accu variables
            ) globals (File.Dot_config.Sections.available c)
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
          
  | Includes names ->
      let deps = get_transitive_dependencies t names in
      let includes =
        List.fold_left (fun accu nv ->
          Dirname.to_string (Path.C.lib t.compiler (NV.name nv)) :: accu
        ) [] deps in
      Globals.msg "%s\n" (String.concat " " includes)

  | Compil c ->

      match c.options with

      | _ -> failwith "TODO"
            
(*
    let version name =
      match
        match req, N_map.Exceptionless.find name installed with
          | Ocp, None -> Option.map V_set.max_elt (find_from_name name l_index)
          | _, o -> o
      with
        | None -> unknown_package name
        | Some v -> v in

    let rec iter_with_spaces f = function
      | []   -> ()
      | [h]  -> f h 
      | h::t -> f h; Globals.msg " "; iter_with_spaces f t in

    (* Get the list of installed packages *)
    let l_deb = debpkg_of_nv t installed l_index in

    (* return the dependencies of [names] *)
    let get_dependencies rec_search names =
      let versions = List.map (fun n -> n, version n) names in
      (* Get the packages we are looking for in the list of debian packages *)
      let l_pkg =
        List.filter
          (fun pkg ->
            let name, version = Namespace.nv_of_dpkg pkg in
            List.exists (fun (n,v) -> n=name && v=version) versions)
          l_deb in
      (* Compute the transitive closure of dependencies *)
      let depends =
        let d = 
          Solver.filter_backward_dependencies l_pkg l_deb in
        if rec_search = Recursive_large then 
          d
        else
          List.filter (fun p -> not (List.mem (fst (Namespace.nv_of_dpkg p)) names)) d in
(*      log "get_depends: %s => %s" (String.concat "," (List.map Namespace.string_of_name names))
        (String.concat ", " (List.map (fun k -> Namespace.to_string (Namespace.nv_of_dpkg k)) depends)); *)
      List.map  Namespace.nv_of_dpkg depends in

    let libraries_of_nv (name, version) =
      match File.PConfig.find (Path.O.pconfig t.home (name, version)) with
      | None        -> []
      | Some config -> File.PConfig.library_names config in

    let one (name, version) =
      let path = match Path.ocaml_options_of_library t.home name with I s -> s in
      match req with
      | Include -> Globals.msg "-I %s" path
      | Ocp     ->
          let libraries = libraries_of_nv (name, version) in
          let rec_search = match rec_search with Not_recursive -> Not_recursive | _ -> Recursive_strict in
          let depends = get_dependencies rec_search [name] in
          let requires =
            List.fold_left
              (fun accu dep -> libraries_of_nv dep @ accu)
              []
              depends in
          let requires = String.concat " " (List.map (Printf.sprintf "%S") requires) in
          let one lib =
            Globals.msg
              "begin library %S\n  generated=true\n  dirname=%S\n  requires=[%s]\nend\n\n"
              lib path requires in
          List.iter one libraries
      | link    ->
          let config = File.PConfig.find_err (Path.O.pconfig t.home (name, version)) in
          let libraries = File.PConfig.library_names config in
          let link_options = File.PConfig.link_options config in
          let asmlink_options = File.PConfig.link_options config in
          let bytelink_options = File.PConfig.link_options config in
          let options = function
            | [] -> ""
            | l  -> String.concat " " l ^ " " in
          let files ext  = String.concat " " (List.map (fun f -> f ^ ext) libraries) in
          match link with
          | Asmlink ->
              Globals.msg "-I %s %s%s"
                path
                (options (link_options@asmlink_options))
                (files ".cmxa")
          | Bytelink ->
              Globals.msg "-I %s %s%s"
                path
                (options (link_options@bytelink_options))
                (files ".cma")
          | _ -> assert false in

    match rec_search with
      | Not_recursive ->

      (* If we don't need to look at the dependencies, simply call [one] for
         each pair (name x version) *)
      iter_with_spaces one (List.map (fun n -> n, version n) names)

      | _ ->

      (* Otherwise, we need to compute the transitive closure of dependencies *)
      iter_with_spaces one (get_dependencies rec_search names)
*)

let remote action =
  log "remote %s" (string_of_remote action);
  (*    let t = load_state () in *)
  failwith "TODO"

(*
    let update_config servers =
      let config = File.Config.find_err (Path.config t.home) in
      let new_config = File.Config.with_sources config servers in
      File.Config.add (Path.config t.home) new_config in
    let add_url url =
      if List.mem url t.servers then
        Globals.error_and_exit "%s is already in the list of remote indexes" (string_of_url url)
      else
        update_config (url :: t.servers) in

    match action with

    | List ->
      List.iter (fun url ->
        match url.uri with
        | Some Git -> Globals.msg "git   %s\n" url.hostname
        | _        -> Globals.msg "OPAM  %s\n" (string_of_url url)
      ) t.servers

    | Add s    -> add_url (url s)

    | AddGit s ->
        let url = url ~uri:Git s in
        Run.Git.safe_remote_add
          (Path.string_of_filename (Path.index t.home None))
          url.hostname;
        add_url url

    | Rm s     ->
        let s = Run.U.normalize s in
        let server =
          try List.find (fun t -> string_of_url t = s || t.hostname = s) t.servers
          with Not_found ->
            Globals.error_and_exit "%s is not a remote index" s in
        if server.uri = Some Git then
          Run.Git.safe_remote_rm
            (Path.string_of_filename (Path.index t.home None))
            server.hostname;
        update_config (List.filter ((!=) server) t.servers)
*)

let switch name =
  log "switch %s" (OCaml_V.to_string name);
  (*    let t = load_state () in *)
  failwith "TODO"

(*
    let compile compil =
      failwith "TODO" in
    if Filename.check_suffix name ".compil" then begin
      (* we switch to a fresh OCaml install *)
      let compil = File.Compil.parse (Run.U.read name) in
      let name = File.Compil.name compil in
      let compil_f = Path.compil t.home name in
      if Path.file_exists compil_f then
        Globals.error_and_exit "Compiler spec %s already exists" name;
      File.Compil.add compil_f compil;
      compile compil
    end else begin
      let compil_f = Path.compil t.home name in
      let compil = File.Compil.find compil_f in
      compile compil
    end
*)
