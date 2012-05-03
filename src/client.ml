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
open Solver

let log fmt =
  Globals.log "CLIENT" fmt

type remote_request =
  | List
  | Add of string
  | Rm of string

type options =
  | Include  of N.t list
  | Bytecomp of (N.t * string) list
  | Asmcomp  of (N.t * string) list
  | Bytelink of (N.t * string) list
  | Asmlink  of (N.t * string) list

type compil_option = {
  recursive: bool;
  options  : options;
}

type config_request =
  | Compil   of compil_option
  | Variable of (N.t * Variable.t) list
  | Subst    of Filename.t list

type upload_request = {
  opam   : Filename.t;
  descr  : Filename.t;
  archive: Filename.t;
}

module type CLIENT =
sig
  type t

  (** Initializes the client a consistent state. *)
  val init : repository list -> unit

  (** Displays all available packages *)
  val list : unit -> unit

  (** Displays a general summary of a package. *)
  val info : N.t -> unit

  (** Depending on request, returns options or directories where the package is installed. *)
  val config : config_request -> unit

  (** Installs the given package. *)
  val install : N.t -> unit

  (** Downloads the latest packages available. *)
  val update : unit -> unit

  (** Finds a consistent state where most of the installed packages are
      upgraded to their latest version. *)
  val upgrade : unit -> unit

  (** Sends a new created package to the server. *)
  val upload : upload_request -> unit

  (** Removes the given package. *)
  val remove : N.t -> unit

  (** Manage remote repositories *)
  val remote : remote_request -> unit

end

module Client : CLIENT = struct
  open File

  type t = {
    global      : Path.G.t;                     (* ~/.opam/ *)
    compiler    : Path.C.t;                     (* ~/.opam/$oversion/ *)
    repositories: (repository * Path.R.t) list; (* ~/.opam/repo/$repo/ *)
    installed   : NV.Set.t;                     (* ~/.opam/$oversion/installed contents *)
    config      : File.Config.t;                (* ~/.opam/config contents *)
    repo_index  : File.Repo_index.t;            (* ~/.opam/repo/index contents *)
  }

  (* Look into the content of ~/.opam/config to build the client state *)
  (* Do not call RemoteServer functions here, as it implies a
     network roundtrip *)
  let load_state () =
    let global = Path.G.create (d !Globals.root_path) in
    let config = File.Config.read (Path.G.config global) in
    let ocaml_version = File.Config.ocaml_version config in
    let compiler = Path.C.create global ocaml_version in
    let repositories = File.Config.repositories config in
    let repositories = List.map (fun r -> r, Path.R.create global r) repositories in
    let repo_index = File.Repo_index.read (Path.G.repo_index global) in
    let installed = File.Installed.read (Path.C.installed compiler) in
    { global; compiler; repositories; installed; repo_index; config }

  let update () =
    let t = load_state () in
    List.map (fun (r,p) -> Repositories.opam_update p r) t.repositories

  let init repos =
    log "init %s" (String.concat " " (List.map Repository.to_string repos));
    let root = Path.G.create (Dirname.of_string !Globals.root_path) in
    let config_f = Path.G.config root in
    if Filename.exists config_f then
      Globals.error_and_exit "%s already exist" (Filename.to_string config_f)
    else
      let opam_version = OPAM_V.of_string Globals.opam_version in
      let ocaml_version = OCaml_V.of_string Sys.ocaml_version in
      let config = File.Config.create opam_version repos ocaml_version in
      File.Config.write config_f config;
      let compiler = Path.C.create root ocaml_version in
      File.Installed.write (Path.C.installed compiler) File.Installed.empty;
      let repositories = File.Config.repositories config in
      List.iter (fun r ->
        let p = Path.R.create root r in
        Repositories.opam_init p r
      ) repositories

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

  let find_from_name name l =
    List.find_all (fun (n,_) -> n = name) l

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
      let available = Path.G.available t.global in
      let v_set = 
        try N.Map.find package (NV.to_map available)
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
    let to_install = File.To_install.read (Path.C.install t.compiler name) in

    (* lib *) 
    let lib = Path.C.lib t.compiler name in
    List.iter (fun f -> Filename.copy_in f lib) (File.To_install.lib to_install);
  
    (* bin *) 
    List.iter (fun (src, dst) -> Filename.copy src dst) (File.To_install.bin to_install);
  
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
      ) (File.To_install.misc to_install)

  let proceed_todelete t nv =
    log "deleting %s" (NV.to_string nv);
    let name = NV.name nv in

    (* Remove the libraries *)
    Dirname.remove (Path.C.lib t.compiler name);
          
    (* Remove the binaries *)
    let to_install = File.To_install.read (Path.C.install t.compiler name) in
    List.iter (fun (_,dst) -> Filename.remove dst) (File.To_install.bin to_install);

    (* Remove the misc files *)
    List.iter (fun (_,dst) ->
      if Filename.exists dst then begin
        Globals.msg "Removing %s." (Filename.to_string dst);
        if confirm "Continue ?" then
          Filename.remove dst
      end
    ) (File.To_install.misc to_install)

  let get_archive t nv =
    log "get_archive %s" (NV.to_string nv);
    let name = NV.name nv in
    let repo = N.Map.find name t.repo_index in
    let src = Path.R.archive (List.assoc repo t.repositories) nv in
    let dst = Path.G.archive t.global nv in
    Filename.link src dst;
    dst

  let proceed_tochange t nv_old nv =
    (* First, uninstall any previous version *)
    (match nv_old with 
      | Was_installed nv_old -> proceed_todelete t nv_old
      | Was_not_installed    -> ());

    (* Then, untar the archive *)
    let p_build = Path.C.build t.compiler nv in
    Dirname.remove p_build;
    Filename.extract (get_archive t nv) p_build;

    (* Call the build script and copy the output files *)
    let opam = File.OPAM.read (Path.G.opam t.global nv) in
    let commands =
      List.map
        (fun cmd -> String.concat " " (List.map (Printf.sprintf "'%s'") cmd))
        (File.OPAM.build opam) in
    let err = Run.commands commands in
    if err = 0 then
      proceed_toinstall t nv
    else
      Globals.error_and_exit
        "Compilation failed with error %d" err

  (* We need to clean-up things before recompiling.

     NB: Currently, the implementation follows only a simple parallelism scheme.
     Determine if we need to clean-up when the parallelism scheme is full. *)
  let proceed_torecompile t nv =
    proceed_tochange t (Was_installed nv) nv

  let debpkg_of_nv t =
    List.map
      (fun nv ->
        let opam = File.OPAM.read (Path.G.opam t.global nv) in
        let installed = NV.Set.mem nv t.installed in
        File.OPAM.to_package opam installed)

  let resolve t l_index map_installed request = 
    let l_pkg = debpkg_of_nv t l_index in

    match Solver.resolve_list l_pkg request with
    | [] -> Globals.msg "No solution has been found.\n"
    | l -> 
        let nb_sol = List.length l in

        let rec aux pos = 
          Globals.msg "[%d/%d] The following solution has been found:\n" pos nb_sol;
          function
          | [x] ->
              (* Only 1 solution exists *)
              print_solution NV.to_string x;
              if Solver.delete_or_update x then
                if confirm "Continue ?" then
                  Some x
                else
                  None
              else
                Some x

          | x :: xs ->
              (* Multiple solution exist *)
              print_solution NV.to_string x;
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
            let child n = match n.action with
              | To_change (o, nv) -> proceed_tochange t o nv
              | To_recompile nv   -> proceed_torecompile t nv
              | To_delete _       -> assert false in

            let pre _ = () in

            (* Update the installed file in the parent process *)
            let post n = match n.action with
              | To_delete _    -> assert false
              | To_recompile _ -> ()
              | To_change (Was_not_installed, nv) ->
                  installed := NV.Set.add nv !installed;
                  write_installed ()
              | To_change (Was_installed o, nv)   ->
                  installed := NV.Set.add nv (NV.Set.remove o !installed);
                  write_installed () in

            let error n =
              let f msg nv =
                Globals.error_and_exit "Command failed while %s %s" msg (NV.to_string nv) in
              match n.action with
              | To_change (Was_installed _, nv)   -> f "upgrading" nv
              | To_change (Was_not_installed, nv) -> f "installing" nv
              | To_recompile nv -> f "recompiling" nv
              | To_delete _     -> assert false in

            try G.P.iter Globals.cores sol.to_add ~pre ~child ~post
            with G.P.Error n -> error n

  let vpkg_of_nv nv =
    let name = NV.name nv in
    let version = NV.version nv in
    N.to_string name, Some ("=", V.to_string version)

  let unknown_package name =
    Globals.error_and_exit "Unable to locate package %S\n" (N.to_string name)

  let install name = 
    log "install %s" name;
    let t = load_state () in
    let l_index = Path.index_list t.home in
    let map_installed = File.Installed.Map.find (Path.O.installed t.home) in
    let package = Namespace.name_of_string name in

    (* Fail if the package is already installed *)
    if N_map.mem package map_installed then
      Globals.error_and_exit
        "Package %s is already installed (current version is %s)"
        name
        (Namespace.string_of_version (N_map.find package map_installed));

    match find_from_name package l_index with

      | None   ->
          if Namespace.is_valid_nv name then begin
            let n, v = Namespace.nv_of_string name in
            Globals.msg
              "Package %s not found, looking for package %s version %s\n"
              name (Namespace.string_of_name n) (Namespace.string_of_version v);
            (match File.Spec.find (Path.index t.home (Some (n, v))) with
            | None   -> unknown_package n
            | Some _ ->
              resolve t
                l_index
                map_installed
                [ { Action.wish_install = 
                    List.map vpkg_of_nv ((n, v) :: N_map.bindings (N_map.remove n map_installed))
                  ; wish_remove = [] 
                  ; wish_upgrade = [] } ])
          end else
            unknown_package (Namespace.name_of_string name)

      | Some v ->
          let name = Namespace.name_of_string name in
          resolve t
            l_index
            map_installed
            [ { Action.wish_install = 
                List.map vpkg_of_nv ((name, V_set.max_elt v) :: N_map.bindings (N_map.remove name map_installed))
              ; wish_remove = [] 
              ; wish_upgrade = [] } ]

  let remove name =
    log "remove %s" (Namespace.string_of_name name);
    let t = load_state () in
    let l_index = Path.index_list t.home in
    let installed = File.Installed.Map.find (Path.O.installed t.home) in

    let dependencies = 
      NV_set.of_list
        (List.map Namespace.nv_of_dpkg
           (Solver.filter_forward_dependencies
              (match N_map.Exceptionless.find name installed with 
                | None -> []
                | Some v -> debpkg_of_nv t installed [name, v])
              (debpkg_of_nv t installed l_index))) in

    resolve t 
      l_index
      installed
      [ { Action.wish_install = 
          List.filter_map 
            (fun nv ->
              if NV_set.mem nv dependencies then
                None
              else
                Some (vpkg_of_nv nv)) 
            (N_map.bindings (N_map.remove name installed))
        ; wish_remove = [ Namespace.string_of_name name, None ]
        ; wish_upgrade = [] } ]
      
  let upgrade () =
    log "upgrade";
    let t = load_state () in
    let l_index = Path.index_list t.home in
    let installed = File.Installed.Map.find (Path.O.installed t.home) in
    (* mark git repo with updates *)
    let installed =
      N_map.mapi (fun n -> function
        | Head _ as v ->
          let repo = Path.string_of_filename (Path.index t.home (Some (n, v))) in
          if Run.Git.get_updates repo = [] then
            Head `uptodate
          else begin
            Run.Git.update repo;
            Head `behind
          end
        | v -> v
      ) installed in
    resolve t
      l_index
      installed
      [ { Action.wish_install = []
        ; wish_remove = []
        ; wish_upgrade = 
          List.map
            (fun (name, _) -> 
              match find_from_name name l_index with 
                | None -> assert false (* an already installed package must figure in the index *) 
                | Some v -> vpkg_of_nv (name, V_set.max_elt v))
            (N_map.bindings installed) } ]

  (* XXX: ask the user on which repo she wants to upload the new package *)
  (* XXX: hanlde git repo as well ... *)
  let iter_upload_server fn servers =
    let one server =
      if server.uri = Some Git then
        None
      else begin
        if List.length servers <= 1 || confirm (Printf.sprintf "Upload to %s ?" server.hostname) then
          Some (fn server)
        else
          None
      end in
    List.fold_left (fun k server ->
      let nk = one server in
      if k <> None && k <> nk then
        Globals.error_and_exit "upload keys differ!"
      else
        nk
    ) None servers

  let newArchive servers nv spec archive =
    iter_upload_server (fun server ->
      RemoteServer.newArchive server nv spec archive
    ) servers

  let updateArchive servers nv spec archive k =
    let (_ : unit option) =
      iter_upload_server (fun server ->
        RemoteServer.updateArchive server nv spec archive k
      ) servers in
    ()

  (* Upload reads NAME.spec (or NAME if it ends .spec) to get the current package version.
     Then it looks for NAME-VERSION.tar.gz in the same directory (if it exists).
     If not, it looks for provided URLs.
     Then, it sends both NAME.spec and NAME-VERSION.tar.gz to the server *)
  let upload name =
    log "upload %s" name;
    let t = load_state () in

    (* Get the current package version *)
    let spec_f =
      if Filename.check_suffix name "spec" then
        name
      else
        name ^ ".spec" in
    let spec_s = Run.U.read spec_f in
    let spec = File.Spec.parse spec_s in
    let version = File.Spec.version spec in
    let name = File.Spec.name spec in
    let spec_b = Raw_binary (File.Spec.to_string spec) in

    (* look for the archive *)
    let archive_filename = Namespace.string_of_nv name version ^ ".tar.gz" in
    let archive =
      if Sys.file_exists archive_filename then
        Raw_binary (Run.U.read archive_filename)
      else
        let sources = File.Spec.sources spec in
        let patches = File.Spec.patches spec in
        (* the ".spec" being processed contains only local patches *)
        let nv = name, version in
        let tmp_nv = Path.concat Path.cwd (B (Namespace.string_of_nv (fst nv) (snd nv))) in
        let () =
          begin                 
            (* try to check that patches are well-parsed before the copy.
               Currently, only ".install" are processed. *)
            List.iter
              (function Internal p, _ ->
                if not (Sys.is_directory p) && Filename.check_suffix p "install" then
                  (try ignore (File.To_install.parse (Run.U.read p)) with e -> 
                    Globals.error_and_exit "%s\nwhile parsing '%s'." (Printexc.to_string e) p)
                else
                  () (* TODO perform the recursive check for a directory. 
                        Change [Path.add_rec] such that it accepts an optional checking function. *)
                | _ -> ())
              patches;
              
            (* include the patches inside the downloaded directory *)
            Path.add_rec tmp_nv (Path.extract nv (Links { sources ; patches }));
            Path.to_archive archive_filename tmp_nv;
            Path.remove tmp_nv;
          end in
        Raw_binary (Run.U.read archive_filename) in

    (* Upload both files to the server and update the client
       filesystem to reflect the new uploaded packages *)
    let local_server = server_init !Globals.root_path in

    let o_key = File.Security_key.find (Path.keys t.home name) in
    match o_key with
    | None   ->
        let k1 = newArchive t.servers (name, version) spec_b archive in
        let k2 = Server.newArchive local_server (name, version) spec_b archive in
        let k = match k1 with
          | None   -> k2
          | Some k -> k in
        File.Security_key.add (Path.keys t.home name) k
    | Some k ->
        updateArchive t.servers (name, version) spec_b archive k;
        Server.updateArchive local_server (name, version) spec_b archive k

  let config rec_search req names =
    log "config %s" (String.concat "," (List.map Namespace.string_of_name names));
    let t = load_state () in

    let l_index = Path.index_list t.home in

    let installed = File.Installed.Map.find (Path.O.installed t.home) in

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

  let string_of_remote_action = function
    | List     -> "list"
    | Add s    -> Printf.sprintf "add %s" s
    | AddGit s -> Printf.sprintf "add-git %s" s
    | Rm s     -> Printf.sprintf "rm %s" s

  let remote action =
    log "remote %s" (string_of_remote_action action);
    let t = load_state () in
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

  let switch name =
    log "switch %s" name;
    let t = load_state () in
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
      
end
