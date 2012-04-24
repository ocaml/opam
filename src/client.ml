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

open ExtList
open Namespace
open Path
open Server
open Solver
open Uri
open Protocol

let log fmt =
  Globals.log "CLIENT" fmt

type remote_action =
  | List
  | Add of string
  | AddGit of string
  | Rm of string

type config_recursive = 
  | Not_recursive
  | Recursive_large (* package + dependencies *)
  | Recursive_strict (* only dependencies, not package *)

type config_request = Include | Bytelink | Asmlink | Ocp

module type CLIENT =
sig
  type t

  (** Initializes the client a consistent state. *)
  val init : url list -> unit

  (** Displays all available packages *)
  val list : unit -> unit

  (** Displays a general summary of a package. *)
  val info : name -> unit

  (** Depending on request, returns options or directories where the package is installed. *)
  val config : config_recursive (* search power *) -> config_request -> name list -> unit

  (** Installs the given package. *)
  val install : string -> unit

  (** Downloads the latest packages available. *)
  val update : unit -> unit

  (** Finds a consistent state where most of the installed packages are
      upgraded to their latest version. *)
  val upgrade : unit -> unit

  (** Sends a new created package to the server. *)
  val upload : string -> unit

  (** Removes the given package. *)
  val remove : name -> unit

  (** Manage remote indexes *)
  val remote : remote_action -> unit

  (** Switch to an other version of ocaml *)
  val switch : string -> unit
end

module Client : CLIENT = struct
  open File

  type t = 
      { servers: url list
      ; home   : Path.t (* ~/.opam *) }

  (* Look into the content of ~/.opam/config to build the client state *)
  (* Do not call RemoteServer functions here, as it implies a
     network roundtrip *)
  let load_state () =
    let home = Path.init !Globals.root_path in
    let config = File.Config.find_err (Path.config home) in
    let servers = File.Config.sources config in
    let Version v = File.Config.ocaml_version config in
    { servers ; home = Path.O.set_version home v }

  let update_remote server home =
    log "update-remote-server %s%s"
      server.hostname
      (match server.port with Some p -> ":" ^ string_of_int p | None -> "");
    let packages = RemoteServer.getList server in
    List.iter
      (fun (n, v) -> 
        let spec_f = Path.index home (Some (n, v)) in
        if not (Path.file_exists spec_f) then
          let spec = RemoteServer.getSpec server (n, v) in
          Path.add spec_f (Path.File (Binary spec));
          Globals.msg "New package available: %s\n" (Namespace.string_of_nv n v)
      ) packages

  let update_git server home =
    log "update-git-server %s" server.hostname;
    let index_path = Path.string_of_filename (Path.index home None) in
    if not (Sys.file_exists index_path) then begin
      Unix.mkdir index_path 0o750;
      Run.Git.init index_path;
    end;
    Run.Git.safe_remote_add index_path server.hostname;
    let newfiles = Run.Git.get_updates index_path in
    Run.Git.update index_path;
    let package_of_file file =
      if Filename.check_suffix file ".spec" then
        Some (Namespace.nv_of_string (Filename.chop_extension file))
      else
        None in
    let packages = List.fold_left
      (fun accu file ->
        match package_of_file file with
        | None   -> accu
        | Some nv -> NV_set.add nv accu)
      NV_set.empty
      newfiles in
    NV_set.iter (fun (n, v) ->
      Globals.msg "New package available: %s\n" (Namespace.string_of_nv n v)
    ) packages

  let update () =
    let t = load_state () in
    let one server =
      match server.uri with
      | Some Git -> update_git server t.home
      | _        -> update_remote server t.home in
    List.iter one t.servers

  let init urls =
    log "init %s" (String.concat " " (List.map string_of_url urls));
    let home = Path.init !Globals.root_path in
    let config_f = Path.config home in
    match File.Config.find config_f with
    | Some c ->
        Globals.error_and_exit "%s already exist" (Path.string_of_filename config_f)
    | None   ->
      let Version ocaml_version = File.Config.ocaml_version File.Config.empty in
      let config =
        File.Config.create
          Globals.api_version
          urls
          (Version ocaml_version) in
      File.Config.add config_f config;
      let home = Path.O.set_version home ocaml_version in
      File.Installed.add (Path.O.installed home) File.Installed.empty;
      try update ()
      with Connection_error msg ->
        Run.safe_rmdir !Globals.root_path;
        Globals.error_and_exit "%s" msg

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
    N_map.Exceptionless.find 
      name
      (List.fold_left
         (fun map (n, v) -> 
           N_map.modify_def V_set.empty n (V_set.add v) map) N_map.empty l)

  let s_not_installed = "--"

  let list () =
    log "list";
    let t = load_state () in
    (* Get all the installed packages *)
    let installed = File.Installed.find_err (Path.O.installed t.home) in
    let install_set = NV_set.of_list installed in
    let map, max_n, max_v = 
      List.fold_left
        (fun (map, max_n, max_v) (name, version as n_v) ->
          match N_map.Exceptionless.find name map with
          | Some (Some _, _) -> map, max_n, max_v
          | _ -> 
              (* If the packet has not been processed yet or 
                 if it has been processed but the version processed was not installed *)
              let installed = NV_set.mem n_v install_set in
              let index = File.Spec.find_err (Path.index t.home (Some n_v)) in
              let map =
                N_map.add name ((if installed then Some version else None), File.Spec.description index) map in
              let max_n = max max_n (String.length (Namespace.string_of_name (fst n_v))) in
              let max_v =
                if installed then
                  max max_v (String.length (Namespace.string_of_version (snd n_v)))
                else
                  max_v in
              map, max_n, max_v)
        (N_map.empty, min_int, String.length s_not_installed)
        (Path.index_list t.home) in

    N_map.iter (fun name (version, description) ->
      let description = match description with
      | []   -> ""
      | h::_ -> h in
      let version = match version with
      | None   -> s_not_installed
      | Some v -> Namespace.string_of_version v in
      Globals.msg "%s  %s  %s\n" 
        (indent_left  (Namespace.string_of_name name) max_n)
        (indent_right version max_v)
        description) map

  let info package =
    log "info %s" (Namespace.string_of_name package);
    let t = load_state () in
    let find_from_name = find_from_name package in
    let installed = File.Installed.find_err (Path.O.installed t.home) in
    let o_v = 
      Option.map
        V_set.choose (* By definition, there is exactly 1 element, we choose it. *) 
        (find_from_name installed) in

    let v_set =
      let v_set = 
        match find_from_name (Path.index_list t.home) with
        | None -> V_set.empty
        | Some v -> v in
      match o_v with
      | None -> v_set
      | Some v -> V_set.remove v v_set in

    List.iter
      (fun (tit, desc) -> Globals.msg "%s: %s\n" tit desc)
      (  ("package    ", Namespace.string_of_name package)

         :: ("version    ",
             match o_v with
             | None   -> s_not_installed
             | Some v -> Namespace.string_of_version v)

         :: ("versions   ", V_set.to_string Namespace.string_of_version v_set)

         ::
           match
             match o_v with
             | None -> if V_set.is_empty v_set then None else Some (V_set.max_elt v_set)
             | Some v -> Some v
           with
           | None -> []
           | Some v ->

               [ "description", "\n  " ^ 
                 let opam =
                   File.Spec.find_err (Path.index t.home (Some (package, v))) in
                 String.concat "" (File.Spec.description opam) ]
      )

  let confirm msg = 
    Globals.msg "%s [Y/n] " msg;
    match read_line () with
      | "y" | "Y"
      | "" -> true
      | _  -> false

  let iter_toinstall f_add_rec t (name, v) = 

    let to_install = File.To_install.find_err (Path.O.to_install t.home (name, v)) in

    let filename_of_path_relative t path = 
      Path.R_filename (File.To_install.filename_of_path_relative
                         (Path.O.build t.home (Some (name, v))) 
                         path) in
    
    let add_rec f_lib t path = 
      f_add_rec
        (f_lib t.home name (* warning : we assume that this result is a directory *))
        (filename_of_path_relative t path) in

    (* lib *) 
    List.iter (add_rec Path.O.lib t) (File.To_install.lib to_install);
  
    (* bin *) 
    List.iter (fun m ->
      let root = Path.O.build t.home (Some (name, v)) in
      let src = File.To_install.path_from m in
      let src = match File.To_install.filename_of_path_relative root src with
        | [f] -> f
        | _   -> Globals.error_and_exit "'bin' files cannot contain * patterns" in

      let dst = File.To_install.path_to m in
      let dst = match dst with
        | (Relative, [], Exact s) -> Path.concat (Path.O.bin t.home) (B s)
        | p -> Globals.error_and_exit "invalid program name %s" (string_of_path p) in

      (* XXX: use the API *)
      Run.copy (Path.string_of_filename src) (Path.string_of_filename dst)
    ) (File.To_install.bin to_install);
  
    (* misc *)
    List.iter 
      (fun misc ->
        Globals.msg "Copy %s.\n" (File.To_install.string_of_move misc);
        if confirm "Continue ?" then
          let path_from =
            filename_of_path_relative t (File.To_install.path_from misc) in
          List.iter 
            (fun path_to -> f_add_rec path_to path_from) 
            (File.To_install.filename_of_path_absolute
               (File.To_install.path_to misc)))
      (File.To_install.misc to_install)

  let proceed_todelete t (n, v0) map_installed =
    log "deleting %s" (Namespace.to_string (n, v0));
    match N_map.Exceptionless.find n map_installed with
      | Some v when v = v0 ->
          (* Remove the libraries *)
          Path.remove (Path.O.lib t.home n);
          
          (* Remove the binaries *)
          let to_install =
            File.To_install.find_err (Path.O.to_install t.home (n, v0)) in
          let bins =
            let file m =
              File.To_install.filename_of_path
                (Path.O.bin t.home)
                (File.To_install.path_to m) in
            List.flatten (List.map file (File.To_install.bin to_install)) in
          List.iter Path.remove bins;

          List.iter 
            (fun misc ->
              List.iter 
                (fun path_to ->                   
                  Globals.msg "The complete directory '%s' will be removed.\n" (Path.string_of_filename path_to);
                  if confirm "Continue ?" then
                    Path.remove path_to)
                (File.To_install.filename_of_path_absolute
                   (File.To_install.path_to misc)))
            (File.To_install.misc to_install)

      | _ -> assert false (* check for example if the solver has returned a wrong version or not *)

  (* Iterate over the list of servers to find one with the corresponding archive *)
  let getArchive servers nv =
    let rec aux = function
    | []   -> None
    | h::t ->
      if h.uri = Some Git then
        None
      else match RemoteServer.getArchive h nv with
      | None   -> aux t
      | Some a -> Some a in
    aux servers

  let proceed_tochange t nv_old (name, v as nv) =
    let map_installed = File.Installed.Map.find (Path.O.installed t.home) in
    (* First, uninstall any previous version *)
    (match nv_old with 
    | Was_installed nv_old -> proceed_todelete t nv_old map_installed
    | Was_not_installed    -> ());

    let spec = File.Spec.find_err (Path.index t.home (Some nv)) in

    (* Then, untar the archive *)
    let p_build = Path.O.build t.home (Some nv) in
    Path.remove p_build;
    (* XXX: maybe we want to follow the external urls first *)
    (* XXX: at one point, we would need to check SHA1 consistencies as well *)
    let archive = match getArchive t.servers nv with
      | Some tgz -> Archive tgz
      | None     ->
          let sources = File.Spec.sources spec in
          let patches = File.Spec.patches spec in
          Links { sources; patches } in
    let archive = Path.extract nv archive in
    log "Process %s archive" (Namespace.to_string nv);
    Path.add_rec p_build archive;

    (* Call the build script and copy the output files *)
    let buildsh = File.Spec.make spec in
    log "Run %s" (File.Spec.string_of_command buildsh);
    let err = Path.exec t.home nv buildsh in
    if err = 0 then
      iter_toinstall Path.add_rec t nv
    else
      Globals.error_and_exit
        "Compilation failed with error %d" err

  (* We need to clean-up things before recompiling.

     NB: Currently, the implementation follows only a simple parallelism scheme.
     Determine if we need to clean-up when the parallelism scheme is full. *)
  let proceed_torecompile t nv =
    proceed_tochange t (Was_installed nv) nv

  let debpkg_of_nv t map_installed =
    List.map
      (fun n_v ->
        let opam = File.Spec.find_err (Path.index t.home (Some n_v)) in
        File.Spec.to_package opam
          (match N_map.Exceptionless.find (fst n_v) map_installed with
            | Some v -> v = snd n_v
            | _ -> false))

  let resolve t l_index map_installed request = 
    let l_pkg = debpkg_of_nv t map_installed l_index in

    match Solver.resolve_list l_pkg request with
    | [] -> Globals.msg "No solution has been found.\n"
    | l -> 
      let nb_sol = List.length l in

      let rec aux pos = 
        Globals.msg "[%d/%d] The following solution has been found:\n" pos nb_sol;
        function
        | [x] ->
          (* Only 1 solution exists *)
          Action.solution_print Namespace.to_string x;
          if Solver.delete_or_update x then
            if confirm "Continue ?" then
              Some x
            else
              None
          else
            Some x

        | x :: xs ->
          (* Multiple solution exist *)
          Action.solution_print Namespace.to_string x;
          if Solver.delete_or_update x then
            if confirm "Continue ? (press [n] to try another solution)" then
              Some x
            else
              aux (succ pos) xs
          else
            Some x

        | [] -> assert false in

      match aux 1 l with
      | Some sol -> 
        begin
          List.iter 
            (fun nv -> 
              File.Installed.Map.modify_def (Path.O.installed t.home) 
                (fun map_installed ->
                  let () = proceed_todelete t nv map_installed in
                  (* Remove the package from the installed package file *)
                  N_map.remove (fst nv) map_installed))
            sol.Action.to_remove;

          (let module Graph = Action.NV_graph.PG_topo_para in
           let module Process = Run.Process in
           let include_state = List.map (fun x -> Process.Not_yet_begun, x) in
           let rec aux proc graph = function
            | [] -> ()
            | l -> 
                let proc, l, (v_end, ()) = Process.filter_finished proc l in
                let () = 
                  (* NOTE we modify here the location of [Path.O.installed], by adding an element.
                     This side effect is not important for futur concurrent execution in [Process.filter_finished] 
                     because we suppose that each call to [Path.O.installed] is done with a different (name, version) as argument. *)
                  match v_end with
                    | { Action.NV_graph.PkgV.action = Action.To_change (Was_not_installed, (name, v)) ; _ } -> 
                        (* Mark the packet as installed *)
                        File.Installed.Map.modify_def (Path.O.installed t.home) (N_map.add name v)
                    | _ -> () in
                let graph, children = Graph.children graph v_end in
                aux proc graph (List.concat [ l ; include_state children ]) in
           let graph, root = Graph.root sol.Action.to_add in
           aux
             (Process.init
                Process.cores
                (function { Action.NV_graph.PkgV.action ; _ } -> 
                  (* WARNING side effects should be carefully studied as this function is executed concurrently *)
                  match action with
                    | Action.To_change (o, n_v) -> proceed_tochange t o n_v
                    | Action.To_delete _ -> assert false
                    | Action.To_recompile n_v -> proceed_torecompile t n_v)

                (function { Action.NV_graph.PkgV.action ; _ } ->
                  let f msg (name, v) =
                    Printf.sprintf "(%s) %s-%s" msg (Namespace.string_of_name name) (Namespace.string_of_version v) in
                  match action with
                    | Action.To_change (Was_installed _, nv) -> f "Change" nv
                    | Action.To_change (Was_not_installed, nv) -> f "Instal" nv
                    | Action.To_recompile nv -> f "Recomp" nv
                    | Action.To_delete _ -> assert false))
             graph
             (include_state root));
        end
      | None -> ()

  let vpkg_of_nv (name, v) =
    Namespace.string_of_name name, Some ("=", Namespace.string_of_version v)

  let unknown_package name =
    Globals.error_and_exit
      "Unable to locate package \"%s\"\n"
      (Namespace.string_of_name  name)

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
    let spec_s = Run.read spec_f in
    let spec = File.Spec.parse spec_s in
    let version = File.Spec.version spec in
    let name = File.Spec.name spec in
    let spec_b = Raw_binary (File.Spec.to_string spec) in

    (* look for the archive *)
    let archive_filename = Namespace.string_of_nv name version ^ ".tar.gz" in
    let archive =
      if Sys.file_exists archive_filename then
        Raw_binary (Run.read archive_filename)
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
                  (try ignore (File.To_install.parse (Run.read p)) with e -> 
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
        Raw_binary (Run.read archive_filename) in

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
        let s = Run.normalize s in
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
      let compil = File.Compil.parse (Run.read name) in
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
