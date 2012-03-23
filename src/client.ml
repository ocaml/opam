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

let log fmt =
  Globals.log "CLIENT" fmt

module type CLIENT =
sig
  type t

  (** Initializes the client a consistent state. *)
  val init : url -> unit

  (** Displays the installed package. [None] : a general summary is given. *)
  val info : Namespace.name option -> unit

  type config_request = Include | Bytelink | Asmlink

  (** Depending on request, returns options or directories where the package is installed. *)
  val config : bool (* true : recursive search *) -> config_request -> Namespace.name list -> unit

  (** Installs the given package. *)
  val install : Namespace.name -> unit

  (** Downloads the latest packages available. *)
  val update : unit -> unit

  (** Finds a consistent state where most of the installed packages are
      upgraded to their latest version. *)
  val upgrade : unit -> unit

  (** Sends a new created package to the server. *)
  val upload : string -> unit

  (** Removes the given package. *)
  val remove : Namespace.name -> unit
end

module Client : CLIENT = struct
  open File

  type t = 
      { server : url
      ; home   : Path.t (* ~/.opam *) }


  (* Look into the content of ~/.opam/config to build the client state *)
  let load_state () =
    let home = Path.init !Globals.root_path in
    let config = File.Config.find_err (Path.config home) in
    let server = File.Config.sources config in
    if RemoteServer.acceptedVersion server Globals.version then
      { server ;  home }
    else
      begin
        Globals.msg "The version of this program is different than the one at server side.\n";
        exit 1;
      end

  let update_t t =
    let packages = RemoteServer.getList t.server in
    List.iter
      (fun (n, v) -> 
        let opam_file = Path.index t.home (Some (n, v)) in
        if not (Path.file_exists opam_file) then
          let opam = RemoteServer.getOpam t.server (n, v) in
          Path.add opam_file (Path.File opam);
          Globals.msg "New package available: %s" (Namespace.string_of_nv n v)
      ) packages

  let update () =
    update_t (load_state ())

  let init url =
    log "init %s" (string_of_url url);
    let home = Path.init !Globals.root_path in
    let config =
      File.Config.create
        Globals.api_version
        url
        (Version Globals.ocaml_version) in
    File.Config.add (Path.config home) config;
    File.Installed.add (Path.installed home) File.Installed.empty;
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

  let find_from_name name l = 
    N_map.Exceptionless.find 
      name
      (List.fold_left
         (fun map (n, v) -> 
            N_map.modify_def V_set.empty n (V_set.add v) map) N_map.empty l)

  let info package =
    log "info %s" (match package with
      | None -> "ALL"
      | Some p -> Namespace.string_of_name p);
    let t = load_state () in
    let s_not_installed = "--" in
    match package with

    | None -> 
        (* Get all the installed packages *)
        let installed = File.Installed.find_err (Path.installed t.home) in
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
                let map = N_map.add name ((if installed then Some version else None), File.Spec.description index) map in
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

    | Some name -> 
        let find_from_name = find_from_name name in
        let installed = File.Installed.find_err (Path.installed t.home) in
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
          (  ("package    ", Namespace.string_of_name name)

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
                  File.Spec.find_err (Path.index t.home (Some (name, v))) in
                String.concat "" (File.Spec.description opam) ]
          )

  let confirm msg = 
    Globals.msg "%s [Y/n] " msg;
    match read_line () with
      | "y" | "Y"
      | "" -> true
      | _  -> false

  let iter_toinstall f_add_rec t (name, v) = 

    let to_install = File.To_install.find_err (Path.to_install t.home (name, v)) in

    let filename_of_path_relative t path = 
      Path.R_filename (File.To_install.filename_of_path_relative
                         (Path.build t.home (Some (name, v))) 
                         path) in
    
    let add_rec f_lib t path = 
      f_add_rec
        (f_lib t.home name (* warning : we assume that this result is a directory *))
        (filename_of_path_relative t path) in

    (* lib *) 
    List.iter (add_rec Path.lib t) (File.To_install.lib to_install);
  
    (* bin *) 
    List.iter (fun m ->
      let root = Path.build t.home (Some (name, v)) in
      let src = File.To_install.path_from m in
      let src = match File.To_install.filename_of_path_relative root src with
        | [f] -> f
        | _   -> Globals.error_and_exit "'bin' files cannot contain * patterns" in

      let dst = File.To_install.path_to m in
      let dst = match dst with
        | (Relative, [], Exact s) -> Path.concat (Path.bin t.home) (B s)
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

  let proceed_todelete t (n, v0) =
    log "deleting %s" (Namespace.to_string (n, v0));
    File.Installed.modify_def (Path.installed t.home) 
      (fun map_installed -> 
        match N_map.Exceptionless.find n map_installed with
          | Some v when v = v0 ->
              (* Remove the libraries *)
              Path.remove (Path.lib t.home n);

              (* Remove the binaries *)
              let to_install =
                File.To_install.find_err (Path.to_install t.home (n, v0)) in
              let bins =
                let file m =
                  File.To_install.filename_of_path
                    (Path.bin t.home)
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
                (File.To_install.misc to_install);

              (* Remove the package from the installed package file *)
              N_map.remove n map_installed
          | _ -> map_installed)

  let delete_or_update l =
    let action = function
      | Solver.To_change(Was_installed _,_ )
      | Solver.To_delete _ -> true
      | _ -> false in
    let parallel (Solver.P l) = List.exists action l in
    List.exists parallel l

  let proceed_tochange t nv_old (name, v as nv) =
    (* First, uninstall any previous version *)
    (match nv_old with 
    | Was_installed nv_old -> proceed_todelete t nv_old
    | Was_not_installed    -> ());

    (* Then, untar the archive *)
    let p_build = Path.build t.home (Some nv) in
    Path.remove p_build;
    let tgz = Path.extract_targz nv (RemoteServer.getArchive t.server nv) in
    log "untar archive for %s" (Namespace.to_string nv);
    Path.add_rec p_build tgz;

    (* Call the build script and copy the output files *)
    let buildsh = File.Spec.make (File.Spec.find_err (Path.index t.home (Some nv))) in
    log "Run %s" (BatIO.to_string (BatList.print BatString.print) buildsh);
    let err = Path.exec t.home nv buildsh in
    if err = 0 then
      iter_toinstall Path.add_rec t nv
    else
      Globals.error_and_exit
        "Compilation failed with error %d" err;

    (* Mark the packet as installed *)
    File.Installed.modify_def (Path.installed t.home) (N_map.add name v)

  (* we need to clean-up things before recompiling *)
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
        Globals.msg "{%d/%d} The following solution has been found:\n" pos nb_sol;
        function
      | [x] ->
          (* Only 1 solution exists *)
          Solver.solution_print Namespace.to_string x;
          if delete_or_update x then
            if confirm "Continue ?" then
              Some x
            else
              None
          else
            Some x

      | x :: xs ->
          (* Multiple solution exist *)
          Solver.solution_print Namespace.to_string x;
          if delete_or_update x then
            if confirm "Continue ? (press [n] to try another solution)" then
              Some x
            else
              aux (succ pos) xs
          else
            Some x

      | [] -> assert false in

        match aux 1 l with
          | Some sol -> 
            List.iter (fun(Solver.P l) -> 
              List.iter (function
                | Solver.To_change (o,n)  -> proceed_tochange t o n
                | Solver.To_delete n_v    -> proceed_todelete t n_v
                | Solver.To_recompile n_v -> proceed_torecompile t n_v
              ) l
            ) sol
          | None -> ()

  let vpkg_of_nv (name, v) = Namespace.string_of_name name, Some ("=", v.Namespace.deb)

  let unknown_package name =
    Globals.error_and_exit
      "ERROR: Unable to locate package \"%s\"\n"
      (Namespace.string_of_name  name)

  let install name = 
    log "install %s" (Namespace.string_of_name name);
    let t = load_state () in
    let l_index = Path.index_list t.home in
    match find_from_name name l_index with
      | None -> unknown_package name
      | Some v -> 
        let map_installed = File.Installed.find_map (Path.installed t.home) in
        resolve t
          l_index
          map_installed
          [ { Solver.wish_install = 
              List.map vpkg_of_nv ((name, V_set.max_elt v) :: N_map.bindings (N_map.remove name map_installed))
            ; wish_remove = [] 
            ; wish_upgrade = [] } ]

  let remove name =
    log "remove %s" (Namespace.string_of_name name);
    let t = load_state () in
    let l_index = Path.index_list t.home in
    let installed = File.Installed.find_map (Path.installed t.home) in

    let dependencies = 
      NV_set.of_list
        (List.map
           (fun d -> Namespace.Name d.Debian.Packages.name, { Namespace.deb = d.Debian.Packages.version })
           (Solver.filter_forward_dependencies
              (match N_map.Exceptionless.find name installed with 
                | None -> []
                | Some v -> debpkg_of_nv t installed [name, v])
              (debpkg_of_nv t installed l_index))) in

    resolve t 
      l_index
      installed
      [ { Solver.wish_install = 
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
    let installed = File.Installed.find_map (Path.installed t.home) in
    resolve t
      l_index
      installed
      [ { Solver.wish_install = []
        ; wish_remove = []
        ; wish_upgrade = 
          List.map
            (fun (name, _) -> 
              match find_from_name name l_index with 
                | None -> assert false (* an already installed package must figure in the index *) 
                | Some v -> vpkg_of_nv (name, V_set.max_elt v))
            (N_map.bindings installed) } ]
    
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
    let spec_b = binary spec_s in

    (* look for the archive *)
    let archive_filename =
      Namespace.string_of_nv (Namespace.Name name) version ^ ".tar.gz" in
    let archive =
      if Sys.file_exists archive_filename then
        Tar_gz (binary (Run.read archive_filename))
      else
        let urls = File.Spec.urls spec in
        if urls = [] then
          Globals.error_and_exit "Cannot find %s" archive_filename
        else
          Tar_gz (Filename (Raw_links urls)) in

    (* Upload both files to the server and update the client
       filesystem to reflect the new uploaded packages *)
    let name = Namespace.Name name in
    let local_server = Server.init !Globals.root_path in

    let o_key0 = File.Security_key.find (Path.keys t.home name) in
    let o_key1 = 
      match o_key0 with
        | None -> 
          let o = RemoteServer.newArchive t.server (name, version) spec_b archive in
          let () = assert (o = Server.newArchive local_server (name, version) spec_b archive) in
          o
        | Some k -> 
          let b = RemoteServer.updateArchive t.server (name, version) spec_b archive k in
          let () = assert (b = Server.updateArchive local_server (name, version) spec_b archive k) in
          if b then Some k else None in

    match o_key1 with
      | Some k1 when o_key0 <> o_key1 -> File.Security_key.add (Path.keys t.home name) k1
      | None -> Globals.msg "The key given to upload was not accepted.\n"
      | _ -> ignore "The server has returned the same key than currently stored.\n"

  type config_request = Include | Bytelink | Asmlink

  let config is_rec req names =
    log "config %s" (String.concat "," (List.map Namespace.string_of_name names));
    let t = load_state () in

    let l_index = Path.index_list t.home in

    let installed = File.Installed.find_map (Path.installed t.home) in

    let version name =
      match N_map.Exceptionless.find name installed with
      | None   -> unknown_package name
      | Some v -> v in

    let rec iter_with_spaces f = function
      | []   -> ()
      | [h]  -> f h 
      | h::t -> f h; Globals.msg " "; iter_with_spaces f t in

    let versions = List.map (fun n -> n, version n) names in

    let one (name, version) =
      let path = match Path.ocaml_options_of_library t.home name with I s -> s in
      match req with
      | Include ->Globals.msg "-I %s" path
      | link    ->
          let config = File.PConfig.find_err (Path.pconfig t.home (name, version)) in
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

    if not is_rec then

      (* If we don't need to look at the dependencies, simply call [one] for
         each pair (name x version) *)
      iter_with_spaces one versions

    else

      (* Otherwise, we need to compute the transitive closure of dependencies *)
      
      (* So first, get the list of installed packages *)
      let l_deb = debpkg_of_nv t installed l_index in
      let nv pkg =
        Namespace.Name pkg.Debian.Packages.name,
        Namespace.version_of_string pkg.Debian.Packages.version in
            
      (* Then, get the packages we are looking for *)
      let l_pkg =
        List.filter
          (fun pkg ->
            let name, version = nv pkg in
            List.exists (fun (n,v) -> n=name && v=version) versions)
          l_deb in

      (* Compute the transitive closure of dependencies *)
      let dependencies =
        Solver.filter_backward_dependencies l_pkg l_deb in

      iter_with_spaces one (List.map nv dependencies)

end
