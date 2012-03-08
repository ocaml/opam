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

  type config_request = Dir

  (** Returns the directory where the package is installed,
      in a form suitable to OCaml compilers (i.e. like "-I ..."). *)
  val config : config_request -> Namespace.name -> unit

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
    let config = Path.read File.Config.find (Path.config home) in
    { server = File.Config.sources config
    ;  home }

  let update_t t =
    let packages = RemoteServer.getList t.server in
    List.iter
      (fun (n, v) -> 
        let opam_file = Path.index_opam t.home (Some (n, v)) in
        if not (Path.file_exists opam_file) then
          let opam = RemoteServer.getOpam t.server (n, v) in
          Path.add opam_file (Path.File opam);
          Printf.printf "New package available: %s\n%!" (Namespace.string_of_nv n v)
      ) packages

  let update () =
    update_t (load_state ())

  let init url =
    log "init %s" (string_of_url url);
    let home = Path.init !Globals.root_path in
    let config =
      File.Config.create
        (Version Globals.opam_version)
        url
        (Version Globals.ocaml_version) in
    File.Config.add (Path.config home) config;
    update ()

  let indent_left s nb = s ^ String.make nb ' '

  let indent_right s nb = String.make nb ' ' ^ s

  let find_from_name name l = 
    N_map.Exceptionless.find 
      name
      (List.fold_left
         (fun map (n, v) -> 
            N_map.modify_def V_set.empty n (V_set.add v) map) N_map.empty l)

  let info package =
    log "info %s" (match package with None -> "ALL" | Some p -> Namespace.string_of_name p);
    let t = load_state () in
    let s_not_installed = "--" in
    match package with
    | None -> 
        (* Get all the installed packages *)
        let installed = Path.read File.Installed.find (Path.installed t.home) in
        let install_set = NV_set.of_enum (BatList.enum installed) in
        let map, max_n, max_v = 
          List.fold_left
            (fun (map, max_n, max_v) n_v -> 
              let b = NV_set.mem n_v install_set in
              let opam = Path.read File.Opam.find (Path.index_opam t.home (Some n_v)) in
              let new_map = NV_map.add n_v (b, File.Opam.description opam) map in
              let new_max_n = max max_n (String.length (Namespace.string_user_of_name (fst n_v))) in
              let new_max_v =
                if b then max max_v (String.length (Namespace.string_user_of_version (snd n_v))) else max_v in
            new_map, new_max_n, new_max_v)
            (NV_map.empty, min_int, String.length s_not_installed)
            (Path.index_opam_list t.home) in

        NV_map.iter (fun n_v (b, description) -> 
          Printf.printf "%s %s %s\n" 
            (indent_left (Namespace.string_user_of_name (fst n_v)) max_n)
            (indent_right (if b then Namespace.string_user_of_version (snd n_v) else s_not_installed) max_v)
            description) map;
        Printf.printf "\n"

    | Some name -> 
        let find_from_name = find_from_name name in
        let installed = Path.read File.Installed.find (Path.installed t.home) in
        let o_v = 
          BatOption.map
            V_set.choose (* By definition, there is exactly 1 element, we choose it. *) 
            (find_from_name installed) in

        let v_set =
          let v_set = 
            match find_from_name (Path.index_opam_list t.home) with
            | None -> V_set.empty
            | Some v -> v in
          match o_v with
          | None -> v_set
          | Some v -> V_set.remove v v_set in

        List.iter
          (fun (tit, desc) -> Printf.printf "%s: %s\n" tit desc)
          [ "package", Namespace.string_user_of_name name
          ; "version", (match o_v with None -> s_not_installed | Some v -> Namespace.string_user_of_version v)
          ; "versions",
            BatIO.to_string
              (V_set.print ~first:"" ~last:"" ~sep:", "
                 (fun oc v -> BatString.print oc (Namespace.string_user_of_version v))
              ) v_set
          ; "description", "\n" ^ 
            match o_v with None -> ""
            | Some v ->
                let opam = Path.read File.Opam.find (Path.index_opam t.home (Some (name, v))) in
                File.Opam.description opam
          ]

  let confirm msg = 
    Printf.printf "%s\nContinue ? [y/N] " msg;
    match read_line () with
      | "y" | "Y" -> true
      | _         -> false

  let iter_toinstall f_add_rec t (name, v) = 

    let to_install = Path.read File.To_install.find (Path.to_install t.home (name, v)) in

    let filename_of_path_relative t path = 
      Path.R_filename (File.To_install.filename_of_path_relative t.home
                         (Path.build t.home (Some (name, v))) 
                         path) in
    
    let add_rec f_lib t path = 
      f_add_rec t
        (f_lib t.home name (* warning : we assume that this result is a directory *))
        (filename_of_path_relative t path) in

    (* lib *) 
    List.iter (add_rec Path.lib t) (File.To_install.lib to_install);
  
    (* bin *) 
    add_rec (fun t _ -> Path.bin t) t (File.To_install.bin to_install);
  
    (* misc *)
    List.iter 
      (fun misc -> 
        if confirm (File.To_install.string_of_misc misc) then
          let path_from = filename_of_path_relative t (File.To_install.path_from misc) in
          List.iter 
            (fun path_to -> Path.add_rec path_to path_from) 
            (File.To_install.filename_of_path_absolute t.home (File.To_install.path_to misc)))
      (File.To_install.misc to_install)

  let proceed_todelete t (n, v0) = 
    File.Installed.modify_def (Path.installed t.home) 
      (fun map_installed -> 
        match N_map.Exceptionless.find n map_installed with
          | Some v when v = v0 ->
            iter_toinstall
              (fun t file -> function
                | Path.R_filename l -> 
                  List.iter (fun f -> Path.remove (Path.concat file (Path.basename f))) l
                | _ -> failwith "to complete !")
              t
              (n, v);
            N_map.remove n map_installed
              
          | _ -> map_installed)

  let proceed_torecompile t (name, v) =
    begin
      Path.exec_buildsh t.home (name, v);
      iter_toinstall  
        (fun t file contents ->
          Path.add_rec file contents)
        t
        (name, v);
    end

  let proceed_tochange t (nv_old, (name, v)) =
    begin 
      (match nv_old with 
        | Was_installed nv_old -> proceed_todelete t nv_old
        | Was_not_installed ->
          let p_targz, p_build = 
            Path.archives_targz t.home (Some (name, v)),
            Path.build t.home (Some (name, v)) in
          if Path.file_exists p_targz then
            ()
          else
            let tgz = Path.extract_targz (RemoteServer.getArchive t.server (name, v)) in
            Path.add_rec p_build tgz);
      proceed_torecompile t (name, v);
      File.Installed.modify_def (Path.installed t.home) (N_map.add name v);
    end

  module PkgMap = BatMap.Make (struct type t = Cudf.package let compare = compare end)

  let resolve t l_index request = 

    let rec aux = function
    | x :: xs -> 
        let msg =
          Printf.sprintf
            "%s This solution will be performed or another will be tried if existed."
            (if x = [] then
                "Solution found : The current state of the repository can be kept to satisfy the constraints given."
             else
                BatIO.to_string
                  (Solver.solution_print
                     (fun oc (_, v) -> BatString.print oc (Namespace.string_user_of_version v))) x) in
        if confirm msg then
          Some x
        else
          aux xs
            
    | [] -> None in
    
    let l_pkg, map_pkg = 
      List.fold_left
        (fun (l, map) n_v ->
          let opam = Path.read File.Opam.find (Path.index_opam t.home (Some n_v)) in
          let pkg = File.Opam.package opam in
          pkg :: l, PkgMap.add pkg n_v map) ([], PkgMap.empty) l_index in
    let l =
      BatList.map (Solver.solution_map (fun p -> PkgMap.find p map_pkg)) (Solver.resolve l_pkg request) in

    match aux l with
    | Some sol -> 
        List.iter (fun(Solver.P l) -> 
          List.iter (function
          | Solver.To_change n_v -> proceed_tochange t n_v
          | Solver.To_delete n_v -> proceed_todelete t n_v
          | Solver.To_recompile n_v -> proceed_torecompile t n_v
          ) l
        ) sol
    | None -> ()

  let vpkg_of_nv (name, v) = Namespace.string_of_name name, Some (`Eq, v.Namespace.cudf)

  let install name = 
    log "install %s" (Namespace.string_of_name name);
    let t = load_state () in
    let l_index = Path.index_opam_list t.home in
    match find_from_name name l_index with
      | None ->
          let msg =
            Printf.sprintf "Package \"%s\" not found. An update of package will be performed."
              (Namespace.string_user_of_name name) in
          if confirm msg then
            update_t t
      | Some v -> 
          let _ = resolve t l_index { Solver.wish_install = [ vpkg_of_nv (name, V_set.max_elt v) ]
                                    ; wish_remove = [] 
                                    ; wish_upgrade = [] } in
          ()

  let remove name =
    log "remove %s" (Namespace.string_of_name name);
    let t = load_state () in
    let installed = Path.read File.Installed.find (Path.installed t.home) in
    let r = match BatList.Exceptionless.assoc name installed with
    | None ->
        let msg =
          Printf.sprintf "Package \"%s\" not found. We will call the solver to see its output."
            (Namespace.string_user_of_name name) in
        if confirm msg then
          Some None
        else
          None
    | Some v -> Some (Some (`Eq, v.Namespace.cudf)) in
    match r with
    | Some (o_v) -> 
        let l_index = Path.index_opam_list t.home in
        resolve t l_index { Solver.wish_install = []
                          ; wish_remove = [ Namespace.string_of_name name, o_v ]
                          ; wish_upgrade = [] }
    | None -> ()

  let upgrade () =
    log "upgrade";
    let t = load_state () in
    let installed = Path.read File.Installed.find (Path.installed t.home) in
    resolve t (Path.index_opam_list t.home) 
      { Solver.wish_install = []
      ; wish_remove = []
      ; wish_upgrade = BatList.map vpkg_of_nv installed }
    
  (* Upload reads NAME.opam to get the current package version.
     Then it looks for NAME-VERSION.tar.gz in the same directory.
     Then, it sends both NAME.opam and NAME-VERSION.tar.gz to the server *)
  let upload name =
    log "upload %s" name;
    let t = load_state () in

    (* Get the current package version *)
    let opam_filename = name ^ ".opam" in
    let opam_binary = U.read_content opam_filename in
    let opam = File.Opam.parse opam_binary in
    let version = File.Opam.version opam in
    let opam = binary opam_binary in

    (* look for the archive *)
    let archive_filename = Namespace.string_of_nv (Namespace.Name name) version ^ ".tar.gz" in
    let archive =
      if Sys.file_exists archive_filename then
        Tar_gz (binary (U.read_content archive_filename))
      else
        Globals.error_and_exit "Cannot find %s" archive_filename in

    (* Upload both files to the server and update the client
       filesystem to reflect the new uploaded packages *)
    let nv = Namespace.Name name, version in
    let local_server = Server.init !Globals.root_path in
    RemoteServer.newArchive t.server nv opam archive;
    Server.newArchive local_server nv opam archive

  type config_request = Dir
  let config Dir name =
    log "config %s" (Namespace.string_of_name name);
    let t = load_state () in
    match find_from_name name (Path.index_opam_list t.home) with

    | None -> 
        let msg =
          Printf.sprintf "Package \"%s\" not found. An update of package will be performed."
            (Namespace.string_user_of_name name) in
        if confirm msg then
          update_t t

    | Some _ -> 
        Printf.printf "-I %s" 
          (match Path.ocaml_options_of_library t.home name with I s -> s)
end
