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
    let home = Path.init Globals.opam_path in
    let config = File.Config.find home (Path.config home) in
    { server = File.Config.sources config
    ;  home }

  let update_aux t =
    let map =
      List.fold_left
        (fun map (n, v) -> 
          let index_nv = Path.index_opam t.home (Some (n, v)) in
          if Path.file_exists index_nv then
            map
          else begin
            File.Cudf.add t.home index_nv
              (File.Cudf.cudf
                 (Version Globals.default_opam_version)
                 (match snd (RemoteServer.getOpam t.server (n, v)) with
                 | None -> assert false
                 | Some pkg -> pkg));
            N_map.modify_def V_set.empty n (V_set.add v) map
          end)
        N_map.empty
        (RemoteServer.getList t.server) in
    
      Printf.printf "%s\n"
        (BatIO.to_string 
           (N_map.print 
              (fun oc name -> BatString.print oc (Namespace.string_user_of_name name))
              (V_set.print (fun oc version -> BatString.print oc (Namespace.string_user_of_version version)))
           ) map)

  let update () =
    update_aux (load_state ())

  let init url =
    log "init %s" (string_of_url url);
    let config =
      File.Config.config
        (Version Globals.default_opam_version)
        url
        (Version Globals.default_ocaml_version) in
    let home = Path.init Globals.opam_path in
    File.Config.add home (Path.config home) config;
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
    let t = load_state () in
    let s_not_installed = "--" in
    match package with
    | None -> 
        let install_set = NV_set.of_enum (BatList.enum (File.Installed.find t.home (Path.installed t.home))) in
        let map, max_n, max_v = 
          List.fold_left
            (fun (map, max_n, max_v) n_v -> 
              let b = NV_set.mem n_v install_set in
              NV_map.add n_v
                (b, 
                 File.Cudf.description
                   (File.Cudf.package (File.Cudf.find t.home (Path.index_opam t.home (Some n_v)))))
                map,
              max max_n (String.length (Namespace.string_user_of_name (fst n_v))), 
              if b then max max_v (String.length (Namespace.string_user_of_version (snd n_v))) else max_v)
            (NV_map.empty, min_int, String.length s_not_installed)
            (Path.index_opam_list t.home) in

        NV_map.iter (fun n_v (b, description) -> 
          Printf.printf "%s %s %s" 
            (indent_left (Namespace.string_user_of_name (fst n_v)) max_n)
            (indent_right (if b then Namespace.string_user_of_version (snd n_v) else s_not_installed) max_v)
            description) map;
        Printf.printf "\n"

    | Some name -> 
        let find_from_name = find_from_name name in

        let o_v = 
          BatOption.map
            V_set.choose (* By definition, there is exactly 1 element, we choose it. *) 
            (find_from_name (File.Installed.find t.home (Path.installed t.home))) in

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
                File.Cudf.description
                  (File.Cudf.package (File.Cudf.find t.home (Path.index_opam t.home (Some (name, v)))))
          ]

  let confirm msg = 
    Printf.printf "%s\nContinue ? [y/N] " msg;
    match read_line () with
      | ("y"|"Y") -> true
      | _         -> false

  let iter_toinstall f_build f_add_rec t (name, v) = 
    let tgz = 
      let p_targz, p_build = 
        Path.archives_targz t.home (Some (name, v)),
        Path.build t.home (Some (name, v)) in
      if Path.file_exists p_targz then
        Path.R_filename
          (BatList.map (Path.concat p_build)
             (match Path.find t.home p_build with
             | Path.Directory l -> l
             | _ -> []))
      else
        let tgz = Path.extract_targz t.home
          (RemoteServer.getArchive t.server (RemoteServer.getOpam t.server (name, v))) in
        Path.add_rec t.home p_build tgz;
        tgz in
    
    f_build t tgz;

    let to_install = File.To_install.find t.home (Path.to_install t.home (name, v)) in

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
            (fun path_to -> Path.add_rec t.home path_to path_from) 
            (File.To_install.filename_of_path_absolute t.home (File.To_install.path_to misc)))
      (File.To_install.misc to_install)

  let proceed_todelete t (n, v0) = 
    let map_installed = N_map.of_enum (BatList.enum (File.Installed.find t.home (Path.installed t.home))) in
    match N_map.Exceptionless.find n map_installed with
      | Some v when v = v0 ->
          iter_toinstall
            (fun t _ -> t)
            (fun t file -> function
              | Path.R_filename l -> 
                List.iter (fun f -> Path.remove t.home (Path.concat file (Path.basename f))) l
              | _ -> failwith "to complete !")
            t
            (n, v);
        File.Installed.add t.home (Path.installed t.home) (N_map.bindings (N_map.remove n map_installed))

      | _ -> ()

  let proceed_torecompile t (name, v) =
    iter_toinstall  
      (fun t tgz ->
        Path.add_rec t.home (Path.build t.home (Some (name, v))) tgz;
        Path.exec_buildsh t.home (name, v))
      (fun t file contents ->
        Path.add_rec t.home file contents)
      t
      (name, v)

  let proceed_tochange t (nv_old, nv) =
    begin match nv_old with 
    | Was_installed n_v -> proceed_todelete t n_v
    | Was_not_installed -> ()
    end;
    proceed_torecompile t nv

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
          let pkg = File.Cudf.package (File.Cudf.find t.home (Path.index_opam t.home (Some n_v))) in
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
    let t = load_state () in
    let l_index = Path.index_opam_list t.home in
    match find_from_name name l_index with
      | None ->
          let msg =
            Printf.sprintf "Package \"%s\" not found. An update of package will be performed."
              (Namespace.string_user_of_name name) in
          if confirm msg then
            update_aux t
      | Some v -> 
          let _ = resolve t l_index { Solver.wish_install = [ vpkg_of_nv (name, V_set.max_elt v) ]
                                    ; wish_remove = [] 
                                    ; wish_upgrade = [] } in
          ()

  let remove name =
    let t = load_state () in
    let r = match BatList.Exceptionless.assoc name (File.Installed.find t.home (Path.installed t.home)) with
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
    let t = load_state () in
      resolve t (Path.index_opam_list t.home) 
        { Solver.wish_install = []
        ; wish_remove = []
        ; wish_upgrade = BatList.map vpkg_of_nv (File.Installed.find t.home (Path.installed t.home)) }
    
  let upload s_filename =
    let t = load_state () in
    let filename = Path.package t.home s_filename in
    let o =
      let f msg v =
        let msg = Printf.sprintf "Path \"%s\" %s. It will be uploaded anyway." s_filename msg in
        if confirm msg then
          Some v
        else
          None in
      match Path.find t.home filename with
        | Path.File binary -> Some (Tar_gz binary)
        | Path.Directory _ -> f "is a directory" (Path.raw_targz filename)
        | Path.Not_exists  -> f "has not been found" Empty in

    match o with
      | Some v ->
          RemoteServer.newArchive t.server
            (RemoteServer.getOpam t.server
               (Path.nv_of_extension Namespace.default_version (Path.basename filename))
            ) v
      | None -> ()

  type config_request = Dir
  let config Dir name =
    let t = load_state () in
    match find_from_name name (Path.index_opam_list t.home) with

      | None -> 
          let msg =
            Printf.sprintf "Package \"%s\" not found. An update of package will be performed."
              (Namespace.string_user_of_name name) in
          if confirm msg then
            update_aux t

      | Some _ -> 
          Printf.printf "-I %s" 
            (match Path.ocaml_options_of_library t.home name with I s -> s)
end
