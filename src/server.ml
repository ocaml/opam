open Namespace
open Path
open File
open Unix

module type SERVER =
sig
  type t

  (** [None] : the current repository does not contain the package
      associated to the [name] and [version] *)
  type opam = name_version * Cudf.package option

  (** Returns the list of the available versions for all packages. *)
  val getList : t -> name_version list

  (** Returns the representation of the OPAM file for the
      corresponding package version. *)
  val getOpam : t -> name_version -> opam

  (** Returns the corresponding package archive. *)
  val getArchive : t -> opam -> binary_data archive

  (** Receives an upload, it contains an OPAM file and the
      corresponding package archive. *)
  val newArchive : t -> opam -> binary_data archive -> unit

end

type server_state =
    { mutable current_repository : Cudf.package NV_map.t
    ; home : Path.t (* ~/.opam-server *)
    ; version_package_manager : internal_version }

module Server = struct

  type t = server_state

  type opam = name_version * Cudf.package option

  let read_index home =
    List.fold_left
      (fun map nv -> 
        NV_map.add
          nv
          (File.Cudf.package (File.Cudf.find home (Path.index_opam home (Some nv)))) 
          map) NV_map.empty 
      (Path.index_opam_list home)

  let init () = 
    let home = Path.init Globals.opam_server_path in
    { current_repository = read_index home
    ; home
    ; version_package_manager = Version Globals.default_opam_version }

  let getList t = BatList.map fst (NV_map.bindings t.current_repository)
  let getOpam t n_v = n_v, NV_map.Exceptionless.find n_v t.current_repository
  let getArchive t = function
    | _, None -> Empty
    | n_v, Some _ -> 
        match Path.find t.home (Path.archives_targz t.home (Some n_v)) with
          | Path.File s -> Tar_gz s
          | _ -> Empty

  let newArchive t (n_v, o_pack) arch = 
    Path.add 
      t.home 
      (Path.archives_targz t.home (Some n_v)) 
      (match arch with 
      | Empty -> Path.Not_exists
      | Tar_gz s -> Path.File s);

    let new_package = File.Cudf.new_package n_v "" in

    File.Cudf.add 
      t.home 
      (Path.index_opam t.home (Some n_v))
      (File.Cudf.cudf t.version_package_manager new_package);
    
    match o_pack with
    | None   -> t.current_repository <- NV_map.add n_v new_package t.current_repository
    | Some _ -> ()

end

type input_api =
  | IgetList
  | IgetOpam of name_version
  | IgetArchive of Server.opam
  | InewArchive of Server.opam * binary_data archive

type output_api =
  | OgetList of name_version list
  | OgetOpam of Server.opam
  | OgetArchive of binary_data archive
  | OnewArchive
  | Oerror of string (* server error *)

module RemoteServer : SERVER with type t = url = struct

  type t = url
  type opam = Server.opam

  (* untyped message exchange *)
  let send url (m : input_api) =
    let host = (gethostbyname url.hostname).h_addr_list.(0) in
    let addr = ADDR_INET (host, url.port) in
    try
      let stdin, stdout = open_connection addr in
      output_value stdout m;
      flush stdout;
      (input_value stdin : output_api)
    with _ ->
      Globals.error "The server (%s) is unreachable. Please check your network configuration."
        (string_of_url url);
      exit 1

  let dyn_error str =
    failwith ("Protocol error: " ^ str)

  let error msg =
    Globals.error "[SERVER] %s" msg;
    exit 1

  let getList t =
    match send t IgetList with
    | OgetList nl -> nl
    | Oerror s    -> error s
    | _           -> dyn_error "getList"

  let getOpam t name_version =
    match send t (IgetOpam name_version) with
    | OgetOpam o -> o
    | Oerror s   -> error s
    | _          -> dyn_error "getOpam"

  let getArchive t opam =
    match send t (IgetArchive opam) with
    | OgetArchive a -> a
    | Oerror s      -> error s
    | _             -> dyn_error "getArchive"

  let newArchive t opam archive =
    match send t (InewArchive (opam, archive)) with
    | OnewArchive -> ()
    | Oerror s    -> error s
    | _           -> dyn_error "newArchive"

end
