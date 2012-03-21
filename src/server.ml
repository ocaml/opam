open Namespace
open Path
open File
open Unix

type 'a boolean = 'a -> bool

module type SERVER =
sig
  type t

  (** Return [true] in case the given version is accepted by the server. *)
  val acceptedVersion : t -> string -> bool

  (** Returns the list of the available versions for all packages. *)
  val getList : t -> name_version list

  (** Returns the representation of the OPAM file for the
      corresponding package version. *)
  val getOpam : t -> name_version -> binary_data

  (** Returns the corresponding package archive. *)
  val getArchive : t -> name_version -> binary_data archive

  (** Receives a first upload, it contains an OPAM file and the
      corresponding package archive. *)
  val newArchive : t -> name_version -> binary_data -> binary_data archive -> security_key option
    (* [None] : An archive with the same NAME already exists, we cancel the reception.
       [Some key] : The reception is accepted, a new unique key associated to the NAME is returned. *)

  (** Receives an upload, it contains an OPAM file and the
      corresponding package archive. *)
  val updateArchive : t -> name_version -> binary_data -> binary_data archive -> security_key boolean
    (* [false] : An archive with the same NAME already exists, we cancel the reception. *)
end

type server_state =
    { home : Path.t (* ~/.opam-server *)
    ; opam_version : internal_version }

module Server = struct

  type t = server_state

  (* Return all the .opam files *)
  let read_index home =
    List.fold_left
      (fun map nv ->
        let file = File.Opam.find_err (Path.index_opam home (Some nv)) in
        NV_map.add nv file map)
      NV_map.empty
      (Path.index_opam_list home)

  let string_of_nv (n, v) = Namespace.string_of_nv n v

  let init home = 
    { home = Path.init home
    ; opam_version = Version Globals.opam_version }

  let acceptedVersion t s =
    s = Globals.version

  let getList t =
    Path.index_opam_list t.home

  let getOpam t n_v =
    let index = read_index t.home in
    try binary (File.Opam.to_string (NV_map.find n_v index))
    with Not_found -> failwith (string_of_nv n_v ^ " not found")

  let getArchive t n_v = 
    let p = Path.archives_targz t.home (Some n_v) in
    match Path.is_directory p with
      | Some dir -> Tar_gz (Filename dir)
      | None -> 
        match Path.find_binary p with
          | Path.File s -> Tar_gz (Binary s)
          | _           -> failwith ("Cannot find " ^ string_of_nv n_v)

  let f_archive t n_v opam archive =
    let opam_file = Path.index_opam t.home (Some n_v) in
    let archive_file = Path.archives_targz t.home (Some n_v) in
    begin match opam with
    | Binary (Raw_binary s) -> File.Opam.add opam_file (File.Opam.parse s)
    | f                     -> Path.add opam_file (Path.File f)
    end;
    begin match archive with
    | Tar_gz f -> Path.add archive_file (Path.File f)
    end

  let mapArchive t name f o_key = 
    let hashes = Path.hashes t.home name in
    let o_key = 
      match o_key, File.Security_key.find hashes with 
        | None, None ->
          let key = Random_key.new_key () in
          let () = File.Security_key.add hashes key in
          Some key
        | Some k0, Some k1 -> 
          if k0 = k1 then
            Some k0
          else
            None
        | _ -> None in
    let () = 
      if o_key = None then
        () (* execution canceled *)
      else
        f t in
    o_key

  let newArchive t n_v opam archive = 
    mapArchive t (fst n_v) (fun t -> f_archive t n_v opam archive) None

  let updateArchive t n_v opam archive key =
    None <> mapArchive t (fst n_v) (fun t -> f_archive t n_v opam archive) (Some key)
end

module RemoteServer : SERVER with type t = url = struct

  open Protocol

  type t = url

  (* untyped message exchange *)
  let send url m =
    let host = (gethostbyname url.hostname).h_addr_list.(0) in
    let addr = ADDR_INET (host, url.port) in
    try
      Protocol.find (open_connection addr) m
    with _ ->
      Globals.error "The server (%s) is unreachable. Please check your network configuration."
        (string_of_url url);
      exit 1

  let dyn_error str =
    failwith ("Protocol error: " ^ str)

  let error msg =
    Globals.error "[SERVER] %s" msg;
    exit 1

  let acceptedVersion t s =
    match send t (IacceptedVersion s) with
    | OacceptedVersion nl -> nl
    | Oerror s    -> error s
    | _           -> dyn_error "acceptedVersion"

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

  let getArchive t nv =
    match send t (IgetArchive nv) with
    | OgetArchive a -> a
    | Oerror s      -> error s
    | _             -> dyn_error "getArchive"

  let read_archive = function
    | Tar_gz (Filename (Raw_filename s)) -> Tar_gz (Binary (Raw_binary (U.read_content s)))
    | x                                  -> x
      
  let newArchive t nv opam archive = 
    match send t (InewArchive (nv, opam, read_archive archive)) with
    | OnewArchive o -> o
    | Oerror s    -> error s
    | _           -> dyn_error "newArchive"

  let updateArchive t nv opam archive k = 
    match send t (IupdateArchive (nv, opam, read_archive archive, k)) with
    | OupdateArchive b -> b
    | Oerror s    -> error s
    | _           -> dyn_error "updateArchive"
end
