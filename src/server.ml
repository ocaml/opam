open Namespace
open Path
open File
open Unix

module type SERVER =
sig
  type t

  (** Returns the list of the available versions for all packages. *)
  val getList : t -> name_version list

  (** Returns the representation of the OPAM file for the
      corresponding package version. *)
  val getOpam : t -> name_version -> binary_data

  (** Returns the corresponding package archive. *)
  val getArchive : t -> name_version -> binary_data archive

  (** Receives an upload, it contains an OPAM file and the
      corresponding package archive. *)
  val newArchive : t -> name_version -> binary_data -> binary_data archive -> unit

end

type server_state =
    { home : Path.t (* ~/.opam-server *)
    ; opam_version : internal_version }

module Server = struct

  type t = server_state

  (* Return all the .opam files *)
  let read_index home =
    List.fold_left
      (fun map nv -> NV_map.add nv (File.Opam.find (Path.index_opam home (Some nv))) map)
      NV_map.empty 
      (Path.index_opam_list home)

  let string_of_nv (n, v) = Namespace.string_of_nv n v

  let init path = 
    { home = Path.init path
    ; opam_version = Version Globals.opam_version }

  let getList t =
    Path.index_opam_list t.home

  let getOpam t n_v =
    let index = read_index t.home in
    try binary (File.Opam.to_string (NV_map.find n_v index))
    with Not_found -> failwith (string_of_nv n_v ^ " not found")

  let getArchive t n_v =
    match Path.find (Path.archives_targz t.home (Some n_v)) with
    | Path.File s -> Tar_gz s
    | _           -> failwith ("Cannot find " ^ string_of_nv n_v)

  let newArchive t n_v opam archive =
    let opam_file = Path.index_opam t.home (Some n_v) in
    let archive_file = Path.archives_targz t.home (Some n_v) in
    begin match opam with
    | Binary (Raw_binary s) -> File.Opam.add opam_file (File.Opam.parse s)
    | f                     -> Path.add opam_file (Path.File f)
    end;
    begin match archive with
    | Tar_gz f -> Path.add archive_file (Path.File f)
    end;

end

type input_api =
  | IgetList
  | IgetOpam of name_version
  | IgetArchive of name_version
  | InewArchive of name_version * binary_data * binary_data archive

type output_api =
  | OgetList of name_version list
  | OgetOpam of binary_data
  | OgetArchive of binary_data archive
  | OnewArchive
  | Oerror of string (* server error *)

module RemoteServer : SERVER with type t = url = struct

  type t = url

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

  let getArchive t nv =
    match send t (IgetArchive nv) with
    | OgetArchive a -> a
    | Oerror s      -> error s
    | _             -> dyn_error "getArchive"

  let newArchive t nv opam archive =
    let archive = match archive with
    | Tar_gz (Filename (Raw_filename s)) -> Tar_gz (Binary (Raw_binary (U.read_content s)))
    | Tar_gz _                           -> archive in
    match send t (InewArchive (nv, opam, archive)) with
    | OnewArchive -> ()
    | Oerror s    -> error s
    | _           -> dyn_error "newArchive"

end
