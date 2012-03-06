open Namespace
open Path
open File

module type SERVER =
sig
  type t
  type opam = name_version * Cudf.package option
  type package = Cudf.package

  val getList : t -> name_version list
    (** Returns the list of the available versions for all
        packages. *)

  val getOpam : t -> name_version -> opam
    (** Returns the representation of
        the OPAM file for the corresponding package version. *)

  val getArchive : t -> opam -> binary_data archive
    (** Returns the corresponding package archive. *)

  val newArchive : t -> opam -> binary_data archive -> t
    (** Receives an upload, it contains an OPAM file and the
        corresponding package archive. *)

  val package : opam -> package option 
    (** [None] : the [opam] associated to the [(name, version)] does not exist. 
        Note that every [(name, version)] given by [getList] do exist. *)
end

type server_state =
    { current_repository : Cudf.package NV_map.t
    ; home : Path.t (* ~/.opam-server *)
    ; version_package_manager : internal_version }


module Server : SERVER with type t = server_state = struct

  type t = server_state

  type opam = name_version * Cudf.package option
      (* [None] : the current repository does not contain the package associated to the [name] and [version] *)

  type package = Cudf.package

  let read_archives home =
    let archives = Path.archives_targz home None in
      List.fold_left
        (fun map x -> 
           NV_map.add
             (Path.nv_of_extension Namespace.default_version x)
             (File.Cudf.package (File.Cudf.find home (Path.concat archives x))) 
             map) NV_map.empty 
        (match Path.find home archives with
           | Path.Directory l -> l
           | _ -> [])

  let init home = 
    let home = Path.init Globals.opam_server_path in
    { current_repository = read_archives home
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
    let t = 
      { t with 
        home = 
          Path.add 
            t.home 
            (Path.archives_targz t.home (Some n_v)) 
            (match arch with 
            | Empty -> Path.Not_exists
            | Tar_gz s -> Path.File s) } in
    
    match o_pack with
    | None -> { t with current_repository = NV_map.add n_v (File.Cudf.new_package n_v "") t.current_repository }
    | Some _ -> t

  let package = snd
end

type api =
  | GetList of server_state
  | GetOpam of server_state * name_version
  | GetArchive of server_state * Server.opam
  | NewArchive of server_state * Server.opam * binary_data archive

module RemoteServer : SERVER with type t = url = struct

  type t = url
  type opam = Server.opam
  type package = Cudf.package

  let  getList t = failwith "TODO"
  let getOpam t name_version = failwith "TODO"
  let getArchive t opam = failwith "TODO"
  let newArchive t opam archive = failwith "TODO"
  let package opam = failwith "TODO"

end
