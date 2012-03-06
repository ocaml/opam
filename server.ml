open Namespace
open Path
open File

module type SERVER =
sig
  type t
  type opam
  type package

  type api =
    | GetList of t
    | GetOpam of t * name_version
    | GetArchive of t * opam
    | NewArchive of t * opam * binary_data archive

  val init : Path.url option -> t

  val change_url : t -> Path.url -> t

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

  val version_opam : t -> internal_version
  val version_ocaml : t -> internal_version

  val package : opam -> package option 
    (** [None] : the [opam] associated to the [(name, version)] does not exist. 
        Note that every [(name, version)] given by [getList] do exist. *)
end

module Server
  (F_config : File.CONFIG)
  (F_cudf : File.CUDF) 
  : SERVER with type package = Cudf.package =
struct
  module Path_map = BatMap.Make (struct type t = Path.t let compare = Path.compare_computer end)

  type t = 
      { current_repository : Cudf.package NV_map.t
      ; home : Path.t (* ~/.opam-server *)
      ; all_repository : Cudf.package NV_map.t Path_map.t
      ; version_package_manager : internal_version
      ; version_ocaml : internal_version }

  type opam = name_version * Cudf.package option
      (* [None] : the current repository does not contain the package associated to the [name] and [version] *)

  type package = Cudf.package

  type api =
    | GetList of t
    | GetOpam of t * name_version
    | GetArchive of t * opam
    | NewArchive of t * opam * binary_data archive

  let read_archives home =
    let archives = Path.archives_targz home None in
      List.fold_left
        (fun map x -> 
           NV_map.add
             (Namespace.nv_of_string (Path.chop_extension x)) 
             (F_cudf.package (F_cudf.find home (Path.concat archives x))) 
             map) NV_map.empty 
        (match Path.find home archives with
           | Path.Directory l -> l
           | _ -> [])

  let init o = 
    let version_ocaml = F_config.empty_ocaml in
    let home = Path.init o ".opam-server" version_ocaml in
    { current_repository = read_archives home
    ; home
    ; all_repository = Path_map.empty
    ; version_package_manager = F_config.empty_package_manager
    ; version_ocaml }

  let change_url t url = 
    let home = Path.change_url t.home url in
    { t with
        current_repository = (match Path_map.Exceptionless.find home t.all_repository with
                                | None -> read_archives home
                                | Some v -> v);
        home;
        all_repository = Path_map.add t.home t.current_repository t.all_repository }

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
            (Path.File (match arch with 
                          | Empty -> failwith "create an empty tar.gz here" 
                          | Tar_gz s -> s)) } in
      
    match o_pack with
      | None -> { t with current_repository = NV_map.add n_v (F_cudf.new_package n_v "") t.current_repository }
      | Some _ -> t

  let version_opam t = t.version_package_manager
  let version_ocaml t = t.version_ocaml
  let package = snd
end
