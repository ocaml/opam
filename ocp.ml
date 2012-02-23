module Namespace =
struct
  open Printf

  type name = Name of string
  let name_compare (Name n1) (Name n2) = failwith "to complete !"

  type version = Version of string
  let version_compare (Version v1) (Version v2) = failwith "to complete !"

  let string_of_nv (Name n) (Version v) = sprintf "%s-%s" n v
  let string_of_name (Name n) = n
  let string_user_of_name (Name n) = n
  let string_user_of_version (Version v) = v

  let nv_of_string s = 
    let n, v = BatString.split s "-" in
      Name n, Version v
end

type name_version = Namespace.name * Namespace.version

module N_map = BatMap.Make (struct open Namespace type t = name let compare = name_compare end)
module V_set = BatSet.Make (struct open Namespace type t = version let compare = version_compare end)

module NV_orderedtype = 
struct
  open Namespace
  type t = name_version
  let compare (n1, v1) (n2, v2) = 
    let c = name_compare n1 n2 in
      if c = 0 then
        version_compare v1 v2
      else
        c
end

module NV_map = BatMap.Make (NV_orderedtype)
module NV_set = BatSet.Make (NV_orderedtype)


type archive = 
  | Tar_gz of string
  | Empty

type 'a ocaml_options = 
  | I of 'a

module type PATH =
sig

  type t
  type basename
  type filename
  type url

  type 'a contents = 
    | Directory of basename list
    | File of 'a
    | Not_exists

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) list
    | R_file of 'a
    | R_filename of filename

  val init : url option (* [None] : local *) -> string (* $HOME_OPAM *) -> t


  (** definitions of some shortcuts *)

  val proot : t -> filename (* $PWD *)
    (** path in the packager filesystem, contains the collection of libraries and programs *)
  val lib : t -> Namespace.name -> filename (* $HOME_OPAM/lib/NAME *)
    (** installed libraries for the package (at most one version installed) *)
  val bin : t -> filename (* $HOME_OPAM/bin *)
    (** contain installed binaries *)
  val config : t -> filename (* $HOME_OPAM/config *)
    (** main configuration file *)
  val installed : t -> filename (* $HOME_OPAM/installed *)
    (** list of installed packages with their version *)
  val index_opam : t -> name_version option -> filename (* $HOME_OPAM/index/NAME-VERSION.opam *)
    (** OPAM files for all versions and all packages *)
  val archives_targz : t -> name_version option -> filename (* $HOME_OPAM/archives/NAME-VERSION.tar.gz *)
    (** source archives for all versions of all packages *)
  val build : t -> name_version option -> filename (* $HOME_OPAM/build/NAME-VERSION *)
    (** tempory folders used to decompress the corresponding archives *)
  val to_install : t -> name_version -> filename (* $HOME_OPAM/build/NAME-VERSION/NAME.install *)
    (** compiled files in the extracted archive to install *)

  (** **)

  val find : t -> filename -> string (* raw binary *) contents
    (** Retrieves the contents from the hard disk. *)

  val add : t -> filename -> string contents -> t
    (** Removes everything in [filename] if existed, then write [contents] instead. *)

  val add_rec : t -> filename -> string contents_rec -> t
    (** Removes everything in [filename] if existed, then write [contents_rec] inside [filename]. *)

  val extract_targz : t -> archive -> string contents_rec
    (** Returns the same meaning as [archive] but in extracted form. *)

  val exec_buildsh : t -> name_version -> t
    (* $HOME_OPAM/build/NAME-VERSION/build.sh *)
    (** Executes this particularly named script. *)

  val basename : filename -> basename
    (** see [Filename.basename] *)

  val chop_extension : basename -> string
    (** see [Filename.chop_extension] *)

  val concat : filename -> basename -> filename
    (** see [Filename.concat] *)

  val file_exists : filename -> bool
    (** see [Sys.file_exists] *)

  val ocaml_options_of_library : t -> Namespace.name -> string ocaml_options 
    (* $HOME_OPAM/lib/NAME *)
    (** Returns the exact path to give to the OCaml compiler (ie. -I ...) *)

  val url : string (* hostname *) -> int option (* port *) -> url

  val change_url : t -> url -> t

  val compare_computer : t -> t -> int
end

module Path : PATH =
struct
  open Printf

  type url = U of string

  type basename = B of string 

  type filename = 
    | Normalized of string
    | Raw of string

  type t = { computer : url option (* [None] : local *)
           ; home_opam : string }

  type 'a contents = 
    | Directory of basename list
    | File of 'a
    | Not_exists

  type 'a contents_rec = 
    | R_directory of (basename * 'a contents_rec) list
    | R_file of 'a
    | R_filename of filename

  let s_of_filename = function
    | Normalized s -> s
    | Raw s -> s

  let filename_map f = function
    | Normalized s -> Normalized (f s)
    | Raw s -> Raw (f s)

  let normalize s = 
    let getchdir s = 
      let p = Unix.getcwd () in
      let () = Unix.chdir s in
      p in

    Normalized (getchdir (getchdir s))

  let home = Unix.getenv "HOME"
  let (//) = sprintf "%s/%s"
  let concat f (B s) = filename_map (fun filename -> filename // s) f
  let (///) = concat
  let init o s = { computer = o ; home_opam = home // s }

  let proot _ = normalize "."
  let lib t (Namespace.Name n) = Raw (t.home_opam // "lib" // n)
  let bin t = Raw (t.home_opam // "bin")

  let mk_name_version d ext t n v = Raw (t.home_opam // d // sprintf "%s%s" (Namespace.string_of_nv n v) ext)

  let mk_name_version_o name ext t = 
      function
        | None -> Raw (t.home_opam // name)
        | Some (n, v) -> mk_name_version name ext t n v

  let index_opam = mk_name_version_o "index" ".opam"
  let archives_targz = mk_name_version_o "archives" ".tar.gz"

  let build = mk_name_version_o "build" ""
  let installed t = Raw (t.home_opam // "installed")
  let config t = Raw (t.home_opam // "config")

  let to_install t (n, v) = build t (Some (n, v)) /// B ((Namespace.string_of_name n) ^ ".install")

  let url x o = U (sprintf "%s%s" x (match o with None -> "" | Some i -> sprintf ":%d" i))

  let change_url t u = { t with computer = Some u }

  let contents f_dir f_fic f_not_exists t f = 
    match t.computer with 
      | None -> 
        let fic = s_of_filename f in
          if Sys.file_exists fic then
            (if Sys.is_directory fic then f_dir else f_fic) fic
          else
            f_not_exists
      | Some _ -> failwith "to complete !"


  let find = 
    contents
      (fun fic -> Directory (BatList.of_enum (BatEnum.map (fun s -> B s) (BatSys.files_of fic))))
      (fun fic -> File (BatFile.with_file_in fic BatIO.read_all))
      Not_exists

  let chop_extension (B s) = Filename.chop_extension s
  let file_exists f = Sys.file_exists (s_of_filename f)

  let add t f =
    function 
      | Directory d -> failwith "to complete !"
      | File cts -> 
          let () = contents (fun _ -> failwith "to complete !") Unix.unlink () t f in
          let fic = s_of_filename f in
          let () = BatFile.with_file_out fic (fun oc -> BatString.print oc cts) in
            t
      | Not_exists -> failwith "to complete !"

(*  let modify_def t cts f f_update = 
*)  
  let compare_computer t1 t2 = compare t1.computer t2.computer

  let exec_buildsh t n_v = 
    let _ = Sys.chdir (s_of_filename (build t (Some n_v))) in
    let _ = Sys.command "build.sh" in
      t
  let basename s = B (Filename.basename (s_of_filename s))

  let extract_targz _ = failwith "to complete !"
  let add_rec _ = failwith "to complete !"

  let ocaml_options_of_library t name = 
    I (Printf.sprintf "%s" (s_of_filename (lib t name)))
end

module File =
struct
  open Namespace

  module type PRINTF =
  sig
    type t
    type out_channel

    val init : unit -> t
    val read_line : t -> string * t
    val printf : t -> ('a, out_channel, t) format -> 'a
  end

  module type CUDF =
  sig
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t

    type package

    val opam_version : t -> version
    val package : t -> package
    val cudf : version -> package -> t

    (** fields related to package *)
    val name : package -> name
    val version : package -> version
    val description : package -> string
    val new_package : name_version -> string (* description *) -> package
  end
  (*
  module Cudf : CUDF =
  struct
    type cudf = string
    type package 
    type request
  end
  *)

  module type CONFIG =
  sig
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t

    val version : t -> version
    val sources : t -> Path.url option
  end

  module type INSTALLED =
  sig
    type t = name_version list

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t
  end

  module type TO_INSTALL =
  sig
    type suffix = 
      | CMI 
      | CMO
      | CMA
      | CMX
      | Explicit of string

    type prefix = 
      | Absolute (** consider that the path begins at "/" *)
      | Relative

    type path = prefix * Path.basename list * suffix (** By default, every [suffix] will be considered (ie. the regexp equivalent to ".*" ). *)
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t

    val is_relative : path -> bool

    val lib : t -> path list
    val bin : t -> path
    val misc : t -> (path (* from *) * path (* to *)) list

    val string_of_path : path -> string
    val filename_of_path_relative : Path.filename (* prefix *) -> path -> Path.filename
    val filename_of_path_absolute : path -> Path.filename
  end
end

module type SOLVER =
sig
  type package (* name, version, conflicts, dependencies *)

  type 'a request =
      { wish_install : 'a list
      ; wish_remove : 'a list
      ; wish_upgrade : 'a list }

  type 'a action = 
    | To_change of 'a 
        (* Version to install. The package could have been present or not, 
           but if present, it is another version than the proposed solution. *)
    | To_delete (* The package have been installed. *)
    | To_recompile (* The package is already installed, we just recompile it. *)

  type solution = (Namespace.name * Namespace.version action) list
      (** Sequence describing the action to perform.
          Order natural : first element to execute is the first element of the list. *)

  val resolve : package list -> name_version request -> solution list
    (** Given a description of packages, it returns a list of solution preserving the consistency of the initial description. *)
end

module type SERVER =
sig
  type t
  type opam
  type package

  val init : Path.url option -> t

  val change_url : t -> Path.url -> t

  val getList : t -> name_version list
    (** Returns the list of the available versions for all
        packages. *)

  val getOpam : t -> name_version -> opam
    (** Returns the representation of
        the OPAM file for the corresponding package version. *)

  val getArchive : t -> opam -> archive
    (** Returns the corresponding package archive. *)

  val newArchive : t -> opam -> archive -> t
    (** Receives an upload, it contains an OPAM file and the
        corresponding package archive. *)

  val version : t -> Namespace.version

  val package : opam -> package option 
    (** [None] : the [opam] associated to the [(name, version)] does not exist. 
        Note that every [(name, version)] given by [getList] do exist. *)
end

module Server
  (F_cudf : File.CUDF) 
  : SERVER =
struct
  module Path_map = BatMap.Make (struct type t = Path.t let compare = Path.compare_computer end)

  type t = 
      { current_repository : F_cudf.package NV_map.t
      ; home_opam : Path.t (* ~/.opam-server *)
      ; all_repository : F_cudf.package NV_map.t Path_map.t
      ; package_manager : Namespace.version }

  type opam = name_version * F_cudf.package option
      (* [None] : the current repository does not contain the package associated to the [name] and [version] *)

  type package = F_cudf.package

  let read_archives home_opam =
    let archives = Path.archives_targz home_opam None in
      List.fold_left
        (fun map x -> 
           NV_map.add
             (Namespace.nv_of_string (Path.chop_extension x)) 
             (F_cudf.package (F_cudf.find home_opam (Path.concat archives x))) 
             map) NV_map.empty 
        (match Path.find home_opam archives with
           | Path.Directory l -> l
           | _ -> [])

  let init o = 
    let home_opam = Path.init o ".opam-server" in
    { current_repository = read_archives home_opam
    ; home_opam
    ; all_repository = Path_map.empty
    ; package_manager = Namespace.Version "1.0" }

  let change_url t url = 
    let home_opam = Path.change_url t.home_opam url in
    { t with
        current_repository = (match Path_map.Exceptionless.find home_opam t.all_repository with
                                | None -> read_archives home_opam
                                | Some v -> v);
        home_opam;
        all_repository = Path_map.add t.home_opam t.current_repository t.all_repository }

  let getList t = BatList.map fst (NV_map.bindings t.current_repository)
  let getOpam t n_v = n_v, NV_map.Exceptionless.find n_v t.current_repository
  let getArchive t = function
    | _, None -> Empty
    | n_v, Some _ -> 
        match Path.find t.home_opam (Path.archives_targz t.home_opam (Some n_v)) with
          | Path.File s -> Tar_gz s
          | _ -> Empty

  let newArchive t (n_v, o_pack) arch = 
    let t = 
      { t with 
        home_opam = 
          Path.add 
            t.home_opam 
            (Path.archives_targz t.home_opam (Some n_v)) 
            (Path.File (match arch with 
                          | Empty -> failwith "create an empty tar.gz here" 
                          | Tar_gz s -> s)) } in
      
    match o_pack with
      | None -> { t with current_repository = NV_map.add n_v (F_cudf.new_package n_v "") t.current_repository }
      | Some _ -> t

  let version t = t.package_manager
  let package = snd
end

module type CLIENT =
sig
  type t

  val init0 : unit -> t

  val init : t -> Path.url (* repository address *) option (* [None] : default is opam.ocamlpro.com, port = 9999 *) -> t
    (** Initializes in a consistent state. *)

  val info : t -> Namespace.name option -> t
    (** Displays the installed package. [None] : a general summary is given. *)

  type config_request = Dir
  val config : t -> config_request -> Namespace.name -> t
    (** Returns the directory where the package is installed,
        in a form suitable to OCaml compilers (i.e. like "-I ..."). *)

  val install : t -> Namespace.name -> t
    (** Installs the given package. *)

  val update : t -> t
    (** Downloads the latest packages available. *)

  val upgrade : t -> t
    (** Finds a consistent state where most of the installed packages are
        upgraded to their latest version. *)

  val upload : t -> Path.filename -> t
    (** Sends a new created package to the server. *)
end
