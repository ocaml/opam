module Namespace =
struct
  open Printf

  type name = Name of string
  let name_compare (Name n1) (Name n2) = failwith "to complete !"

  type version = Version of string
  let version_compare (Version v1) (Version v2) = failwith "to complete !"
  let version_max = max

  let string_of_nv (Name n) (Version v) = sprintf "%s-%s" n v
  let string_user_of_name (Name n) = n
  let string_user_of_version (Version v) = v

  let nv_of_string s = 
    let n, v = BatString.split s "-" in
      Name n, Version v
end

module N_map = BatMap.Make (struct open Namespace type t = name let compare = name_compare end)
module NV_map = 
  BatMap.Make 
    (struct
       open Namespace
       type t = name * version
       let compare (n1, v1) (n2, v2) = 
         let c = name_compare n1 n2 in
           if c = 0 then
             version_compare v1 v2
           else
             c
     end)

type archive = 
  | Tar_gz of string
  | Empty

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

  val init : url option (* [None] : local *) -> string (* $HOME_OPAM *) -> t

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
  val index_opam : t -> (Namespace.name * Namespace.version) option -> filename (* $HOME_OPAM/index/NAME-VERSION.opam *)
    (** OPAM files for all versions and all packages *)
  val archives_targz : t -> (Namespace.name * Namespace.version) option -> filename (* $HOME_OPAM/archives/NAME-VERSION.tar.gz *)
    (** source archives for all versions of all packages *)
  val build : t -> Namespace.name -> Namespace.version -> filename (* $HOME_OPAM/build/NAME-VERSION *)
    (** tempory folders used to decompress the corresponding archives *)

  val find : t -> filename -> string (* raw binary *) contents
    (** Retrieves the contents from the hard disk. *)
  val add : t -> filename -> string contents -> t
    (** Removes everything in [filename] if existed, then write [contents] instead. *)
(*  val modify_def : t -> string contents -> filename -> (string contents -> string contents) -> t
    (** Replaces the associated contents of [filename] by the update function. 
        The default [contents] will be used in case no associated contents is found. *)
*)
  val chop_extension : basename -> string
  val concat : filename -> basename -> filename
  val file_exists : filename -> bool

  val url : string (* hostname *) -> int option (* port *) -> url
  val change_url : t -> url -> t
  val compare_computer : t -> t -> int
end

module Path =
struct
  type t
  type basename
  type filename
  type url
  type 'a contents
end

module File =
struct
  module type PRINTF =
  sig
    type t

    val init : unit -> t
    val printf : t -> ('a, out_channel, unit) format -> t
  end

  module type CUDF =
  sig
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t

    type package

    val opam_version : t -> Namespace.version
    val package : t -> package
    val cudf : Namespace.version -> package -> t

    (** fields related to package *)
    val name : package -> Namespace.name
    val version : package -> Namespace.version
    val description : package -> string
    val new_package : Namespace.name * Namespace.version -> string (* description *) -> package
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

    val version : t -> Namespace.version
    val sources : t -> Path.url option
  end

  module type INSTALLED =
  sig
    type t

    val find : Path.t -> Path.filename -> t
    val add : Path.t -> Path.filename -> t -> Path.t

    val installed : t -> (Namespace.name * Namespace.version) list
  end
end

module type SOLVER =
sig
  type package (* name, version, conflicts, dependencies *)
  type request (* list install, list remove, list upgrade *)

  type 'a action = 
    | To_change of 'a 
        (* Version to install. The package could have been present or not, 
           but if present, it is another version than the proposed solution. *)
    | To_delete (* The package have been installed. *)

  type solution = (Namespace.name * Namespace.version action) list
      (** Sequence describing the action to perform.
          Order natural : first element to execute is the first element of the list. *)

  val resolve : package list -> request -> solution list
    (** Given a description of packages, it returns a list of solution preserving the consistency of the initial description. *)
end

module type SERVER =
sig
  type t
  type opam
  type package

  val init : Path.url option -> t

  val change_url : t -> Path.url -> t

  val getList : t -> (Namespace.name * Namespace.version) list
    (** Returns the list of the available versions for all
        packages. *)
  val getOpam : t -> Namespace.name * Namespace.version -> opam
    (** Returns the representation of
        the OPAM file for the corresponding package version. *)
  val getArchive : t -> opam -> archive
    (** Returns the corresponding package archive. *)
  val newArchive : t -> opam -> archive -> t
    (** Receives an upload, it contains an OPAM file and the
        corresponding package archive. *)

  val version : t -> Namespace.version
  val package : opam -> package option 
    (** [None] : the [opam] associated to the [(name, version)] does not exist. *)
    (** Every [(name, version)] given by [getList] exists. *)
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
  val update : t -> unit
    (** Downloads the latest packages available. *)
  val upgrade : t -> t
    (** Finds a consistent state where most of the installed packages are
        upgraded to their latest version. *)
  val upload : t -> Namespace.name -> t
    (** Sends a new created package to the server. *)
end
