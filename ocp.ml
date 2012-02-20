type name = Name of string
type version = Version of string
type archive = Archive of string

module type PATH =
sig
  type filename
  type url

  val home_opam : filename
    (** end-user home path *) (* ~/.opam *)
  val proot : filename
    (** path in the packager filesystem, contains the collection of libraries and programs *)
  val lib : filename
    (** contain installed libraries *) (* ~/.opam/lib *)
  val bin : filename
    (** contain installed binaries *) (* ~/.opam/bin *)
  val index_opam : name -> version -> filename
    (** OPAM files for all versions and all packages *)
  val archives_targz : name -> version -> filename
    (** source archives for all versions of all packages *)

  val url : string (* hostname *) * int option (* port *) -> url
end

module Path (*: PATH*) =
struct
  type filename
  type url
end

module type SOLVER =
sig
  type cudf_packages (* name, version, conflicts, dependencies *)
  type cudf_request (* list install, list remove, list upgrade *)

  type 'a action = 
    | To_change of 'a 
        (* Version to install. The package could have been present or not, 
           but if present, it is another version than the proposed solution. *)
    | To_delete (* The package have been installed. *)

  type solution = (name * version action) list
      (** Sequence describing the action to perform.
          Order natural : first element to execute is the first element of the list. *)

  val resolve : cudf_packages -> cudf_request -> solution list
    (** Given a description of packages, it returns a list of solution preserving the consistency of the initial description. *)
end

module type SERVER =
sig
  type t
  type opam

  val init : unit -> t

  val getList : t -> (name * version) list
    (** Returns the list of the available versions for all
        packages. *)
  val getOpam : t -> name -> version -> opam
    (** Returns the representation of
        the OPAM file for the corresponding package version. *)
  val getArchive : t -> name -> version -> archive
    (** Returns the corresponding package archive. *)
  val newArchive : t -> opam -> archive -> t
    (** Receives an upload, it contains an OPAM file and the
        corresponding package archive. *)
end

module type CLIENT =
sig
  type t

  val init : Path.url (* repository address *) option (* [None] : default is opam.ocamlpro.com, port = 9999 *) -> t
    (** Initializes in a consistent state. *)
  val info : t -> name option -> t
    (** Displays the installed package. [None] : a general summary is given. *)
  type config_request = Dir
  val config : t -> config_request -> name -> t
    (** Returns the directory where the package is installed,
        in a form suitable to OCaml compilers (i.e. like "-I ..."). *)
  val install : t -> name -> t
    (** Installs the given package. *)
  val update : t -> t
    (** Downloads the latest packages available. *)
  val upgrade : t -> t
    (** Finds a consistent state where most of the installed packages are
        upgraded to their latest version. *)
  val upload : t -> name -> t
    (** Sends a new created package to the server. *)


  module type PATH =
  sig
    open Path
    val config : filename
      (** main configuration file *)
    val installed : filename
      (** list of installed packages with their version *)
    val build : name -> version -> filename
      (** tempory folders used to decompress the corresponding archives *)
    val lib : name -> filename
      (** installed libraries for the package (at most one version installed) *)
  end
end
