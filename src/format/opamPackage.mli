(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** The package type, and package name type (name+version, values often called
    "nv" in the code) *)

(** {2 Package name and versions} *)

(** Versions *)
module Version: sig

  include OpamStd.ABSTRACT

  (** Compare two versions using the Debian version scheme *)
  val compare: t -> t -> int

  (** Are two package versions equal? *)
  val equal: t -> t -> bool

  (** Default version used when no version is given *)
  val default : t
end

(** Names *)
module Name: sig
  include OpamStd.ABSTRACT

  (** Compare two package names *)
  val compare: t -> t -> int

  (** Are two package names equal? *)
  val equal: t -> t -> bool
end

type t = private {
  name: Name.t;
  version: Version.t;
}

(** Package (name x version) pairs *)
include OpamStd.ABSTRACT with type t := t

(** Return the package name *)
val name: t -> Name.t

(** Return None if [nv] is not a valid package name *)
val of_string_opt: string -> t option

(** Return the version name *)
val version: t -> Version.t

(** Create a new pair (name x version) *)
val create: Name.t -> Version.t -> t

(** To fit in the GenericPackage type, for generic display functions *)
val name_to_string: t -> string
val version_to_string: t -> string

(** Guess the package name from a filename. This function extracts
    [name] and [version] from {i /path/to/$name.$version/opam}, or
    {i /path/to/$name.$version.opam} *)
val of_filename: OpamFilename.t -> t option

(** Guess the package name from a directory name. This function extracts {i
    $name} and {i $version} from {i /path/to/$name.$version/} *)
val of_dirname: OpamFilename.Dir.t -> t option

(** Guess the package name from an archive file. This function extract
    {i $name} and {i $version} from {i
    /path/to/$name.$version+opam.tar.gz} *)
val of_archive: OpamFilename.t -> t option

(** Convert a set of pairs to a map [name -> versions] *)
val to_map: Set.t -> Version.Set.t Name.Map.t

(** The converse of [to_map] *)
val of_map: Version.Set.t Name.Map.t -> Set.t

(** Returns the keys in a package map as a package set *)
val keys: 'a Map.t -> Set.t

(** Extract the versions from a collection of packages *)
val versions_of_packages: Set.t -> Version.Set.t

(** Return the list of versions for a given package *)
val versions_of_name: Set.t -> Name.t -> Version.Set.t

(** Extract the naes from a collection of packages *)
val names_of_packages: Set.t -> Name.Set.t

(** Returns true if the set contains a package with the given name *)
val has_name: Set.t -> Name.t -> bool

(** Return all the packages with the given name *)
val packages_of_name: Set.t -> Name.t -> Set.t
val packages_of_name_map: 'a Map.t -> Name.t -> 'a Map.t

(** Return a package with the given name *)
val package_of_name: Set.t -> Name.t -> t

(** Return a package with the given name, if any *)
val package_of_name_opt: Set.t -> Name.t -> t option

(** Return all the packages with one of the given names *)
val packages_of_names: Set.t -> Name.Set.t -> Set.t

(** Removes all packages with the given name from a set of packages *)
val filter_name_out: Set.t -> Name.t -> Set.t

(** Return the maximal available version of a package name from a set.
    @raise Not_found if no such package available. *)
val max_version: Set.t -> Name.t -> t

(** Compare two packages *)
val compare: t -> t -> int

(** Are two packages equal? *)
val equal: t -> t -> bool

(** Hash a package *)
val hash: t -> int

(** Return all the package descriptions in a given directory *)
val list: OpamFilename.Dir.t -> Set.t

(** Return all the package descriptions in the current directory (and
    their eventual prefixes). *)
val prefixes: OpamFilename.Dir.t -> string option Map.t

module Uniset : sig
  (** Set of packages containing at most one version of any given package name *)

  exception Multiple_versions of Name.t

  type package = t
  type t

  val empty : t
  val is_empty : t -> bool
  val is_singleton : t -> bool
  val compare : t -> t -> int
  val find : Name.t -> t -> package
  val find_opt : Name.t -> t -> package option
  val find_set : Name.Set.t -> t -> t
  val choose : t -> package
  val has_name : Name.t -> t -> bool
  val mem : package -> t -> bool
  val elements : t -> package list
  val fold : (package -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val iter : (package -> unit) -> t -> unit
  val filter : (package -> bool) -> t -> t
  val exists : (package -> bool) -> t -> bool

  (** [map ~conflict f t] returns the uniset [t'] containing the results
      of applying [f] to elements of [t]. [conflict] is called to determine
      what to do with conflicting versions of the same package. *)
  val map :
    ?conflict:(package -> package -> package option) ->
    (package -> package) ->
    t ->
    t

  (** [partition pred t] returns [t1 , t2] where [t1] is all elements of [t]
      that satisfy [pred] and [t2] all elements that do not. *)
  val partition: (package -> bool) -> t -> t * t

  (** [add pkg t] adds package [pkg] to the uniset [t]. If another version of this
      package was already in [t], replace it with [pkg]. *)
  val add : package -> t -> t

  (** [remove pkg t] removes [pkg] from [t] if it was included in the uniset,
      returns it unchanged otherwise. *)
  val remove : package -> t -> t

  (** [remove_name name t] removes the package named [name] from the [t] if present,
      returns [t] unchanged otherwise. *)
  val remove_name : Name.t -> t -> t

  (** [union ~on_conflit t1 t2] returns the union of the uniset [t1] and [t2].
      When [t1] and [t2] contain a different version of a given package each,
      [conflict] is called to determine what to do.
      By default, [conflict] will choose the version from the second
      argument. *)
  val union :
    ?conflict: (package -> package -> package option) -> t -> t -> t

  val strict_union : t -> t -> Set.t

  (** [inter ~on_conflit t1 t2] returns the intersection of the uniset
      [t1] and [t2]. *)
  val inter : t -> t -> t

  (** [diff ~on_conflit t1 t2] returns the diff of the uniset [t1] and [t2],
      i.e. packages that are in [t1] and not in [t2]. *)
  val diff : t -> t -> t

  (** [union_set t set] returns the union of the given uniset and set. *)
  val union_set : t -> Set.t -> Set.t

  (** [set_inter t set] returns the intersection between the given uniset and
      set, i.e. elements of [t] that are also in [set]. *)
  val set_inter : t -> Set.t -> t

  (** [set_diff t set] returns the diff between the given uniset and
      set, i.e. elements of [t] that are not in [set]. *)
  val set_diff : t -> Set.t -> t

  (** opposite of {!set_diff}. *)
  val diff_set : Set.t -> t -> Set.t

  (** Builds a uniset from a set of packages.
      When the given set contains more than one version of a given package,
      [conflict] is called (first argument is the one already in the uniset).
      By default, raises [Multiple_version _] if the given set contains more
      than one version of any given package. *)
  val of_set :
    ?conflict: (package -> package -> package option) -> Set.t -> t

  val to_set : t -> Set.t

  (** Same as {!of_set} but with keys of a package map. *)
  val of_map :
    ?conflict: (package -> package -> package option) -> _ Map.t -> t

  (** Same as {!of_set} but with list input. *)
  val of_list :
    ?conflict: (package -> package -> package option) -> package list -> t

  val to_list : t -> package list

  val to_list_map : (package -> 'a) -> t -> 'a list

  val names : t -> Name.Set.t

  (** [to_string t] returns a pretty-printed version of [t] for debugging
      purposes. *)
  val to_string : t -> string

  module Op : sig

    (** Infix uniset union with default conflict handling *)
    val (++) : t -> t -> t

    (** Infix uniset difference *)
    val (--) : t -> t -> t

    (** Infix uniset intersection *)
    val (%%) : t -> t -> t
  end
end

(** {2 Errors} *)

(** Parallel executions. *)
module Graph: OpamParallel.GRAPH with type V.t = t
