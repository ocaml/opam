(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** {2 Package name and versions} *)

(** Versions *)
module Version: sig

  include OpamMisc.ABSTRACT

  (** Compare two versions using the Debian version scheme *)
  val compare: t -> t -> int
end

(** Names *)
module Name: sig
  include OpamMisc.ABSTRACT

  (** default package *)
  val default: t
end

(** Package (name x version) pairs *)
include OpamMisc.ABSTRACT

(** Return the package name *)
val name: t -> Name.t

(** Return None if [nv] is not a valid package name *)
val of_string_opt: string -> t option

(** Return the version name *)
val version: t -> Version.t

(** Create a new pair (name x version) *)
val create: Name.t -> Version.t -> t

(** Create a new pair from a filename. This function extracts {i
    $name} and {i $version} from {i /path/to/$name.$version.XXX} with
    various heuristics. If [all] is unset, discard "opam" and "url"
    files. *)
val of_filename: all:bool -> OpamFilename.t -> t option

(** Create a new pair from a directory name. This function extracts {i
    $name} and {i $version} from {i /path/to/$name.$version/} *)
val of_dirname: OpamFilename.Dir.t -> t option

(** Convert a set of pairs to a map [name -> versions] *)
val to_map: Set.t -> Version.Set.t Name.Map.t

(** Extract the versions from a collection of packages *)
val versions_of_packages: Set.t -> Version.Set.t

(** Return the list of versions for a given package *)
val versions_of_name: Set.t -> Name.t -> Version.Set.t

(** Extract the naes from a collection of packages *)
val names_of_packages: Set.t -> Name.Set.t

(** Return all the packages with the given names *)
val packages_of_name: Set.t -> Name.t -> Set.t

(** Compare two packages *)
val compare: t -> t -> int

(** Are two packages equal ? *)
val equal: t -> t -> bool

(** Hash a package *)
val hash: t -> int

(** List all the .opam files and the package directories in a given
    path *)
val list: OpamFilename.Dir.t -> Set.t

(** {2 Errors} *)

(** Unknown package: either the name is unknown, or the version does
    not exist. *)
val unknown: Name.t -> Version.t option -> 'a

(** Unavailable package: the package exists in the database, but it is
    not available due to compiler/OS constraints. *)
val unavailable: Name.t -> Version.t option -> 'a

(** Unavailable because the package is pinned. *)
val unavailable_because_pinned: Name.t -> Version.t option -> 'a
