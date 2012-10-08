(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** {2 Package name and versions} *)

(** Versions *)
module Version: sig

  include OpamMisc.ABSTRACT

  (** Compare two versions using the Debian version scheme *)
  val compare: t -> t -> int
end

(** Names *)
module Name: OpamMisc.ABSTRACT

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
    $name} and {i $version} from {i /path/to/$name.$version.XXX}
    with various heuristics.*)
val of_filename: OpamFilename.t -> t option

(** Create a new pair from a directory name. This function extracts {i
    $name} and {i $version} from {i /path/to/$name.$version/} *)
val of_dirname: OpamFilename.Dir.t -> t option

(** Create a new pair from a debian package *)
val of_dpkg: Debian.Packages.package -> t

(** Create a new pair from a cudf package *)
val of_cudf: Debian.Debcudf.tables -> Cudf.package -> t

(** Convert a set of pairs to a map [name -> versions] *)
val to_map: Set.t -> Version.Set.t Name.Map.t

(** Extract the versions from a collection of packages *)
val versions_of_packages: Set.t -> Version.Set.t

(** Look for all .opam files in directory *)
val opam_files: OpamFilename.Dir.t -> Set.t
