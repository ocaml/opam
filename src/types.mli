(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Define the basic types on which OPAM operates *)

(** {2 Abstract types} *)

(** All abstract types should implement this signature *)
module type Abstract = sig

  (** Abstract type *)
  type t

  (** Create an abstract value from a string *)
  val of_string: string -> t

  (** Convert an abstract value to a string *)
  val to_string: t -> string

  (** Collection of abstract values *)
  module Set: Set.S with type elt = t

  (** Dictionaries of abstract values *)
  module Map: Map.S with type key = t
end

(** {2 Filenames} *)

(** Absolute directory names *)
module Dirname : sig

  include Abstract

  (** Remove a directory *)
  val rmdir: t -> unit

  (** Create a directory *)
  val mkdir: t -> unit

  (** Execute a list of commands in a given directory *)
  val exec: t -> string list -> int

  (** Change the current directory *)
  val chdir: t -> unit
end

(** Shortcut to directory type *)
type dirname = Dirname.t

(** Concatenate a directory and a string *)
val (/): dirname -> string -> dirname

(** Basenames *)
module Basename : Abstract

(** Shortcut to basename type *)
type basename = Basename.t

(** Raw file contents *)
module Raw : Abstract

(** Shortcut to raw file content type *)
type raw = Raw.t

(** Stdlib [Filename] module *)
module Stdlib_filename : sig
  val concat: string -> string -> string
end

(** non-directory filenames *)
module Filename : sig

  include Abstract

  (** Create a filename from a dirname and a basename *)
  val create: dirname -> basename -> t

  (** Return the directory name *)
  val dirname: t -> dirname

  (** Retrieves the contents from the hard disk. *)
  val read: t -> Raw.t

  (** Removes everything in [filename] if existed. *)
  val remove: t -> unit

  (** Removes everything in [filename] if existed, then write [contents] instead. *)
  val write: t -> Raw.t -> unit

  (** see [Sys.file_exists] *)
  val exists: t -> bool

  (** Check whether a file has a given suffix *)
  val check_suffix: t -> string -> bool

  (** List all the filenames (ie. which are not directories) in a directory *)
  val list: dirname -> t list

  (** Apply a function on the contents of a file *)
  val with_raw: (Raw.t -> 'a) -> t -> 'a

  (** Copy a file in a directory *)
  val copy_in: t -> dirname -> unit

  (** Symlink a file in a directory *)
  val link_in: t -> dirname -> unit

  (** Copy a file *)
  val copy: t -> t -> unit

  (** Symlink a file. If symlink is not possible on the system, use copy instead. *)
  val link: t -> t -> unit

  (** Extract an archive in a given directory (it rewrites the root to
      match [dirname] dir if needed) *)
  val extract: t -> dirname -> unit

end

(** Shortcut to file names *)
type filename = Filename.t

(** Concatenate a directory and a string to create a filename *)
val (//): dirname -> string -> filename

(** {2 Package name and versions} *)

(** Versions *)
module V : Abstract

(** Names *)
module N : Abstract

(** Package (name x version) pairs *)
module NV : sig
  include Abstract

  (** Return the package name *)
  val name : t -> N.t

  (** Return the version name *)
  val version: t -> V.t

  (** Create a new pair (name x version) *)
  val create: N.t -> V.t -> t

  (** Create a new pair from a filename. This function extracts
      [$name] and [$version] from [/path/to/$name.$version.XXX with
      various heuristics.*)
  val of_filename: filename -> t option

  (** Create a new pair from a debian package *)
  val of_dpkg: Debian.Packages.package -> t

  (** Create a new pair from a cudf package *)
  val of_cudf: Debian.Debcudf.tables -> Cudf.package -> t

  (** Convert a set of pairs to a map [name -> versions] *)
  val to_map: Set.t -> V.Set.t N.Map.t

  (** Convert a set of pairs to a string *)
  val string_of_set: Set.t -> string
end

(** OCaml version *)
module OCaml_V : Abstract

(** OPAM version *)
module OPAM_V : Abstract

(** {2 Repositories} *)

(** OPAM repositories *)
module Repository : sig

  include Abstract

  (** Create a repository *)
  val create: name:string -> kind:string -> address:string -> t

  (** Default repository *)
  val default: t
  
  (** Get the repository name *)
  val name: t -> string
  
  (** Get the repository kind *)
  val kind: t -> string

  (** Get the repository address *)
  val address: t -> string

end

(** Shortcut to repository type *)
type repository = Repository.t

(** {2 Variable names} *)

(** Variable names are used in .config files *)
module Variable : Abstract

(** Shortcut to variable type *)
type variable = Variable.t

(** {2 Command line arguments} *)

(** Upload arguments *)
type upload = {
  opam   : filename;
  descr  : filename;
  archive: filename;
}

(** Pretty-print *)
val string_of_upload: upload -> string

(** Remote arguments *)
type remote =
  | List
  | Add of string
  | Rm of string

(** Pretty-print *)
val string_of_remote: remote -> string

(** Configuration requests *)
type config_option =
  | Includes of N.t list
  | Bytecomp of (N.t * string) list
  | Asmcomp  of (N.t * string) list
  | Bytelink of (N.t * string) list
  | Asmlink  of (N.t * string) list

type rec_config_option = {
  recursive: bool;
  options  : config_option;
}

type config =
  | Compil   of rec_config_option
  | Variable of (N.t * Variable.t) list
  | Subst    of Filename.t list

(** Pretty-print *)
val string_of_config: config -> string
