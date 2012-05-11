open Types

(** Functions to file read and write configuration files in a typed
    way. *)

module type IO_FILE = sig

  (** File contents *)
  type t

  (** Empty file *)
  val empty: t

  (** Write some contents to a file *)
  val write: filename -> t -> unit
  
  (** Read file contents. Raise an error if the file does not exist. *)
  val read: filename -> t

  (** Read file contents. Return [empty] if the file does not exist. *)
  val safe_read: filename -> t

end

(** Configuration file: [$opam/config] *)
module Config: sig

  include IO_FILE

  (** Creation *)
  val create: OPAM_V.t -> repository list -> OCaml_V.t -> t

  (** Return the OPAM version *)
  val opam_version: t  -> OPAM_V.t

  (** Return the list of repository *)
  val repositories: t  -> repository list

  (** Return the OCaml version *)
  val ocaml_version: t -> OCaml_V.t

end

(** OPAM files *)
module OPAM: sig 

  include IO_FILE

  (** Package name *)
  val name: t -> N.t

  (** Package version *)
  val version: t -> V.t

  (** Package maintainer *)
  val maintainer: t -> string

  (** File substitutions *)
  val substs: t -> filename list

  (** List of command to run for building the package *)
  val build: t -> string list list

  (** Package dependencies *)
  val depends: t -> Debian.Format822.vpkgformula

  (** Package conflicts *)
  val conflicts: t -> Debian.Format822.vpkglist

  (** List of exported libraries *)
  val libraries: t -> string list

  (** List of exported syntax extensions *)
  val syntax: t -> string list

  (** Convert to Debian packages to feed the solver *)
  val to_package: t -> installed:bool -> Debian.Packages.package

end

(** Package descriptions: [$opam/descr/] *)
module Descr: sig
  include IO_FILE

  (** Return the first line *)
  val synopsis: t -> string

  (** Return the full description *)
  val full: t -> string
end

(** List of installed packages: [$opam/$oversion/installed] *)
module Installed: IO_FILE with type t = NV.Set.t

(** List of packages to reinstall: [$opam/$oversion/reinstall] *)
module Reinstall: IO_FILE with type t = NV.Set.t

(** List of updated packages: [$opam/$repo/$repo/updated] *)
module Updated: IO_FILE with type t = NV.Set.t

(** {2 Configuration files} *)

(** .install files *)
module Dot_install: sig

  include IO_FILE

  (** List of files to install in $lib/ *)
  val lib:  t -> filename list

  (** List of files to install in $bin/ *)
  val bin:  t -> (filename * filename) list

  (** List of other files to install *)
  val misc: t -> (filename * filename) list

end

(** .config files *)
module Dot_config: sig
  
  include IO_FILE

  module type SECTION = sig

    (** List the available sections *)
    val available: t -> section list

    (** Return the section kind *)
    val kind: t -> section -> string

    (** Return the list of included directory for a given section *)
    val includes: t -> section -> string list

    (** Return the list of native-compiler options *)
    val asmcomp: t -> section -> string list

    (** Return the list of bytecode-compiler options *)
    val bytecomp: t -> section -> string list

    (** Return the list of native-code linking options *)
    val asmlink: t -> section -> string list

    (** Return the list of bytecode linking options *)
    val bytelink: t -> section -> string list

    (** Return the value of variables *)
    val variable: t -> section -> variable  -> variable_contents

    (** The list of local variables *)
    val variables: t -> section -> variable list

  end

  (** All library and syntax sections *)
  module Sections: SECTION

  (** Sections starting by [library] *)
  module Library: SECTION

  (** Sections starting by [syntax] *)
  module Syntax: SECTION

  (** Top-level variables *)
  val variable: t -> variable  -> variable_contents

  (** The list of top-level variables *)
  val variables: t -> variable list

end 

(** {2 Repository files} *)

(** Association between package names and repository: [$opam/repo/index] *)
module Repo_index: IO_FILE with type t = string N.Map.t

(** Repository config: [$opam/repo/$repo/config] *)
module Repo_config: IO_FILE with type t = repository
