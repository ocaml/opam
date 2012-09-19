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

open Types

(** Functions to read and write configuration files in a typed way. *)
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

  (** Return the file contents *)
  val to_raw: t -> raw

  (** Convert a raw string into a file *)
  val of_raw: raw -> t

end

(** Configuration file: [$opam/config] *)
module Config: sig

  include IO_FILE

  (** Creation *)
  val create: OPAM_V.t -> repository list -> int -> t

  (** OCaml alias updates *)
  val with_ocaml_version : t -> Alias.t -> t

  (** Repository updates *)
  val with_repositories: t -> repository list -> t

  (** system-wide's OCaml version updates *)
  val with_system_ocaml_version: t -> OCaml_V.t -> t

  (** Return the OPAM version *)
  val opam_version: t  -> OPAM_V.t

  (** Return the list of repository *)
  val repositories: t  -> repository list

  (** Return the OCaml alias *)
  val ocaml_version: t -> Alias.t

  (** Return the system's OCaml version *)
  val system_ocaml_version: t -> OCaml_V.t option

  (** Return the number of cores *)
  val cores: t -> int

end

(** OPAM files *)
module OPAM: sig

  include IO_FILE

  (** Create an opam file *)
  val create: nv -> t

  (** Full constructor *)
  val make:
    name:name -> version:version -> maintainer:string ->
    substs:basename list -> build_env:(string * string * string) list ->
    build:string list list -> remove:string list list ->
    depends:cnf_formula -> depopts:cnf_formula -> conflicts:and_formula ->
    libraries:section list -> syntax:section list ->
    others:(string * File_format.value) list ->
    ocaml_version:ocaml_constraint option -> t

  (** Package name *)
  val name: t -> name

  (** Package version *)
  val version: t -> version

  (** Compiler constraint *)
  val ocaml_version: t -> ocaml_constraint option

  (** Package maintainer *)
  val maintainer: t -> string

  (** File substitutions *)
  val substs: t -> basename list

  (** List of environment variables to set-up for the build *)
  val build_env: t -> (string * string * string) list

  (** List of command to run for building the package *)
  val build: t -> string list list

  (** List of command to run for removing the package *)
  val remove: t -> string list list

  (** Package dependencies *)
  val depends: t -> cnf_formula

  (** Optional dependencies *)
  val depopts: t -> cnf_formula

  (** Package conflicts *)
  val conflicts: t -> and_formula

  (** List of exported libraries *)
  val libraries: t -> section list

  (** List of exported syntax extensions *)
  val syntax: t -> section list

  (** Convert to Debian packages to feed the solver *)
  val to_package: t -> installed:bool -> Debian.Packages.package

  (** deptopts (optional dependencies) string *)
  val s_depopts: string

  (** Construct as [depends] *)
  val with_depends : t -> cnf_formula -> t

  (** Construct as [depopts] *)
  val with_depopts : t -> cnf_formula -> t

  (** Construct as [build] *)
  val with_build: t -> string list list -> t

  (** Construct as [remove] *)
  val with_remove : t -> string list list -> t

  (** Construct as [libraries] *)
  val with_libraries : t -> section list -> t

  (** Construct as [substs] *)
  val with_substs : t -> basename list -> t

  (** Construct as [ocaml_version] *)
  val with_ocaml_version: t -> ocaml_constraint option -> t

  (** Construct as [maintainer] *)
  val with_maintainer: t -> string -> t
end

(** Package descriptions: [$opam/descr/] *)
module Descr: sig

  include IO_FILE

  (** Create a description file *)
  val create: string -> t

  (** Return the first line *)
  val synopsis: t -> string

  (** Return the full description *)
  val full: t -> string
end

(** Compiler aliases: [$opam/aliases] *)
module Aliases: IO_FILE with type t = (Alias.t * OCaml_V.t) list

(** List of installed packages: [$opam/$oversion/installed] *)
module Installed: IO_FILE with type t = NV.Set.t

(** List of packages to reinstall: [$opam/$oversion/reinstall] *)
module Reinstall: IO_FILE with type t = NV.Set.t

(** List of updated packages: [$opam/$repo/$repo/updated] *)
module Updated: IO_FILE with type t = NV.Set.t

(** Environement variables *)
module Env: IO_FILE with type t = (string * string) list

(** Compiler version [$opam/compilers/] *)
module Comp: sig

  include IO_FILE

  (** Create a pre-installed compiler description file *)
  val create_preinstalled:
    OCaml_V.t -> and_formula -> (string * string * string) list -> t

  (** Is it a pre-installed compiler description file *)
  val preinstalled: t -> bool

  (** Return the compiler name *)
  val name: t -> OCaml_V.t

  (** Return the url of the compiler *)
  val src: t -> filename option

  (** Return the list of patches to apply *)
  val patches: t -> filename list

  (** Options to give to the "./configure" command *)
  val configure: t -> string list

  (** Options to give to the "make" command *)
  val make: t -> string list

  (** Options to give to build the package. If this one is provided,
      nothing should be specified for [configure] and [make]. *)
  val build: t -> string list list

  (** Packages to install immediately after the creation of OCaml *)
  val packages: t -> and_formula

  (** Linking options to give to the native code compiler *)
  val asmlink: t -> string list

  (** Compilation options to give to the native code compiler *)
  val asmcomp: t -> string list

  (** Linking options to give to the bytecode compiler *)
  val bytelink: t -> string list

  (** Compilation options to give to the bytecode compiler *)
  val bytecomp: t -> string list

  (** Linking options to give to the native code compiler *)
  val asmcomp: t -> string list

  (** Libraries to link with *)
  val requires: t -> section list

  (** Preprocessing options *)
  val pp: t -> ppflag option

  (** Environment variable to set-up before running commands in the
      subtree *)
  val env: t -> (string * string * string) list

end

(** {2 Configuration files} *)

(** .install files *)
module Dot_install: sig

  module Raw : sig

    include IO_FILE

    (** List of files to install in $lib/ *)
    val lib:  t -> string list

    (** List of files to install in $bin/ *)
    val bin:  t -> (string * string option) list

    (** List of other files to install *)
    val misc: t -> (string * string option) list

    (** List of toplevel files *)
    val toplevel: t -> string list

    (** Construct as [bin] *)
    val with_bin: t -> (string * string option) list -> t

    (** Construct as [lib] *)
    val with_lib: t -> string list -> t

    (** Construct as [toplevel] *)
    val with_toplevel: t -> string list -> t

  end

  include IO_FILE

  (** List of files to install in $lib/ *)
  val lib:  t -> filename list

  (** List of files to install in $bin/ *)
  val bin:  t -> (filename * basename) list

  (** List of toplevel files *)
  val toplevel: t -> filename list

  (** List of other files to install *)
  val misc: t -> (filename * filename) list

end

(** .config files *)
module Dot_config: sig

  include IO_FILE

  (** Create a new .config file (containing only variables) *)
  val create: (variable * variable_contents) list -> t

  module type SECTION = sig

    (** List the available sections *)
    val available: t -> section list

    (** Return the section kind *)
    val kind: t -> section -> string

    (** Return the list of native-compiler options *)
    val asmcomp: t -> section -> string list

    (** Return the list of bytecode-compiler options *)
    val bytecomp: t -> section -> string list

    (** Return the list of native-code linking options *)
    val asmlink: t -> section -> string list

    (** Return the list of bytecode linking options *)
    val bytelink: t -> section -> string list

    (** Return the build requirements *)
    val requires: t -> section -> section list

    (** Return the value of variables *)
    val variable: t -> section -> variable  -> variable_contents

    (** The list of local variables *)
    val variables: t -> section -> variable list

  end

  (** All library and syntax sections *)
  module Section: SECTION

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
module Repo_index: IO_FILE with type t = string list N.Map.t

(** Repository config: [$opam/repo/$repo/config] *)
module Repo_config: IO_FILE with type t = repository

(** Pinned package files *)
module Pinned: IO_FILE with type t = pin_option N.Map.t

(** {2 Substitution files} *)

(** Substitution files *)
module Subst: sig

  include IO_FILE

  (** Substitute the variables appearing in a file *)
  val replace: t ->  (full_variable -> variable_contents)-> t

  (** Substitute the variables appearing in a string *)
  val replace_string: string -> (full_variable -> variable_contents) -> string

end

(** {2 Urls for OPAM repositories} *)
module URL: sig

  include IO_FILE

  (** URL address *)
  val url: t -> string

  (** Backend kind (could be curl/rsync/git at the moment) *)
  val kind: t -> string option

  (** Archive checksum *)
  val checksum: t -> string option

  (** Constructor *)
  val create: ?checksum:string -> string -> t

  (** Constructor *)
  val with_checksum: t -> string option -> t

end

(** {2 urls.txt file *} *)
module Urls_txt: IO_FILE with type t = Remote_file.Set.t

(** List of filenames *)
module Filenames: IO_FILE with type t = Filename.Set.t
