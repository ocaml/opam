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

open OpamTypes

(** Functions to read and write OPAM configuration files in a typed way *)

(** All Configuration files satisfies this signature *)
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

  (** Read from channel. *)
  val read_from_channel: in_channel -> t

  (** Write to channel. *)
  val write_to_channel: out_channel -> t -> unit

end

(** Configuration file: [$opam/config] *)
module Config: sig

  include IO_FILE

  (** Creation *)
  val create:
    opam_version ->
    switch ->
    repository_name list ->
    int ->
    t

  (** OCaml switch updates *)
  val with_switch : t -> switch -> t

  (** Repository updates *)
  val with_repositories: t -> repository_name list -> t

  (** Update opam-version to the current one *)
  val with_current_opam_version: t -> t

  (** Return the OPAM version *)
  val opam_version: t  -> opam_version

  (** Return the list of repository *)
  val repositories: t  -> repository_name list

  (** Return the OCaml switch *)
  val switch: t -> switch

  (** Return the number of jobs *)
  val jobs: t -> int

end

(** OPAM files *)
module OPAM: sig

  include IO_FILE

  (** Create an opam file *)
  val create: package -> t

  (** Package name *)
  val name: t -> name

  (** Package version *)
  val version: t -> version

  (** Compiler constraint *)
  val ocaml_version: t -> compiler_constraint option

  (** OS constraint *)
  val os: t -> (bool * string) generic_formula

  (** Package maintainer *)
  val maintainer: t -> string

  (** File substitutions *)
  val substs: t -> basename list

  (** List of environment variables to set-up for the build *)
  val build_env: t -> (string * string * string) list

  (** List of command to run for building the package *)
  val build: t -> command list

  (** List of command to run for removing the package *)
  val remove: t -> command list

  (** Package dependencies *)
  val depends: t -> formula

  (** Optional dependencies *)
  val depopts: t -> formula

  (** External dependencies *)
  val depexts: t -> tags option

  (** Package conflicts *)
  val conflicts: t -> formula

  (** List of exported libraries *)
  val libraries: t -> section list

  (** List of exported syntax extensions *)
  val syntax: t -> section list

  (** Patches *)
  val patches: t -> (basename * filter option) list

  (** Homepage *)
  val homepage: t -> string option

  (** Authors *)
  val authors: t -> string list

  (** License *)
  val license: t -> string option

  (** API documentation *)
  val doc: t -> string option

  (** Classification tags *)
  val tags: t -> string list

  (** Commands to build and run the tests *)
  val build_test: t -> command list

  (** Commands to build the documentation *)
  val build_doc: t -> command list

  (** Construct as [depends] *)
  val with_depends : t -> formula -> t

  (** Construct as [depopts] *)
  val with_depopts : t -> formula -> t

  (** Construct as [build] *)
  val with_build: t -> command list -> t

  (** Construct as [remove] *)
  val with_remove : t -> command list -> t

  (** Construct as [libraries] *)
  val with_libraries : t -> section list -> t

  (** Construct as [substs] *)
  val with_substs : t -> basename list -> t

  (** Construct as [compiler_version] *)
  val with_ocaml_version: t -> compiler_constraint option -> t

  (** Construct as [maintainer] *)
  val with_maintainer: t -> string -> t

  (** Construct as [patches] *)
  val with_patches: t -> (basename * filter option) list -> t

end

(** Package descriptions: [$opam/descr/] *)
module Descr: sig

  include IO_FILE

  (** Return the first line *)
  val synopsis: t -> string

  (** Return the full description *)
  val full: t -> string

end

(** Compiler aliases: [$opam/aliases] *)
module Aliases: IO_FILE with type t = compiler switch_map

(** Import/export file. This difference with [installed] is that we
    are explicit about root packages. *)
module Export: IO_FILE with type t = package_set * package_set

(** List of installed packages: [$opam/$oversion/installed] *)
module Installed: IO_FILE with type t = package_set

(** List of packages explicitely installed by the user:
    [$opam/$switch/installed.user] *)
module Installed_roots: IO_FILE with type t = package_set

(** List of packages to reinstall: [$opam/$oversion/reinstall] *)
module Reinstall: IO_FILE with type t = package_set

(** List of updated packages: [$opam/$repo/$repo/updated] *)
module Updated: IO_FILE with type t = package_set

(** Compiler version [$opam/compilers/] *)
module Comp: sig

  include IO_FILE

  (** Create a pre-installed compiler description file *)
  val create_preinstalled:
    compiler -> compiler_version -> name list -> (string * string * string) list -> t

  (** Is it a pre-installed compiler description file *)
  val preinstalled: t -> bool

  (** Return the compiler name *)
  val name: t -> compiler

  (** Return the compiler version *)
  val version: t -> compiler_version

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
  val packages: t -> formula

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

(** Compiler descriptions *)
module Comp_descr: IO_FILE with type t = string

(** {2 Configuration files} *)

(** .install files *)
module Dot_install: sig

  include IO_FILE

  (** List of files to install in $bin/ *)
  val bin:  t -> (basename optional * basename option) list

  (** List of files to install in $lib/ *)
  val lib:  t -> basename optional list

  (** List of toplevel files *)
  val toplevel: t -> basename optional list

  (** List of shared files *)
  val share: t -> basename optional list

  (** List of doc files *)
  val doc: t -> basename optional list

  (** List of other files to install *)
  val misc: t -> (basename optional * filename) list

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
module Repo_index: IO_FILE with type t = repository_name list name_map

(** Repository config: [$opam/repo/$repo/config] *)
module Repo_config: IO_FILE with type t = repository

(** Pinned package files *)
module Pinned: IO_FILE with type t = pin_option name_map

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

  (** Backend kind (could be curl/rsync/git/darcs at the moment) *)
  val kind: t -> repository_kind option

  (** Archive checksum *)
  val checksum: t -> string option

  (** Constructor *)
  val with_checksum: t -> string -> t

end

(** {2 urls.txt file *} *)
module Urls_txt: IO_FILE with type t = file_attribute_set

(** List of filenames *)
module Filenames: IO_FILE with type t = filename_set

(** Prefix of package directories *)
module Prefix: IO_FILE with type t = string name_map

(** Display statistics about file access. *)
val print_stats: unit -> unit
