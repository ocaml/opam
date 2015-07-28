(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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
  val read_from_channel: ?filename:filename -> in_channel -> t

  val read_from_string: ?filename:filename -> string -> t

  (** Write to channel. *)
  val write_to_channel: out_channel -> t -> unit

end

(** Lines of space-separated words. *)
module Lines: IO_FILE with type t = string list list

(** Configuration file: [$opam/config] *)
module Config: sig

  include IO_FILE

  (** Creation *)
  val create:
    switch ->
    repository_name list ->
    ?criteria:(OpamTypes.solver_criteria * string) list ->
    ?solver:(arg list) ->
    int ->
    ?download_tool:(arg list) ->
    int ->
    t

  (** OCaml switch updates *)
  val with_switch : t -> switch -> t

  (** Repository updates *)
  val with_repositories: t -> repository_name list -> t

  (** Update opam-version *)
  val with_opam_version: t -> OpamVersion.t -> t

  val with_criteria: t -> (solver_criteria * string) list -> t

  val with_solver: t -> arg list option -> t

  (** Return the OPAM version *)
  val opam_version: t  -> opam_version

  (** Return the list of repository *)
  val repositories: t  -> repository_name list

  (** Return the OCaml switch *)
  val switch: t -> switch

  (** Return the number of jobs *)
  val jobs: t -> int

  val dl_tool: t -> arg list option

  (** Return the number of download jobs *)
  val dl_jobs: t -> int

  val criteria: t -> (solver_criteria * string) list

  val solver: t -> arg list option

end

(** OPAM files *)
module OPAM: sig

  include IO_FILE

  val empty: t

  (** Create an opam file *)
  val create: package -> t

  (** Create an OPAM package template filled with common options *)
  val template: package -> t

  (** Runs several sanity checks on the opam file; returns a list of warnings.
      [`Error] level should be considered unfit for publication, while
      [`Warning] are advisory but may be accepted. The int is an identifier for
      this specific warning/error. *)
  val validate: t -> (int * [`Warning|`Error] * string) list

  (** Same as [validate], but operates on a file, which allows catching parse
      errors too. You can specify an expected name and version *)
  val validate_file: filename ->
    (int * [`Warning|`Error] * string) list * t option

  (** Like [validate_file], but takes the file contents as a string *)
  val validate_string: filename -> string ->
    (int * [`Warning|`Error] * string) list * t option

  (** Utility function to print validation results *)
  val warns_to_string: (int * [`Warning|`Error] * string) list -> string

  (** Returns true if the given OPAM file contains 'name' or 'version' fields *)
  val is_explicit: filename -> bool

  (** Get OPAM version. *)
  val opam_version: t -> opam_version

  (** Package name *)
  val name: t -> name
  val name_opt: t -> name option

  (** Package version *)
  val version: t -> version
  val version_opt: t -> version option

  (** Compiler constraint *)
  val ocaml_version: t -> compiler_constraint option

  (** OS constraint *)
  val os: t -> (bool * string) generic_formula

  (** Availability formula (OS + compiler constraints) *)
  val available: t -> filter

  (** Package maintainer(s) *)
  val maintainer: t -> string list

  (** File substitutions *)
  val substs: t -> basename list

  (** List of environment variables to set-up for the build *)
  val build_env: t -> (string * string * string) list

  (** List of command to run for building the package *)
  val build: t -> command list

  (** List of command to run for installing the package *)
  val install: t -> command list

  (** List of command to run for removing the package *)
  val remove: t -> command list

  (** Package dependencies *)
  val depends: t -> ext_formula

  (** Optional dependencies *)
  val depopts: t -> ext_formula

  (** External dependencies *)
  val depexts: t -> tags option

  val extra_sources: t -> (address * string * basename option) list

  (** All extended "x-" fields as a map *)
  val extensions: t -> value OpamStd.String.Map.t

  (** Parse a single extended field (reports proper file position) *)
  val extended: t -> string -> (value -> 'a) -> 'a option

  val with_messages: t -> (string * filter option) list -> t

  val with_post_messages: t -> (string * filter option) list -> t

  (** Package conflicts *)
  val conflicts: t -> formula

  (** Contents of the 'features' field *)
  val features: t -> (OpamVariable.t * string * filter) list

  (** List of exported libraries *)
  val libraries: t -> (string * filter option) list

  (** List of exported syntax extensions *)
  val syntax: t -> (string * filter option) list

  (** Patches *)
  val patches: t -> (basename * filter option) list

  (** Homepage(s) *)
  val homepage: t -> string list

  (** Author(s) *)
  val author: t -> string list

  (** License(s) *)
  val license: t -> string list

  (** API documentation *)
  val doc: t -> string list

  (** Classification tags *)
  val tags: t -> string list

  (** Commands to build and run the tests *)
  val build_test: t -> command list

  (** Commands to build the documentation *)
  val build_doc: t -> command list

  (** Messages to display before taking action *)
  val messages: t -> (string * filter option) list

  (** Messages to display at end of install *)
  val post_messages: t -> (string * filter option) list

  (** Where to post bug reports. *)
  val bug_reports: t -> string list

  (** The package flags that are present for this package. *)
  val flags: t -> package_flag list

  (** Check the package for the given flag. Allows flags specified through tags
      for compatibility *)
  val has_flag: package_flag -> t -> bool

  (** Sets the opam version *)
  val with_opam_version: t -> opam_version -> t

  (** The package source repository address *)
  val dev_repo: t -> pin_option option

  (** construct as [name] *)
  val with_name: t -> name -> t
  val with_name_opt: t -> name option -> t

  (** construct as [version] *)
  val with_version: t -> version -> t
  val with_version_opt: t -> version option -> t

  (** Construct as [depends] *)
  val with_depends : t -> ext_formula -> t

  (** Construct as [depopts] *)
  val with_depopts : t -> ext_formula -> t

  val with_conflicts : t -> formula -> t

  val with_features : t -> (OpamVariable.t * string * filter) list -> t

  (** Construct as [build] *)
  val with_build: t -> command list -> t

  val with_install: t -> command list -> t

  (** Construct as [remove] *)
  val with_remove : t -> command list -> t

  (** Construct as [libraries] *)
  val with_libraries : t -> (string * filter option) list -> t

  (** Replace the [syntax] field of the given OPAM file. *)
  val with_syntax: t -> (string * filter option) list -> t

  (** Construct as [substs] *)
  val with_substs : t -> basename list -> t

  (** Construct as [compiler_version] *)
  val with_ocaml_version: t -> compiler_constraint option -> t

  val with_os: t -> (bool * string) generic_formula -> t

  val with_available : t -> filter -> t

  (** Construct as [maintainer] *)
  val with_maintainer: t -> string list -> t

  (** Construct as [patches] *)
  val with_patches: t -> (basename * filter option) list -> t

  (** Construct using [bug_reports] *)
  val with_bug_reports: t -> string list -> t

  (** Construct using [depexts] *)
  val with_depexts: t -> tags option -> t

  val with_flags: t -> package_flag list -> t

  val with_dev_repo: t -> pin_option option -> t

  val with_extra_sources: t -> (address * string * basename option) list -> t

  val with_extensions: t -> value OpamStd.String.Map.t -> t

  val add_extension: t -> string -> value -> t

  (** Convert to OPAM 1.0 *)
  val to_1_0: file -> file

end

(** Package descriptions: [$opam/descr/] *)
module Descr: sig

  include IO_FILE

  val create: string -> t

  (** Create an abstract description file from a string *)
  val of_string: filename -> string -> t

  (** Return the first line *)
  val synopsis: t -> string

  (** Return the body *)
  val body: t -> string

  (** Return the full description *)
  val full: t -> string

end

(** Compiler aliases: [$opam/aliases] *)
module Aliases: IO_FILE with type t = compiler switch_map

(** Import/export file. This difference with [installed] is that we
    are explicit about root packages. *)
module Export: IO_FILE with type t =
  package_set * package_set * pin_option OpamPackage.Name.Map.t

(** List of installed packages: [$opam/$oversion/installed] *)
module Installed: IO_FILE with type t = package_set

(** List of packages explicitly installed by the user:
    [$opam/$switch/installed.user] *)
module Installed_roots: IO_FILE with type t = package_set

(** List of packages to reinstall: [$opam/$oversion/reinstall] *)
module Reinstall: IO_FILE with type t = package_set

(** Compiler version [$opam/compilers/] *)
module Comp: sig

  include IO_FILE

  (** Create a pre-installed compiler description file *)
  val create_preinstalled:
    compiler -> compiler_version -> name list -> (string * string * string) list -> t

  (** Is it a pre-installed compiler description file *)
  val preinstalled: t -> bool

  (** Get OPAM version *)
  val opam_version: t -> opam_version

  (** Return the compiler name *)
  val name: t -> compiler

  (** Return the compiler version *)
  val version: t -> compiler_version

  (** Return the url of the compiler *)
  val src: t -> address option

  (** Return the url kind *)
  val kind: t -> repository_kind

  (** Return the list of patches to apply *)
  val patches: t -> filename list

  (** Options to give to the "./configure" command *)
  val configure: t -> string list

  (** Options to give to the "make" command *)
  val make: t -> string list

  (** Options to give to build the package. If this one is provided,
      nothing should be specified for [configure] and [make]. *)
  val build: t -> command list

  (** Packages to install immediately after the creation of OCaml *)
  val packages: t -> formula

  (** Environment variable to set-up before running commands in the
      subtree *)
  val env: t -> (string * string * string) list

  val tags: t -> string list

  val with_src: t -> address option -> t
  val with_patches: t -> filename list -> t
  val with_configure: t -> string list -> t
  val with_make: t -> string list -> t
  val with_build: t -> command list -> t
  val with_packages: t -> formula -> t

  (** Convert to OPAM 1.0 *)
  val to_1_0: file -> file

end

(** {2 Configuration files} *)

(** .install files *)
module Dot_install: sig

  include IO_FILE

  (** List of files to install in $bin/ *)
  val bin:  t -> (basename optional * basename option) list

  (** List of files to install in $sbin/ *)
  val sbin: t -> (basename optional * basename option) list

  (** List of files to install in $lib/ *)
  val lib:  t -> (basename optional * basename option) list

  (** List of toplevel files *)
  val toplevel: t -> (basename optional * basename option) list

  (** C bindings *)
  val stublibs: t -> (basename optional * basename option) list

  (** List of architecture-independent files *)
  val share: t -> (basename optional * basename option) list

  (** List of files under the more general share prefix *)
  val share_root: t -> (basename optional * basename option) list

  (** List of etc files *)
  val etc: t -> (basename optional * basename option) list

  (** List of doc files *)
  val doc: t -> (basename optional * basename option) list

  (** Man pages *)
  val man: t -> (basename optional * basename option) list

  (** Executable files under lib/ *)
  val libexec: t -> (basename optional * basename option) list

  (** List of other files to install *)
  val misc: t -> (basename optional * filename) list

end

(** .config files *)
module Dot_config: sig

  include IO_FILE

  (** Create a new .config file (containing only variables) *)
  val create: (variable * variable_contents) list -> t

  (** Top-level variables *)
  val variable: t -> variable  -> variable_contents option

  (** The list of top-level variables *)
  val variables: t -> variable list

  (** Lists all the bindings in the file *)
  val bindings: t -> (variable * variable_contents) list

  (** Sets the given variable, overriding any previous definition.
      With [None], unsets the variable*)
  val set: t -> variable -> variable_contents option -> t

end

(** {2 Repository files} *)

(** Association between package names and repositories *)
module Package_index: IO_FILE with
  type t = (repository_name * string option) package_map

(** Association between compiler names and repositories *)
module Compiler_index: IO_FILE with
  type t = (repository_name * string option) compiler_map

(** Repository config: [$opam/repo/$repo/config] *)
module Repo_config: IO_FILE with type t = repository

(** Pinned package files *)
module Pinned: IO_FILE with type t = pin_option name_map

(** Repository metadata *)
module Repo: sig

  include IO_FILE

  val create:
    ?browse:string -> ?upstream:string -> ?opam_version:string ->
    ?redirect:(string * filter option) list -> unit -> t

  (** The minimum OPAM version required for this repository *)
  val opam_version : t -> OpamVersion.t

  (** Base URL for browsing packages on the WWW *)
  val browse: t -> string option

  (** Base URL for browsing OPAM repository source on the WWW *)
  val upstream: t -> string option

  (** Redirections. *)
  val redirect: t -> (string * filter option) list

end

(** {2 Substitution files} *)

(** {2 Urls for OPAM repositories} *)
module URL: sig

  include IO_FILE

  val create: repository_kind -> ?mirrors:address list -> address -> t

  (** URL address *)
  val url: t -> address

  val mirrors: t -> address list

  (** Backend kind (could be curl/rsync/git/darcs/hg at the moment) *)
  val kind: t -> repository_kind

  (** Archive checksum *)
  val checksum: t -> string option

  (** Constructor *)
  val with_checksum: t -> string -> t

end

(** {2 urls.txt file *} *)
module File_attributes: IO_FILE with type t = file_attribute_set

(** List of filenames *)
module Filenames: IO_FILE with type t = filename_set

(** Prefix of package directories *)
module Prefix: IO_FILE with type t = string name_map

(** Display statistics about file access. *)
val print_stats: unit -> unit


(**/**)

module type F = sig
  val internal : string
  type t
  val empty : t
  val of_channel : filename -> in_channel  -> t
  val of_string : filename -> string -> t
  val to_string : filename -> t -> string
end

module Make (F : F) : sig
  val write: filename -> F.t -> unit
  val read: filename -> F.t
  val safe_read: filename -> F.t
  val read_from_channel: ?filename:filename -> in_channel -> F.t
  val read_from_string: ?filename:filename -> string -> F.t
  val write_to_channel: out_channel -> F.t -> unit
end

module Syntax : sig
  type t = file
  val of_channel: filename -> in_channel -> t
  val to_string: t -> string
  val of_string: filename -> string -> t
  val check:
    ?allow_major:bool -> ?versioned:bool -> ?allow_extensions:bool ->
    t -> string list -> bool
end
