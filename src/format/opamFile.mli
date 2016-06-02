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

(** Handles all OPAM file formats as record types and submodules, conversion to
    and from syntax *)

open OpamTypes

(** Functions to read and write OPAM configuration files in a typed way *)

(** Associate a type to a filename through a phantom type *)
type 'a t = private filename

type 'a typed_file = 'a t

val make: filename -> 'a t
val filename: 'a t -> filename
val to_string: 'a t -> string
val exists: 'a t -> bool

(** All Configuration files satisfies this signature *)
module type IO_FILE = sig

  (** File contents *)
  type t

  (** Empty file *)
  val empty: t

  (** Write some contents to a file *)
  val write: t typed_file -> t -> unit

  (** Read file contents. Raise an error if the file does not exist. *)
  val read: t typed_file -> t

  (** Returns [None] on non-existing file *)
  val read_opt: t typed_file -> t option

  (** Read file contents. Return [empty] if the file does not exist. *)
  val safe_read: t typed_file -> t

  val read_from_channel: ?filename:t typed_file -> in_channel -> t

  val read_from_string: ?filename:t typed_file -> string -> t

  val write_to_channel: ?filename:t typed_file -> out_channel -> t -> unit

  val write_to_string: ?filename:t typed_file -> t -> string

end

(** Lines of space-separated words. *)
module Lines: IO_FILE with type t = string list list

(** Configuration file: [$opam/config] *)
module Config: sig

  include IO_FILE

  (** Creation *)
  val create:
    switch list ->
    switch option ->
    repository_name list ->
    ?criteria:(OpamTypes.solver_criteria * string) list ->
    ?solver:(arg list) ->
    int ->
    ?download_tool:(arg list) ->
    int ->
    t

  (** OCaml switch updates *)
  val with_switch: switch -> t -> t
  val with_switch_opt: switch option -> t -> t

  val with_installed_switches: switch list -> t -> t

  (** Repository updates *)
  val with_repositories: repository_name list -> t -> t

  (** Update opam-version *)
  val with_opam_version: OpamVersion.t -> t -> t

  val with_criteria: (solver_criteria * string) list -> t -> t

  val with_solver: arg list -> t -> t

  val with_wrap_build: arg list -> t -> t
  val with_wrap_install: arg list -> t -> t
  val with_wrap_remove: arg list -> t -> t

  (** Return the OPAM version *)
  val opam_version: t  -> opam_version

  (** Return the list of repository *)
  val repositories: t  -> repository_name list

  (** Return the OCaml switch *)
  val switch: t -> switch option

  val installed_switches: t -> switch list

  (** Return the number of jobs *)
  val jobs: t -> int

  val dl_tool: t -> arg list option

  (** Return the number of download jobs *)
  val dl_jobs: t -> int

  val criteria: t -> (solver_criteria * string) list

  val solver: t -> arg list option

  val wrap_build: t -> arg list
  val wrap_install: t -> arg list
  val wrap_remove: t -> arg list

end

(** Package descriptions: [$opam/descr/] *)
module Descr: sig

  include IO_FILE

  val create: string -> t

  (** Create an abstract description file from a string *)
  val of_string: t typed_file -> string -> t

  (** Return the first line *)
  val synopsis: t -> string

  (** Return the body *)
  val body: t -> string

  (** Return the full description *)
  val full: t -> string

end

(** {2 Urls for OPAM repositories} *)
module URL: sig

  include IO_FILE

  val create: ?mirrors:url list -> url -> t

  (** URL address *)
  val url: t -> url

  val mirrors: t -> url list

  (** Archive checksum *)
  val checksum: t -> string option

  (** Constructor *)
  val with_checksum: string -> t -> t

end

(** OPAM files *)
module OPAM: sig

  type t = private {
    opam_version: opam_version;

    (* Package ident *)
    name       : name option;
    version    : version option;

    (* Relationships; solver and availability info *)
    depends    : filtered_formula;
    depopts    : filtered_formula;
    conflicts  : formula;
    available  : filter;
    flags      : package_flag list;
    env        : env_update list;

    (* Build instructions *)
    build      : command list;
    build_test : command list;
    build_doc  : command list;
    install    : command list;
    remove     : command list;

    (* Auxiliary data affecting the build *)
    substs     : basename list;
    patches    : (basename * filter option) list;
    build_env  : env_update list;
    features   : (OpamVariable.t * string * filter) list;
    extra_sources: (url * string * basename option) list;

    (* User-facing data used by opam *)
    messages   : (string * filter option) list;
    post_messages: (string * filter option) list;
    depexts    : tags option;
    libraries  : (string * filter option) list;
    syntax     : (string * filter option) list;
    dev_repo   : url option;

    (* Package database details *)
    maintainer : string list;
    author     : string list;
    license    : string list;
    tags       : string list;
    homepage   : string list;
    doc        : string list;
    bug_reports: string list;

    (* Extension fields (x-foo: "bar") *)
    extensions : (pos * value) OpamStd.String.Map.t;

    (* Extra sections *)
    url        : URL.t option;
    descr      : Descr.t option;

    (* Related metadata directory (not an actual field of the file)
       This can be used to locate e.g. the files/ overlays *)
    metadata_dir: dirname option;

    (* Names and hashes of the files below files/ *)
    extra_files: (OpamFilename.Base.t * string) list option;


    (* Deprecated, for compat and proper linting *)
    ocaml_version: (OpamFormula.relop * string) OpamFormula.formula option;
    os         : (bool * string) generic_formula;
  }

  include IO_FILE with type t := t

  val empty: t

  (** Create an opam file *)
  val create: package -> t

  (** Returns the opam value (including url, descr) with all non-effective (i.e.
      user-directed information that doesn't change opam's view on the package)
      fields set to their empty values. Useful for comparisons. *)
  val effective_part: t -> t

  (** Returns true if the effective parts of the two package definitions are
      equal *)
  val effectively_equal: t -> t -> bool

  (** Compares two package definitions, ignoring the virtual fields bound to
      file location ([metadata_dir]...) *)
  val equal: t -> t -> bool

  (** Get OPAM version. *)
  val opam_version: t -> opam_version

  (** Package name *)
  val name: t -> name
  val name_opt: t -> name option

  (** Package version *)
  val version: t -> version
  val version_opt: t -> version option

  (** The informations in both the name and version fields, as a package *)
  val package: t -> package

  (** Availability formula (OS + compiler constraints) *)
  val available: t -> filter

  (** Package maintainer(s) *)
  val maintainer: t -> string list

  (** File substitutions *)
  val substs: t -> basename list

  (** List of environment variables to set-up for the build *)
  val build_env: t -> env_update list

  (** List of command to run for building the package *)
  val build: t -> command list

  (** List of command to run for installing the package *)
  val install: t -> command list

  (** List of command to run for removing the package *)
  val remove: t -> command list

  (** Package dependencies *)
  val depends: t -> filtered_formula

  (** Optional dependencies *)
  val depopts: t -> filtered_formula

  (** External dependencies *)
  val depexts: t -> tags option

  val extra_sources: t -> (url * string * basename option) list

  (** All extended "x-" fields as a map *)
  val extensions: t -> value OpamStd.String.Map.t

  (** Parse a single extended field (reports proper file position) *)
  val extended: t -> string -> (value -> 'a) -> 'a option

  val with_messages: (string * filter option) list -> t -> t

  val with_post_messages: (string * filter option) list -> t -> t

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

  (** The environment variables that this package exports *)
  val env: t -> env_update list

  val descr: t -> Descr.t option

  val url: t -> URL.t option

  val get_url: t -> url option

  (** Related metadata directory (not an actual field of the file, linked to the
      file location).
      This can be used to locate e.g. the files/ overlays *)
  val metadata_dir: t -> dirname option

  (** Names and hashes of the files below files/ *)
  val extra_files: t -> (OpamFilename.Base.t * string) list option

  (** Looks up the extra files, and returns their full paths, relative path to
      the package source, and hash. Doesn't check the hashes. *)
  val get_extra_files: t -> (filename * basename * string) list

  (** Sets the opam version *)
  val with_opam_version: opam_version -> t -> t

  (** The package source repository address *)
  val dev_repo: t -> url option

  (** construct as [name] *)
  val with_name: name -> t -> t
  val with_name_opt: name option -> t -> t

  (** construct as [version] *)
  val with_version: version -> t -> t
  val with_version_opt: version option -> t -> t

  (** Construct as [depends] *)
  val with_depends: filtered_formula -> t -> t

  (** Construct as [depopts] *)
  val with_depopts: filtered_formula -> t -> t

  val with_conflicts: formula -> t -> t

  val with_features: (OpamVariable.t * string * filter) list -> t -> t

  (** Construct as [build] *)
  val with_build: command list -> t -> t

  val with_install: command list -> t -> t

  (** Construct as [remove] *)
  val with_remove: command list -> t -> t

  (** Construct as [libraries] *)
  val with_libraries: (string * filter option) list -> t -> t

  (** Replace the [syntax] field of the given OPAM file. *)
  val with_syntax: (string * filter option) list -> t -> t

  (** Construct as [substs] *)
  val with_substs: basename list -> t -> t

  val with_available: filter -> t -> t

  (** Construct as [maintainer] *)
  val with_maintainer: string list -> t -> t

  val with_author: string list -> t -> t

  val with_homepage: string list -> t -> t

  val with_license: string list -> t -> t

  (** Construct as [patches] *)
  val with_patches: (basename * filter option) list -> t -> t

  (** Construct using [bug_reports] *)
  val with_bug_reports: string list -> t -> t

  (** Construct using [depexts] *)
  val with_depexts: tags -> t -> t

  val with_flags: package_flag list -> t -> t

  val with_env: env_update list -> t -> t

  val with_dev_repo: url -> t -> t

  val with_extra_sources: (url * string * basename option) list -> t -> t

  val with_extensions: value OpamStd.String.Map.t -> t -> t

  val add_extension: t -> string -> value -> t

  val with_descr: Descr.t -> t -> t
  val with_descr_opt: Descr.t option -> t -> t
  val with_url: URL.t -> t -> t
  val with_url_opt: URL.t option -> t -> t

  val with_metadata_dir: dirname option -> t -> t

  val with_extra_files: (OpamFilename.Base.t * string) list -> t -> t
  val with_extra_files_opt: (OpamFilename.Base.t * string) list option -> t -> t

  (** Prints to a string, while keeping the format of the original file as much
      as possible *)
  val to_string_with_preserved_format:
    ?format_from:(t typed_file) -> t typed_file -> t -> string

  (** Writes an opam file, but preserving the existing formatting as much as
      possible. The format is taken from the file that is being overwritten
      unless [format_from] is specified. *)
  val write_with_preserved_format:
    ?format_from:(t typed_file) -> t typed_file -> t -> unit

  (** Low-level values used for linting and similar processing *)

  (** Allow 'flag:xxx' tags as flags, for compat *)
  val flag_of_tag: string -> package_flag option

  val fields: (t, value) OpamFormat.Pp.I.fields_def

  val sections: (t, opamfile_item list) OpamFormat.Pp.I.fields_def

  (** Doesn't handle package name encoded in directory name *)
  val pp_raw_fields: strict:bool -> (opamfile_item list, t) OpamFormat.Pp.t

  (** Returns the raw print-AST contents of the file *)
  val contents: ?filename:'a typed_file -> t -> opamfile

  (** Returns all fields of the file as print-AST. Fields within sections are
      accessed through dot-separated paths (e.g. [url.checksum]) *)
  val to_list: ?filename:'a typed_file -> t -> (string * value) list

end

(** Compiler aliases: [$opam/aliases]. Deprecated, used only for migration *)
module Aliases: IO_FILE with type t = string switch_map

(** Switch state file as table, also used for import/export. This includes
    compiler and root packages information, as well as pinned packages and their
    target (but not their local metadata). *)
module State: sig
  type t = switch_selections
  include IO_FILE with type t := t
end

(** A newer format for switch state, using the opam file syntax rather than a
    table. This is more readable and extensible. *)
module SwitchSelections: sig
  type t = switch_selections
  include IO_FILE with type t := t
end

(** An extended version of SwitchSelections that can include full opam files as
    [package "name" {}] sections, for storing overlays *)
module SwitchExport: sig
  type t = {
    selections: switch_selections;
    overlays: OPAM.t OpamPackage.Name.Map.t;
  }
  include IO_FILE with type t := t
end

(** A simple list of packages and versions: (used for the older
    [$opam/$switch/{installed,installed_roots}], still needed to
    migrate from 1.2 repository, and for reinstall) *)
module PkgList: IO_FILE with type t = package_set

(** Cached environment updates (<switch>/environment) *)
module Environment: IO_FILE with type t = env_update list

(** Compiler version [$opam/compilers/]. Deprecated, only used to upgrade old
    data *)
module Comp: sig

  include IO_FILE

  type compiler = string
  type compiler_version = string

  (** Create a pre-installed compiler description file *)
  val create_preinstalled:
    compiler -> compiler_version -> name list -> env_update list -> t

  (** Is it a pre-installed compiler description file *)
  val preinstalled: t -> bool

  (** Get OPAM version *)
  val opam_version: t -> opam_version

  (** Return the compiler name *)
  val name: t -> compiler

  (** Return the compiler version *)
  val version: t -> compiler_version

  (** Return the url of the compiler *)
  val src: t -> url option

  (** Return the list of patches to apply *)
  val patches: t -> url list

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
  val env: t -> env_update list

  val tags: t -> string list

  val with_src: url option -> t -> t
  val with_patches: url list -> t -> t
  val with_configure: string list -> t -> t
  val with_make: string list -> t -> t
  val with_build: command list -> t -> t
  val with_packages: formula -> t -> t

  (** Converts a compiler definition to package metadata. For compat. *)
  val to_package: name -> t -> Descr.t option -> OPAM.t

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

(** .changes files, bound to the OpamDirTrack module *)
module Changes: sig
  type t = OpamDirTrack.t
  include IO_FILE with type t := t
end

(** .config files *)
module Dot_config: sig

  include IO_FILE

  (** Create a new .config file (containing only variables) *)
  val create: (variable * variable_contents) list -> t

  (** Dependency towards file-system paths and their hashes *)
  val file_depends: t -> (filename * string) list

  val with_file_depends: (filename * string) list -> t -> t

  (** Sets all bindings in the file *)
  val with_vars: (variable * variable_contents) list -> t -> t

  (** Top-level variables *)
  val variable: t -> variable  -> variable_contents option

  (** The list of top-level variables *)
  val variables: t -> variable list

  (** Lists all the variable bindings in the file *)
  val bindings: t -> (variable * variable_contents) list

  (** Sets the given variable, overriding any previous definition.
      With [None], unsets the variable*)
  val set: t -> variable -> variable_contents option -> t

end

(** {2 Repository files} *)

(** Association between package names and repositories *)
module Package_index: IO_FILE with
  type t = (repository_name * string option) package_map

(** Repository config: [$opam/repo/$repo/config]. Deprecated, for migration
    only *)
module Repo_config_legacy: IO_FILE with type t = repository

module Repos_config: IO_FILE with type t = url option OpamRepositoryName.Map.t

(** Pinned package files (only used for migration from 1.2, the inclusive State
    module is now used instead) *)
module Pinned_legacy: sig
  type pin_option =
    | Version of version
    | Source of url
  include IO_FILE with type t = pin_option name_map
end

(** Repository metadata *)
module Repo: sig

  include IO_FILE

  val create:
    ?browse:string -> ?upstream:string -> ?opam_version:OpamVersion.t ->
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

(** {2 urls.txt file *} *)
module File_attributes: IO_FILE with type t = file_attribute_set

module Stats: sig

  (** Display statistics about file access. *)
  val print: unit -> unit

end

(** Helper module for manipulation of the raw syntax ([opamfile]) format.
    (the specific file handling modules are derived from this one) *)
module Syntax : sig

  val pp_channel:
    'a typed_file -> in_channel -> out_channel ->
    (unit, opamfile) OpamFormat.Pp.t

  val of_channel: 'a typed_file -> in_channel  -> opamfile
  val to_channel: 'a typed_file -> out_channel -> opamfile -> unit
  val of_string: 'a typed_file -> string -> opamfile
  val to_string: 'a typed_file -> opamfile -> string
  val to_string_with_preserved_format:
    'a typed_file -> ?format_from:'a typed_file ->
    empty:'a ->
    ?sections: ('a, opamfile_item list) OpamFormat.Pp.I.fields_def ->
    fields:('a, value) OpamFormat.Pp.I.fields_def ->
    (opamfile, filename * 'a) OpamFormat.Pp.t ->
    'a -> string

end

(**/**)

module type SyntaxFileArg = sig
  val internal: string
  type t
  val empty: t
  val pp: (opamfile, filename * t) OpamFormat.Pp.t
end

module SyntaxFile(X: SyntaxFileArg) : IO_FILE with type t := X.t

module type LineFileArg = sig
  val internal: string
  type t
  val empty: t
  val pp: (string list list, t) OpamFormat.Pp.t
end

module LineFile (X: LineFileArg) : IO_FILE with type t := X.t
