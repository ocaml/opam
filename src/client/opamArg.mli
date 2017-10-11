(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Command-line argument parsers and helpers *)

open OpamTypes
open Cmdliner

(** {2 Helpers and argument constructors} *)

val mk_flag: ?section:string -> string list -> string -> bool Term.t

val mk_opt:
  ?section:string -> ?vopt:'a -> string list -> string -> string ->
  'a Arg.converter -> 'a -> 'a Term.t

val mk_opt_all:
  ?section:string -> ?vopt:'a -> ?default:'a list ->
  string list -> string -> string ->
  'a Arg.converter -> 'a list Term.t

(** {2 Flags} *)

(** --short *)
val print_short_flag: bool Term.t

(** --installed-root *)
val installed_roots_flag: bool Term.t

(** --shell *)
val shell_opt: shell Term.t

(** --dot-profile *)
val dot_profile_flag: filename option Term.t

(** --http/ --git/ --local *)
val repo_kind_flag: OpamUrl.backend option Term.t

(** --jobs *)
val jobs_flag: int option Term.t

(** package names *)
val name_list: name list Term.t

(** parameters *)
val param_list: string list Term.t

(** package list with optional constraints *)
val atom_list: OpamFormula.atom list Term.t

(** package list with optional constraints *)
val nonempty_atom_list: OpamFormula.atom list Term.t

val atom_or_local_list:
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] list
    Term.t

(** Generic argument list builder *)
val arg_list: string -> string -> 'a Arg.converter -> 'a list Term.t

(** Generic argument list builder *)
val nonempty_arg_list: string -> string -> 'a Arg.converter -> 'a list Term.t

(** {3 Global options} *)

(** Type for global options *)
type global_options = {
  debug_level: int option;
  verbose: int;
  quiet : bool;
  color : [ `Always | `Never | `Auto ] option;
  opt_switch : string option;
  yes : bool;
  strict : bool;
  opt_root : dirname option;
  git_version : bool;
  external_solver : string option;
  use_internal_solver : bool;
  cudf_file : string option;
  solver_preferences : string option;
  best_effort: bool;
  safe_mode : bool;
  json : string option;
  no_auto_upgrade : bool;
  working_dir : bool;
  ignore_pin_depends : bool;
}

(** Global options *)
val global_options: global_options Term.t

(** Apply global options *)
val apply_global_options: global_options -> unit


(** {3 Build options} *)

(** Abstract type for build options *)
type build_options

val build_option_section: string

(** Build options *)
val build_options: build_options Term.t

(** Applly build options *)
val apply_build_options: build_options -> unit


(** {3 Package listing and filtering options} *)

(** Man section name *)
val package_selection_section: string

(** Build a package selection filter *)
val package_selection: OpamListCommand.selector list Term.t

(** Man section name *)
val package_listing_section: string

(** Package selection filter based on the current state of packages (installed,
    available, etc.) *)
val package_listing:
  (force_all_versions:bool -> OpamListCommand.package_listing_format) Term.t

(** {3 Converters} *)

(** Repository name converter *)
val repository_name: repository_name Arg.converter

(** URL converter *)
val url: url Arg.converter

(** Filename converter *)
val filename: filename Arg.converter

(** Filename converter also accepting "-" for stdin/stdout *)
val existing_filename_or_dash: filename option Arg.converter

(** Dirnam converter *)
val dirname: dirname Arg.converter

val existing_filename_dirname_or_dash:
  OpamFilename.generic_file option Arg.converter

val positive_integer: int Arg.converter

(** Package name converter *)
val package_name: name Arg.converter

(** [name{.version}] (or [name=version]) *)
val package: (name * version option) Arg.converter

(** [name.version] (or [name=version]) *)
val package_with_version: package Arg.converter

(** [name{(.|=|!=|>|<|>=|<=)version}] converter*)
val atom: atom Arg.converter

(** Accepts [atom] but also (explicit) file and directory names *)
val atom_or_local:
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] Arg.converter

(** [var=value,...] argument *)
val variable_bindings: (OpamVariable.t * string) list Arg.converter

(** Warnings string ["+3..10-4"] *)
val warn_selector: (int * bool) list Arg.converter

type 'a default = [> `default of string] as 'a

(** Enumeration with a default command *)
val enum_with_default: (string * 'a default) list -> 'a Arg.converter

val opamlist_columns: OpamListCommand.output_format list Arg.converter

(** {2 Subcommands} *)

type 'a subcommand = string * 'a * string list * string
(** A subcommand [cmds, v, args, doc] is the subcommand [cmd], using
    the documentation [doc] and the list of documentation parameters
    [args]. If the subcommand is selected, return [v]. *)

type 'a subcommands = 'a subcommand list

val mk_subcommands: 'a subcommands -> 'a option Term.t * string list Term.t
(** [subcommands cmds] are the terms [cmd] and [params]. [cmd] parses
    which sub-commands in [cmds] is selected and [params] parses the
    remaining of the command-line parameters as a list of strings. *)

val mk_subcommands_with_default:
  'a default subcommands -> 'a option Term.t * string list Term.t
(** Same as {!mk_subcommand} but use the default value if no
    sub-command is selected. *)

val make_command_alias:
  'a Term.t * Term.info -> ?options:string -> string ->
  'a Term.t * Term.info
(** Create an alias for an existing command. [options] can be used to add extra
    options after the original command in the doc (eg like `unpin` is an alias
    for `pin remove`). *)

val bad_subcommand:
  'a default subcommands -> (string * 'a option * string list) -> 'b Term.ret
(** [bad_subcommand cmds cmd] is a command return value
    denoting a parsing error of sub-commands. *)

val mk_subdoc :
  ?defaults:(string * string) list -> 'a subcommands -> Manpage.block list
(** [mk_subdoc cmds] is the documentation block for [cmds]. *)

(** {2 Misc} *)


(** {2 Documentation} *)

val global_option_section: string
val help_sections: Manpage.block list
val term_info: string -> doc:string -> man:Manpage.block list -> Term.info
