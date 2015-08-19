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

(** OPAM command-line arguments *)

open OpamTypes
open Cmdliner

(** {2 Helpers and argument constructors} *)

val mk_flag: ?section:string -> string list -> string -> bool Cmdliner.Term.t

val mk_opt:
  ?section:string -> ?vopt:'a -> string list -> string -> string ->
  'a Cmdliner.Arg.converter -> 'a -> 'a Cmdliner.Term.t

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
val repo_kind_flag: repository_kind option Term.t

(** --jobs *)
val jobs_flag: int option Term.t

(** patterns *)
val pattern_list: string list Term.t

(** package names *)
val name_list: name list Term.t

(** parameters *)
val param_list: string list Term.t

(** package list with optional constraints *)
val atom_list: OpamFormula.atom list Cmdliner.Term.t

(** package list with optional constraints *)
val nonempty_atom_list: OpamFormula.atom list Cmdliner.Term.t

(** Generic argument list builder *)
val arg_list:
  string -> string -> 'a Cmdliner.Arg.converter -> 'a list Cmdliner.Term.t

(** Generic argument list builder *)
val nonempty_arg_list:
  string -> string -> 'a Cmdliner.Arg.converter -> 'a list Cmdliner.Term.t

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
  no_base_packages: bool;
  git_version : bool;
  external_solver : string option;
  use_internal_solver : bool;
  cudf_file : string option;
  solver_preferences : string option;
  safe_mode : bool;
  json : string option;
}

(** Global options *)
val global_options: global_options Term.t

val create_global_options:
  bool -> bool -> int option -> 'a list -> bool -> [ `Always | `Auto | `Never ] option ->
  string option -> bool -> bool -> OpamTypes.dirname option -> bool -> string option ->
  bool -> string option -> string option -> bool -> string option
  -> global_options

(** Apply global options *)
val apply_global_options: global_options -> unit

(** {3 Build options} *)

(** Abstract type for build options *)
type build_options

(** Build options *)
val build_options: build_options Term.t

(** Applly build options *)
val apply_build_options: build_options -> unit

(** {3 Converters} *)

(** Repository name converter *)
val repository_name: repository_name Arg.converter

(** Repository address converter *)
val address: address Arg.converter

(** Filename converter *)
val filename: filename Arg.converter

(** Dirnam converter *)
val dirname: dirname Arg.converter

(** Compiler converter *)
val compiler: compiler Arg.converter

(** Package name converter *)
val package_name: name Arg.converter

(** [name{.version}] *)
val package: (name * version option) Cmdliner.Arg.converter

(** [name{(.|=|!=|>|<|>=|<=)version}] converter*)
val atom: atom Cmdliner.Arg.converter

type 'a default = [> `default of string] as 'a

(** Enumeration with a default command *)
val enum_with_default:
  (string * 'a default) list -> 'a Arg.converter

(** {2 Subcommands} *)

val mk_subcommands:
  (string list * 'a * 'b * 'c) list ->
  'a option Cmdliner.Term.t * string list Cmdliner.Term.t

val mk_subcommands_with_default:
  (string list * 'a default * 'b * 'c) list ->
  'a option Cmdliner.Term.t * string list Cmdliner.Term.t

val bad_subcommand:
  string ->
  (string list * 'a default * string list * 'b) list ->
  'a option ->
  'c list ->
  [> `Error of bool * string ]

(** {2 Misc} *)


(** {2 Documentation} *)

val global_option_section: string
val help_sections: Manpage.block list
val term_info:
  string -> doc:string -> man:Cmdliner.Manpage.block list ->
  Cmdliner.Term.info

val mk_subdoc :
  ?defaults:(string * string) list ->
  (string list * 'a * string list * string) list ->
  Manpage.block list
