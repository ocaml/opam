(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
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

(** {3 CLI versioning} *)

(* Type of the validity of a flag *)
type validity

val cli2_0: OpamCLIVersion.t
val cli2_1: OpamCLIVersion.t
val cli2_2: OpamCLIVersion.t

(* [cli_from ?experimental since] validity flag since [since], and no removal
   version. If [experimental] is true, it is marked as is (warning and
   documentation update). *)
val cli_from: ?experimental:bool -> OpamCLIVersion.t -> validity

(* [cli_between since until ?default ?replaced] a validity flag introduced in
   [since], removed in [until], [replaced] is the replacement helper message.
   If [option] is [`default], flag validity is not strict, deprecated flags do
   not raise error but only warnings.
   If [option] is [`experimental], it is marked as is (specific messages).  *)
val cli_between:
  ?option:[`experimental | `default] -> OpamCLIVersion.t ->
  ?replaced:string -> OpamCLIVersion.t -> validity

(* Original cli options : [validity] from 2.0 and no removal.
   No new options should use this. *)
val cli_original: validity

(** {3 Common helphers} *)

(* Helpers function takes [cli] as first argument, which is the requested cli
   (via [--cli] or [OPAMCLI]), and a [validity] argument, the validity of the
   flag.
   All arguments must be defined using [mk_*] function, they embed cli
   validation. *)

val mk_flag:
  cli:OpamCLIVersion.Sourced.t -> validity ->
  ?section:string -> string list -> string ->
  bool Term.t

(* Deprecate and replace a [flags]. Constructs a [vflag] with the deprecated
   option and the new one *)
val mk_flag_replaced:
  cli:OpamCLIVersion.Sourced.t -> ?section:string -> (validity * string list) list ->
  string -> bool Term.t

val mk_opt:
  cli:OpamCLIVersion.Sourced.t -> validity ->
  ?section:string -> ?vopt:'a -> string list -> string -> string ->
  'a Arg.conv -> 'a ->
  'a Term.t

val mk_opt_all:
  cli:OpamCLIVersion.Sourced.t -> validity ->
  ?section:string -> ?vopt:'a -> ?default:'a list ->
  string list -> string -> string ->
  'a Arg.conv -> 'a list Term.t

val mk_vflag:
  cli:OpamCLIVersion.Sourced.t ->
  ?section:string -> 'a -> (validity * 'a * string list * string) list ->
  'a Term.t

val mk_vflag_all:
  cli:OpamCLIVersion.Sourced.t ->
  ?section:string -> ?default:'a list ->
  (validity * 'a * string list * string) list ->
  'a list Term.t

(* Escaped Windows directory separator. To use instead of [Filename.dir_sep] for
   manpage strings *)
val dir_sep: string

(* Escape Windows path *)
val escape_path: string -> string

(** {2 Flags} *)

(** --short *)
val print_short_flag:
  ?section:string -> OpamCLIVersion.Sourced.t -> validity -> bool Term.t

(** --shell *)
val shell_opt:
  ?section:string -> OpamCLIVersion.Sourced.t -> validity -> shell option Term.t

(** --dot-profile *)
val dot_profile_flag:
  ?section:string -> OpamCLIVersion.Sourced.t -> validity -> filename option Term.t

(** --http/ --git/ --local *)
val repo_kind_flag:
  ?section:string -> OpamCLIVersion.Sourced.t -> validity -> OpamUrl.backend option Term.t

(** --jobs *)
val jobs_flag:
  ?section:string -> OpamCLIVersion.Sourced.t -> validity -> int option Term.t

(** --formula *)
val formula_flag:
  ?section:string -> OpamCLIVersion.Sourced.t -> formula Term.t

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

val atom_or_dir_list:
  [ `Atom of atom | `Dirname of dirname ] list Term.t

(** Generic argument list builder *)
val arg_list: string -> string -> 'a Arg.conv -> 'a list Term.t

(** Generic argument list builder *)
val nonempty_arg_list: string -> string -> 'a Arg.conv -> 'a list Term.t

(** Confirmation level enum *)
val confirm_enum: (validity * string * OpamStd.Config.answer) list

(** {3 Global options} *)

(** Type for global options *)
type global_options = {
  debug_level: int option;
  verbose: int;
  quiet : bool;
  color : OpamStd.Config.when_ option;
  opt_switch : string option;
  confirm_level : OpamStd.Config.answer option;
  yes: bool option;
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
  cli : OpamCLIVersion.t;
}

(** Global options *)
val global_options: OpamCLIVersion.Sourced.t -> global_options Term.t

(** Apply global options *)
val apply_global_options: OpamCLIVersion.Sourced.t -> global_options -> unit


(** {3 Build options} *)

(** Abstract type for build options *)
type build_options

val man_build_option_section: Manpage.block list

(** Build options *)
val build_options:
  OpamCLIVersion.Sourced.t -> build_options Term.t

(** Install and reinstall options *)
val assume_built:
  ?section:string -> OpamCLIVersion.Sourced.t -> bool Term.t

(* Options common to all path based/related commands, e.g. (un)pin, upgrade,
   remove, (re)install *)
val recurse: ?section:string -> OpamCLIVersion.Sourced.t -> bool Term.t
val subpath: ?section:string -> OpamCLIVersion.Sourced.t -> subpath option Term.t

(** Applly build options *)
val apply_build_options: OpamCLIVersion.Sourced.t -> build_options -> unit

(** Lock options *)
val locked: ?section:string -> OpamCLIVersion.Sourced.t -> bool Term.t
val lock_suffix: ?section:string -> OpamCLIVersion.Sourced.t -> string Term.t

(** {3 Package listing and filtering options} *)

(** Man section name *)
val package_selection_section: string

(** Build a package selection filter *)
val package_selection: OpamCLIVersion.Sourced.t -> OpamListCommand.selector list Term.t

(** Man section name *)
val package_listing_section: string

(** Package selection filter based on the current state of packages (installed,
    available, etc.) *)
val package_listing:
  OpamCLIVersion.Sourced.t ->
  (force_all_versions:bool -> OpamListCommand.package_listing_format) Term.t

(** {3 Converters} *)

(** Repository name converter *)
val repository_name: repository_name Arg.conv

(** URL converter *)
val url: url Arg.conv

(** Filename converter *)
val filename: filename Arg.conv

(** Filename converter also accepting "-" for stdin/stdout *)
val existing_filename_or_dash: filename option Arg.conv

(** Dirnam converter *)
val dirname: dirname Arg.conv

val existing_filename_dirname_or_dash:
  OpamFilename.generic_file option Arg.conv

val positive_integer: int Arg.conv

(** Package name converter *)
val package_name: name Arg.conv

(** Package version converter *)
val package_version: version Arg.conv

(** [name{.version}] (or [name=version]) *)
val package: (name * version option) Arg.conv

(** [name.version] (or [name=version]) *)
val package_with_version: package Arg.conv

(** [name{(.|=|!=|>|<|>=|<=)version}] converter*)
val atom: atom Arg.conv

(** Accepts [atom] but also (explicit) file and directory names *)
val atom_or_local:
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] Arg.conv

val atom_or_dir:
  [ `Atom of atom | `Dirname of dirname ] Arg.conv

(** Formula, in the same format as [depends:] in opam files *)
val dep_formula: formula Arg.conv

(** [var=value,...] argument *)
val variable_bindings: (OpamVariable.t * string) list Arg.conv

(** Warnings string ["+3..10-4"] *)
val warn_selector: (int * bool) list Arg.conv

val opamlist_columns: OpamListCommand.output_format list Arg.conv

(** {2 Subcommands} *)

type 'a subcommand = validity * string * 'a * string list * string
(** A subcommand [cmds, v, args, doc] is the subcommand [cmd], using
    the documentation [doc] and the list of documentation parameters
    [args]. If the subcommand is selected, return [v] value. *)

type 'a subcommands = 'a subcommand list

val mk_subcommands:
  cli:OpamCLIVersion.Sourced.t ->
  'a subcommands -> 'a option Term.t * string list Term.t
(** [subcommands cmds] are the terms [cmd] and [params]. [cmd] parses
    which sub-commands in [cmds] is selected and [params] parses the
    remaining of the command-line parameters as a list of strings. *)

type 'a default = [> `default of string] as 'a

(* unused
(** Enumeration with a default command *)
val enum_with_default: (string * 'a default) list -> 'a Arg.converter
*)

val mk_subcommands_with_default:
  cli:OpamCLIVersion.Sourced.t ->
  'a default subcommands -> 'a option Term.t * string list Term.t
(** Same as {!mk_subcommand} but use the default value if no
    sub-command is selected. *)

val bad_subcommand:
  cli:OpamCLIVersion.Sourced.t ->
  'a default subcommands -> (string * 'a option * string list) -> 'b Term.ret
(** [bad_subcommand cmds cmd] is a command return value
    denoting a parsing error of sub-commands. *)

val mk_subdoc :
  cli:OpamCLIVersion.Sourced.t ->
  ?defaults:(string * string) list ->
  ?extra_defaults:(validity * string * string) list ->
  'a subcommands -> Manpage.block list
(** [mk_subdoc cmds] is the documentation block for [cmds]. *)

val make_command_alias:
  cli:OpamCLIVersion.Sourced.t ->
  'a Term.t * Cmd.info -> ?options:string -> string ->
  'a Term.t * Cmd.info
(** Create an alias for an existing command. [options] can be used to add extra
    options after the original command in the doc (eg like `unpin` is an alias
    for `pin remove`). *)

(** {2 Commands} *)

(* All commands must be defined using [mk_command] and [mk_command_ret] for
   prior cli validation. *)

type command = unit Term.t * Cmd.info

val mk_command:
  cli:OpamCLIVersion.Sourced.t -> validity -> string -> doc:string ->
  man:Manpage.block list -> (unit -> unit) Term.t -> command
  (* [mk_command cli validity name doc man term] is the command [name] with its
     [doc] and [man], and using [term]. Its [validity] is checked at runtime
     against requested [cli], updates its documentation and errors if not
     valid. *)

val mk_command_ret:
  cli:OpamCLIVersion.Sourced.t -> validity -> string -> doc:string ->
  man:Manpage.block list -> (unit -> unit Term.ret) Term.t -> command
  (* Same as {!mk_command} but [term] returns a [Cmdliner.Term.ret] *)

(** {2 Documentation} *)

val global_option_section: string
val help_sections: OpamCLIVersion.Sourced.t -> Manpage.block list


(** {2 Environment variables} *)

val preinit_opam_env_variables: unit -> unit
val init_opam_env_variabes: OpamCLIVersion.Sourced.t -> unit
val scrubbed_environment_variables: string list
