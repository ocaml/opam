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

(** OPAM types *)

(** {2 Filenames} *)

(** Basenames *)
type basename = OpamFilename.Base.t

(** Directory names *)
type dirname = OpamFilename.Dir.t

(** Filenames *)
type filename = OpamFilename.t

(** Set of files *)
type filename_set = OpamFilename.Set.t

(** Map of files *)
type 'a filename_map = 'a OpamFilename.Map.t

(** Generalized file type *)
type generic_file =
  | D of dirname
  | F of filename

(** Download result *)
type 'a download =
  | Up_to_date of 'a
  | Not_available
  | Result of 'a

(** {2 Packages} *)

(** Packages are ([name] * [version]) tuple *)
type package = OpamPackage.t

(** Set of packages *)
type package_set = OpamPackage.Set.t

(** Map of packages *)
type 'a package_map = 'a OpamPackage.Map.t

(** Package names *)
type name = OpamPackage.Name.t

(** Set of package names *)
type name_set = OpamPackage.Name.Set.t

(** Map of package names *)
type 'a name_map = 'a OpamPackage.Name.Map.t

(** Package versions *)
type version = OpamPackage.Version.t

(** Set of package versions *)
type version_set = OpamPackage.Version.Set.t

(** {2 Compilers} *)

(** Compiler names *)
type compiler = OpamCompiler.t

(** Set of compiler names *)
type compiler_set = OpamCompiler.Set.t

(** Compiler versions *)
type compiler_version = OpamCompiler.Version.t

(** OPAM versions *)
type opam_version = OpamVersion.t

(** Compiler constraints *)
type compiler_constraint = OpamCompiler.Version.constr

(** {2 Variables} *)

(** Variables *)
type variable = OpamVariable.t

(** Fully qualified variables (ie. with the name of
    sections/sub-sections they appear in) *)
type full_variable = OpamVariable.Full.t

(** Section names *)
type section = OpamVariable.Section.t

(** Fully qualified section names *)
type full_section = OpamVariable.Section.Full.t

(** Content of user-defined variables *)
type variable_contents = OpamVariable.variable_contents =
  | B of bool
  | S of string

(** Content of [pp] variables *)
type ppflag =
  | Camlp4 of string list
  | Cmd of string list

(** {2 Formulas} *)

(** A generic formula *)
type 'a generic_formula = 'a OpamFormula.formula =
  | Empty
  | Atom of 'a
  | Block of 'a generic_formula
  | And of 'a generic_formula * 'a generic_formula
  | Or of 'a generic_formula * 'a generic_formula

(** Formula over versionned packages *)
type formula = OpamFormula.t

(** AND formulat *)
type conjunction = OpamFormula.conjunction

(** {2 Repositories} *)

(** Repository names *)
type repository_name = OpamRepositoryName.t

(** Maps of repository names *)
type 'a repository_name_map = 'a OpamRepositoryName.Map.t

(** Repositories *)
type repository = {
  repo_name    : repository_name;
  repo_kind    : string;
  repo_address : dirname;
  repo_priority: int;
}

(** {2 Solver} *)

(** The solver answer is a list of actions *)
type action =

  (** The package must be installed. The package could have been
      present or not, but if present, it is another version than the
      proposed solution. *)
  | To_change of package option * package

  (** The package must be deleted. *)
  | To_delete of package

  (** The package is already installed, but it must be recompiled. *)
  | To_recompile of package

(** Solver result *)
type ('a, 'b) result =
  | Success of 'a
  | Conflicts of (unit -> 'b)


(** Solver request *)
type request = {
  wish_install: conjunction;
  wish_remove : conjunction;
  wish_upgrade: conjunction;
}

(** Solver solution *)
type 'a solution = {
  to_remove: package list;
  to_add   : 'a;
}

(** {2 Command line arguments} *)

(** Upload arguments *)
type upload = {
  upl_opam   : filename;
  upl_descr  : filename;
  upl_archive: filename;
}

(** Pretty-print *)
val string_of_upload: upload -> string

(** Remote arguments *)
type remote =
  | RList
  | RAdd of repository_name * string * dirname * int option
  | RRm of repository_name
  | RPriority of repository_name * int

(** Pretty-print or remote args *)
val string_of_remote: remote -> string

(** Pinned packages options *)
type pin_option =
  | Version of version
  | Path of dirname
  | Git of dirname
  | Unpin

(** Pinned packages *)
type pin = {
  pin_package: name;
  pin_arg: pin_option;
}

(** Pretty-printing of pinned packages *)
val string_of_pin: pin -> string

(** Read pin options args *)
val pin_option_of_string: ?kind:string -> string -> pin_option

val path_of_pin_option: pin_option -> string

val kind_of_pin_option: pin_option -> string

(** Configuration requests *)
type config_option = {
  conf_is_rec : bool;
  conf_is_byte: bool;
  conf_is_link: bool;
  conf_options: OpamVariable.Section.Full.t list;
}

type config =
  | CEnv
  | CList
  | CVariable of full_variable
  | CIncludes of bool * (name list)
  | CCompil   of config_option
  | CSubst    of basename list

(** {2 Filtered commands} *)

(** Symbols *)
type symbol =
  | Eq | Neq | Le | Ge | Lt | Gt

(** Filter *)
type filter =
  | FBool of bool
  | FString of string
  | FOp of filter * symbol * filter
  | FAnd of filter * filter
  | FOr of filter * filter

(** Command argument *)
type arg = string * filter option

(** Command *)
type command = arg list * filter option

(** {2 Untyped generic file format} *)

(** Base values *)
type value =
  | Bool of bool
  | Int of int
  | String of string
  | Symbol of string
  | Ident of string
  | List of value list
  | Group of value list
  | Option of value * value list

(** A file section *)
type file_section = {
  section_kind  : string;
  section_name  : string;
  section_items : file_item list;
}

(** A file is composed of sections and variable definitions *)
and file_item =
  | Section of file_section
  | Variable of string * value

(** A file is a list of items and the filename *)
type file = {
  file_contents: file_item list;
  file_name    : string;
}


(** {2 Aliases} *)

(** Compiler aliases *)
type alias = OpamAlias.t

(** Set of compiler aliases *)
type alias_set = OpamAlias.Set.t

(** Map of compile aliases *)
type 'a alias_map = 'a OpamAlias.Map.t

(** {2 Misc} *)

(** A line in {i urls.tx} *)
type file_attribute = OpamFilename.Attribute.t

(** All the lines in {i urls.txt} *)
type file_attribute_set = OpamFilename.Attribute.Set.t

(** Optional contents *)
type 'a optional = {
  c       : 'a;   (** Contents *)
  optional: bool; (** Is the contents optional *)
}

(** Upgrade statistics *)
type stats = {
  s_install  : int;
  s_reinstall: int;
  s_upgrade  : int;
  s_downgrade: int;
  s_remove   : int;
}
type env = {
  add_to_env : (string * string) list;
  add_to_path: dirname;
  new_env    : (string * string) list;
}
