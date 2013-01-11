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

(** Common types used by other modules *)

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

(** Formula atoms *)
type atom = OpamFormula.atom

(** Formula over versionned packages *)
type formula = OpamFormula.t

(** AND formulat *)
type 'a conjunction = 'a OpamFormula.conjunction

(** OR formulat *)
type 'a disjunction = 'a OpamFormula.disjunction

(** {2 Repositories} *)

(** Repository names *)
type repository_name = OpamRepositoryName.t

(** Maps of repository names *)
type 'a repository_name_map = 'a OpamRepositoryName.Map.t

(** Repository kind *)
type repository_kind = [`http|`local|`git|`darcs]

(** Pretty-print repository kinds. *)
val string_of_repository_kind: [`http|`local|`git|`darcs] -> string

(** Parser of repository kinds. Raise an error if the kind is not valid. *)
val repository_kind_of_string: string -> [`http|`local|`git|`darcs]

(** Repository address *)
type address = dirname

(** Repository root *)
type repository_root = dirname

(** Repositories *)
type repository = {
  repo_name    : repository_name;
  repo_kind    : repository_kind;
  repo_address : address;
  repo_priority: int;
}

(** {2 Solver} *)

(** The solver answers a list of actions to perform *)
type 'a action =

  (** The package must be installed. The package could have been
      present or not, but if present, it is another version than the
      proposed solution. *)
  | To_change of 'a option * 'a

  (** The package must be deleted. *)
  | To_delete of 'a

  (** The package is already installed, but it must be recompiled.
      The second parameter is the collection of packages causing the
      reinstallation. An empty list means that the package has been
      modified upstream. *)
  | To_recompile of 'a

(** The possible causes of an action. *)
 type 'a cause =
  | Use of 'a list
  | Required_by of 'a list
  | Upstream_changes
  | Unknown

(** Extract a package from a package action. *)
val action_contents: 'a action -> 'a

(** Graph of package actions *)
module type ACTION_GRAPH = sig

  type package

  include Graph.Sig.I with type V.t = package action

  include Graph.Oper.S with type g = t

  (** Parallel iterator *)
  module Parallel: OpamParallel.SIG
    with type G.t = t
     and type G.V.t = V.t

  (** Topological traversal *)
  module Topological: sig
    val iter: (package action -> unit) -> t -> unit
    val fold: (package action -> 'a -> 'a) -> t -> 'a -> 'a
  end

  (** Solver solution *)
  type solution = {
    to_remove : package list;
    to_process: t;
    root_causes: (package * package cause) list;
  }

end

(** Signature of packages *)
module type PKG = sig

  include Graph.Sig.COMPARABLE

  (** Pretty printing of packages *)
  val to_string: t -> string

  (** Pretty-printing of package actions *)
  val string_of_action: ?causes:(t -> t cause) -> t action -> string

end

(** Functor to create a graph af actions *)
module MakeActionGraph (Pkg:PKG): ACTION_GRAPH with type package = Pkg.t

(** Package actions *)
module PackageAction: PKG with type t = package

(** Graph of OPAM packages *)
module PackageActionGraph: ACTION_GRAPH with type package = package

(** The type for solutions *)
type solution = PackageActionGraph.solution

(** Solver result *)
type solver_result =
  | Nothing_to_do
  | OK
  | Aborted
  | No_solution
  | Error of package action list

(** Solver result *)
type ('a, 'b) result =
  | Success of 'a
  | Conflicts of (unit -> 'b)

(** Solver request *)
type 'a request = {
  wish_install: 'a conjunction;
  wish_remove : 'a conjunction;
  wish_upgrade: 'a conjunction;
}

(** user request action *)
type user_action =
  | Install of name_set (** The 'root' packages to be installed *)
  | Upgrade of package_set (** The subset of packages to upgrade *)
  | Reinstall
  | Depends
  | Init of name_set (** The 'root' packages to be installed *)
  | Remove
  | Switch of name_set  (** The 'root' packages to be installed *)
  | Import of name_set  (** The 'root' packages to be installed *)

(** Solver universe *)
type universe = {
  u_installed: package_set;
  u_available: package_set;
  u_depends  : formula package_map;
  u_depopts  : formula package_map;
  u_conflicts: formula package_map;
  u_action   : user_action;
  u_installed_roots: package_set;
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
  | RAdd of repository_name * repository_kind * dirname * int option
  | RRm of repository_name
  | RPriority of repository_name * int

(** Pretty-print or remote args *)
val string_of_remote: remote -> string

(** Pinned packages options *)
type pin_option =
  | Version of version
  | Local of dirname
  | Git of address
  | Darcs of address
  | Unpin

(** Pinned packages *)
type pin = {
  pin_package: name;
  pin_option : pin_option;
}

(** Pretty-printing of pinned packages *)
val string_of_pin: pin -> string

(** Pin kind *)
type pin_kind = [`version|`git|`darcs|`local|`unpin]

(** Pretty-printing of pin kinds. *)
val pin_kind_of_string: string -> pin_kind

(** Parsing of pin kinds. Raise an error if it is not a valid value. *)
val string_of_pin_kind: pin_kind -> string

(** Read pin options args *)
val pin_option_of_string: ?kind:pin_kind -> string -> pin_option

val path_of_pin_option: pin_option -> string

val kind_of_pin_option: pin_option -> pin_kind

(** Configuration requests *)
type config_option = {
  conf_is_rec : bool;
  conf_is_byte: bool;
  conf_is_link: bool;
  conf_options: OpamVariable.Section.Full.t list;
}

type config =
  | CEnv of bool
  | CList of name list
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
  | FIdent of string
  | FOp of filter * symbol * filter
  | FAnd of filter * filter
  | FOr of filter * filter

(** A command argument *)
type simple_arg =
  | CString of string
  | CIdent of string

(** Command argument *)
type arg = simple_arg * filter option

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


(** {2 Switches} *)

(** Compiler switches *)
type switch = OpamSwitch.t

(** Set of compiler switches *)
type switch_set = OpamSwitch.Set.t

(** Map of compile switches *)
type 'a switch_map = 'a OpamSwitch.Map.t

(** {2 Misc} *)

(** The different kinds of locks *)
type lock =

  (** The function does not modify anything, but it needs the state
      not to change while it is running. *)
  | Read_lock of (unit -> unit)

  (** Take the global lock, all subsequent calls to OPAM are
      blocked. *)
  | Global_lock of (unit -> unit)

  (** Take the lock only for [OpamGlobals.current_switch] if it not
      [None], otherwise for the current lock. We do not pass the
      switch directly as argument as we might need to read some
      configuration file and we thus need to take the global loock for
      a short time. *)
  | Switch_lock of (unit -> unit)

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

(** Environement variables *)
type env = (string * string) list

(** Environment updates *)
type env_updates = (string * string * string) list
