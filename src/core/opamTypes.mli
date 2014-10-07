(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2014 OCamlPro                                        *)
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

(** Common types used by other modules *)

(** {2 Error and continuation handling} *)
type 'a success = [ `Successful of 'a ]
type 'a error = [
  | `Error of 'a
  | `Exception of exn
]
type ('a,'b) status = [ 'a success | 'b error ]

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
type generic_file = OpamFilename.generic_file =
  | D of dirname
  | F of filename

(** Download result *)
type 'a download =
  | Up_to_date of 'a
  | Not_available of string
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

(** Maps of compiler names *)
type 'a compiler_map = 'a OpamCompiler.Map.t

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

(** Content of user-defined variables *)
type variable_contents = OpamVariable.variable_contents =
  | B of bool
  | S of string

(** A map from variables to their contents (i.e an environment) *)
type variable_map = OpamVariable.variable_contents OpamVariable.Map.t

(** Opam package flags *)
type package_flag =
  | LightUninstall (** The package doesn't require downloading to uninstall *)
  | AllSwitches (** The package is pervasive on all switches *)

(** Flags on dependencies *)
type package_dep_flag =
  | Depflag_Build
  | Depflag_Test
  | Depflag_Doc

(** At some point we want to abstract so that the same functions can be used
    over CUDF and OPAM packages *)
module type GenericPackage = sig
  include OpamParallel.VERTEX
  val name_to_string: t -> string
  val version_to_string: t -> string
end

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

(** Formula over versionned packages *)
type ext_formula = package_dep_flag list OpamFormula.ext_package_formula

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
type repository_kind = [`http|`local|`git|`darcs|`hg]

(** Repository address *)
type address = string * string option

(** Repository root *)
type repository_root = dirname

(** Repositories *)
type repository = {
  repo_root    : repository_root;
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

  (** The package is already installed, but it must be recompiled. *)
  | To_recompile of 'a

(** The possible causes of an action. *)
type 'a cause =
  | Use of 'a list
  | Required_by of 'a list
  | Conflicts_with of 'a list
  | Upstream_changes
  | Requested
  | Unknown

(** Solver result *)
type solver_result =
  | Nothing_to_do
  | OK of package action list (** List of successful actions *)
  | Aborted
  | No_solution
  | Error of package action list * package action list * package action list
  (** List of successful actions, list of actions with errors,
      list of remaining undone actions *)

(** Solver result *)
type ('a, 'b) result =
  | Success of 'a
  | Conflicts of 'b

type solver_criteria = [ `Default | `Upgrade | `Fixup ]

(** Solver request *)
type 'a request = {
  criteria: solver_criteria;
  wish_install: 'a conjunction;
  wish_remove : 'a conjunction;
  wish_upgrade: 'a conjunction;
}

(** user request action *)
type user_action =
  | Install of name_set (** The 'root' packages to be installed *)
  | Upgrade of package_set (** The subset of packages to upgrade *)
  | Reinstall of package_set
  | Depends
  | Init of name_set (** The 'root' packages to be installed *)
  | Remove
  | Switch of name_set  (** The 'root' packages to be installed *)
  | Import of name_set  (** The 'root' packages to be installed *)

(** Solver universe *)
type universe = {
  u_packages : package_set;
  u_installed: package_set;
  u_available: package_set;
  u_depends  : ext_formula package_map;
  u_depopts  : ext_formula package_map;
  u_conflicts: formula package_map;
  u_action   : user_action;
  u_installed_roots: package_set;
  u_pinned   : package_set;
  u_base     : name_set;
}

(** {2 Command line arguments} *)

(** Upload arguments *)
type upload = {
  upl_opam   : filename;
  upl_descr  : filename;
  upl_archive: filename;
}

(** Pinned packages options *)
type pin_option =
  | Version of version
  | Local of dirname
  | Git of address
  | Darcs of address
  | Hg of address
  | Http of address

(** Pin kind *)
type pin_kind = [`version|`http|`git|`darcs|`hg|`local]

(** Shell compatibility modes *)
type shell = [`fish|`csh|`zsh|`sh|`bash]

(** Global configuration option *)
type global_config = {
  complete   : bool;
  switch_eval: bool;
}

(** User configuration option *)
type user_config = {
  shell      : shell;
  ocamlinit  : bool;
  dot_profile: filename option;
}

(** {2 Filtered commands} *)

type relop = OpamFormula.relop
type logop = [ `And | `Or ]
type pfxop = [ `Not ]

(** Filter *)
type filter =
  | FBool of bool
  | FString of string
  | FIdent of string
  | FOp of filter * relop * filter
  | FAnd of filter * filter
  | FOr of filter * filter
  | FNot of filter

(** A command argument *)
type simple_arg =
  | CString of string
  | CIdent of string

(** Command argument *)
type arg = simple_arg * filter option

(** Command *)
type command = arg list * filter option

(** {2 Untyped generic file format} *)

(** Source file positions: filename, line, column *)
type pos = filename * int * int

(** Base values *)
type value =
  | Bool of pos * bool
  | Int of pos * int
  | String of pos * string
  | Relop of pos * relop * value * value
  | Prefix_relop of pos * relop * value
  | Logop of pos * logop * value * value
  | Pfxop of pos * pfxop * value
  | Ident of pos * string
  | List of pos * value list
  | Group of pos * value list
  | Option of pos * value * value list
  | Env_binding of pos * string * value * value

(** A file section *)
type file_section = {
  section_kind  : string;
  section_name  : string;
  section_items : file_item list;
}

(** A file is composed of sections and variable definitions *)
and file_item =
  | Section of pos * file_section
  | Variable of pos * string * value

(** A file is a list of items and the filename *)
type file = {
  file_contents: file_item list;
  file_name    : string;
  file_format  : opam_version;
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

  (** Call the function in a global lock, then relax to a switch
      lock and call the function it returned *)
  | Global_with_switch_cont_lock of (unit -> switch * (unit -> unit))

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

(** Tags *)
type tags = OpamMisc.StringSet.t OpamMisc.StringSetMap.t

(** {2 Repository and global states} *)

(** Checksums *)
type checksums = string list

(** {2 JSON} *)
type json = OpamJson.t

(** {2 Updates} *)
type 'a updates = {
  created: 'a;
  updated: 'a;
  deleted: 'a;
  changed: 'a;
}
