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
type package = OpamPackage.t = private {
  name: OpamPackage.Name.t;
  version: OpamPackage.Version.t;
}

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

(** OPAM versions *)
type opam_version = OpamVersion.t

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
  | Pkgflag_LightUninstall (** The package doesn't require downloading to uninstall *)
  | Pkgflag_AllSwitches (** The package is pervasive on all switches (unimplemented) *)
  | Pkgflag_Verbose (** The package's scripts output is to be displayed to the user *)
  | Pkgflag_Plugin (** The package is an opam plugin that will install a
                       [opam-<name>] exec, and may be auto-installed when doing
                       [opam <name>] *)
  | Pkgflag_Compiler (** Package may be used for 'opam switch' *)
  | Pkgflag_Virtual (** Virtual package: no install or remove instructions,
                        .install, but likely has depexts *)
  | Pkgflag_Unknown of string (** Used for error reporting, otherwise ignored *)

(** Flags on dependencies *)
type package_dep_flag =
  | Depflag_Build
  | Depflag_Test
  | Depflag_Doc
  | Depflag_Dev
  | Depflag_Unknown of string (** Used for error reporting, otherwise ignored *)

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

type url = OpamUrl.t (*= {
  transport: string;
  path: string;
  hash: string option;
  backend: OpamUrl.backend;
} *)

(** Repositories *)
type repository = {
  repo_root    : dirname; (** The root of opam's local mirror for this repo *)
  repo_name    : repository_name;
  repo_url     : url;
  repo_priority: int;
}

(** {2 Solver} *)

(** Used internally when computing sequences of actions *)
type 'a atomic_action = [
  | `Remove of 'a
  | `Install of 'a
]

(** Used to compact the atomic actions and display to the user in a more
    meaningful way *)
type 'a highlevel_action = [
  | 'a atomic_action
  | `Change of [ `Up | `Down ] * 'a * 'a
  | `Reinstall of 'a
]

(** Sub-type of [highlevel_action] corresponding to an installed package that
    changed state or version *)
type 'a inst_action = [
  | `Install of 'a
  | `Change of [ `Up | `Down ] * 'a * 'a
]

(** Used when applying solutions, separates build from install *)
type 'a concrete_action = [
  | 'a atomic_action
  | `Build of 'a
]

type 'a action = [
  | 'a atomic_action
  | 'a highlevel_action
  | 'a concrete_action
]

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
  extra_attributes: string list;
}

(** user request action *)
type user_action =
  | Install of name_set (** The 'root' packages to be installed *)
  | Upgrade of package_set (** The subset of packages to upgrade *)
  | Reinstall of package_set
  | Depends
  | Init
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
  u_dev      : package_set; (** packages with a version-controlled upstream *)
  (* NOTE: only needed for the dev depflag, remove and pre-compute instead *)
  u_base     : package_set;
  u_attrs    : (string * package_set) list;

  (* extra CUDF attributes for the given packages *)
  u_test     : bool; (** Test dependencies should be honored *)
  u_doc      : bool; (** Doc dependencies should be honored *)
  (* NOTE: pre-compute these also *)
}

(** {2 Command line arguments} *)

(** Pinned packages options *)
type pin_option =
  | Version of version
  | Source of url

(** Pin kind *)
type pin_kind = [ `version | OpamUrl.backend ]

(** Shell compatibility modes *)
type shell = [`fish|`csh|`zsh|`sh|`bash]

(** {2 Filtered commands} *)

type relop = OpamFormula.relop
type logop = [ `And | `Or ]
type pfxop = [ `Not ]

(** Filter *)
type filter =
  | FBool of bool
  | FString of string
  | FIdent of (name list * variable * (string * string) option)
  (** packages, variable name, string converter (val_if_true, val_if_false_or_undef) *)
  | FOp of filter * relop * filter
  | FAnd of filter * filter
  | FOr of filter * filter
  | FNot of filter
  | FUndef

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

type env_update_op = Eq | PlusEq | EqPlus | ColonEq | EqColon | EqPlusEq

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
  | Env_binding of pos * value * env_update_op * value

(** An opamfile section *)
type opamfile_section = {
  section_kind  : string;
  section_name  : string option;
  section_items : opamfile_item list;
}

(** An opamfile is composed of sections and variable definitions *)
and opamfile_item =
  | Section of pos * opamfile_section
  | Variable of pos * string * value

(** A file is a list of items and the filename *)
type opamfile = {
  file_contents: opamfile_item list;
  file_name    : string;
}

(** {2 Switches} *)

(** Compiler switches *)
type switch = OpamSwitch.t

(** Set of compiler switches *)
type switch_set = OpamSwitch.Set.t

(** Map of compile switches *)
type 'a switch_map = 'a OpamSwitch.Map.t

type switch_selections = {
  sel_installed: package_set;
  sel_roots: package_set;
  sel_compiler: package_set;
  sel_pinned: (version * pin_option) name_map;
}


(** {2 Misc} *)

(** The different kinds of locks *)
type lock =

  | Read_lock of (unit -> unit)
  (** The function does not modify anything, but it needs the state
      not to change while it is running. *)

  | Global_lock of (unit -> unit)
  (** Take the global lock, all subsequent calls to OPAM are
      blocked. *)

  | Switch_lock of (unit -> switch) * (unit -> unit)
  (** Take a global read lock and a switch lock. The first function is
      called with the read lock, then the second function is called with
      the returned switch write-locked. *)

  | Global_with_switch_cont_lock of (unit -> switch * (unit -> unit))
  (** Call the function in a global lock, then relax to a switch
      lock and call the function it returned *)

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

(** Environement variables: var name, value, optional comment *)
type env = (string * string * string option) list

(** Environment updates *)
type env_update = string * env_update_op * string * string option
(** var, update_op, value, comment *)

(** Tags *)
type tags = OpamStd.String.Set.t OpamStd.String.SetMap.t

(** {2 Repository and global states} *)

(** Checksums *)
type checksums = string list

(** {2 JSON} *)
type json = OpamJson.t
