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

type basename = OpamFilename.Base.t

type dirname = OpamFilename.Dir.t

type filename = OpamFilename.t

type filename_set = OpamFilename.Set.t

type 'a filename_map = 'a OpamFilename.Map.t

type 'a download =
  | Up_to_date of 'a
  | Not_available
  | Result of 'a

type generic_file =
  | D of dirname
  | F of filename

type version = OpamPackage.Version.t

type version_set = OpamPackage.Version.Set.t

type name = OpamPackage.Name.t

type name_set = OpamPackage.Name.Set.t

type 'a name_map = 'a OpamPackage.Name.Map.t

type package = OpamPackage.t

type package_set = OpamPackage.Set.t

type 'a package_map = 'a OpamPackage.Map.t

type kind = string

type address = dirname

type repository_name = OpamRepositoryName.t

type 'a repository_name_map = 'a OpamRepositoryName.Map.t

type repository = {
  repo_name    : repository_name;
  repo_kind    : string;
  repo_address : dirname;
  repo_priority: int;
}

type variable = OpamVariable.t

type full_variable = OpamVariable.Full.t

type section = OpamVariable.Section.t

type full_section = OpamVariable.Section.Full.t

type alias = OpamAlias.t

type alias_set = OpamAlias.Set.t

type file_attribute = OpamFilename.Attribute.t

type file_attribute_set = OpamFilename.Attribute.Set.t

type compiler = OpamCompiler.t

type compiler_set = OpamCompiler.Set.t

type compiler_version = OpamCompiler.Version.t

type opam_version = OpamVersion.t

type 'a generic_formula = 'a OpamFormula.formula =
  | Empty
  | Atom of 'a
  | Block of 'a generic_formula
  | And of 'a generic_formula * 'a generic_formula
  | Or of 'a generic_formula * 'a generic_formula

type formula = OpamFormula.t

type conjunction = OpamFormula.conjunction

type compiler_constraint = OpamCompiler.Version.constr

type ppflag =
  | Camlp4 of string list
  | Cmd of string list

(* Command line arguments *)

(* Upload arguments *)
type upload = {
  upl_opam   : filename;
  upl_descr  : filename;
  upl_archive: filename;
}

let string_of_upload u =
  Printf.sprintf "opam=%s descr=%s archive=%s"
    (OpamFilename.to_string u.upl_opam)
    (OpamFilename.to_string u.upl_descr)
    (OpamFilename.to_string u.upl_archive)

(* Remote arguments *)
type remote =
  | RList
  | RAdd of repository_name * string * dirname * int
  | RRm of repository_name
  | RPriority of repository_name * int

let string_of_remote = function
  | RList -> "list"
  | RAdd (r, k, d, p) ->
    Printf.sprintf "add %s %s %s %d"
      (OpamRepositoryName.to_string r)
      (OpamFilename.Dir.to_string d)
      k p
  | RRm  r ->
    Printf.sprintf "rm %s"
      (OpamRepositoryName.to_string r)
  | RPriority (r, p) ->
    Printf.sprintf "priority %s %d"
      (OpamRepositoryName.to_string r) p

type config_option = {
  conf_is_rec : bool;
  conf_is_byte: bool;
  conf_is_link: bool;
  conf_options: OpamVariable.Section.Full.t list;
}

type pin_option =
  | Version of version
  | Path of dirname
  | Git of dirname
  | Unpin

let pin_option_of_string ?kind s =
  match kind with
  | Some "version" -> Version (OpamPackage.Version.of_string s)
  | Some "git"     ->
    if Sys.file_exists s then
      Git (OpamFilename.Dir.of_string s)
    else
      Git (OpamFilename.raw_dir s)
  | Some "rsync"   -> Path (OpamFilename.Dir.of_string s)
  | None | Some _  ->
    let d = OpamSystem.real_path s in
    if s = "none" then
      Unpin
    else if Sys.file_exists d then
      Path (OpamFilename.Dir.of_string s)
    else if OpamMisc.contains d ('/') then
      Git (OpamFilename.raw_dir s)
    else
      Version (OpamPackage.Version.of_string s)

type pin = {
  pin_package: name;
  pin_arg: pin_option;
}

let path_of_pin_option = function
  | Version v -> OpamPackage.Version.to_string v
  | Git p
  | Path p    -> OpamFilename.Dir.to_string p
  | Unpin     -> "none"

let kind_of_pin_option = function
  | Version _ -> "version"
  | Git _     -> "git"
  | Path _    -> "rsync"
  | Unpin     -> "<none>"

let string_of_pin p =
  Printf.sprintf "{package=%s; path=%s; kind=%s}"
    (OpamPackage.Name.to_string p.pin_package)
    (path_of_pin_option p.pin_arg)
    (kind_of_pin_option p.pin_arg)

type config =
  | CEnv
  | CList
  | CVariable of full_variable
  | CIncludes of bool * (name list)
  | CCompil   of config_option
  | CSubst    of basename list

(** Variable contents *)
type variable_contents = OpamVariable.variable_contents =
  | B of bool
  | S of string

type symbol =
  | Eq | Neq | Le | Ge | Lt | Gt

type filter =
  | FBool of bool
  | FString of string
  | FOp of filter * symbol * filter
  | FAnd of filter * filter
  | FOr of filter * filter

type arg = string * filter option

type command = arg list * filter option

type 'a optional = {
  c: 'a;
  optional: bool;
}

type stats = {
  s_install  : int;
  s_reinstall: int;
  s_upgrade  : int;
  s_downgrade: int;
  s_remove   : int;
}


(** Untyped generic file format. *)

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

type action =
  | To_change of package option * package
  | To_delete of package
  | To_recompile of package

type ('a, 'b) result =
  | Success of 'a
  | Conflicts of (unit -> 'b)

type request = {
  wish_install: conjunction;
  wish_remove : conjunction;
  wish_upgrade: conjunction;
}

type 'a solution = {
  to_remove: package list;
  to_add   : 'a;
}

type env = {
  add_to_env : (string * string) list;
  add_to_path: dirname;
  new_env    : (string * string) list;
}
