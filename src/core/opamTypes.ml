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

open OpamMisc.OP

exception Lexer_error of string

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

type address = dirname

type repository_name = OpamRepositoryName.t

type repository_root = dirname

type 'a repository_name_map = 'a OpamRepositoryName.Map.t

type repository_kind = [`http|`local|`git|`darcs]

type repository = {
  repo_name    : repository_name;
  repo_kind    : repository_kind;
  repo_address : address;
  repo_priority: int;
}

let string_of_repository_kind = function
  | `http  -> "http"
  | `local -> "local"
  | `git   -> "git"
  | `darcs -> "darcs"

let repository_kind_of_string = function
  | "wget"
  | "curl"
  | "http"  -> `http
  | "rsync"
  | "local" -> `local
  | "git"   -> `git
  | "darcs" -> `darcs
  | s -> OpamGlobals.error_and_exit "%s is not a valid repository kind." s

type variable = OpamVariable.t

type full_variable = OpamVariable.Full.t

type section = OpamVariable.Section.t

type full_section = OpamVariable.Section.Full.t

type switch = OpamSwitch.t

type switch_set = OpamSwitch.Set.t

type 'a switch_map = 'a OpamSwitch.Map.t

type file_attribute = OpamFilename.Attribute.t

type file_attribute_set = OpamFilename.Attribute.Set.t

type compiler = OpamCompiler.t

type compiler_set = OpamCompiler.Set.t

type 'a compiler_map = 'a OpamCompiler.Map.t

type compiler_version = OpamCompiler.Version.t

type opam_version = OpamVersion.t

type 'a generic_formula = 'a OpamFormula.formula =
  | Empty
  | Atom of 'a
  | Block of 'a generic_formula
  | And of 'a generic_formula * 'a generic_formula
  | Or of 'a generic_formula * 'a generic_formula

type atom = OpamFormula.atom

type formula = OpamFormula.t

type 'a conjunction = 'a OpamFormula.conjunction

type 'a disjunction = 'a OpamFormula.disjunction

type compiler_constraint = OpamCompiler.Version.constr

type ppflag =
  | Camlp4 of string list
  | Cmd of string list

type shell = [`csh|`zsh|`sh]

type global_config = {
  complete   : bool;
  switch_eval: bool;
}

type user_config = {
  shell      : shell;
  ocamlinit  : bool;
  dot_profile: filename option;
}

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

type config = {
  conf_is_rec : bool;
  conf_is_byte: bool;
  conf_is_link: bool;
  conf_options: OpamVariable.Section.Full.t list;
}

type pin_option =
  | Version of version
  | Local of dirname
  | Git of address
  | Darcs of address
  | Unpin

type pin_kind = [`version|`git|`darcs|`local|`unpin]

let mk_git str =
  let path, commit = OpamMisc.git_of_string str in
  if Sys.file_exists path then
    let real_path = OpamFilename.Dir.of_string path in
    match commit with
    | None   -> Git real_path
    | Some c ->
      let path = Printf.sprintf "%s#%s" (OpamFilename.Dir.to_string real_path) c in
      Git (OpamFilename.Dir.of_string path)
  else
    Git (OpamFilename.raw_dir str)

let pin_option_of_string ?kind s =
  match kind with
  | Some `version -> Version (OpamPackage.Version.of_string s)
  | Some `git     -> mk_git s
  | Some `darcs   ->
    if Sys.file_exists s then
      Darcs (OpamFilename.Dir.of_string s)
    else
      Darcs (OpamFilename.raw_dir s)
  | Some `local   -> Local (OpamFilename.Dir.of_string s)
  | Some `unpin   -> Unpin
  | None          ->
    if s = "none" then
      Unpin
    else if Sys.file_exists s then
      Local (OpamFilename.Dir.of_string s)
    else if OpamMisc.contains s ('/') then
      mk_git s
    else
      Version (OpamPackage.Version.of_string s)

let string_of_pin_kind = function
  | `version -> "version"
  | `git     -> "git"
  | `darcs   -> "darcs"
  | `local   -> "local"
  | `unpin   -> "unpin"

let pin_kind_of_string = function
  | "version" -> `version
  | "git"     -> `git
  | "darcs"   -> `darcs
  | "rsync"
  | "local"   -> `local
  | "unpin"   -> `unpin
  | s -> OpamGlobals.error_and_exit "%s is not a valid kind of pinning." s

type pin = {
  pin_package: name;
  pin_option : pin_option;
}

let path_of_pin_option = function
  | Version v -> OpamPackage.Version.to_string v
  | Git p
  | Darcs p
  | Local p   -> OpamFilename.Dir.to_string p
  | Unpin     -> "none"

let kind_of_pin_option = function
  | Version _ -> `version
  | Git _     -> `git
  | Darcs _   -> `darcs
  | Local _   -> `local
  | Unpin     -> `unpin

let string_of_pin p =
  Printf.sprintf "{package=%s; path=%s; kind=%s}"
    (OpamPackage.Name.to_string p.pin_package)
    (path_of_pin_option p.pin_option)
    (string_of_pin_kind (kind_of_pin_option p.pin_option))

(** Variable contents *)
type variable_contents = OpamVariable.variable_contents =
  | B of bool
  | S of string

type symbol =
  | Eq | Neq | Le | Ge | Lt | Gt

type filter =
  | FBool of bool
  | FString of string
  | FIdent of string
  | FOp of filter * symbol * filter
  | FAnd of filter * filter
  | FOr of filter * filter

type simple_arg =
  | CString of string
  | CIdent of string

type arg = simple_arg * filter option

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

type 'a action =
  | To_change of 'a option * 'a
  | To_delete of 'a
  | To_recompile of 'a

type 'a cause =
  | Use of 'a list
  | Required_by of 'a list
  | Upstream_changes
  | Unknown

let action_contents = function
  | To_change (_, p)
  | To_recompile p
  | To_delete p -> p

module type ACTION_GRAPH = sig

  type package

  include Graph.Sig.I with type V.t = package action

  include Graph.Oper.S with type g = t

  module Parallel: OpamParallel.SIG
    with type G.t = t
     and type G.V.t = V.t

  module Topological: sig
    val iter: (package action -> unit) -> t -> unit
    val fold: (package action -> 'a -> 'a) -> t -> 'a -> 'a
  end

  type solution = {
    to_remove : package list;
    to_process: t;
    root_causes: (package * package cause) list;
  }

end

module type PKG = sig
  include Graph.Sig.COMPARABLE
  val to_string: t -> string
  val string_of_action: ?causes:(t -> t cause) -> t action -> string
end

module MakeActionGraph (Pkg: PKG) = struct
  type package = Pkg.t
  module Vertex =  struct
    type t = Pkg.t action
    let compare t1 t2 = Pkg.compare (action_contents t1) (action_contents t2)
    let hash t = Pkg.hash (action_contents t)
    let equal t1 t2 = Pkg.equal (action_contents t1) (action_contents t2)
  end
  module PG = Graph.Imperative.Digraph.ConcreteBidirectional (Vertex)
  module Topological = Graph.Topological.Make (PG)
  module Traverse = Graph.Traverse.Dfs(PG)
  module Components = Graph.Components.Make(PG)
  module O = Graph.Oper.I (PG)
  module Parallel = OpamParallel.Make(struct
      include PG
      include Topological
      include Traverse
      include Components
    end)
  include PG
  include O
  type solution = {
    to_remove : package list;
    to_process: t;
    root_causes: (package * package cause) list;
  }
end

module PackageAction = struct
  include OpamPackage

  let string_of_name = OpamPackage.name |> OpamPackage.Name.to_string

  let string_of_names ps =
    String.concat ", " (List.map string_of_name ps)

  let string_of_cause = function
    | Upstream_changes -> "[upstream changes]"
    | Use pkgs        -> Printf.sprintf "[use %s]" (string_of_names pkgs)
    | Required_by pkgs -> Printf.sprintf "[required by %s]" (string_of_names pkgs)
    | Unknown          -> ""

  let string_of_raw_action = function
    | To_change (None, p)   -> Printf.sprintf " - install %s" (OpamPackage.to_string p)
    | To_change (Some o, p) ->
      let f action =
        Printf.sprintf " - %s %s to %s" action
          (OpamPackage.to_string o) (OpamPackage.Version.to_string (OpamPackage.version p)) in
      if OpamPackage.Version.compare (OpamPackage.version o) (OpamPackage.version p) < 0 then
        f "upgrade"
      else
        f "downgrade"
    | To_recompile p -> Printf.sprintf " - recompile %s" (OpamPackage.to_string p)
    | To_delete p    -> Printf.sprintf " - remove %s" (OpamPackage.to_string p)

  let string_of_action ?causes a =
    let causes = match causes with
      | None   -> ""
      | Some f -> string_of_cause (f (action_contents a)) in
    match causes with
    | "" -> string_of_raw_action a
    | _  -> Printf.sprintf "%s %s" (string_of_raw_action a) causes

end

module PackageActionGraph = MakeActionGraph(PackageAction)

type solution = PackageActionGraph.solution

type solver_result =
  | Nothing_to_do
  | OK
  | Aborted
  | No_solution
  | Error of package action list

type ('a, 'b) result =
  | Success of 'a
  | Conflicts of (unit -> 'b)

type 'a request = {
  wish_install: 'a conjunction;
  wish_remove : 'a conjunction;
  wish_upgrade: 'a conjunction;
}

type env = (string * string) list

type env_updates = (string * string * string) list

type user_action =
  | Install of name_set
  | Upgrade of package_set
  | Reinstall
  | Depends
  | Init of name_set
  | Remove
  | Switch of name_set
  | Import of name_set

type universe = {
  u_packages : package_set;
  u_installed: package_set;
  u_available: package_set;
  u_depends  : formula package_map;
  u_depopts  : formula package_map;
  u_conflicts: formula package_map;
  u_action   : user_action;
  u_installed_roots: package_set;
}

type lock =
  | Read_lock of (unit -> unit)
  | Global_lock of (unit -> unit)
  | Switch_lock of (unit -> unit)

type tags = OpamMisc.StringSet.t OpamMisc.StringSetMap.t
