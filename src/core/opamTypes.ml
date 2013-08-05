(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

open OpamMisc.OP

exception Lexer_error of string

type json = OpamJson.t

type basename = OpamFilename.Base.t

type dirname = OpamFilename.Dir.t

type filename = OpamFilename.t

type filename_set = OpamFilename.Set.t

type 'a filename_map = 'a OpamFilename.Map.t

type 'a download =
  | Up_to_date of 'a
  | Not_available of string
  | Result of 'a

type generic_file = OpamFilename.generic_file =
  | D of dirname
  | F of filename

let download_map fn = function
  | Up_to_date f    -> Up_to_date (fn f)
  | Result  f       -> Result (fn f)
  | Not_available d -> Not_available d

let download_dir = download_map (fun d -> D d)
let download_file = download_map (fun f -> F f)

type version = OpamPackage.Version.t

type version_set = OpamPackage.Version.Set.t

type name = OpamPackage.Name.t

type name_set = OpamPackage.Name.Set.t

type 'a name_map = 'a OpamPackage.Name.Map.t

type package = OpamPackage.t

type package_set = OpamPackage.Set.t

type 'a package_map = 'a OpamPackage.Map.t

type address = string * string option

let string_of_address = function
  | url, None   -> url
  | url, Some c -> Printf.sprintf "%s#%s" url c

let address_of_string str =
  match OpamMisc.cut_at str '#' with
  | None       -> str, None
  | Some (a,c) -> OpamSystem.real_path a, Some c

type repository_name = OpamRepositoryName.t

type repository_root = dirname

type 'a repository_name_map = 'a OpamRepositoryName.Map.t

type repository_kind = [`http|`local|`git|`darcs|`hg]

let guess_repository_kind kind (address, ext) =
  match kind with
  | Some k -> k
  | None   ->
    if ext = None && Sys.file_exists address then
      `local
    else
      if OpamMisc.starts_with ~prefix:"git" address
      || OpamMisc.ends_with ~suffix:"git" address then
        `git
      else if OpamMisc.starts_with ~prefix:"hg" address then
        `hg
      else
        `http

type repository = {
  repo_root    : repository_root;
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
  | `hg    -> "hg"

let repository_kind_of_string = function
  | "wget"
  | "curl"
  | "http"  -> `http
  | "rsync"
  | "local" -> `local
  | "git"   -> `git
  | "darcs" -> `darcs
  | "hg"    -> `hg
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

type shell = [`fish|`csh|`zsh|`sh|`bash]

let string_of_shell = function
  | `fish -> "fish"
  | `csh  -> "csh"
  | `zsh  -> "zsh"
  | `sh   -> "sh"
  | `bash -> "bash"

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
  | Hg of address
  | Unpin
  | Edit

type pin_kind = [`version|`git|`darcs|`hg|`local]

let repository_kind_of_pin_kind = function
  | `version -> None
  | (`git|`darcs|`hg|`local as k) -> Some k

let pin_option_of_string ?kind s =
  match kind with
  | Some `version -> Version (OpamPackage.Version.of_string s)
  | Some `git     -> Git (address_of_string s)
  | Some `hg      -> Hg (address_of_string s)
  | Some `darcs   -> Darcs (address_of_string s)
  | Some `local   -> Local (OpamFilename.Dir.of_string s)
  | Some `unpin   -> Unpin
  | None          ->
    if s = "none" then
      Unpin
    else if Sys.file_exists s then
      Local (OpamFilename.Dir.of_string s)
    else if OpamMisc.contains s ('/') then
      Git (address_of_string s)
    else
      Version (OpamPackage.Version.of_string s)

let string_of_pin_kind = function
  | `version -> "version"
  | `git     -> "git"
  | `darcs   -> "darcs"
  | `hg      -> "hg"
  | `local   -> "local"
  | `unpin   -> "unpin"

let pin_kind_of_string = function
  | "version" -> `version
  | "git"     -> `git
  | "darcs"   -> `darcs
  | "hg"      -> `hg
  | "rsync"
  | "local"   -> `local
  | s -> OpamGlobals.error_and_exit "%s is not a valid kind of pinning." s

type pin = {
  pin_package: name;
  pin_option : pin_option;
}

let string_of_pin_option = function
  | Version v -> OpamPackage.Version.to_string v
  | Git p
  | Darcs p
  | Hg p      -> string_of_address p
  | Local p   -> OpamFilename.Dir.to_string p
  | Unpin     -> "unpin"
  | Edit      -> "edit"

let kind_of_pin_option = function
  | Version _ -> Some `version
  | Git _     -> Some `git
  | Darcs _   -> Some `darcs
  | Hg _      -> Some `hg
  | Local _   -> Some `local
  | _         -> None

let option fn = function
  | None   -> ""
  | Some k -> fn k

let string_of_pin p =
  Printf.sprintf "{package=%s; path=%s; kind=%s}"
    (OpamPackage.Name.to_string p.pin_package)
    (string_of_pin_option p.pin_option)
    (option string_of_pin_kind (kind_of_pin_option p.pin_option))

(** Variable contents *)
type variable_contents = OpamVariable.variable_contents =
  | B of bool
  | S of string

type variable_map = OpamVariable.variable_contents OpamVariable.Map.t

type symbol =
  | Eq | Neq | Le | Ge | Lt | Gt

let string_of_symbol = function
  | Eq  -> "="
  | Neq -> "!="
  | Ge  -> ">="
  | Le  -> "<="
  | Gt  -> ">"
  | Lt  -> "<"

type filter =
  | FBool of bool
  | FString of string
  | FIdent of string
  | FOp of filter * symbol * filter
  | FAnd of filter * filter
  | FOr of filter * filter
  | FNot of filter

let rec string_of_filter = function
  | FBool b    -> string_of_bool b
  | FString s  -> Printf.sprintf "%S" s
  | FIdent i   -> i
  | FOp(e,s,f) ->
    Printf.sprintf "%s %s %s"
      (string_of_filter e) (string_of_symbol s) (string_of_filter f)
  | FAnd (e,f) -> Printf.sprintf "%s & %s" (string_of_filter e) (string_of_filter f)
  | FOr (e,f)  -> Printf.sprintf "%s | %s" (string_of_filter e) (string_of_filter f)
  | FNot e     -> Printf.sprintf "!%s" (string_of_filter e)

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
    let string_of_vertex v = Pkg.to_string (action_contents v)
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
  | OK of package action list
  | Aborted
  | No_solution
  | Error of package action list * package action list * package action list

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

type 'a updates = {
  created: 'a;
  updated: 'a;
  deleted: 'a;
  changed: 'a;
}

type lock =
  | Read_lock of (unit -> unit)
  | Global_lock of (unit -> unit)
  | Switch_lock of (unit -> unit)

type tags = OpamMisc.StringSet.t OpamMisc.StringSetMap.t

type checksums = string list
