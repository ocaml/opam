(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)


(* open BuildBase *)
(* open Stdlib2 *)
(* open OcpLang *)
open BuildOCPTree

(*
type dependency_flag =
| PREPROCESS
| COMPILE
| LINK

module DepFlagsSet = Set.Make(struct
    type t = dependency_flag
    let compare = compare
end)
*)

type package = {
  package_name : string; (* basename of project *)
  mutable package_dirname : string; (* where the project files are *)
  mutable package_source_kind : string; (* meta or ocp ? *)
  mutable package_provides : string; (* TODO: what the project provides,
					default "" => same as name.
					if provides is specified, then
					the name of the object should
					be that one. TODO: it should
					be an option, since it should
					apply to modules too. *)
  mutable package_type : package_type; (* what it generates *)

(*
  mutable package_tag : string; (* TODO: if tags are specified, then the
				   following rules apply when choosing
				   among several projects providing
				   the same name,

				   1/ if a tag is used in one of the
				   other projects, then the project
				   with that tag is preferred.

				   2/ If several tags are used in
				   other projects, and these tags are
				   present in different projects, then
				   a conflict is found and an error is
				   raised.

				   For example, if tags "debug" and
				   "threads" are used, then a conflict
				   is found if "stdlib" has two
				   versions, "debug" and "threads",
				   but the conflict is resolved if
				   "stdlib" also has a
				   "threads"+"debug" version.

				   Automatic tags: a "debug" version
				   is always compiled for bytecode and
				   native code, and a "profile"
				   version is always compiled for
				   native code. They are stored in
				   project+"+debug" and
				   project+"+profile", using the
				   interfaces from project. (how to do that ?)
				*)
*)
  mutable package_version : string;
  mutable package_auto : string option; (* unused: TODO *)

  package_loc : int;
(* Where this package is defined : *)
  package_filename : string;
(* All the .ocp files whose content can influence this package *)
  package_filenames : (string * Digest.t option) list;

  (* at the end of "load_project", we rename package_identifiers to be
     continuous *)
  mutable package_id : int;
  mutable package_validated : bool;
  package_node : LinearToposort.node;

  (* list of projects, on which compilation depends *)
  mutable package_deps_map : string package_dependency StringMap.t;
(*  mutable package_deps_sorted : string package_dependency list; *)
  (* bool = should the project be linked (true) or just a dependency (false) *)

  mutable package_requires : package package_dependency list;
  mutable package_requires_map : package package_dependency IntMap.t;
  mutable package_added : bool;

  mutable package_options : BuildOCPVariable.env;
}

and 'a package_dependency =
    {
      dep_project : 'a;
      mutable dep_link : bool;
      mutable dep_syntax : bool;
      mutable dep_optional : bool;
      dep_options : BuildOCPVariable.env;
    }

and project = {
(*  project_files : File.t list; *)
  mutable project_disabled : package array;
  mutable project_incomplete : package array;
  mutable project_sorted : package array;
  mutable project_missing : (string * package list) list;
  mutable project_conflicts :  (package * package *  package) list;
}

(*
let disabled_projects = ref ([] : BuildTypes.package_info list)
let incomplete_projects = ref ([] : BuildTypes.package_info list)
*)











