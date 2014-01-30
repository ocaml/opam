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
open BuildOCPTypes

val print_loaded_ocp_files : bool ref
val print_package_deps : bool ref

val init_packages : unit -> BuildOCPInterp.state
val load_ocp_files :
  BuildOCPInterp.config ->
  BuildOCPInterp.state -> File.t list -> int
val verify_packages : BuildOCPInterp.state -> BuildOCPTypes.project

(* val load_project : File.t list -> project * int *)

(* returns the number of errors while reading the files *)

val find_root : File.t -> string list -> File.t

(*
val save_project : File.t -> project -> unit
*)

val scan_root : File.t -> File.t list


(* [find_package pj file] returns the list of packages in
   project [pj] containing [file] as a source. *)
val find_package : project -> File.t -> package list


val save_project_state : project -> File.t -> unit
val load_project_state : File.t -> project

val find_obuild : (string -> unit) -> string -> unit


val empty_config : unit -> BuildOCPInterp.config
val generated_config : unit -> BuildOCPInterp.config

val print_conflicts : project -> bool -> unit

val eprint_project : string -> project -> unit

