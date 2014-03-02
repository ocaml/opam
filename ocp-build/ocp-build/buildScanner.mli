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

exception IgnoreDirectory

val ignore_file_or_directory : unit -> 'a

val scan_directory_for_suffix :
 (* directory *) string -> (* extension *) string ->
  (string -> unit) -> unit

val scan_directory_for_files :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

val scan_directory_for_extensions :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

(*
val scan_directory_for_extensions2 :
 (* directory *) string ->
 (* extensions handlers *)
  (string ->  (* relative filename *)
   string ->  (* full filename *)
   unit) StringMap.t ->
  unit
*)


val scan_directory : (string (*dirname*) ->
                      string (*basename*) ->
                      string (*fullname*) -> unit)
  -> string -> unit

