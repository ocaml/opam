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

open BuildOCPTypes
open BuildOCPVariable



(* Misc *)
val number_of_cores : unit -> int
val split_version : string -> string * string * string
val find_in_PATH : string -> string list -> string
val get_PATH : unit -> string list
val set_PATH : string list -> unit

val find_first_in_path :
  string list -> (string -> bool) -> string list -> string option
