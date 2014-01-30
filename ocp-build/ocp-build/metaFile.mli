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

 (* comma or space separated *)

val empty : unit -> MetaTypes.meta
val create_meta_file : string -> MetaTypes.meta -> unit
val add_requires : MetaTypes.meta -> (string * bool) list -> string list -> unit
val add_archive : MetaTypes.meta -> (string * bool) list -> string list -> unit
