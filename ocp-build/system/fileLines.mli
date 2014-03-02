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

(** Get all the lines of a files *)
val of_file : string -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val to_file : string -> string list -> unit

(** [iter_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iter : (string -> unit) -> string -> unit

(** [iteri_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iteri : (int -> string -> unit) -> string -> unit

(** [sub_lines filename off len] returns [len] lines of [filename], starting at [off] *)
val sub : string -> int -> int -> string list
