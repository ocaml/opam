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

(** Interface of File with labels. *)

(** Cut a filename at the last extension position *)
val cut_last_extension : basename:string -> string * string

(** Get the contents of a channel *)
val string_of_channel : ic:in_channel -> string

(** Output a line in a channel *)
val output_line : oc:out_channel -> str:string -> unit

(** Get all the lines of a file (possibly discarding some lines).  If
    [line_break] is set, '\n' are kept (default is false).*)
val lines_of_file :
  ?line_break:bool -> ?discard:(string -> bool)
  -> string -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val file_of_lines : filename:string -> string list -> unit

(** Get the contents of a file *)
val string_of_file : filename:string -> string

(** [file_of_string name str] saves [str] into the file [name]. *)
val file_of_string : filename:string -> str:string -> unit

(*
  val genlex_of_file : keywords:string list -> ?discard:(string -> bool)
  -> filename:string -> OcpGenlex.t
*)
