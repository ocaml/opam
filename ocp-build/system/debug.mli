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

(** Conditional logging. *)

(** {4 Control debugging} *)

(** You may set the following reference to control whether, and where
    debug information is written. *)

(** Initially false *)
val verbose : bool ref

val set_verbose : unit -> unit
val set_quiet : unit -> unit

(** Initially stderr *)
val debug_channel : out_channel ref

(** Open the specified filename to write debug in it rather than stderr *)
val append_log_to : string -> unit


(** {4 Printing} *)

(** This function always print to stdout, ignoring [!debug_channel] ! *)
val log : string -> ('a, unit, string, unit) format4 -> 'a

(** The following functions all print on [debug_channel] (if [verbose]
    is set), always flush their output, and they do not apply %t and
    %a formats if [!verbose] is false.

    Examples:
    - debug "2 + 2 = %d" (2+2) *)

val debug : ('a, out_channel, unit) format -> 'a
val debugln : ('a, out_channel, unit) format -> 'a
val fdebug : ('a, Format.formatter, unit, unit) format4 -> 'a
val fdebugln : ('a, Format.formatter, unit, unit) format4 -> 'a

(** It is possible to write to this formatter directly, which will
    only actually write if [verbose] is set. However, it may be more
    efficient to write for example, [fdebug "%t" (print x)] instead of
    [print x debug_formatter] (or similarly with "%a"), as the latter
    will only (totally) apply [print] if [verbose] is set. *)
val debug_formatter : Format.formatter

(** {4 Customizable debugging} *)

(** The following allows a more flexible logging scheme. To use it,
    you will typically replace [open Debug] in the appropriate places
    by [include Debug.Tag(struct let tag = "<tag>" end)], and then
    control the debugging in the specified scope by calling
    [set_verbose_tag "<tag>" true]. *)

(** Signature of the Debuging API.  *)
module type S = sig

  val debug : ('a, out_channel, unit) format -> 'a
  val debugln : ('a, out_channel, unit) format -> 'a
  val fdebug : ('a, Format.formatter, unit, unit) format4 -> 'a
  val fdebugln : ('a, Format.formatter, unit, unit) format4 -> 'a
  val debug_formatter : Format.formatter

end

(** Return a debugging module with a dedicated [verbose] reference,
    (still using the above [debug_channel]. *)
module Tag : functor (X : sig
  val tag : string
end) -> S

(** Control debugging for the given tag. *)
val set_verbose_tag : string -> bool -> unit

(** Set debugging for all tags. *)
val set_verbose_all : bool -> unit

(** Return the set of currently defined tags, with their control
    reference. *)
val all_tags : unit -> (string * bool ref) list

(** {4 Most general version, with a functor} *)

module Make : functor(X : sig
  val debug_channel : unit -> out_channel option
  val prefix : unit -> string option
end) -> S
