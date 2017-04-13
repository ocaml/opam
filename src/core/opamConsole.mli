(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Console output, ANSI color, logging and user querying *)

(** Global configuration parameters (read from OpamGlobalConfig, and the
    environment when necessary) *)

val debug: unit -> bool
val verbose: unit -> bool
val color: unit -> bool
val utf8: unit -> bool
val utf8_extended: unit -> bool
val disp_status_line: unit -> bool

(** General text formatting *)

(** Settable attributes for ANSI terminal output. Nesting is generally not
    handled. *)
type text_style =
  [ `black
  | `blue
  | `bold
  | `crossed
  | `cyan
  | `green
  | `magenta
  | `red
  | `underline
  | `white
  | `yellow ]

(** Helper coloring functions. Returns the string unchanged if color is
    disabled *)
val colorise : text_style -> string -> string
val colorise' : text_style list -> string -> string
val acolor : text_style -> out_channel -> string -> unit
val acolor_w : int -> text_style -> out_channel -> string -> unit

(** Logging *)

(** Timers, only active when debug is on. Returns the time between the
    application to each argument, in seconds *)
val timer : unit -> unit -> float

(** [log section ~level fmt args]. Used for debug messages, default
    level is 1 *)
val log : string -> ?level:int -> ('a, out_channel, unit) format -> 'a

(** Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
    rather than [log "%s" (to_string x)] to avoid costly unneeded
    stringifications *)
val slog : ('a -> string) -> out_channel -> 'a -> unit

val error : ('a, unit, string, unit) format4 -> 'a
val warning : ('a, unit, string, unit) format4 -> 'a
val note : ('a, unit, string, unit) format4 -> 'a

(** Message without prefix, reformat or newline, to stderr (useful to continue
    error messages without repeating "[ERROR]") *)
val errmsg : ('a, out_channel, unit, unit, unit, unit) format6 -> 'a

val error_and_exit : ?num:int -> ('a, unit, string, 'b) format4 -> 'a
val msg : ('a, out_channel, unit, unit) format4 -> 'a
val formatted_msg : ?indent:int -> ('a, unit, string, unit) format4 -> 'a
val header_msg : ('a, unit, string, unit) format4 -> 'a
val header_error :
  ('a, unit, string, ('b, unit, string, unit) format4 -> 'b) format4 -> 'a

(** Display a dynamic status line to stdout, that will be erased on next output.
    The message should not be wider than screen nor contain newlines. *)
val status_line : ('a, unit, string, unit) format4 -> 'a

(** Ask the user to press Y/y/N/n to continue (returns a boolean).
    Defaults to true (yes) if unspecified *)
val confirm: ?default:bool -> ('a, unit, string, bool) format4 -> 'a

(** Read some input from the user (returns a string option) *)
val read: ('a, unit, string, string option) format4 -> 'a
