(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** OS-specific functions requiring C code on at least one platform.

    Most functions are Windows-specific and raise an exception on other
    platforms. *)

include module type of struct include OpamStubsTypes end

val getpid : unit -> int
  (** On Windows, this returns the actual process ID, rather than the non-unique
      faked process ID returned by the Microsoft C Runtime
      (see https://caml.inria.fr/mantis/view.php?id=4034).

      On all other platforms, this is just an alias for [Unix.getpid]. *)

val getStdHandle : stdhandle -> handle
  (** Windows only. Return a standard handle. *)

val getConsoleScreenBufferInfo : handle -> console_screen_buffer_info
  (** Windows only. Return current Console screen buffer information. *)

val setConsoleTextAttribute : handle -> int -> unit
  (** Windows only. Set the console's text attribute setting. *)

val fillConsoleOutputCharacter : handle -> char -> int -> int * int -> bool
  (** Windows only. [fillConsoleOutputCharacter buffer c n (x, y)] writes [c]
      [n] times starting at the given coordinate (and wrapping if required). *)

val getConsoleMode : handle -> int
  (** Windows only. Returns the input/output mode of the console screen buffer
      referred to by the handle.

      @raise Not_found If the handle does not refer to a console. *)

val setConsoleMode : handle -> int -> bool
  (** Windows only. Sets the input/output mode of the console screen buffer
      referred to by the handle, returning [true] if the operation isr
      successful. *)

val getWindowsVersion : unit -> int * int * int * int
  (** Windows only. Returns the Windows version as
      [(major, minor, build, revision)]. This function only works if opam is
      compiled OCaml 4.06.0 or later, it returns [(0, 0, 0, 0)] otherwise. *)
