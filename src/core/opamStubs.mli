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

val getCurrentProcessID : unit -> int32
  (** Windows only. As {!getpid}, but without the possibility of truncating the
      ID on 32-bit platforms. *)

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

val getArchitecture : unit -> windows_cpu_architecture
  (** Windows only. Equivalent of [uname -m]. *)

val waitpids : int list -> int -> int * Unix.process_status
  (** Windows only. Given a list [pids] with [length] elements,
      [waitpids pids length] behaves like [Unix.wait], returning the pid and
      exit status of the first process to terminate. *)

val writeRegistry :
  registry_root -> string -> string -> 'a registry_value -> 'a -> unit
  (** Windows only. [writeRegistry root key name value_type value] sets the
      value [name] of type [value_type] in registry key [key] of [root] to
      [value].

      @raise Failure If the value could not be set.
      @raise Not_found If [key] does not exist. *)

val getConsoleOutputCP : unit -> int
(** Windows only. Retrieves the current Console Output Code Page. *)

val getCurrentConsoleFontEx : handle -> bool -> console_font_infoex
(** Windows only. Gets information on the current console output font. *)

val create_glyph_checker : string -> handle * handle
(** Windows only. Given a font name, returns a pair consisting of a screen DC
    and a font object, which will have been selected into the DC.

    @raise Failure If anything goes wrong with the GDI calls. *)

val delete_glyph_checker : handle * handle -> unit
(** Windows only. Given [(dc, font)], deletes the font object and releases the
    DC. *)

val has_glyph : handle * handle -> Uchar.t -> bool
(** Windows only. [has_glyph (dc, font) scalar] returns [true] if [font]
    contains a glyph for [scalar].

    @raise Failure If the call to [GetGlyphIndicesW] fails. *)

val getProcessArchitecture : int32 option -> windows_cpu_architecture
(** Windows only. Returns the CPU architecture of the given process ID (or the
    current process). *)

val process_putenv : int32 -> string -> string -> bool
(** Windows only. [process_putenv pid name value] sets the environment variable
    [name] to [value] in given process ID ([Unix.putenv] must also be called to
    update the value in the current process). This function must not be called
    if the target process is 32-bit and the current process is 64-bit or vice
    versa (outcomes vary from a no-op to a segfault). *)

val getPathToHome : unit -> string
val getPathToSystem : unit -> string
val getPathToLocalAppData : unit -> string
(** Windows only. retrieves the location of the wanted directory *)

val sendMessageTimeout :
  nativeint -> int -> int -> ('a, 'b, 'c) winmessage -> 'a -> 'b -> int * 'c
(** Windows only. [sendMessageTimeout hwnd timeout flags message wParam lParam]
    sends a message to the given handle, but is guaranteed to return within
    [timeout] milliseconds. The result consists of two parts, [fst]  is the
    return value from SendMessageTimeout, [snd] depends on both the message and
    [fst]. See https://msdn.microsoft.com/en-us/library/windows/desktop/ms644952.aspx *)

val getProcessAncestry : unit -> (int32 * string) list
(** Windows only. Returns the pid and full path to the image for each entry in
    the ancestry list for this process, starting with the process itself. If an
    image name can't be determined, then [""] is returned; on failure, returns
    [[]]. *)

val getConsoleAlias : string -> string -> string
(** Windows only. [getConsoleAlias alias exeName] retrieves the value for a
    given executable or [""] if the alias is not defined. See
    https://docs.microsoft.com/en-us/windows/console/getconsolealias *)

val win_create_process : string -> string -> string option -> Unix.file_descr ->
                         Unix.file_descr -> Unix.file_descr -> int
(** Windows only. Provided by OCaml's win32unix library. *)

val getConsoleWindowClass : unit -> string option
(** Windows only. Returns the name of the class for the Console window or [None]
    if there is no console. *)

val setErrorMode : int -> int
(** Windows only. Directly wraps SetErrorMode. *)

val getErrorMode : unit -> int
(** Windows only. Directly wraps GetErrorMode. *)

val setConsoleToUTF8 : unit -> unit
(** Windows only. Directly wraps SetConsoleOutputCP(CP_UTF8). *)

val uptime : unit -> float
(** Returns the number of seconds the system has been running on, or [0.0] if
    this cannot be determined. *)
