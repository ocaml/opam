(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

external getpid : unit -> int = "OPAMW_GetCurrentProcessID"
(* Polymorphic parameters below are used as placeholders for types in
 * OpamStubsTypes - it's not worth the effort of propagating the types here,
 * even if it does result in some ugly-looking primitives!
 *)
external getStdHandle : 'a -> 'b = "OPAMW_GetStdHandle"
external getConsoleScreenBufferInfo : 'a -> 'b = "OPAMW_GetConsoleScreenBufferInfo"
external setConsoleTextAttribute : 'a -> int -> unit = "OPAMW_SetConsoleTextAttribute"
external fillConsoleOutputCharacter : 'a -> char -> int -> int * int -> bool = "OPAMW_FillConsoleOutputCharacter"
external getConsoleMode : 'a -> int = "OPAMW_GetConsoleMode"
external setConsoleMode : 'a -> int -> bool = "OPAMW_SetConsoleMode"
external getWindowsVersion : unit -> int * int * int * int = "OPAMW_GetWindowsVersion"
external isWoW64 : unit -> bool = "OPAMW_IsWoW64"
external waitpids : int list -> int -> int * Unix.process_status = "OPAMW_waitpids"
external writeRegistry : 'a -> string -> string -> 'b -> 'c -> unit = "OPAMW_WriteRegistry"
