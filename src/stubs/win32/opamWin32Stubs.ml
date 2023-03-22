(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

external getCurrentProcessID : unit -> int32 = "OPAMW_GetCurrentProcessID"
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
external getConsoleOutputCP : unit -> int = "OPAMW_GetConsoleOutputCP"
external getCurrentConsoleFontEx : 'a -> bool -> 'b = "OPAMW_GetCurrentConsoleFontEx"
external create_glyph_checker : string -> 'a * 'a = "OPAMW_CreateGlyphChecker"
external delete_glyph_checker : 'a * 'a -> unit = "OPAMW_DeleteGlyphChecker"
external has_glyph : 'a * 'a -> Uchar.t -> bool = "OPAMW_HasGlyph"
external isWoW64Process : int32 -> bool = "OPAMW_IsWoW64Process"
external process_putenv : int32 -> string -> string -> bool = "OPAMW_process_putenv"
external shGetFolderPath : int -> 'a -> string = "OPAMW_SHGetFolderPath"
external sendMessageTimeout : nativeint -> int -> int -> 'a -> 'b -> 'c -> int * 'd = "OPAMW_SendMessageTimeout_byte" "OPAMW_SendMessageTimeout"
external getParentProcessID : int32 -> int32 = "OPAMW_GetParentProcessID"
external getProcessName : int32 -> string = "OPAMW_GetProcessName"
external getConsoleAlias : string -> string -> string = "OPAMW_GetConsoleAlias"
