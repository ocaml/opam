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
external getArchitecture : unit -> 'a = "OPAMW_GetArchitecture"
external waitpids : int list -> int -> int * Unix.process_status = "OPAMW_waitpids"
external writeRegistry : 'a -> string -> string -> 'b -> 'c -> unit = "OPAMW_WriteRegistry"
external getConsoleOutputCP : unit -> int = "OPAMW_GetConsoleOutputCP"
external getCurrentConsoleFontEx : 'a -> bool -> 'b = "OPAMW_GetCurrentConsoleFontEx"
external create_glyph_checker : string -> 'a * 'a = "OPAMW_CreateGlyphChecker"
external delete_glyph_checker : 'a * 'a -> unit = "OPAMW_DeleteGlyphChecker"
external has_glyph : 'a * 'a -> Uchar.t -> bool = "OPAMW_HasGlyph"
external getProcessArchitecture : int32 option -> 'a = "OPAMW_GetProcessArchitecture"
external process_putenv : int32 -> string -> string -> bool = "OPAMW_process_putenv"
external shGetFolderPath : int -> 'a -> string = "OPAMW_SHGetFolderPath"
external sendMessageTimeout : nativeint -> int -> int -> 'a -> 'b -> 'c -> int * 'd = "OPAMW_SendMessageTimeout_byte" "OPAMW_SendMessageTimeout"
external getProcessAncestry : unit -> (int32 * string) list = "OPAMW_GetProcessAncestry"
external getConsoleAlias : string -> string -> string = "OPAMW_GetConsoleAlias"
external getConsoleWindowClass : unit -> string option = "OPAMW_GetConsoleWindowClass"
