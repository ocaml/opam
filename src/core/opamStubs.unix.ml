(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include OpamStubsTypes

let that's_a_no_no _ = failwith "Windows only. This function isn't implemented."

let getCurrentProcessID = that's_a_no_no
let getpid = Unix.getpid
let getStdHandle = that's_a_no_no
let getConsoleScreenBufferInfo = that's_a_no_no
let setConsoleTextAttribute _ = that's_a_no_no
let fillConsoleOutputCharacter _ _ _ = that's_a_no_no
let getConsoleMode = that's_a_no_no
let setConsoleMode _ = that's_a_no_no
let getWindowsVersion = that's_a_no_no
let getArchitecture = that's_a_no_no
let waitpids _ = that's_a_no_no
let readRegistry _ _ _ = that's_a_no_no
let enumRegistry _ _ = that's_a_no_no
let writeRegistry _ _ _ = that's_a_no_no
let getConsoleOutputCP = that's_a_no_no
let getCurrentConsoleFontEx _ = that's_a_no_no
let create_glyph_checker = that's_a_no_no
let delete_glyph_checker = that's_a_no_no
let has_glyph _ = that's_a_no_no
let getProcessArchitecture = that's_a_no_no
let process_putenv _ = that's_a_no_no
let getPathToHome = that's_a_no_no
let getPathToSystem = that's_a_no_no
let getPathToLocalAppData = that's_a_no_no
let sendMessageTimeout _ _ _ _ _ = that's_a_no_no
let getProcessAncestry = that's_a_no_no
let getConsoleAlias _ = that's_a_no_no
let win_create_process _ _ _ _ _ = that's_a_no_no
let getConsoleWindowClass = that's_a_no_no
let setErrorMode = that's_a_no_no
let getErrorMode = that's_a_no_no
let setConsoleToUTF8 = that's_a_no_no
let getVersionInfo = that's_a_no_no
let get_initial_environment = that's_a_no_no

external get_stdout_ws_col : unit -> int = "opam_stdout_ws_col"
