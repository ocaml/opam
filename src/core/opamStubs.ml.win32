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
include OpamWin32Stubs
let getpid () = Int32.to_int (getCurrentProcessID ())

external win_create_process : string -> string -> string option ->
                              Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int
                            = "win_create_process" "win_create_process_native"
