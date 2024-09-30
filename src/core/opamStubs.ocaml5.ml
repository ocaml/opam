(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include OpamWin32Stubs

external win_create_process : string -> string -> string option ->
                              Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int
                            = "caml_unix_create_process" "caml_unix_create_process_native"
