(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type ctxt

val failure :
  ctxt: ctxt ->
  fun_name: string ->
  test_name: string ->
  expected: string ->
  got: string ->
  unit ->
  unit

val success :
  ctxt: ctxt -> fun_name: string -> test_name: string -> unit -> unit

val skip :
  ctxt: ctxt ->
  fun_name: string ->
  test_name: string ->
  reason: string ->
  unit ->
  unit

val run_tests : (ctxt -> unit) -> unit
