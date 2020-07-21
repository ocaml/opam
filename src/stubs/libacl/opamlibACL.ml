(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

external get_acl_executable_info : string -> int -> int list option = "OPAM_get_acl_executable_info"
