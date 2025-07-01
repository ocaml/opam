(**************************************************************************)
(*                                                                        *)
(*    Copyright 2024 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Compatibility module for Stdlib.In_channel which was added in OCaml 4.14 *)

(** Emulates or aliases [Stdlib.In_channel.input_all] *)
val input_all : Stdlib.in_channel -> string
