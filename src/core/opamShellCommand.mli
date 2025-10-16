(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Shell command syntax manipulation *)

val of_string : string -> string list
(** [of_string s] parses [s] and returns the list of [command :: arguments]
    according to the POSIX shell syntax. *)
