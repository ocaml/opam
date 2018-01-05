(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** OS-specific functions requiring C code on at least one platform.

    Most functions are Windows-specific and raise an exception on other
    platforms. *)

val getpid : unit -> int
  (** On Windows, this returns the actual process ID, rather than the non-unique
      faked process ID returned by the Microsoft C Runtime
      (see https://caml.inria.fr/mantis/view.php?id=4034).

      On all other platforms, this is just an alias for [Unix.getpid]. *)
