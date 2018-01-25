(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Various implementations of the low-level CUDF resolution, most of them
    relying on external solvers (aspcud, etc.). Used for calling-back below
    Dose. *)

include module type of struct include OpamCudfSolverSig end

module Aspcud : S
module Aspcud_old : S
module Mccs : S
module Packup : S

(** The list of supported solvers, in decreasing order of preference *)
val default_solver_selection: (module S) list

(** Generates a custom solver implementation from a user command. Contains some
    magic:
    - if the command matches one of the predefined ones, the default criteria
      are taken from there
    - if the command is a singleton and matches, it is expanded similarly from
      the pre-defined solvers
*)
val custom_solver : OpamTypes.arg list -> (module S)

(** Like [custom_solver], but takes a simple command as a string *)
val solver_of_string : string -> (module S)

(** Gets the first present solver from the list. Exits with error if none was found. *)
val get_solver : ?internal:bool -> (module S) list -> (module S)

val has_builtin_solver : unit -> bool
