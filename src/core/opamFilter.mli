(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** Manage filters *)

open OpamTypes

(** Pretty-print *)
val to_string: filter -> string

(** Type of filter environment. *)
 type env = full_variable -> variable_contents option

(** Return the contents of a variable. Return [None] if the variable
    is not defined in the given environment. *)
val contents_of_variable: env -> full_variable -> variable_contents option

(** Return the contents of a variable. Fail if an exception if the
    variable is not defined in the given environment. *)
val contents_of_variable_exn: env -> full_variable -> variable_contents

(** Substitute a string. *)
val substitute_string: env -> string -> string

(** Substitute a file. *)
val substitute_file: env -> basename -> unit

(** Evaluate a filter. *)
val eval: env -> filter -> bool

(** Evaluate an optional filter. *)
val eval_opt: env -> filter option -> bool

(** Filter a list of commands by:
    - evaluating the substitution strings; and
    - removing the commands with a filter evaluating to "false" *)
val commands: env -> command list -> string list list
