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

(** Folds on the tree of a filter *)
val fold_down_left: ('a -> filter -> 'a) -> 'a -> filter -> 'a

(** Returns all the variables appearing in a filter *)
val variables: filter -> full_variable list

(** Type of filter environment. *)
type env = full_variable -> variable_contents option

(** Return the contents of a variable. Return [None] if the variable
    is not defined in the given environment. *)
val contents_of_variable: env -> full_variable -> variable_contents option

(** Return the contents of a variable. Raises [Not_found] if the
    variable is not defined in the given environment. *)
val contents_of_variable_exn: env -> full_variable -> variable_contents

(** Replace variables escapes "%{var}%" using the given resolution function on a
    string *)
val replace_variables: string -> (full_variable -> variable_contents) -> string

(** Substitutes variables escapes "%{var}%" with their values in a string *)
val substitute_string: env -> string -> string

(** Substitutes variables escapes "%{var}%" with their values in file [base.in]
    and writes the results to [base] *)
val substitute_file: env -> basename -> unit

(** Evaluate a filter. May raise Not_found if the filter contains undefined
    variables *)
val eval: env -> filter -> bool

(** Evaluate an optional filter. *)
val eval_opt: env -> filter option -> bool

(** Filter a list of commands by:
    - evaluating the substitution strings; and
    - removing the commands with a filter evaluating to "false" *)
val commands: env -> command list -> string list list
