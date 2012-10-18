(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Management of formulas *)

(** binary operations *)
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]

(** Pretty-printing of relops *)
val string_of_relop: relop -> string

(** Parsing relops *)
val relop_of_string: string -> relop

(** Formula atoms for OPAM *)
type atom = OpamPackage.Name.t * (relop * OpamPackage.Version.t) option

(** Pretty-printing of atoms *)
val string_of_atom: atom -> string

(** AND formulas *)
type 'a conjunction = 'a list

(** Pretty print AND formulas *)
val string_of_conjunction: ('a -> string) -> 'a conjunction -> string

(** CNF formulas *)
type 'a cnf = 'a list list

(** Pretty print CNF formulas *)
val string_of_cnf: ('a -> string) -> 'a cnf -> string

(** General formulas *)
type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

(** Eval a formula *)
val eval: ('a -> bool) -> 'a formula -> bool

(** Pretty print a formula *)
val string_of_formula: ('a -> string) -> 'a formula -> string

(** Map function *)
val map: ('a -> 'b) -> 'a formula -> 'b formula

(** Iter function *)
val iter: ('a -> unit) -> 'a formula -> unit

(** Fold function *)
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b formula -> 'a

(** An atom is: [name] * ([relop] * [version]) formula.
    Examples of valid formulaes:
    - "foo" \{> "1" & (<"3" | ="5")\}
    - "foo" \{= "1" | > "4"\} | ("bar" "bouh") *)
type t = (OpamPackage.Name.t * (relop * OpamPackage.Version.t) formula) formula

(** Return all the atoms *)
val atoms: t -> atom list

(** Pretty print the formula *)
val to_string: t -> string

(** Return a conjunction. If the initial formula is not a
    conjunction, fail. *)
val to_conjunction: t -> atom conjunction

(** Return an equivalent CNF formula *)
val to_cnf: t -> atom cnf
