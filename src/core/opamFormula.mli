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

(** OR formulas *)
type 'a disjunction = 'a list

(** Pretty print OR formulas *)
val string_of_disjunction: ('a -> string) -> 'a disjunction -> string

(** CNF formulas (Conjunctive Normal Form) *)
type 'a cnf = 'a disjunction conjunction

(** DNF formulas (Disjunctive Normal Form) *)
type 'a dnf = 'a conjunction disjunction

(** Pretty print CNF formulas *)
val string_of_cnf: ('a -> string) -> 'a cnf -> string

(** Pretty print DNF formulas *)
val string_of_dnf: ('a -> string) -> 'a dnf -> string

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

(** Convert a list of formulas to an AND-formula *)
val ands: 'a formula list -> 'a formula

(** Convert a list of formulas to an OR-formula *)
val ors: 'a formula list -> 'a formula

(** Map function *)
val map: ('a -> 'b formula) -> 'a formula -> 'b formula

(** Iter function *)
val iter: ('a -> unit) -> 'a formula -> unit

(** Fold function *)
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b formula -> 'a

(** An atom is: [name] * ([relop] * [version]) formula.
    Examples of valid formulaes:
    - "foo" \{> "1" & (<"3" | ="5")\}
    - "foo" \{= "1" | > "4"\} | ("bar" "bouh") *)
type t = (OpamPackage.Name.t * (relop * OpamPackage.Version.t) formula) formula

(** Convert a formula to CNF *)
val cnf_of_formula: 'a formula -> 'a formula

(** Convert a formula to DNF *)
val dnf_of_formula: 'a formula -> 'a formula

(** Transform a formula where versions can be expressed using formulas
    to a flat atom formula *)
val to_atom_formula: t -> atom formula

(** Convert an atom-formula to a t-formula *)
val of_atom_formula: atom formula -> t

(** {2 Atoms} *)

(** Return all the atoms *)
val atoms: t -> atom list

(** Pretty print the formula *)
val to_string: t -> string

(** Return a conjunction. If the initial formula is not a
    conjunction, then fail. *)
val to_conjunction: t -> atom conjunction

(** Return a formula from a conjunction of atoms *)
val of_conjunction: atom conjunction -> t

(** Return a disjunction. It the initial formula is not a
    disjunction, then fail. *)
val to_disjunction: t -> atom disjunction

(** Return a formula from a disjunction of atoms *)
val of_disjunction: atom disjunction -> t

(** Return an equivalent CNF formula *)
val to_cnf: t -> atom cnf

(** Return an equivalent DNF formula *)
val to_dnf: t -> atom dnf
