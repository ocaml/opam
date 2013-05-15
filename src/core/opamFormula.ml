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

type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]

let string_of_relop = function
  | `Eq  -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt  -> ">"
  | `Leq -> "<="
  | `Lt  -> "<"

let relop_of_string = function
  | "="  -> `Eq
  | "!=" -> `Neq
  | ">=" -> `Geq
  | ">"  -> `Gt
  | "<=" -> `Leq
  | "<"  -> `Lt
  | x    -> failwith (x ^ " is not a valid relop")

type atom = OpamPackage.Name.t * (relop * OpamPackage.Version.t) option

let string_of_atom = function
  | n, None       -> OpamPackage.Name.to_string n
  | n, Some (r,c) ->
    Printf.sprintf "%s (%s %s)"
      (OpamPackage.Name.to_string n)
      (string_of_relop r)
      (OpamPackage.Version.to_string c)

type 'a conjunction = 'a list

let string_of_conjunction string_of_atom c =
  Printf.sprintf "(%s)" (String.concat " & " (List.rev_map string_of_atom c))

type 'a disjunction = 'a list

let string_of_disjunction string_of_atom c =
  Printf.sprintf "(%s)" (String.concat " | " (List.rev_map string_of_atom c))

type 'a cnf = 'a list list

let string_of_cnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "(%s)" (String.concat " | " (List.rev_map string_of_atom c)) in
  Printf.sprintf "(%s)" (String.concat " & " (List.rev_map string_of_clause cnf))

type 'a dnf = 'a list list

let string_of_dnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "(%s)" (String.concat " & " (List.rev_map string_of_atom c)) in
  Printf.sprintf "(%s)" (String.concat " | " (List.rev_map string_of_clause cnf))

type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

let string_of_formula string_of_a f =
  let rec aux = function
    | Empty    -> "0"
    | Atom a   -> string_of_a a
    | Block x  -> Printf.sprintf "(%s)" (aux x)
    | And(x,y) -> Printf.sprintf "%s & %s" (aux x) (aux y)
    | Or(x,y)  -> Printf.sprintf "%s | %s" (aux x) (aux y) in
  aux f

let rec map f = function
  | Empty    -> Empty
  | Atom x   -> f x
  | Block x  -> Block (map f x)
  | And(x,y) -> And (map f x, map f y)
  | Or(x,y)  -> Or (map f x, map f y)

let rec iter f = function
  | Empty    -> ()
  | Atom x   -> f x
  | Block x  -> iter f x
  | And(x,y) -> iter f x; iter f y
  | Or(x,y)  -> iter f x; iter f y

let rec fold_left f i = function
  | Empty    -> i
  | Atom x   -> f i x
  | Block x  -> fold_left f i x
  | And(x,y) -> fold_left f (fold_left f i x) y
  | Or(x,y)  -> fold_left f (fold_left f i x) y

type t = (OpamPackage.Name.t * (relop * OpamPackage.Version.t) formula) formula

let rec eval atom = function
  | Empty    -> true
  | Atom x   -> atom x
  | Block x  -> eval atom x
  | And(x,y) -> eval atom x && eval atom y
  | Or(x,y)  -> eval atom x || eval atom y

let to_string t =
  let string_of_constraint (relop, version) =
    Printf.sprintf "%s %s" (string_of_relop relop) (OpamPackage.Version.to_string version) in
  let string_of_pkg = function
    | n, Empty -> OpamPackage.Name.to_string n
    | n, c     ->
      Printf.sprintf "%s %s"
        (OpamPackage.Name.to_string n)
        (string_of_formula string_of_constraint c) in
  string_of_formula string_of_pkg t

(* convert a formula to a CNF *)
let cnf_of_formula t =
  let rec mk_left x y = match y with
    | Block y   -> mk_left x y
    | And (a,b) -> And (mk_left x a, mk_left x b)
    | Empty     -> x
    | _         -> Or (x,y) in
  let rec mk_right x y = match x with
    | Block x   -> mk_right x y
    | And (a,b) -> And (mk_right a y, mk_right b y)
    | Empty     -> y
    | _         -> mk_left x y in
  let rec mk = function
    | Empty     -> Empty
    | Block x   -> mk x
    | Atom x    -> Atom x
    | And (x,y) -> And (mk x, mk y)
    | Or (x,y)  -> mk_right (mk x) (mk y) in
  mk t

(* convert a formula to DNF *)
let dnf_of_formula t =
  let rec mk_left x y = match y with
    | Block y  -> mk_left x y
    | Or (a,b) -> Or (mk_left x a, mk_left x b)
    | _        -> And (x,y) in
  let rec mk_right x y = match x with
    | Block x  -> mk_right x y
    | Or (a,b) -> Or (mk_right a y, mk_right b y)
    | _        -> mk_left x y in
  let rec mk = function
    | Empty     -> Empty
    | Block x   -> mk x
    | Atom x    -> Atom x
    | Or (x,y)  -> Or (mk x, mk y)
    | And (x,y) -> mk_right (mk x) (mk y) in
  mk t

(* Convert a t an atom formula *)
let to_atom_formula (t:t): atom formula =
  let atom (r,v) = Atom (r, v) in
  let atoms (x, c) =
    match cnf_of_formula (map atom c) with
    | Empty -> Atom (x, None)
    | cs    -> map (fun c -> Atom (x, Some c)) cs in
  map atoms t

(* Convert an atom formula to a t-formula *)
let of_atom_formula (a:atom formula): t =
  let atom (n, v) =
    match v with
    | None       -> Atom (n, Empty)
    | Some (r,v) -> Atom (n, Atom (r,v)) in
  map atom a

(* Convert a formula to CNF *)
let to_cnf (t : t) =
  let rec or_formula = function
    | Atom (x,None)      -> [x, None]
    | Atom (x,Some(r,v)) -> [x, Some(r,v)]
    | Or(x,y)            -> or_formula x @ or_formula y
    | Empty
    | Block _
    | And _      -> assert false in
  let rec aux t = match t with
    | Empty    -> []
    | Block _  -> assert false
    | Atom _
    | Or _     -> [or_formula t]
    | And(x,y) -> aux x @ aux y in
  aux (cnf_of_formula (to_atom_formula t))

(* Convert a formula to DNF *)
let to_dnf t =
  let rec and_formula = function
    | Atom (x,None)      -> [x, None]
    | Atom (x,Some(r,v)) -> [x, Some(r,v)]
    | And(x,y)           -> and_formula x @ and_formula y
    | Empty
    | Block _
    | Or _      -> assert false in
  let rec aux t = match t with
    | Empty   -> []
    | Block _ -> assert false
    | Atom _
    | And _   -> [and_formula t]
    | Or(x,y) -> aux x @ aux y in
  aux (dnf_of_formula (to_atom_formula t))

let to_conjunction t =
  match to_dnf t with
  | []  -> []
  | [x] -> x
  | _   -> OpamGlobals.error_and_exit "%s is not a valid conjunction" (to_string t)

let ands = function
  | []   -> Empty
  | h::t -> List.fold_left (fun acc elt -> And(acc, elt)) h t

let of_conjunction c =
  of_atom_formula (ands (List.rev_map (fun x -> Atom x) c))

let to_disjunction t =
  match to_cnf t with
  | []  -> []
  | [x] -> x
  | _   -> OpamGlobals.error_and_exit "%s is not a valid disjunction" (to_string t)

let ors = function
  | []   -> Empty
  | h::t -> List.fold_left (fun acc elt -> Or(acc, elt)) h t

let of_disjunction d =
  of_atom_formula (ors (List.rev_map (fun x -> Atom x) d))

let atoms t =
  fold_left (fun accu x -> x::accu) [] (to_atom_formula t)
