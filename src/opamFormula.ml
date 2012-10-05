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

type conjunction = Debian.Format822.vpkglist

type cnf = Debian.Format822.vpkgformula

let string_of_vpkg = function
  | ((n,_), None)       -> n
  | ((n,_), Some (r,c)) -> Printf.sprintf "%s (%s %s)" n r c

let string_of_conjunction c =
  Printf.sprintf "(%s)" (String.concat " & " (List.map string_of_vpkg c))

let string_of_cnf cnf =
  let string_of_clause c =
    Printf.sprintf "(%s)" (String.concat " | " (List.map string_of_vpkg c)) in
  Printf.sprintf "(%s)" (String.concat " & " (List.map string_of_clause cnf))

type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

let string_of_formula string_of_a f =
  let rec aux = function
    | Empty    -> ""
    | Atom a   -> string_of_a a
    | Block x  -> Printf.sprintf "(%s)" (aux x)
    | And(x,y) -> Printf.sprintf "%s & %s" (aux x) (aux y)
    | Or(x,y)  -> Printf.sprintf "%s | %s" (aux x) (aux y) in
  aux f

let rec map f = function
  | Empty    -> Empty
  | Atom x   -> Atom (f x)
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

type t = (OpamPackage.Name.t * (string * OpamPackage.Version.t) formula) formula

let to_string t =
  let string_of_constraint (relop, version) =
    Printf.sprintf "%s %s" relop (OpamPackage.Version.to_string version) in
  let string_of_pkg = function
    | n, Empty -> OpamPackage.Name.to_string n
    | n, c     ->
      Printf.sprintf "%s %s"
        (OpamPackage.Name.to_string n)
        (string_of_formula string_of_constraint c) in
  string_of_formula string_of_pkg t

(* unroll to a CNF formula *)
let rec unroll f t =
  let rec mk_left x y = match y with
    | Block y   -> mk_left x y
    | And (a,b) -> And (mk_left x a, mk_left x b)
    | _         -> Or (x,y) in
  let rec mk_right x y = match x with
    | Block x   -> mk_right x y
    | And (a,b) -> And (mk_right a y, mk_right b y)
    | _         -> mk_left x y in
  let rec mk = function
    | Empty      -> Empty
    | Block x    -> mk x
    | Atom x     -> f x
    | And (x,y)  -> And (mk x, mk y)
    | Or (x,y)   -> mk_right (mk x) (mk y) in
  mk t

let unroll t =
  let atom (r,v) = Atom (r, v) in
  let vpkg (x, c) =
    match unroll atom c with
    | Empty -> Atom (x, None)
    | cs    -> map (fun c -> x, Some c) cs in
  unroll vpkg t

let atoms t =
  fold_left (fun accu x -> x::accu) [] (unroll t)

(* Convert to dose-CNF *)
let to_cnf t =
  let rec or_formula = function
    | Atom (x,None)      -> [(OpamPackage.Name.to_string x, None), None]
    | Atom (x,Some(r,v)) -> [(OpamPackage.Name.to_string x, None),
                             Some(r, OpamPackage.Version.to_string v)]
    | Or(x,y)            -> or_formula x @ or_formula y
    | Empty
    | Block _
    | And _      -> assert false in
  let rec aux t = match t with
    | Empty    -> []
    | Block x  -> assert false
    | Atom _
    | Or _     -> [or_formula t]
    | And(x,y) -> aux x @ aux y in
  aux (unroll t)

let to_conjunction t =
  let rec aux = function
    | []     -> []
    | [x]::t -> x::aux t
    | _      ->
      OpamGlobals.error_and_exit "%s is not a valid conjunction" (to_string t) in
  aux (to_cnf t)
