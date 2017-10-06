(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]

let neg_relop = function
  | `Eq -> `Neq
  | `Neq -> `Eq
  | `Geq -> `Lt
  | `Gt -> `Leq
  | `Leq -> `Gt
  | `Lt -> `Geq

type version_constraint = relop * OpamPackage.Version.t

type atom = OpamPackage.Name.t * version_constraint option

let string_of_atom = function
  | n, None       -> OpamPackage.Name.to_string n
  | n, Some (r,c) ->
    Printf.sprintf "%s (%s %s)"
      (OpamPackage.Name.to_string n)
      (OpamPrinter.relop r)
      (OpamPackage.Version.to_string c)

let short_string_of_atom = function
  | n, None       -> OpamPackage.Name.to_string n
  | n, Some (`Eq,c) ->
    Printf.sprintf "%s.%s"
      (OpamPackage.Name.to_string n)
      (OpamPackage.Version.to_string c)
  | n, Some (r,c) ->
    Printf.sprintf "%s%s%s"
      (OpamPackage.Name.to_string n)
      (OpamPrinter.relop r)
      (OpamPackage.Version.to_string c)

let string_of_atoms atoms =
  OpamStd.List.concat_map " & " short_string_of_atom atoms

type 'a conjunction = 'a list

let string_of_conjunction string_of_atom c =
  Printf.sprintf "(%s)" (OpamStd.List.concat_map " & " string_of_atom c)

type 'a disjunction = 'a list

let string_of_disjunction string_of_atom c =
  Printf.sprintf "(%s)" (OpamStd.List.concat_map " | " string_of_atom c)

type 'a cnf = 'a list list

let string_of_cnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "(%s)" (OpamStd.List.concat_map " | " string_of_atom c) in
  Printf.sprintf "(%s)" (OpamStd.List.concat_map " & " string_of_clause cnf)

type 'a dnf = 'a list list

let string_of_dnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "(%s)" (OpamStd.List.concat_map " & " string_of_atom c) in
  Printf.sprintf "(%s)" (OpamStd.List.concat_map " | " string_of_clause cnf)

type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

let make_and a b = match a, b with
  | Empty, r | r, Empty -> r
  | a, b -> And (a, b)

let make_or a b = match a, b with
  | Empty, r | r, Empty -> r (* we're not assuming Empty is true *)
  | a, b -> Or (a, b)

let string_of_formula string_of_a f =
  let rec aux ?(in_and=false) f =
    let paren_if ?(cond=false) s =
      if cond || OpamFormatConfig.(!r.all_parens)
      then Printf.sprintf "(%s)" s
      else s
    in
    match f with
    | Empty    -> "0"
    | Atom a   -> paren_if (string_of_a a)
    | Block x  -> Printf.sprintf "(%s)" (aux x)
    | And(x,y) ->
      paren_if
        (Printf.sprintf "%s & %s"
           (aux ~in_and:true x) (aux ~in_and:true y))
    | Or(x,y)  ->
      paren_if ~cond:in_and
        (Printf.sprintf "%s | %s" (aux x) (aux y))
  in
  aux f

let rec map f = function
  | Empty    -> Empty
  | Atom x   -> f x
  | And(x,y) -> make_and (map f x) (map f y)
  | Or(x,y)  -> make_or (map f x) (map f y)
  | Block x  ->
    match map f x with
    | Empty -> Empty
    | x -> Block x

(* Maps top-down *)
let rec map_formula f t =
  let t = f t in
  match t with
  | Block x  -> Block (map_formula f x)
  | And(x,y) -> make_and (map_formula f x) (map_formula f y)
  | Or(x,y)  -> make_or (map_formula f x) (map_formula f y)
  | x -> x

let neg neg_atom =
  map_formula
    (function
      | And(x,y) -> Or(x,y)
      | Or(x,y) -> And(x,y)
      | Atom x -> Atom (neg_atom x)
      | x -> x)

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

type version_formula = version_constraint formula

type t = (OpamPackage.Name.t * version_formula) formula

let rec eval atom = function
  | Empty    -> true
  | Atom x   -> atom x
  | Block x  -> eval atom x
  | And(x,y) -> eval atom x && eval atom y
  | Or(x,y)  -> eval atom x || eval atom y

let rec partial_eval atom = function
  | Empty -> `Formula Empty
  | Atom x -> atom x
  | And(x,y) ->
    (match partial_eval atom x, partial_eval atom y with
     | `False, _ | _, `False -> `False
     | `True, f | f, `True -> f
     | `Formula x, `Formula y -> `Formula (And (x,y)))
  | Or(x,y) ->
    (match partial_eval atom x, partial_eval atom y with
     | `True, _ | _, `True -> `True
     | `False, f | f, `False -> f
     | `Formula x, `Formula y -> `Formula (Or (x,y)))
  | Block x -> partial_eval atom x

let check_relop relop c = match relop with
  | `Eq  -> c =  0
  | `Neq -> c <> 0
  | `Geq -> c >= 0
  | `Gt  -> c >  0
  | `Leq -> c <= 0
  | `Lt  -> c <  0

let eval_relop relop v1 v2 =
  check_relop relop (OpamPackage.Version.compare v1 v2)

let check_version_formula f v =
  eval (fun (relop, vref) -> eval_relop relop v vref) f

let check (name,cstr) package =
  name = OpamPackage.name package &&
  match cstr with
  | None -> true
  | Some (relop, v) -> eval_relop relop (OpamPackage.version package) v

let packages_of_atoms pkgset atoms =
  (* Conjunction for constraints over the same name, but disjunction on the
     package names *)
  let by_name =
    List.fold_left (fun acc (n,_ as atom) ->
        OpamPackage.Name.Map.update n (fun a -> atom::a) [] acc)
      OpamPackage.Name.Map.empty atoms
  in
  OpamPackage.Name.Map.fold (fun name atoms acc ->
      OpamPackage.Set.union acc @@
      OpamPackage.Set.filter
        (fun nv -> List.for_all (fun a -> check a nv) atoms)
        (OpamPackage.packages_of_name pkgset name))
    by_name OpamPackage.Set.empty

let to_string t =
  let string_of_constraint (relop, version) =
    Printf.sprintf "%s %s" (OpamPrinter.relop relop)
      (OpamPackage.Version.to_string version) in
  let string_of_pkg = function
    | n, Empty -> OpamPackage.Name.to_string n
    | n, (Atom _ as c) ->
      Printf.sprintf "%s %s"
        (OpamPackage.Name.to_string n)
        (string_of_formula string_of_constraint c)
    | n, c ->
      Printf.sprintf "%s (%s)"
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

let verifies f nv =
  let name_formula =
    map (fun ((n, _) as a) -> if n = OpamPackage.name nv then Atom a else Empty)
      (dnf_of_formula f)
  in
  name_formula <> Empty &&
  eval (fun (_name, cstr) ->
      check_version_formula cstr (OpamPackage.version nv))
    name_formula

let packages pkgset f =
  let names =
    fold_left (fun acc (name, _) ->
        OpamPackage.Name.Set.add name acc)
      OpamPackage.Name.Set.empty f
  in
  (* dnf allows us to transform the formula into a union of intervals, where
     ignoring atoms for different package names works. *)
  let dnf = dnf_of_formula f in
  OpamPackage.Name.Set.fold (fun name acc ->
      (* Ignore conjunctions where [name] doesn't appear *)
      let name_formula =
        map (fun ((n, _) as a) -> if n = name then Atom a else Empty) dnf
      in
      OpamPackage.Set.union acc @@
      OpamPackage.Set.filter (fun nv ->
          let v = OpamPackage.version nv in
          eval (fun (_name, cstr) -> check_version_formula cstr v)
            name_formula)
        (OpamPackage.packages_of_name pkgset name))
    names OpamPackage.Set.empty

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
  | _   ->
    failwith (Printf.sprintf "%s is not a valid conjunction" (to_string t))

let ands l = List.fold_left make_and Empty l

let rec ands_to_list = function
  | Empty -> []
  | And (e,f) | Block (And (e,f)) -> ands_to_list e @ ands_to_list f
  | x -> [x]

let of_conjunction c =
  of_atom_formula (ands (List.rev_map (fun x -> Atom x) c))

let to_disjunction t =
  match to_cnf t with
  | []  -> []
  | [x] -> x
  | _   ->
    failwith (Printf.sprintf "%s is not a valid disjunction" (to_string t))

let ors l = List.fold_left make_or Empty l

let rec ors_to_list = function
  | Empty -> []
  | Or (e,f) | Block (Or (e,f)) -> ors_to_list e @ ors_to_list f
  | x -> [x]

let of_disjunction d =
  of_atom_formula (ors (List.rev_map (fun x -> Atom x) d))

let atoms t =
  fold_left (fun accu x -> x::accu) [] (to_atom_formula t)

let get_disjunction_formula version_set cstr =
  List.map (fun ff ->
      match ands_to_list ff with
      | [] -> assert false
      | [Atom _] as at -> at
      | _ ->
        OpamPackage.Version.Set.filter (check_version_formula ff) version_set |>
        OpamPackage.Version.Set.elements |>
        List.map (fun v -> Atom (`Eq, v)))
    (ors_to_list cstr) |>
  List.flatten

let set_to_disjunction set t =
  List.map (function
      | And _ ->
        failwith (Printf.sprintf "%s is not a valid disjunction" (to_string t))
      | Or _ | Block _ | Empty -> assert false
      | Atom (name, Empty) -> [name, None]
      | Atom (name, cstr) ->
        get_disjunction_formula
          (OpamPackage.versions_of_name set name)
          cstr |>
        List.map (function
            | Atom (relop, v) -> name, Some (relop, v)
            | _ -> assert false))
    (ors_to_list t) |>
  List.flatten

let simplify_ineq_formula vcomp f =
  (* backported from OWS/WeatherReasons *)
  let vmin a b = if vcomp a b <= 0 then a else b in
  let vmax a b = if vcomp a b >= 0 then a else b in
  let and_cstrs c1 c2 = match c1, c2 with
    | (`Gt, a), (`Gt, b) -> [`Gt, vmax a b]
    | (`Geq,a), (`Geq,b) -> [`Geq, vmax a b]
    | (`Gt, a), (`Geq,b) | (`Geq,b), (`Gt, a) ->
        if vcomp a b >= 0 then [(`Gt, a)] else [(`Geq,b)]
    | (`Lt, a), (`Lt, b) -> [`Lt, vmin a b]
    | (`Leq,a), (`Leq,b) -> [`Leq, vmin a b]
    | (`Lt, a), (`Leq,b) | (`Leq,b), (`Lt, a) ->
        if vcomp a b <= 0 then [`Lt, a] else [`Leq,b]
    | (`Geq,a), (`Eq, b) | (`Eq, b), (`Geq,a) when vcomp a b <= 0 -> [`Eq, b]
    | (`Gt, a), (`Eq, b) | (`Eq, b), (`Gt, a) when vcomp a b <  0 -> [`Eq, b]
    | (`Leq,a), (`Eq, b) | (`Eq, b), (`Leq,a) when vcomp a b >= 0 -> [`Eq, b]
    | (`Leq,a), (`Geq,b) | (`Geq,b), (`Leq,a) when vcomp a b =  0 -> [`Eq, a]
    | (`Lt, a), (`Eq, b) | (`Eq, b), (`Lt, a) when vcomp a b >  0 -> [`Eq, b]
    | (`Geq,a), (`Neq,b) | (`Neq,b), (`Geq,a) when vcomp a b >  0 -> [`Geq,a]
    | (`Gt, a), (`Neq,b) | (`Neq,b), (`Gt, a) when vcomp a b >= 0 -> [`Gt, a]
    | (`Leq,a), (`Neq,b) | (`Neq,b), (`Leq,a) when vcomp a b <  0 -> [`Leq,a]
    | (`Lt, a), (`Neq,b) | (`Neq,b), (`Lt, a) when vcomp a b <= 0 -> [`Lt, a]
    | c1, c2 -> if c1 = c2 then [c1] else [c1;c2]
  in
  let or_cstrs c1 c2 = match c1, c2 with
    | (`Gt, a), (`Gt, b) -> [`Gt, vmin a b]
    | (`Geq,a), (`Geq,b) -> [`Geq, vmin a b]
    | (`Gt, a), (`Geq,b) | (`Geq,b), (`Gt, a) ->
        if vcomp a b < 0 then [`Gt, a] else [`Geq,b]
    | (`Lt, a), (`Lt, b) -> [`Lt, vmax a b]
    | (`Leq,a), (`Leq,b) -> [`Leq,vmax a b]
    | (`Lt, a), (`Leq,b) | (`Leq,b), (`Lt, a) ->
        if vcomp a b > 0 then [`Lt, a] else [`Leq,b]
    | (`Geq,a), (`Eq, b) | (`Eq, b), (`Geq,a) when vcomp a b <= 0 -> [`Geq,a]
    | (`Gt, a), (`Eq, b) | (`Eq, b), (`Gt, a) when vcomp a b <  0 -> [`Gt, a]
    | (`Leq,a), (`Eq, b) | (`Eq, b), (`Leq,a) when vcomp a b >= 0 -> [`Leq,a]
    | (`Leq,a), (`Geq,b) | (`Geq,b), (`Leq,a) when vcomp a b =  0 -> []
    | (`Lt, a), (`Eq, b) | (`Eq, b), (`Lt, a) when vcomp a b >  0 -> [`Lt, a]
    | (`Geq,a), (`Neq,b) | (`Neq,b), (`Geq,a) when vcomp a b >  0 -> [`Neq,b]
    | (`Gt, a), (`Neq,b) | (`Neq,b), (`Gt, a) when vcomp a b >= 0 -> [`Neq,b]
    | (`Leq,a), (`Neq,b) | (`Neq,b), (`Leq,a) when vcomp a b <  0 -> [`Neq,b]
    | (`Lt, a), (`Neq,b) | (`Neq,b), (`Lt, a) when vcomp a b <= 0 -> [`Neq,b]
    | c1, c2 -> if c1 = c2 then [c1] else [c1;c2]
  in
  let rec add_cstr join c = function
    | [] -> [c]
    | c1::r -> match join c c1 with
      | [c] -> add_cstr join c r
      | _ -> c1 :: add_cstr join c r
  in
  let rec merge mk join fl =
    let subs,cstrs =
      List.fold_left (fun (sub,cstrs) fl ->
          match aux fl with
          | Atom c -> sub, add_cstr join c cstrs
          | f -> mk sub f, cstrs)
        (Empty,[]) fl
    in
    List.fold_left (fun f c -> mk f (Atom c)) subs cstrs
  and aux = function
    | And _ as f -> merge make_and and_cstrs (ands_to_list f)
    | Or _ as f -> merge make_or or_cstrs (ors_to_list f)
    | Block f -> aux f
    | (Atom _ | Empty) as f -> f
  in
  aux f

let simplify_version_formula f =
  simplify_ineq_formula OpamPackage.Version.compare f

(** Takes an ordered list of atoms and a predicate, returns a formula describing
    the subset of matching atoms *)
let gen_formula l f =
  let l = List.map (fun x -> f x, x) l in
  let rec aux t x = function
    | (t', y) :: r ->
      if t = t' then aux t x r
      else (t, x) :: aux t' y r
    | [] -> [t, x]
  in
  let rec aux2 = function
    | (true, x) :: (false, y) :: r ->
      And (Atom (`Geq, x), Atom (`Lt, y)) :: aux2 r
    | (false, x) :: r -> Atom (`Lt, x) :: aux2 r
    | (true, x) :: [] -> [Atom (`Geq, x)]
    | [] -> [Empty]
    | (true, _) :: (true, _) :: _ -> assert false
  in
  match l with
  | [] -> Some Empty
  | (t, x) :: r ->
    match aux t x r with
    | [] -> assert false
    | [true, _] -> Some Empty
    | [false, _] -> None
    | _ :: r -> Some (ors (aux2 r))

let formula_of_version_set set subset =
  let module S = OpamPackage.Version.Set in
  match
    gen_formula
      (OpamPackage.Version.Set.elements set)
      (fun x -> OpamPackage.Version.Set.mem x subset)
  with
  | Some f -> f
  | None -> invalid_arg "Empty subset"

let simplify_version_set set f =
  let module S = OpamPackage.Version.Set in
  if S.is_empty set then Empty else
  let set = fold_left (fun set (_relop, v) -> S.add v set) set f in
  gen_formula (S.elements set) (check_version_formula f) |>
  OpamStd.Option.default f

type 'a ext_package_formula =
  (OpamPackage.Name.t * ('a * version_formula)) formula

let formula_of_extended ~filter =
  map (fun (n, (kws,formula)) -> if filter kws then Atom (n, formula) else Empty)

let reduce_extended ~filter =
  map (fun (n, (kws, formula)) ->
      let kws =
        List.fold_left (fun acc kw ->
            match acc with
            | None -> None
            | Some kws -> match filter kw with
              | Some true -> acc
              | Some false -> None
              | None -> Some (kw::kws))
          (Some []) (List.rev kws)
      in
      match kws with
      | None -> Empty
      | Some kws -> Atom (n, (kws, formula)))
