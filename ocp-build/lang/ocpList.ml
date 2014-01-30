(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open List

(***********************************************************************)
(*                                                                     *)
(*                                                                     *)
(*                             CLEAN SECTION                           *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

let rec last list =
  match list with
      [] -> raise Not_found
    | [ x ] -> x
    | _ :: tail -> last tail

let _ =
  assert (last [1] = 1);
  assert (last [1;2;3;4] = 4);
  ()


(* Fabrice: [drop] and [take] fail when they receive a negative number.
Should they fail too when n is bigger than the list length ? Should we
provide alternatives ? *)

let drop n list =
  let rec aux n list =
    if n > 0 then
      match list with
          [] -> []
        | _ :: tail -> aux (n-1) tail
    else list
  in
  if n < 0 then invalid_arg "OcpList.drop";
  aux n list

let _ =
(*  assert (drop (-1) [1] = [1]); NOW FAILS *)
  assert (drop 0 [1] = [1]);
  assert (drop 3 [1;2;3;4] = [4]);
  assert (drop 3 [1;2;3] = []);
  ()

let take n l =
  let rec aux accu n l =
    if n = 0 then List.rev accu else
      match l with
          [] -> List.rev accu
        | h :: t ->
          aux (h :: accu) (n-1) t
  in
  if n < 0 then invalid_arg "OcpList.take";
  aux [] n l

let _ =
  assert (take 0 [ 1;2;3 ] = []);
  assert (take 1 [ 1;2;3 ] = [ 1 ]);
  assert (take 2 [ 1;2;3 ] = [ 1;2 ]);
  assert (take 3 [ 1;2;3 ] = [ 1;2;3 ]);
  assert (take 4 [ 1;2;3 ] = [ 1;2;3 ]);
  ()

let make n x =
  let rec aux accu n x =
    if n > 0 then
      aux (x :: accu) (n-1) x
    else accu
  in
  if n < 0 then invalid_arg "OcpList.make";
  aux [] n x

let _ =
  assert (make 0 1 = []);
  assert (make 1 1 = [ 1 ]);
  assert (make 2 1 = [ 1;1 ]);
  assert (make 3 1 = [ 1;1;1 ]);
  ()

(***********************************************************************)
(*                                                                     *)
(*                                                                     *)
(*                             DIRTY SECTION                           *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(* Fabrice: It is important to show that we are expert in OCaml, and
   that the code that we write is clean and efficient. It is
   unfortunately not the case of all the following functions.

   So, I divided the file between a CLEAN section and a DIRTY section.
   We should rewrite code from DIRTY to CLEAN and only document
   functions in the CLEAN section.
*)

(* (in [a] and not in [b]) or (in [b] and not in [a]) *)
(* [a] and [b] are sorted *)
let diff a b =
  let rec aux accu = function
    | h1::t1, h2::t2 when h1<h2 -> aux (h1::accu) (t1, h2::t2)
    | h1::t1, h2::t2 when h1>h2 -> aux (h2::accu) (h1::t1, t2)
    | h1::t1, h2::t2            -> aux accu (t1, t2)
    | [], l | l, []             -> (List.rev accu) @ l in
  aux [] (a, b)


(* in [a] and not in [b] *)
(* [a] and [b] are sorted *)
let sub a b =
  let rec aux accu = function
    | h1::t1, h2::t2 when h1<h2 -> aux (h1::accu) (t1, h2::t2)
    | h1::t1, h2::t2 when h1>h2 -> aux accu (h1::t1, t2)
    | h1::t1, h2::t2            -> aux accu (t1, t2)
    | [], l                     -> List.rev accu
    | l, []                     -> (List.rev accu) @ l in
  aux [] (a, b)

let take_while fn l =
  let rec aux accu = function
    | h::t when fn h -> aux (h::accu) t
    | _              -> List.rev accu in
  aux [] l

let drop_while fn l =
  let rec aux = function
    | h::t when fn h -> aux t
    | l              -> l in
  aux l

let setify l =
  let rec aux accu = function
    | []   -> accu
    | h::t -> if List.mem h accu then aux accu t else aux (h::accu) t in
  aux [] l

let setify_sorted l =
  let rec aux accu = function
    | []                   -> List.rev accu
    | h1::h2::t when h1=h2 -> aux accu (h2::t)
    | h1::t                -> aux (h1::accu) t in
  aux [] l

let union_set l1 l2 =
  let rec aux accu = function
    | h1::h2::t, l2 when h1=h2       -> aux accu (h2::t, l2)
    | l1, h1::h2::t when h1=h2       -> aux accu (l1, h2::t)
    | [], l | l, []                  -> List.rev accu @ setify_sorted l
    | (h1::t1 as l1), (h2::t2 as l2) ->
      if h1 = h2 then
        aux (h1::accu) (t1, t2)
      else if h1 < h2 then
        aux (h1::accu) (t1, l2)
      else
        aux (h2::accu) (l1, t2) in
  aux [] (l1, l2)

let inter_set l1 l2 =
  let rec aux accu = function
    | h1::h2::t, l2 when h1=h2       -> aux accu (h2::t, l2)
    | l1, h1::h2::t when h1=h2       -> aux accu (l1, h2::t)
    | [], _ | _, []                  -> List.rev accu
    | (h1::t1 as l1), (h2::t2 as l2) ->
      if h1=h2 then
        aux (h1::accu) (t1,t2)
      else if h1<h2 then
        aux accu (t1,l2)
      else
        aux accu (l1,t2) in
  aux [] (l1, l2)

let remove_all_assoc k l =
  let rec aux ll =
    if mem_assoc k ll then
      aux (remove_assoc k ll)
    else
      ll in
  aux l

let replace_assoc k v l =
  (k, v) :: remove_all_assoc k l

let assoc_all k l =
  List.fold_left (fun accu (kk,vv) -> if k=kk then vv::accu else accu) [] l

let rank x l =
  let rec aux i = function
    | []            -> raise Not_found
    | h::_ when h=x -> i
    | _::t          -> aux (i+1) t in
  aux 0 l

let filter_map fn l =
  let l = List.fold_left (fun accu s ->
    match fn s with
      | None -> accu
      | Some s -> s :: accu)
    [] l in
  List.rev l

let filter_opt l =
  filter_map (fun x -> x) l

let rec find_map fn = function
  | []   -> raise Not_found
  | h::t ->
      match fn h with
        | None   -> find_map fn t
        | Some x -> x

let flatten_map fn l =
  let l = List.fold_left (fun accu e -> List.rev (fn e) @ accu) [] l in
  List.rev l

let remove x l =
  List.filter ((<>)x) l

let removeq x l =
  List.filter ((!=)x) l

let prefixes l =
  let rec aux accu = function
    | []   -> accu
    | h::t -> aux ([h] :: (List.map (fun x -> h::x) accu)) t in
  aux [] (List.rev l)

let suffixes l =
  let rec aux accu = function
    | []   -> List.map List.rev accu
    | h::t -> aux ([h] :: (List.map (fun x -> h::x) accu)) t in
  aux [] l

exception Invalid_step

let range ?(step=1) i j =
  if step <= 0 then
    raise Invalid_step;
  let add_step x = if i > j then x - step else x + step in
  let stop x = if i > j then x < j else x > j  in
  let rec aux accu k =
    let n = add_step k in
    if stop n then
      List.rev accu
    else
      aux (n :: accu) n in
  aux [i] i

let filter_out fn l =
  filter (fun x -> not (fn x)) l

let intersperse x l =
  let rec aux accu = function
    | []        -> List.rev accu
    | [h]       -> if accu = [] then [h] else List.rev (h::x::accu)
    | h1::h2::t -> if accu = [] then aux [h1] (h2::t) else aux (h1::x::accu) (h2::t) in
  aux [] l

let intercalate ~pattern:x l =
  let rec aux accu = function
    | []        -> List.rev accu
    | [h]       -> if accu = [] then [h] else List.rev (h::x @ accu)
    | h1::h2::t -> if accu = [] then aux [h1] (h2::t) else aux (h1::x @ accu) (h2::t) in
  aux [] l

let rec inv_assoc x = function
  | [] -> raise Not_found
  | (a,b)::l -> if compare b x = 0 then a else inv_assoc x l

let rec inv_mem_assoc x = function
  | [] -> false
  | (a, b) :: l -> compare b x = 0 || inv_mem_assoc x l

(* Compatibility 3.11 -> 3.12 *)
let rec iteri i f = function
  | [] -> ()
  | a::l -> f i a; iteri (i + 1) f l

let iteri f l = iteri 0 f l

let rec mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l

let mapi f l = mapi 0 f l

let tail_map f list =
  List.rev (List.rev_map f list)
