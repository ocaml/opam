(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let filter_map f l =
  let rec loop accu = function
    | []     -> List.rev accu
    | h :: t ->
	match f h with
	| None   -> loop accu t
	| Some x -> loop (x::accu) t in
  loop [] l

let map_of_list empty add l =
  List.fold_left (fun map (k,v) -> add k v map) empty l

module IntMap = Map.Make(struct type t = int let compare = compare end)  
module IntSet = Set.Make(struct type t = int let compare = compare end)  

let (|>) f g x = g (f x) 

let string_strip str =
  let p = ref 0 in
  let l = String.length str in
  let fn = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false in 
  while !p < l && fn (String.unsafe_get str !p) do
    incr p;
  done;
  let p = !p in
  let l = ref (l - 1) in
  while !l >= p && fn (String.unsafe_get str !l) do
    decr l;
  done;
  String.sub str p (!l - p + 1)

let is_inet_address address =
  try
    let (_:Unix.inet_addr) = Unix.inet_addr_of_string address
    in true
  with _ -> false

module Set = struct
  module type S = sig
    include Set.S

    (* Like [choose] and [Assert_failure _] in case the set is not a singleton. *)
    val choose_one : t -> elt
  end

  module MK (S : Set.S) = struct
    include S

    let choose_one s = 
      match elements s with
        | [x] -> x
        | [] -> raise Not_found
        | _ -> assert false
  end

  module Make (O : Set.OrderedType) = struct
    include MK (Set.Make (O))
  end
end
