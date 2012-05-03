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
