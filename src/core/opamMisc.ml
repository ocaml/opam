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

module type SET = sig
  include Set.S
  val map: (elt -> elt) -> t -> t
  val choose_one : t -> elt
  val of_list: elt list -> t
  val to_string: t -> string
  val find: (elt -> bool) -> t -> elt
end
module type MAP = sig
  include Map.S
  val to_string: ('a -> string) -> 'a t -> string
  val values: 'a t -> 'a list
  val keys: 'a t -> key list
  val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val of_list: (key * 'a) list -> 'a t
end
module type ABSTRACT = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
end

let string_of_list f = function
  | [] -> "{}"
  | l  -> Printf.sprintf "{ %s }" (String.concat ", " (List.map f l))

module Set = struct

  module Make (O : OrderedType) = struct

    module S = Set.Make(O)

    include S

    let choose_one s =
      match elements s with
        | [x] -> x
        | [] -> raise Not_found
        | _  -> invalid_arg "choose_one"

    let of_list l =
      List.fold_left (fun set e -> add e set) empty l

    let to_string s =
      let l = fold (fun nv l -> O.to_string nv :: l) s [] in
      string_of_list (fun x -> x) l

    let map f t =
      of_list (List.map f (elements t))

    let find fn s =
      choose (filter fn s)

  end

end

module Map = struct

  module Make (O : OrderedType) = struct

    module M = Map.Make(O)

    include M

    let values map = List.map snd (bindings map)

    let keys map = List.map fst (bindings map)

    let union f m1 m2 =
      M.fold (fun k v m ->
        if M.mem k m then
          M.add k (f v (M.find k m)) (M.remove k m)
        else
          M.add k v m
      ) m1 m2

    let to_string string_of_value m =
      let s (k,v) = Printf.sprintf "%s:%s" (O.to_string k) (string_of_value v) in
      let l = fold (fun k v l -> s (k,v)::l) m [] in
      string_of_list (fun x -> x) l

    let of_list l =
      List.fold_left (fun map (k,v) -> add k v map) empty l

  end

end

module Base = struct
  type t = string
  let of_string x = x
  let to_string x = x
  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end

let filter_map f l =
  let rec loop accu = function
    | []     -> List.rev accu
    | h :: t ->
        match f h with
        | None   -> loop accu t
        | Some x -> loop (x::accu) t in
  loop [] l

module OInt = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module IntMap = Map.Make(OInt)
module IntSet = Set.Make(OInt)

module OString = struct
  type t = string
  let compare = compare
  let to_string x = x
end

module StringSet = Set.Make(OString)
module StringMap = Map.Make(OString)

module OP = struct

  let (|>) f g x = g (f x)

  let finally f clean =
    let safe_clean () =
      try clean ()
      with _ -> () in
    let result =
      try f ()
      with e ->
        safe_clean ();
        raise e in
    safe_clean ();
    result

end

let strip str =
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

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  String.length s >= String.length suffix
  && String.sub s (String.length s - String.length suffix) (String.length suffix) = suffix

let remove_prefix ~prefix s =
  if starts_with ~prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else
    None

let cut_at_aux fn s sep =
  try
    let i = fn s sep in
    let name = String.sub s 0 i in
    let version = String.sub s (i+1) (String.length s - i - 1) in
    Some (name, version)
  with _ ->
    None

let cut_at = cut_at_aux String.index

let rcut_at = cut_at_aux String.rindex

let contains s c =
  try let _ = String.index s c in true
  with Not_found -> false

let split s c =
  Pcre.split ~rex:(Re_perl.compile (Re.char c)) s

(* Remove from a ':' separated list of string the one with the given prefix *)
let reset_env_value ~prefix v =
  let v = split v ':' in
  List.filter (fun v -> not (starts_with ~prefix v)) v

(* if rsync -arv return 4 lines, this means that no files have changed *)
let rsync_trim = function
  | [] -> []
  | _ :: t ->
      match List.rev t with
      | _ :: _ :: _ :: l -> List.filter ((<>) "./") l
      | _ -> []

let exact_match re s =
  try
    let subs = Re.exec re s in
    let subs = Array.to_list (Re.get_all_ofs subs) in
    let n = String.length s in
    let subs = List.filter (fun (s,e) -> s=0 && e=n) subs in
    List.length subs > 0
  with Not_found ->
    false

(* XXX: not optimized *)
let insert comp x l =
  let rec aux = function
    | [] -> [x]
    | h::t when comp h x < 0 -> h::aux t
    | l -> x :: l in
  aux l

let env = lazy (
  let e = Unix.environment () in
  List.map (fun s ->
    match cut_at s '=' with
    | None   -> s, ""
    | Some p -> p
  ) (Array.to_list e)
)

let getenv n =
  List.assoc n (Lazy.force env)

let env () = Lazy.force env

let indent_left s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    s ^ String.make nb ' '

let indent_right s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    String.make nb ' ' ^ s

let sub_at n s =
  if String.length s <= n then
    s
  else
    String.sub s 0 n

let git_of_string a =
  match cut_at a '#' with
  | None       -> a, None
  | Some (a,c) -> a, Some c

let pretty_backtrace () =
  match Printexc.get_backtrace () with
  | "" -> ""
  | b  ->
    let b = String.concat "\n  " (split b '\n') in
    Printf.sprintf "Backtrace:\n  %s\n" b
