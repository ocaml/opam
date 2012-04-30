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

module type RANDOM_KEY = sig
  type t
  val new_key : unit -> t
  val to_string : t -> string
end

module Random_key : RANDOM_KEY = struct
  type t = string

  let make n =
    String.implode (List.init n (fun _ -> char_of_int (Random.int 255)))

  let len = 128

  let n = ref (make len)

  let new_key _ = 
    let k = !n in
    let () = n := make len in
    k

  let to_string x = x
end















