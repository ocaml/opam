(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Gc = struct
  [@@@warning "-32"]

  let ramp_up f = (f (), ())

  include Gc

  let ramp_up f = fst (ramp_up f)
end

module String = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let exists p s =
    let n = String.length s in
    let rec loop i =
      if i = n then false
      else if p (String.unsafe_get s i) then true
      else loop (succ i) in
    loop 0

  (** NOTE: OCaml >= 4.13 *)
  let starts_with ~prefix s =
    let x = String.length prefix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || prefix.[i] = s.[i] && chk (i+1) in
    chk 0

  (** NOTE: OCaml >= 4.13 *)
  let ends_with ~suffix s =
    let x = String.length suffix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || suffix.[i] = s.[i+n-x] && chk (i+1) in
    chk 0

  (** NOTE: OCaml >= 4.13 *)
  let for_all f s =
    let len = String.length s in
    let rec aux i = i >= len || f s.[i] && aux (i+1) in
    aux 0

  (** NOTE: OCaml >= 4.13 *)
  let fold_left f acc s =
    let acc = ref acc in
    for i = 0 to String.length s - 1 do acc := f !acc s.[i] done;
    !acc

  include Stdlib.String
end

module Seq = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.14 *)
  let rec find_map f xs =
    match xs() with
    | Seq.Nil ->
      None
    | Seq.Cons (x, xs) ->
      match f x with
      | None ->
          find_map f xs
      | Some _ as result ->
          result

  (** NOTE: OCaml >= 4.14 *)
  let to_dispenser seq =
    let r = ref seq in
    fun () ->
      match !r () with
      | Seq.Nil -> None
      | Seq.Cons (x, xs) -> r := xs; Some x

  include Seq
end

module Either = struct
  (** NOTE: OCaml >= 4.12 *)
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module Unix = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let realpath s =
    let orig_cwd = Sys.getcwd () in
    Unix.chdir s;
    let r = Unix.getcwd () in
    Sys.chdir orig_cwd;
    r

  include Unix
end

module Lazy = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let map f x =
    lazy (f (Lazy.force x))

  (** NOTE: OCaml >= 4.13 *)
  let map_val f x =
    if Lazy.is_val x
    then Lazy.from_val (f (Lazy.force x))
    else lazy (f (Lazy.force x))

  include Stdlib.Lazy
end

module List = struct
  [@@@warning "-32"]

  (* NOTE: OCaml >= 4.12 *)
  let rec equal eq x y = match x, y with
    | [], [] -> true
    | [], _::_ | _::_, [] -> false
    | x::_, y::_ when eq x y -> true
    | _::xs, _::ys -> equal eq xs ys

  include Stdlib.List
end

module type MAP = sig
  include Stdlib.Map.S

  (** NOTE: OCaml >= 5.1 *)
  val add_to_list: key -> 'a -> 'a list t -> 'a list t
end

module Map(Ord : Stdlib.Map.OrderedType) = struct
  [@@@warning "-32"]

  module M = Stdlib.Map.Make(Ord)

  (** NOTE: OCaml >= 5.1 *)
  let add_to_list x data m =
    let add = function None -> Some [data] | Some l -> Some (data :: l) in
    M.update x add m

  include M
end

module Pair = struct
  (** NOTE: OCaml >= 5.4 *)
  let equal eq1 eq2 (x1, y1) (x2, y2) =
    eq1 x1 x2 && eq2 y1 y2
end

module Int = struct
  [@@@warning "-32"]

  (* NOTE: OCaml >= 4.13 *)
  let min : int -> int -> int = Stdlib.min

  include Stdlib.Int
end

module Sys = struct
  [@@@warning "-32"]

  (* NOTE: OCaml >= 5.4 *)
  let sigwinch = 28

  include Stdlib.Sys
end
