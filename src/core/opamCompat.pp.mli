(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String
#if OCAML_VERSION >= (4, 13, 0)
= String
#else
: sig
  include module type of struct include String end

  val exists: (char -> bool) -> string -> bool
end
#endif

module Either
#if OCAML_VERSION >= (4, 12, 0)
= Either
#else
: sig
  type ('a, 'b) t =
  | Left of 'a
  | Right of 'b
end
#endif

module Unix : sig
  include module type of struct include Unix end

  (* `realpath` for OCaml >= 4.13.0,
     implementation with double chdir otherwise *)
  val normalise: string -> string
end

module Lazy
#if OCAML_VERSION >= (4, 13, 0)
= Lazy
#else
: sig
  include module type of struct include Lazy end

  val map : ('a -> 'b) -> 'a t -> 'b t
end
#endif
