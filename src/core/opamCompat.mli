(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include sig
  [@@@warning "-33"]
  open OpamCompatPolyfill
  open Stdlib

  module String : sig
    include module type of struct include String end
    val exists: (char -> bool) -> string -> bool
  end

  module Either : sig
    type ('a, 'b) t =
      | Left of 'a
      | Right of 'b
  end

  module Lazy : sig
    include module type of struct include Lazy end
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module Unix : sig
    include module type of struct include Unix end

    (* `realpath` for OCaml >= 4.13.0,
       implementation with double chdir otherwise *)
    val realpath: string -> string
  end
end

