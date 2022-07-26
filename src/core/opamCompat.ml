(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String =
#if OCAML_VERSION >= (4, 3, 0)
  String
#else
struct
  include String

  let lowercase_ascii = lowercase
  let uppercase_ascii = uppercase
  let capitalize_ascii = capitalize
end
#endif

module Char =
#if OCAML_VERSION >= (4, 3, 0)
  Char
#else
struct
  include Char

  let lowercase_ascii = lowercase
  let uppercase_ascii = uppercase
end
#endif

module Either =
#if OCAML_VERSION >= (4, 12, 0)
  Either
#else
struct
  type ('a, 'b) t =
  | Left of 'a
  | Right of 'b
end
#endif

module Printexc =
#if OCAML_VERSION >= (4, 5, 0)
  Printexc
#else
struct
  include Printexc

  let raise_with_backtrace e _bt = raise e
end
#endif

module Unix =
#if OCAML_VERSION >= (4, 6, 0)
  Unix
#else
struct
  include Unix

  let map_file = Bigarray.Genarray.map_file
end
#endif

module Uchar =
#if OCAML_VERSION >= (4, 3, 0)
  Uchar
#else
struct
  type t = int

  let of_int i = i
  external to_int : t -> int = "%identity"
end
#endif

module Buffer =
#if OCAML_VERSION >= (4, 6, 0)
  Buffer
#else
struct
  include Buffer

  let add_utf_8_uchar b u = match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0x007F ->
      add_char b (Char.unsafe_chr u)
  | u when u <= 0x07FF ->
      add_char b (Char.unsafe_chr (0xC0 lor (u lsr 6)));
      add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
  | u when u <= 0xFFFF ->
      add_char b (Char.unsafe_chr (0xE0 lor (u lsr 12)));
      add_char b (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
      add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
  | u when u <= 0x10FFFF ->
      add_char b (Char.unsafe_chr (0xF0 lor (u lsr 18)));
      add_char b (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
      add_char b (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
      add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
  | _ -> assert false
end
#endif

module Filename =
#if OCAML_VERSION >= (4, 4, 0)
  Filename
#else
struct
  include Filename

  let extension fn =
    match Filename.chop_extension fn with
    | base ->
        let l = String.length base in
        String.sub fn l (String.length fn - l)
    | exception Invalid_argument _ ->
        ""
end
#endif

module Result =
#if OCAML_VERSION >= (4, 8, 0)
  Result
#else
struct
  type ('a, 'e) t
#if OCAML_VERSION >= (4, 3, 0)
    = ('a, 'e) result
#endif
    = Ok of 'a | Error of 'e
end
#endif

#if OCAML_VERSION < (4, 7, 0)
module Stdlib = Pervasives
#endif

module Lazy =
#if OCAML_VERSION >= (4, 13, 0)
  Lazy
#else
struct
  include Lazy

  let map f x =
    lazy (f (force x))
end
#endif
