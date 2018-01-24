(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String
#if OCAML_VERSION >= (4, 3, 0)
= String
#else
: sig
  include module type of String

  val lowercase_ascii : string -> string
  val uppercase_ascii : string -> string
  val capitalize_ascii : string -> string
end
#endif

module Char
#if OCAML_VERSION >= (4, 3, 0)
= Char
#else
: sig
  include module type of Char

  val lowercase_ascii: char -> char
end
#endif

module Printexc
#if OCAML_VERSION >= (4, 5, 0)
= Printexc
#else
: sig
  include module type of Printexc

  val raise_with_backtrace: exn -> raw_backtrace -> 'a
end
#endif

module Unix
#if OCAML_VERSION >= (4, 6, 0)
= Unix
#else
: sig
  include module type of Unix

  val map_file : Unix.file_descr -> ?pos:int64 -> ('a, 'b) Bigarray.kind ->
                 'c Bigarray.layout -> bool -> int array ->
                 ('a, 'b, 'c) Bigarray.Genarray.t
end
#endif

module Uchar
#if OCAML_VERSION >= (4, 3, 0)
= Uchar
#else
: sig
  type t

  val of_int : int -> t
  external to_int : t -> int = "%identity"
end
#endif

module Buffer
#if OCAML_VERSION >= (4, 6, 0)
= Buffer
#else
: sig
  include module type of struct include Buffer end

  val add_utf_8_uchar : t -> Uchar.t -> unit
end
#endif
