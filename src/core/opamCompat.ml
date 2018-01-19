(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 OCamlPro                                             *)
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
