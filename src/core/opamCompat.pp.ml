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
#if OCAML_VERSION >= (4, 13, 0)
  String
#else
struct
  include String

  let exists p s =
    let n = length s in
    let rec loop i =
      if i = n then false
      else if p (unsafe_get s i) then true
      else loop (succ i) in
    loop 0
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

module Unix =
struct
  include Unix

  #if OCAML_VERSION >= (4, 13, 0)
  let normalise = realpath
  #else
  let normalise s =
    let getchdir s =
      let p =
        try Sys.getcwd ()
        with Sys_error _ -> Filename.get_temp_dir_name ()
      in
      Unix.chdir s;
      p
    in
    try getchdir (getchdir s) with Unix.Unix_error _ -> s
  #endif

end

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
