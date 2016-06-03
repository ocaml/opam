(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** OCaml installation version and specifics detection *)

(** Some detection of OCaml version and installation specifics. Base functions
    lookup in the PATH, [system_*] functions extract the OPAMROOT paths before
    looking up*)

val where_is_ocamlc: string option Lazy.t
val ocaml_version: string option Lazy.t
val ocaml_opt_available: bool Lazy.t
val ocaml_native_available: bool Lazy.t
val ocaml_natdynlink_available: bool Lazy.t
val system_ocamlc_version: string option Lazy.t
val system_ocamlc_where: string option Lazy.t
