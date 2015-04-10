(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

(** Some detection of OCaml version and installation specifics. Base functions
    lookup in the PATH, [system_*] functions extract the OPAMROOT paths before
    looking up*)

val ocaml_version: string option Lazy.t
val ocaml_opt_available: bool Lazy.t
val ocaml_native_available: bool Lazy.t
val ocaml_natdynlink_available: bool Lazy.t
val system_ocamlc_version: string option Lazy.t
val system_ocamlc_where: string option Lazy.t
val system_compiler: OpamCompiler.t option Lazy.t

(** Extra files included in `opam search\ *)

val search_files: string list
