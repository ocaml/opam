(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
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

open OpamTypes

(** Display all available packages that matches any of the
    regexps. *)
val list:
  print_short:bool ->
  filter:[`all|`installed|`roots|`installable] ->
  order:[`normal|`depends] ->
  exact_name:bool ->
  case_sensitive:bool ->
  ?depends:(atom list) ->
  ?reverse_depends:bool -> ?recursive_depends:bool -> ?resolve_depends:bool ->
  ?depopts:bool -> ?depexts:string list -> ?dev:bool ->
  string list ->
  unit

(** Display a general summary of a collection of packages. *)
val info: fields:string list -> raw_opam:bool -> where:bool -> atom list -> unit
