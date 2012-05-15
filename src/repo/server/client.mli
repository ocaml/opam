(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Client-side for OPAM server repositories *)

open Types

(** Get the list of available packages *)
val get_list: Unix.inet_addr -> NV.Set.t

(** Get an OPAM file *)
val get_opam: Unix.inet_addr -> nv -> File.OPAM.t

(** Get a desrciption file *)
val get_descr: Unix.inet_addr -> nv -> File.Descr.t

(** Get an archive file *)
val get_archive: Unix.inet_addr -> nv -> raw

(** Upload a new package *)
val new_package: Unix.inet_addr -> File.OPAM.t -> File.Descr.t -> raw -> Key.t

(** Upload a new package version *)
val new_version: Unix.inet_addr -> File.OPAM.t -> File.Descr.t -> raw -> Key.t -> unit
