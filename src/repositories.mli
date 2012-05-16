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

(** Generic repository pluggin *)

(** The following functions are wrapper to the corresponding
    scripts *)

open Types

(** Run {i opam-$kind-init} in {i $opam/repo/$repo} *)
val init: repository -> unit

(** Run {i opam-$kind-update} in {i $opam/repo/$repo} *)
val update: repository -> unit

(** Run {i opam-$kind-download} in {i $opam/repo/$repo} *)
val download: repository -> nv -> unit

(** Run {i opam-$kind-upload} in {i $opam/repo/$repo} *)
val upload: repository -> unit
