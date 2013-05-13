(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

(** OPAM versions *)

include OpamMisc.ABSTRACT

(** The current OPAM version *)
val current: t

(** The 'git' version of OPAM *)
val git: t option

(** The full version (current + git) *)
val full: t

(** Magic string *)
val magic: string

(** Display the version message *)
val message: unit -> unit

(** Version comparison *)
val compare: t -> t -> int
