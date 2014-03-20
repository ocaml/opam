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

(** Pin subcommand. *)

open OpamTypes

(** Pin a package. Returns true if the package should be rebuilt *)
val pin: name -> ?edit:bool -> pin_option -> bool

(** Let the user edit a pinned package's opam file.
    Returns true if the package should be rebuilt *)
val edit: name -> bool

(** Unpin a package. Returns true if the package should be rebuilt *)
val unpin: name -> bool

(** List the pinned packages. *)
val list: unit -> unit
