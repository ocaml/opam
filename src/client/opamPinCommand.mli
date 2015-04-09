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

(** Pin subcommand. *)

open OpamTypes

(** Pin a package. Returns [Some is_same_version] if the package should be
    reinstalled (or upgraded if [is_same_version] is false) *)
val pin: name -> ?version:version -> pin_option -> bool option

(** Let the user edit a pinned package's opam file.
    Returns [Some is_same_version] if the package should be rebuilt.
    raises [Not_found] if no valid opam file is available and the user didn't
    succeed in producing one. *)
val edit: OpamState.state -> name -> bool option

(** Unpin packages. Returns the list of packages that should be rebuilt *)
val unpin: ?state:OpamState.state -> name list -> name list

(** List the pinned packages. *)
val list: short:bool -> unit -> unit
