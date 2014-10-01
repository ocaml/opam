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

(** Configuration commands *)

open OpamTypes

(** Display the current environment. Booleans csh, sexp and fish set an alternative
    output (unspecified if more than one is true, sh-style by default).
    [inplace_path] changes how the PATH variable is updated when there is already
    an opam entry: either at the same rank, or pushed in front. *)
val env: csh:bool -> sexp:bool -> fish:bool -> inplace_path:bool -> unit

(** Display the content of all available variables *)
val list: name list -> unit

(** Display the content of a given variable *)
val variable:  full_variable -> unit

(** Substitute files *)
val subst: basename list -> unit

(** Update the global and user configuration to use OPAM. *)
val setup: user_config option -> global_config option -> unit

(** Display the global and user configuration for OPAM. *)
val setup_list: shell -> filename -> unit

(** Execute a command in a subshell *)
val exec: inplace_path:bool -> string list -> unit
