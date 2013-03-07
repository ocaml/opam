(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Configuration commands *)

open OpamTypes

(** Display the current environment *)
val env: csh:bool -> unit

(** Display the content of all available variables *)
val list: name list -> unit

(** Display the content of a given variable *)
val variable:  full_variable -> unit

(** Display includes directives *)
val includes: is_rec:bool -> name list -> unit

(** Display configuration options *)
val config: config -> unit

(** Substitute files *)
val subst: basename list -> unit

(** Update the global and user configuration to use OPAM. *)
val setup: user_config option -> global_config option -> unit

(** Display the global and user configuration for OPAM. *)
val setup_list: shell -> filename -> unit

(** Execute a command in a subshell *)
val exec: string -> unit
