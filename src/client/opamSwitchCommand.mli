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

(** Switch commands. *)

open OpamTypes

(** Install a new switch. *)
val install: quiet:bool -> warning:bool -> switch -> compiler -> unit

(** Import a file which contains the packages to install. *)
val import: filename option -> unit

(** Export a file which contains the installed packages. *)
val export: filename option -> unit

(** Remove the given compiler switch. *)
val remove: switch -> unit

(** Switch to the given compiler switch. *)
val switch: quiet:bool -> warning:bool -> switch -> unit

(** Reinstall the given compiler switch. *)
val reinstall: switch -> unit

(** Display the current compiler switch. *)
val show: unit -> unit

(** List all the available compiler switches. *)
val list: print_short:bool -> installed_only:bool -> unit

