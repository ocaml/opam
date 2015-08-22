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

open Cmdliner

(** {2 Commands} *)

(** Type of commands *)
type command = unit Term.t * Term.info

(** [run default commdands at_exit] build a binary which takes
    [commands] as subcommand and [default] as default argument
    (ie. which will be executed when no subcommand is
    given). [at_exit] is executed before the program exits. *)
val run:command -> command list -> unit

(** The default list of commands *)
val commands: command list

(** opam *)
val default: command

(** opam init *)
val init: command

(** opam list *)
val list: command

(** opam show *)
val show: command

(** opam search *)
val search: command

(** opam install *)
val install: command

(** opam remove *)
val remove: command

(** opam reinstall *)
val reinstall: command

(** opam update *)
val update: command

(** opam upgrade *)
val upgrade: command

(** opam config *)
val config: command

(** opam repository *)
val repository: command

(** opam switch *)
val switch: command

(** opam pin *)
val pin: ?unpin_only:bool -> unit -> command

(** opam help *)
val help: command

(** Create an alias for an existing command. [options] can be used to
    add extra options after the original command in the doc. *)
val make_command_alias:
  unit Term.t * Term.info -> ?options:string -> string -> unit Term.t * Term.info
