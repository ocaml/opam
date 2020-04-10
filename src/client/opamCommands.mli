(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Opam CLI main entry point *)

open Cmdliner

(** {2 Commands} *)

(** Type of commands *)
type command = unit Term.t * Term.info

(** The default list of commands *)
val commands: command list

(** opam *)
val default: command

(** opam init *)
val init: command

(** opam list *)
val list: ?force_search:bool -> unit -> command

(** opam show *)
val show: command

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
