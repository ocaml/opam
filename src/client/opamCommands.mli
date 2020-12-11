(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Opam CLI main entry point *)

(** {2 Commands} *)

(** [is_builtin_command arg] is [true] if [arg] is a prefix of any built-in
    command *)
val is_builtin_command: string -> bool

(** [is_admin_subcommand arg] is [true] if [arg] is a unique prefix of the admin
    sub-command. *)
val is_admin_subcommand: string -> bool

val get_cmdliner_parser: OpamCLIVersion.t -> OpamArg.command * OpamArg.command list
