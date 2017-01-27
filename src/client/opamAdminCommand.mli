(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2017 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val admin_command_doc: string

val admin_subcommands: (unit Cmdliner.Term.t * Cmdliner.Term.info) list

val default_subcommand: unit Cmdliner.Term.t * Cmdliner.Term.info
