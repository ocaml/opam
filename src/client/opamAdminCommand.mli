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

type command = unit Cmdliner.Term.t * Cmdliner.Cmd.info

val get_cmdliner_parser: OpamCLIVersion.Sourced.t -> command * command list
