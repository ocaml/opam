(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

val clear_cache: unit -> unit

val upgradeto_version: OpamVersion.t

val do_upgrade: OpamTypes.dirname -> unit

val do_upgrade_mirror: OpamTypes.dirname -> OpamUrl.t -> unit
