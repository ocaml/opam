(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Given a list of system packages, retrieve their installation status from the
   system and returns a pair of [sys_package] set:
     * first one is available set: package that exist on the default
       repositories, but not installed)
     * second one, not found set: packages not found on the defined repositories
*)
val packages_status:
  OpamSysPkg.Set.t -> OpamSysPkg.Set.t * OpamSysPkg.Set.t

(* Return the commands to run to install given system packages *)
val install_packages_commands: OpamSysPkg.Set.t -> (string * string list) list

(* Install given system packages, by calling local system package manager *)
val install: OpamSysPkg.Set.t -> unit

val update: unit -> unit

(* Determine if special packages may need installing to enable other
   repositories.
   Presently used to check for epel-release on CentOS and RHEL. *)
val repo_enablers : unit -> string option
