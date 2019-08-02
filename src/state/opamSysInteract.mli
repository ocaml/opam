(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* Given a list of system packages, retrieve their installation status from the
   system and returns it: [`available] is for a package that exists on the
   default repostiories, but not installed, and [`not_found] for package not
   found on the defined repositories *)
val packages_status: OpamSysPkg.Set.t -> [`available | `installed | `not_found ] OpamSysPkg.Map.t

(* Return the commands to run to install given system packages *)
val install_packages_commands: OpamSysPkg.Set.t -> string list list

(* Install given system packages, by calling local system package manager *)
val install: OpamSysPkg.Set.t -> unit

val update: unit -> unit
