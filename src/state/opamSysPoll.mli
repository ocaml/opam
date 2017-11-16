(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This module polls various aspects of the host, to define the [arch], [os],
    etc. variables *)

val arch: unit -> string option
val os: unit -> string option
val os_distribution: unit -> string option
val os_version: unit -> string option
val os_family: unit -> string option

val variables: (OpamVariable.t * OpamTypes.variable_contents option Lazy.t) list
