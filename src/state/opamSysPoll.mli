(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStateTypes

(** This module polls various aspects of the host, to define the [arch], [os],
    etc. variables *)

module type IncludeT = functor (Runnable : OpamStd.OpamSysRunnableT) (Runner : OpamStd.Runner) -> sig
  val poll_arch : unit -> string option Runnable(Runner).R.t
  val arch : string option Runnable(Runner).R.t Lazy.t

  val poll_os : unit -> string option Runnable(Runner).R.t
  val os : string option Runnable(Runner).R.t Lazy.t

  val poll_os_version : unit -> string option Runnable(Runner).R.t
  val os_version : string option Runnable(Runner).R.t Lazy.t

  val poll_os_distribution : unit -> string option Runnable(Runner).R.t
  val os_distribution : string option Runnable(Runner).R.t Lazy.t

  val variables : (OpamVariable.t * OpamVariable.variable_contents option Runnable(Runner).R.t lazy_t) list
end

module Include' : IncludeT

(** Functions to get host specification. It checks if variables value is
    defined in the environment map before polling. *)
val arch: gt_variables -> string option
val os: gt_variables -> string option
val os_distribution: gt_variables -> string option
val os_version: gt_variables -> string option
val os_family: gt_variables -> string option

val variables: (OpamVariable.t * OpamTypes.variable_contents option Lazy.t) list

(** The function used internally to get our canonical names for architectures
    (returns its input lowercased if not a recognised arch). This is typically
    called on the output of [uname -m] *)
val normalise_arch: string -> string

(** The function used internally to get our canonical names for OSes (returns
    its input lowercased if not a recognised OS). This is typically called on
    the output of [uname -s] *)
val normalise_os: string -> string

(* Number of cores *)
val cores: unit -> int

(** Returns a string containing arch, os, os-distribution & os-version values,
    unknown if they are not available.
    [env] is used to determine host specification. *)
val to_string: gt_variables -> string
