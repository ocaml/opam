(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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


(** {2 Helper functions to initialise config from the environment} *)

module Config : sig

  val env_bool: string -> bool option

  val env_int: string -> int option

  (* Like [env_int], but accept boolean values for 0 and 1 *)
  val env_level: string -> int option

  val env_string: string -> string option

  val env_float: string -> float option

  val env_when: string -> [ `Always | `Never | `Auto ] option

  val env_when_ext: string -> [ `Extended | `Always | `Never | `Auto ] option

  val resolve_when: auto:(bool Lazy.t) -> [ `Always | `Never | `Auto ] -> bool

  val command_of_string: string -> OpamTypes.arg list

end

(** Sets the OpamCoreConfig options, reading the environment to get default
    values when unspecified *)
val init_config: unit -> unit OpamCoreConfig.options_fun

(** {2 Some globally defined, static strings} *)

(** Name and version of the system compiler *)
val system: string

(** The configured-by-default OPAM repository name *)
val default_repository_name: string

(** The configured-by-default OPAM repository URL *)
val default_repository_address: string
