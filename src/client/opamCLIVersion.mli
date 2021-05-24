(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** CLI Versions *)

include OpamStd.ABSTRACT

(** The current version of the CLI (major and minor of OpamVersion.current *)
val current : t

(* Default CLI version, currently 2.0.
   This value is checked in CI. *)
val default : t

(** Tests whether a valid CLI version is supported by the client library *)
val is_supported : t -> bool

(** ['a option] version of {!to_string} *)
val of_string_opt : string -> t option
val of_string : string -> t

(** Comparison [>]] with [(major, minor)] *)
val ( >= ) : t -> int * int -> bool

(** Comparison [<] with [(major, minor)] *)
val ( < ) : t -> int * int -> bool

val compare : t -> t -> int

(** Returns previous supported version.
    @raise Not_found if there isn't one. *)
val previous: t -> t

(* CLI version extended with provenance *)
module Sourced : sig

  type nonrec t = t * OpamStateTypes.provenance

  (** The current version of the CLI (major and minor of OpamVersion.current *)
  val current : t

  (** Parse the given environment variable result as MAJOR.MINOR *)
  val env: string option -> t option

end

module Op : sig
  val (@<)  : Sourced.t -> t -> bool
  val (@=)  : Sourced.t -> t -> bool
  val (@>=) : Sourced.t -> t -> bool
end
