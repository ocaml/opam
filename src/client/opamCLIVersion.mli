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

(** Tests whether a valid CLI version is supported by the client library *)
val is_supported : t -> bool

(** Parse the given environment variable as MAJOR.MINOR *)
val env: OpamStd.Config.env_var -> t option

(** ['a option] version of {!to_string} *)
val of_string_opt : string -> t option

(** Comparison [>]] with [(major, minor)] *)
val ( >= ) : t -> int * int -> bool

(** Comparison [<] with [(major, minor)] *)
val ( < ) : t -> int * int -> bool

(** Returns previous supported version.
    @raise Not_found if there isn't one. *)
val previous: t -> t
