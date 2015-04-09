(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
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

(** Toggles parsing of the tool's output to detect errors
    (curl returns 0 on a 404) *)
type dl_tool_kind = [ `Curl | `Default ]

type t = {
  download_tool: (OpamTypes.arg list * dl_tool_kind) Lazy.t;
  retries: int;
  force_checksums: bool option;
}

type 'a options_fun =
  ?download_tool:(OpamTypes.arg list * dl_tool_kind) Lazy.t ->
  ?retries:int ->
  ?force_checksums:bool option ->
  unit -> 'a

val default : t

val set : t -> t options_fun

val setk : (t -> 'a) -> (unit -> t) -> 'a options_fun

val r : t ref

val update : unit options_fun
