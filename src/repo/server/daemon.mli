(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Server daemon *)

open Protocol

(** Initialize the server state *)
val init: unit -> unit

(** Main request processing function. [process_request id req]
    processes the client request [req] and procuces a server
    answer. Eventual log messages are tagged with [id]. *)
val process_request: string -> client_to_server -> server_to_client

(** Synchronous processing of client requests. [process channels fn]
    will read incoming requests on channels, compute the server
    response using [fn] and write the result to the channels. *)
val process: (in_channel * out_channel) -> (client_to_server -> server_to_client) -> unit
