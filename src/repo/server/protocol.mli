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

(** Message exchanged between the client and the server *)
type client_to_server =
  | ClientVersion of string (* client version *)
  | GetList
  | GetOPAM    of string (* name *) * string (* version *)
  | GetDescr   of string (* name *) * string (* version *)
  | GetArchive of string (* name *) * string (* version *)
  | NewPackage of string (* name *) * string (* version *)
                * string (* opam *) * string (* descr *)
                * string (* archive *)
  | NewVersion of string (* name *) * string (* version *)
                * string (* opam *) * string (* descr *)
                * string (* archive *) * string (* key *)

(** Message exchanged between the server and the client *)
type server_to_client =
  | ServerVersion of string (* server version *)
  | PackageList   of (string (* name *) * string (* version *)) list
  | OPAM          of string (* opam *)
  | Descr         of string (* descr *)
  | Archive       of string (* archive *)
  | Key           of string (* key *)
  | OK
  | Error         of string (* server error *)

(** Default port *)
val default_port: int

(** Client synchronous processing:
    - send a message to the server
    - wait for the server response *)
val process_client:
  (in_channel * out_channel) ->
  client_to_server -> server_to_client

(** Server synchronous processing:
    - wait for a client message to come
    - answer to the client *)
val process_server:
  (in_channel * out_channel) ->
  (client_to_server -> server_to_client) -> unit

