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

module Lib =
struct
  module Cudf_types = struct type pkgname = string end

  module Debian = struct
    module Format822 = struct type version = string end
  end
end

open Lib

type name = Name of Cudf_types.pkgname

type version =
  | Deb of Debian.Format822.version
  | Head of [`uptodate|`behind] (* Head of a version controled repository *)

type name_version = name * version

type raw_binary = 
  | Raw_binary of string (* contents *)

type security_key = Random of string

type client_to_server =
  | C2S_apidVersion   of int
  | C2S_getList
  | C2S_getSpec       of name_version
  | C2S_getArchive    of name_version
  | C2S_newArchive    of name_version * raw_binary * raw_binary
  | C2S_updateArchive of name_version * raw_binary * raw_binary * security_key

type server_to_client =
  | S2C_apiVersion      of int
  | S2C_getList         of name_version list
  | S2C_getSpec         of raw_binary
  | S2C_getArchive      of raw_binary option
  | S2C_newArchive      of security_key
  | S2C_updateArchive

  | S2C_error of string (* server error *)

type www = client_to_server -> server_to_client

module type PROTOCOL = sig
  val find : in_channel * out_channel -> www
  val add : www -> in_channel -> out_channel -> unit
end

module Protocol : PROTOCOL = struct

  let output_v stdout m = 
    output_value stdout m ;
    flush stdout

  let find (stdin, stdout) m =
    output_v stdout m;
    (input_value stdin : server_to_client)

  let add f stdin stdout =
    output_v stdout (f (input_value stdin : client_to_server))

end
