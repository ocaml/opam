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

open Namespace
open Path

type client_to_server =
  | IacceptedVersion of string
  | IgetList
  | IgetOpam of name_version
  | IgetArchive of name_version
  | InewArchive of name_version * binary_data * binary_data archive
  | IupdateArchive of name_version * binary_data * binary_data archive * security_key

type server_to_client =
  | OacceptedVersion of bool
  | OgetList of name_version list
  | OgetOpam of binary_data
  | OgetArchive of binary_data archive
  | OnewArchive of security_key option
  | OupdateArchive of bool

  | Oerror of string (* server error *)

type www = client_to_server -> server_to_client

module type PROTOCOL = sig
  val find : in_channel * out_channel -> www
  val add : www -> in_channel -> out_channel -> unit
end

module Protocol : PROTOCOL = struct

  let output_v stdout m = 
    begin
      output_value stdout m ;
      flush stdout ;
    end

  let find (stdin, stdout) m =
    let () = output_v stdout m in
    (input_value stdin : server_to_client)

  let add f stdin stdout =
    output_v stdout (f (input_value stdin : client_to_server))

end
