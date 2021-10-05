(**************************************************************************)
(*                                                                        *)
(*    Copyright 2022 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module SWHO = Swhid_core.Object
module SWH_ID = SWHO.Core_identifier
module SWH_Compute = Swhid_core.Compute

type t = SWH_ID.t

let compare = SWH_ID.compare
let equal a b = compare a b = 0

let of_string_opt s =
  match SWH_ID.of_string s with
  | Ok i -> Some i
  | Error _ -> None
let of_string s =
  match of_string_opt s with
  | None -> invalid_arg "OpamSWHSWH_ID.of_string"
  | Some i -> i
let to_string = SWH_ID.to_string

let to_json s = `String (to_string s)
let of_json = function
  | `String s -> of_string_opt s
  | _ -> None

let hash s = SWHO.Hash.to_string (SWH_ID.get_hash s)

module O = struct
  type nonrec t = t
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = SWH_ID.compare
end

module Set = OpamStd.Set.Make(O)
module Map = OpamStd.Map.Make(O)


(** Url handling *)

let prefix = "swhid.opam.ocaml.org/"

let is_valid url =
  let open OpamUrl in
  url.backend = `http
  && (String.equal url.transport "http" || String.equal url.transport "https")
  && OpamStd.String.starts_with ~prefix url.path

let of_url url =
  try
    Some (of_string (OpamStd.String.remove_prefix ~prefix url.OpamUrl.path))
  with Invalid_argument _ -> None

let to_url swh =
  let path = Printf.sprintf "%s%s" prefix (to_string swh) in
  OpamUrl.{ transport = "https"; backend = `http ; hash = None; path }

(** Identifier computing *)

module SHA1 = struct
  let digest_string_to_hex = OpamSHA.sha1_string
end
module OS = struct

  let contents dir =
    try Some (OpamSystem.ls dir)
    with _ -> None

  let typ name =
    try
      Some (if Sys.is_directory name then SWH_Compute.Dir else SWH_Compute.File)
    with _ -> None

  let read_file name =
    try Some (OpamSystem.read name)
    with OpamSystem.File_not_found _ -> None

    (*
      - [0o120000] if [f] is a symlink
      - [0o040000] if [f] is a directory
      - [0o100755] if [f] is an executable file
      - [0o100644] if [f] is a regular file *)
  let permissions name =
    if not (Sys.file_exists name) then raise Not_found else
    let { Unix.st_kind; _ } = Unix.lstat name in
    match st_kind with
    | Unix.S_DIR -> Some 0o040000
    | Unix.S_LNK -> Some 0o120000
    | Unix.S_REG ->
      Some (if OpamSystem.is_exec name then 0o100755 else 0o100644)
    | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK -> None

  let base name = OpamFilename.(Base.to_string (basename (of_string name)))

end

module Compute = SWH_Compute.Make(SHA1)(OS)

let compute dir =
  match Compute.directory_identifier_deep
          (OpamFilename.Dir.to_string dir) with
  | Error _ ->  None
  | Ok identifier -> Some (SWHO.Hash.to_string (SWHO.get_hash identifier))
