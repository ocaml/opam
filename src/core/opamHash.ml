(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCompat

type kind = [ `MD5 | `SHA256 | `SHA512 ]

let default_kind = `MD5

type t = kind * string

let kind = fst
let contents = snd

let pfx_sep_char = '='
let pfx_sep_str = String.make 1 pfx_sep_char

let string_of_kind = function
  | `MD5 -> "md5"
  | `SHA256 -> "sha256"
  | `SHA512 -> "sha512"

let kind_of_string s = match String.lowercase_ascii s with
  | "md5" -> `MD5
  | "sha256" -> `SHA256
  | "sha512" -> `SHA512
  | _ -> invalid_arg "OpamHash.kind_of_string"

let is_hex_str len s =
  String.length s = len &&
  try
    String.iter (function
        | '0'..'9' | 'A'..'F' | 'a'..'f' -> ()
        | _ -> raise Exit)
      s;
    true
  with Exit -> false

let len = function
  | `MD5 -> 32
  | `SHA256 -> 64
  | `SHA512 -> 128

let valid kind = is_hex_str (len kind)

let make kind s =
  if valid kind s then kind, String.lowercase_ascii s
  else invalid_arg ("OpamHash.make_"^string_of_kind kind)

let md5 = make `MD5
let sha256 = make `SHA256
let sha512 = make `SHA512

let of_string_opt s =
  try
    let kind, s =
      match OpamStd.String.cut_at s pfx_sep_char with
      | None -> `MD5, s
      | Some (skind, s) -> kind_of_string skind, s
    in
    if valid kind s then Some (kind, String.lowercase_ascii s)
    else None
  with Invalid_argument _ -> None

let of_string s =
  match of_string_opt s with
  | Some h -> h
  | None -> invalid_arg "OpamHash.of_string"

let to_string (kind,s) =
  String.concat pfx_sep_str [string_of_kind kind; s]

let to_json s = `String (to_string s)

let to_path (kind,s) =
  [string_of_kind kind; String.sub s 0 2; s]

let compute ?target ?(kind=default_kind) file =
  (* TODO This process is woefully inefficient. *)
  let primary =
    match kind with
    | `MD5 -> md5 (Digest.to_hex (Digest.file file))
    | (`SHA256 | `SHA512) as kind ->
      try
        if not OpamCoreConfig.(!r.use_openssl) then raise Exit else
        match
          OpamSystem.read_command_output ["openssl"; string_of_kind kind; file]
        with
        | [l] ->
          let len = len kind in
          make kind (String.sub l (String.length l - len) len)
        | _ -> failwith "openssl error"
      with OpamSystem.Command_not_found _ | Exit ->
        make kind (OpamSHA.hash kind file)
  in
  let probably_binary name =
    List.mem (Filename.extension name) [".zip"; ".tar"; ".xz"; ".lz"; ".gz"; ".tgz"; ".tbz"; ".txz"; ".tlz"]
  in
  let size = Unix.((stat file).st_size) in
  if target = None || target = Some primary || size = 0 || probably_binary file then
    primary
  else
    let buffer = Buffer.create size in
    let ch = open_in_bin file in
    let has_eol_at_eof = lazy (
      seek_in ch (in_channel_length ch - 1);
      input_char ch <> '\n')
    in
    let rec add_cr line =
      Buffer.add_string buffer line;
      match input_line ch with
      | line ->
          Buffer.add_string buffer "\r\n";
          add_cr line
      | exception End_of_file ->
          if Lazy.force has_eol_at_eof then
            Buffer.add_string buffer "\r\n";
          complete ()
    and rem_cr line =
      Buffer.add_substring buffer line 0 (String.length line - 1);
      Buffer.add_char buffer '\n';
      match input_line ch with
      | line ->
          let l = String.length line in
          if l = 0 || line.[l - 1] <> '\r' then begin
            match input_line ch with
            | _ ->
                (* A line didn't end with '\r' - try again adding '\r' instead *)
                seek_in ch 0;
                Buffer.clear buffer;
                add_cr (input_line ch)
            | exception End_of_file ->
                if Lazy.force has_eol_at_eof then begin
                  (* The file should have ended with '\r' - try again *)
                  seek_in ch 0;
                  Buffer.clear buffer;
                  add_cr (input_line ch)
                end else
                  complete ()
          end else
            rem_cr line
      | exception End_of_file ->
          complete ()
    and complete () =
      let content = Buffer.to_bytes buffer in
      match kind with
      | `MD5 ->
          md5 (Digest.to_hex (Digest.bytes content))
      | (`SHA256 | `SHA512) as kind ->
          make kind (OpamSHA.hash_bytes kind content)
    in
    (* Already checked above that the file is non-empty *)
    let line = input_line ch in
    let l = String.length line in
    let result =
      if l > 0 && line.[l - 1] = '\r' then
        rem_cr line
      else
        add_cr line
    in
    close_in ch;
    if Some result = target then
      result
    else
      primary

let compute_from_string ?(kind=default_kind) str = match kind with
  | `MD5 -> md5 (Digest.to_hex (Digest.string str))
  | (`SHA256 | `SHA512) as kind ->
    make kind (OpamSHA.hash_bytes kind (Bytes.of_string str))

let check_file f (kind, _ as target) =
  compute ~target ~kind f = target

let mismatch f (kind, _ as target) =
  let hf = compute ~target ~kind f in
  if hf = target then None else Some hf

let compute = compute ?target:None

module O = struct
  type _t = t
  type t = _t
  let to_string = to_string
  let to_json = to_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)
