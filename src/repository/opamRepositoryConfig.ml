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

open OpamTypes

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
  'a

let default = {
  download_tool = lazy (
    try
      let tools =
        if OpamStd.Sys.(os () = Darwin)
        then ["wget", `Default; "curl", `Curl]
        else ["curl", `Curl; "wget", `Default]
      in
      let cmd, kind =
        List.find (fun (c,_) -> OpamSystem.command_exists c) tools
      in
      [ CIdent cmd, None ], kind
    with Not_found ->
      OpamConsole.error_and_exit
        "Could not find a suitable download command. Please make sure you \
         have either \"curl\" or \"wget\" installed, or specify a custom \
         command through variable OPAMFETCH."
  );
  retries = 3;
  force_checksums = None;
}

let setk k t
    ?download_tool
    ?retries
    ?force_checksums
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    download_tool = t.download_tool + download_tool;
    retries = t.retries + retries;
    force_checksums = t.force_checksums + force_checksums;
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd.Config in
  let open OpamStd.Option.Op in
  let download_tool =
    env_string "FETCH" >>| (fun s ->
        let args = OpamStd.String.split s ' ' in
        let c = List.map (fun a -> OpamTypes.CString a, None) args in
        let kind = match c with
          | (CIdent "curl", None)::_ -> `Curl
          | (CString s, None)::_
            when OpamStd.String.ends_with ~suffix:"curl" s -> `Curl
          | _ -> `Default
        in
        lazy (c, kind)
      )
    >>+ fun () ->
    env_string "CURL" >>| (fun s ->
        lazy ([CString s, None], `Curl))
  in
  let force_checksums =
    match env_bool "REQUIRECHECKSUMS", env_bool "NOCHECKSUMS" with
    | Some true, _ -> Some (Some true)
    | _, Some true -> Some (Some false)
    | None, None -> None
    | _ -> Some None
  in
  setk (setk (fun c -> r := c; k)) !r
    ?download_tool
    ?retries:(env_int "RETRIES")
    ?force_checksums

let init ?noop:_ = initk (fun () -> ())
