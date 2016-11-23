(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

type dl_tool_kind = [ `Curl | `Default ]

type t = {
  download_tool: (arg list * dl_tool_kind) Lazy.t;
  validation_hook: arg list option;
  retries: int;
  force_checksums: bool option;
}

type 'a options_fun =
  ?download_tool:(OpamTypes.arg list * dl_tool_kind) Lazy.t ->
  ?validation_hook:arg list option ->
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
  validation_hook = None;
  retries = 3;
  force_checksums = None;
}

let setk k t
    ?download_tool
    ?validation_hook
    ?retries
    ?force_checksums
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    download_tool = t.download_tool + download_tool;
    validation_hook = t.validation_hook + validation_hook;
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
    env_string "FETCH" >>= (fun s ->
        let args = OpamStd.String.split s ' ' in
        match args with
        | cmd::a ->
          let cmd, kind =
            if OpamStd.String.ends_with ~suffix:"curl" cmd then
              (CIdent "curl", None), `Curl
            else if cmd = "wget" then
              (CIdent "wget", None), `Default
            else
              (CString cmd, None), `Default
          in
          let c = cmd :: List.map (fun a -> OpamTypes.CString a, None) a in
          Some (lazy (c, kind))
        | [] ->
          None
      )
    >>+ fun () ->
    env_string "CURL" >>| (fun s ->
        lazy ([CString s, None], `Curl))
  in
  let validation_hook =
    env_string "VALIDATIONHOOK" >>| fun s ->
    match List.map (fun s -> CString s, None) (OpamStd.String.split s ' ') with
    | [] -> None
    | l -> Some l
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
    ?validation_hook
    ?retries:(env_int "RETRIES")
    ?force_checksums

let init ?noop:_ = initk (fun () -> ())
