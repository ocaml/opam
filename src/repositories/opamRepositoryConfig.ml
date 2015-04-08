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
  unit -> 'a

let default = {
  download_tool = lazy (
    try
      let tools =
        if OpamMisc.Sys.(os () = Darwin)
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

let setk k ft
    ?download_tool
    ?retries
    ?force_checksums
    ()
  =
  let t = ft () in
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    download_tool = t.download_tool + download_tool;
    retries = t.retries + retries;
    force_checksums = t.force_checksums + force_checksums;
  }

let set t = setk (fun x -> x) (fun _ -> t)

let r = ref default

let update = setk (fun cfg -> r := cfg) (fun () -> !r)
