(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamUrl
open! Crowbar
open OpamCrowbar

let version_control = choose [
    const `git;
    const `darcs;
    const `hg;
]
let backend =
  let print ppf b = Format.pp_print_string ppf (string_of_backend b) in
  with_printer print @@ choose [
    const `http;
    const `rsync;
    version_control;
]

let transport = choose [
    const "http";
    const "ssh";
    const "file";
    const "git";
    const "hg";
    const "darcs";
]

let url : OpamUrl.t gen = map [
    transport;
    nice_string;
    option nice_string;
  ] @@ fun
    transport
    path
    hash
  ->
  String.concat ""
      [transport; "://"; path; (match hash with None -> "" | Some h -> h)]
  |> OpamUrl.parse

let check () =
  check_json_roundtrip ~name:"OpamUrl.t"
    url (eq_of_comp OpamUrl.compare) OpamUrl.to_json OpamUrl.of_json;
