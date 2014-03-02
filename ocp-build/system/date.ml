(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open MinUnix

type iso8601 = string

let iso8601 () =
  let t = time () in
  let t = gmtime t in
  Printf.sprintf
    "%04d%02d%02dT%02d:%02d:%02dZ"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let string_of_iso8601 x = x

type timestamp = string

let timestamp () =
  let t = time () in
  let t = gmtime t in
  Printf.sprintf
    "%04d%02d%02d%02d%02d%02d"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let string_of_timestamp x = x
