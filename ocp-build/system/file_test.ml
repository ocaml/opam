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

open File
open Test

let f1 = ".ml"
let f2 = "foo.ml"
let f3 = "fooml"

let strs = [
  "hello";
  "world! ";
  "";
  "it is such ";
  "a nice day!";
]
let str = String.concat "\n" strs

let tmpfile = "/tmp/foo"

let init () =
  let oc = open_out tmpfile in
  output_string oc str;
  close_out oc

let clean () =
  OcpUnix.safe_unlink tmpfile

let cut_last_extension = group "cut_last_extension" [
  test (fun () -> cut_last_extension f1 = ("", "ml"));
  test (fun () -> cut_last_extension f2 = ("foo", "ml"));
  test (fun () -> cut_last_extension f3 = ("fooml",""));
]

let string_of_channel = group "string_of_channel" [
  test (fun () ->
    init ();
    let ic = open_in tmpfile in
    let r = string_of_channel ic in
    close_in ic;
    clean ();
    r = str);
]

let string_of_file = group "string_of_file" [
  test (fun () ->
    init ();
    let r = string_of_file tmpfile in
    clean ();
    r = str);
]

let output_line = group "output_line" [
  test (fun () ->
    let oc = open_out tmpfile in
    let str = List.hd strs in
    output_line oc str;
    close_out oc;
    let ic = open_in tmpfile in
    let r = File.string_of_channel ic in
    close_in ic;
    clean ();
    r = str ^ "\n");
]

let lines_of_file = group "lines_of_file" [
  test (fun () ->
    init ();
    let ls = lines_of_file tmpfile in
    clean ();
    ls = strs);
  test (fun () ->
    init ();
    let ls = FileLabels.lines_of_file ~line_break:true tmpfile in
    clean ();
    ls = List.map (fun s -> s ^ "\n") strs);
  test (fun () ->
    init ();
    let discard s = String.length s = 0 || s.[0] > 'h' in
    let ls = FileLabels.lines_of_file ~discard tmpfile in
    clean ();
    ls = [ "hello"; "a nice day!" ]);
]

let file_of_lines = group "file_of_lines" [
  test (fun () ->
    file_of_lines tmpfile strs;
    let l = File.lines_of_file tmpfile in
    clean ();
    l = strs);
]

let file_of_string = group "file_of_string" [
  test (fun () ->
    file_of_string tmpfile str;
    let l = File.lines_of_file tmpfile in
    clean ();
    l = strs);
]

let sub_lines = group "sub_lines" [
  test (fun () ->
    init ();
    let ls = sub_lines tmpfile 2 3 in
    clean ();
    ls = OcpList.take 3 (OcpList.drop 2 strs));
]

let _ =
  register "File" [
    cut_last_extension;
    string_of_channel;
    string_of_file;
    output_line;
    lines_of_file;
    file_of_lines;
    file_of_string;
    sub_lines;
  ]
