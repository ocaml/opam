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

(* Few lines from xen-api.git/ocaml/xapi/quicktest_common.ml *)
(* Copyright (C) 2006-2009 Citrix Systems Inc. LGPL v2.1 *)

open OcpLang

let total_started = ref 0
let total_passed = ref 0

type status =
  | Pending
  | Success
  | Failed

type vt100 =
  | Control of string
  | Data of string

let length_of_vt100 sequence =
  let length = function
    | Control _ -> 0
    | Data x   -> String.length x in
  List.fold_left (+) 0 (List.map length sequence)

let flatten_vt100 sequence =
  List.fold_left (^) "" (List.map (function Control x -> x | Data x -> x) sequence)

let escape = String.make 1 (char_of_int 0x1b)
let set_attribute attrs =
  Control (Printf.sprintf "%s[%sm" escape (String.concat ";" (List.map string_of_int attrs)))

let reset = 0
let bright = 1
let dim = 2
let red = 31
let green = 32
let blue = 34
let yellow = 33

let basic_string_of_status = function
  | Pending -> [ Data " " ]
  | Success -> [ Data "[ Success ]" ]
  | Failed  -> [ Data "[ Failed ]" ]

let coloured_string_of_status = function
  | Pending -> [ Data " " ]
  | Success ->
    [ Data "[ ";
      set_attribute [ bright; green ];
      Data "Success";
      set_attribute [ reset ];
      Data " ]" ]
  | Failed ->
    [ Data "[ ";
      set_attribute [ bright; red ];
      Data "Failed ";
      set_attribute [ reset ];
      Data " ]" ]

let use_colour = ref true

let cols = 80

let nice_status_output name status =
  let vt100 = (if !use_colour then coloured_string_of_status else basic_string_of_status) status in
  let flattened = flatten_vt100 vt100 in
  let invisible_length = String.length flattened in
  let visible_length = length_of_vt100 vt100 in

  (* Need a bigger string to cope with control characters *)
  let line = String.make (cols + (invisible_length - visible_length)) ' ' in
  (* Stick the test name towards the left *)
  String.blit name 0 line 0 (min cols (String.length name));

  (* Stick the coloured bit towards the right *)
  (* NB we need to use the 'visible length' for positioning but copy all chars, even invis ones *)
  String.blit (flatten_vt100 vt100) 0 line (cols - visible_length) (String.length flattened);
  Printf.printf "%s\n%!" line

module Raw = struct

  type test = {
    name           : string;
    mutable status : status;
  }

  let make_test name = {
    name;
    status = Pending
  }

  let all_tests = ref []
  let failed_tests = ref []

  let mem test =
    List.memq test !all_tests

  let remove test =
    all_tests := List.removeq test !all_tests

  let add test =
    all_tests := test :: !all_tests

  let debug test fmt =
    let fn msg =
      (* Might need to divide into multiple lines *)
      let tab = " " in
      let max_length =
        cols
        - length_of_vt100 (coloured_string_of_status test.status)
        - String.length tab in
      let rec loop start_offset =
        if start_offset < String.length msg then begin
          let length = min (String.length msg - start_offset) max_length in
          let submsg = String.sub msg start_offset length in
          nice_status_output (tab^submsg) Pending;
          loop (start_offset + length)
        end in
      nice_status_output (test.name^":") Pending;
      loop 0 in
    Printf.kprintf fn fmt

  let start test =
    incr total_started;
    add test

  let success test =
    if not (mem test) then
      failwith (Printf.sprintf "Test not started: %s" test.name);
    remove test;
    if test.status = Pending then begin
      incr total_passed;
      test.status <- Success
    end;
    nice_status_output test.name test.status

  let failed test =
    if not (mem test) then
      failwith (Printf.sprintf "Test not started: %s" test.name);
    remove test;
    failed_tests := test :: !failed_tests;
    test.status <- Failed;
    nice_status_output test.name Failed

  let stats ~started ~passed =
    Printf.sprintf "Total test started: %d; total passed: %d (%.2f%%)"
      started passed (float_of_int passed /. (float_of_int started) *. 100.)

  let summarise () =
    Printf.printf "\n\n%s\n%!" (stats ~started:!total_started ~passed:!total_passed);
    if !all_tests <> [] then begin
      Printf.printf "Tests neither succeeded nor failed:\n%!";
      List.iter (fun t -> nice_status_output t.name t.status) !all_tests;
    end;
    if !total_passed <> !total_started then begin
      Printf.printf "Tests failed:\n%!";
      List.iter (fun t -> nice_status_output t.name Failed) (List.rev !failed_tests);
      exit 1;
    end

end

type test =
  | Test_case of (unit -> bool)
  | Test_group of string * test list

let test t = Test_case t

let test_exn exn fn =
  Test_case (fun () ->
    try ignore (fn ()); false
    with e -> e = exn)

let group name l = Test_group (name, l)

let tests = ref []

let register name tl =
  tests := Test_group (name, tl) :: !tests

let process () =
  let rank = ref 1 in
  let fullname path =
    String.concat "." (List.rev path) ^ ":" ^ string_of_int !rank in
  let rec aux path = function
    | Test_case fn ->
      let name = fullname path in
      incr rank;
      let test = Raw.make_test name in
      Raw.start test;
      let result =
        try fn ()
        with e ->
          Raw.debug test "Got exception: %s" (Printexc.to_string e);
          false in
      if result then
        Raw.success test
      else
        Raw.failed test

    | Test_group (n, tl) ->
      let oldrank = !rank in
      rank := 1;
      List.iter (aux (n::path)) tl;
      rank := oldrank in

  List.iter (aux []) !tests;
  Raw.summarise ()
