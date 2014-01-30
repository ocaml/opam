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

open OcpString
open Test

let s0 = "0123456789"
let s1 = String.make 10 '1'
let s2 =
  let s = String.make 10 '1' in
  s.[3] <- '2'; s
let s3_l = "    \t"
let s3_r = "   "
let s3_m = "Hello!\n"
let s3 = s3_l ^ s3_m ^ s3_r
let s4 = "  a b  c d  "
let idx = [|
  "hell";
  "o worl";
  "";
  "d!\n";
|]
let idx_str = String.concat "" (Array.to_list idx)
let eq1 = (=) '1'
let eq2 = (=) '2'


let for_all = group "for_all" [
  test (fun () -> for_all eq1 s1);
]

let exists = group "exists" [
  test (fun () -> exists eq2 s2);
]

let before = group "before" [
  test (fun () -> before s0 3 = "012");
]

let after = group "after" [
  test (fun () -> after s0 3 = "456789");
]

let starts_with = group "starts_with" [
  test (fun () -> starts_with s0 ~prefix:"0123");
  test (fun () -> not (starts_with s0 ~prefix:"0124"));
]

let ends_with = group "ends_with" [
  test (fun () -> ends_with s0 ~suffix:"789");
  test (fun () -> not (ends_with s0 ~suffix:"679"));
]

let cut = group "cut" [
  test (fun () -> cut s0 3 = ("012", "456789"));
  test (fun () -> cut ~keep:true s0 3 = ("012", "3456789"));
]

let cuts = group "cuts" [
  test (fun () -> cuts "123" = [ "","123"; "1","23"; "12","3"; "123",""]);
  test (fun () -> cuts "" = ["", ""]);
]

let cut_at = group "rcut_at" [
  test (fun () -> rcut_at s0 '3' = cut_at s0 '3');
  test (fun () -> rcut_at ~keep:true s0 '3' = cut_at ~keep:true s0 '3');
]

let strip = group "strip" [
  test (fun () -> strip s3 = s3_m);
]

let lstrip = group "lstrip" [
  test (fun () -> lstrip s3 = s3_m ^ s3_r);
]

let rstrip = group "rstrip" [
  test (fun () -> rstrip s3 = s3_l ^ s3_m)
]

let rsplit = group "rsplit" [
  test (fun () -> rsplit s4 ' ' = split s4 ' ');
  test (fun () -> rsplitn 3 s4 ' ' = ["  a b";"c";"d"]);
]

let split = group "split" [
  test (fun () -> split s4 ' ' = ["a";"b";"c";"d"]);
  test (fun () -> split "     1   2   " ' ' = ["1";"2"]);
  test (fun () -> splitn 3 s4 ' ' = ["a";"b"; "c d  "]);
]

let indexes = group "indexes" [
  test (fun () ->
    let r = ref true in
    for n = 0 to String.length idx_str - 1 do
      let i,j = indexes idx n in
      r := !r && idx.(i).[j] = idx_str.[n]
    done;
    !r);
  test_exn Out_of_bounds (fun () -> indexes idx 100);
]

let first_word = group "first_word" [
  test (fun () -> first_word "hello world" = "hello");
  test_exn Not_found (fun () -> first_word "" = "");
]

let compact = group "compact" [
  test (fun () -> compact "Ceci est un Test" = "ceci-est-un-test");
]

let is_ws = group "is_ws" [
  test (fun () -> is_ws ' ' && is_ws '\r' && is_ws '\t');
  test (fun () -> not (is_ws '\n') && not (is_ws 'a'));
]

let is_ws_or_nl = group "is_ws_or_nl" [
  test (fun () -> is_ws_or_nl ' ' && is_ws_or_nl '\r' && is_ws_or_nl '\t');
  test (fun () -> is_ws_or_nl '\n');
  test (fun () -> not (is_ws_or_nl 'a'));
]

let index2_from = group "index2_from" [
  test (fun () ->
    index2_from s0 3 '7' '8' - 3
    = index2 (String.sub s0 3 (String.length s0 - 3)) '7' '8');
]

let index2 = group "index2" [
  test (fun () -> index2 s0 '4' '5' = 4);
  test (fun () -> index2 "0204060800" '0' '0' = 8);
  test_exn Not_found (fun () -> index2 "0123" '3' '4');
]

let prefixes = group "prefixes" [
  test (fun () -> prefixes "123" = ["1"; "12"; "123" ])
]

let suffixes = group "suffixes" [
  test (fun () -> suffixes "123" = ["123"; "23"; "3"])
]

let substrings = group "substrings" [
  test (fun () -> substrings "123" = ["1"; "12"; "2"; "123"; "23"; "3"]);
  test (fun () -> substrings "123" <> []);
]

let split_chars = group "split_chars" [
  test (fun () -> split_chars "a b\t c\t\td  e  \t" [ ' '; '\t' ] = ["a"; "b"; "c"; "d" ;"e"])
]

let _ =
  register "String" [
    for_all;
    exists;
    before;
    after;
    starts_with;
    ends_with;
    cut;
    cuts;
    cut_at;
    strip;
    lstrip;
    rstrip;
    split;
    rsplit;
    indexes;
    first_word;
    compact;
    is_ws;
    index2;
    index2_from;
    prefixes;
    suffixes;
    substrings;
    split_chars;
  ]
