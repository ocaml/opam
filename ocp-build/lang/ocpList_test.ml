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

open OcpList
open Test

let l0 = [1]
let l1 = [1;2;7]
let l2 = [2;4;6;8]
let l0a = [ (1, 0); (1, 1) ]
let eq6 = (=) 6
let leq6 = (>=) 6
let times2 = (( * ) 2)

let last = group "last" [
  test (fun () -> last l0 = 1);
  test (fun () -> last l1 = 7);
  test_exn Not_found (fun () ->  last []);
]

let diff = group "diff" [
  test (fun () -> diff l1 l2 = [1;4;6;7;8]);
  test (fun () -> diff [] l2 = l2);
  test (fun () -> diff l1 [] = l1);
  ]

let sub = group "sub" [
  test (fun () -> sub l1 l2 = [1;7]);
  test (fun () -> sub [] l2 = []);
  test (fun () -> sub l1 [] = l1)
]

let take = group "take" [
  test (fun () -> take 3 l0 = l0);
  test (fun () -> take 2 l1 = [1;2]);
]

let drop = group "drop" [
  test (fun () -> drop 3 l0 = []);
  test (fun () -> drop 2 l1 = [7]);
]

let take_while = group "take_while" [
  test (fun () -> take_while leq6 l2 = [2;4;6]);
]

let drop_while = group "drop_while" [
  test (fun () -> drop_while leq6 l2 = [8]);
]

let setify = group "setify" [
  test (fun () -> setify [] = []);
  test (fun () ->
    let x = setify [1;2;1;1;2;1] in
    x = [1;2] || x = [2;1]);
]

let setify_sorted = group "setify_sorted" [
  test (fun () -> setify_sorted [1;1;2;3;4;4] = [1;2;3;4]);
]

let union_set = group "union_set" [
  test (fun () -> union_set [1;2;3] [2;3;3;4] = [1;2;3;4]);
  test (fun () -> union_set [2;3;4] [1;1;2;3] = [1;2;3;4]);
  test (fun () -> union_set [1;2;3;4;4] [1;2] = [1;2;3;4]);
  test (fun () -> union_set [2;2;3] [1;2;3;4] = [1;2;3;4]);
]

let _ = OcpList.union_set [1;2;3;4;4] [1;2]

let inter_set = group "inter_set" [
  test (fun () -> inter_set [1;2;3;4] [2;2;3;5;5] = [2;3]);
  test (fun () -> inter_set [1;1;2] [] = []);
]

let remove_all_assoc = group "remove_all_assoc" [
  test (fun () -> remove_all_assoc 1 l0a = []);
]

let replace_assoc = group "replace_assoc" [
  test (fun () -> replace_assoc 1 2 l0a = [ 1,2 ]);
]

let assoc_all = group "assoc_all" [
  test (fun () ->
    let x = assoc_all 1 l0a in
    x = [0;1] || x = [1;0]);
]

let rank = group "rank" [
  test_exn Not_found (fun () -> rank 7 l2);
  test (fun () -> rank 6 l2 = 2);
]

let filter_map = group "filter_map" [
  test (fun () ->
    filter_map (fun x -> if leq6 x then Some (times2 x) else None) l2
    = List.map times2 (List.filter leq6 l2));
]

let filter_opt = group "filter_opt" [
  test (fun () -> filter_opt [Some 1; None; Some 3] = [1;3]);
]

let find_map = group "find_opt" [
  test (fun () ->
    find_map (fun x -> if eq6 x then Some (times2 x) else None) l2
    = times2 (List.find eq6 l2));
]

let flatten_map = group "flatten_map" [
  test (fun () ->
    flatten_map (fun x -> [x;times2 x]) l2
    = List.flatten (List.map (fun x -> [x; times2 x]) l2));
]

let remove = group "remove" [
  test (fun () -> remove 1 [1;2;1;3] = [2;3])
]

let removeq = group "removeq" [
  test (fun () ->
    let x = "foo" in
    removeq x [ x; "foo"; x; "bar" ] = [ "foo"; "bar" ])
]

let prefixes = group "prefixes" [
  test (fun () -> prefixes [1;2;3] = [[1];[1;2];[1;2;3]]);
  test (fun () -> prefixes [] = []);
]

let suffixes = group "suffixes" [
  test (fun () -> suffixes [1;2;3] = [[3];[2;3];[1;2;3]]);
]

let range = group "range" [
  test (fun () -> range 1 10 = [1;2;3;4;5;6;7;8;9;10]);
  test (fun () -> range ~step:2 1 6 = [1;3;5]);
  test (fun () -> range 10 8 = [10; 9; 8]);
  test_exn Invalid_step (fun () -> range ~step:0 1 2);
]

let filter_out = group "filter_out" [
  test (fun () -> filter_out ((=)2) [1;2;3] = List.filter ((<>)2) [1;2;3])
]

let intersperse = group "intersperse" [
  test (fun () -> intersperse 0 [1;2;3;4] = [1;0;2;0;3;0;4]);
]

let intercalate = group "intercalate" [
  test (fun () -> intercalate ~pattern:[0;0] [1;2;3;4] = [1;0;0;2;0;0;3;0;0;4]);
]

let inv_assoc = group "inv_assoc" [
  test (fun () -> inv_assoc 3 [ (0,1); (2,3); (4,5) ] = 2);
]

let inv_mem_assoc = group "mem_inv_assoc" [
  test (fun () -> not (inv_mem_assoc 0 [ (0,1); (2,3) ]));
]

let _ =
  register "List" [
    last;
    diff;
    sub;
    take;
    drop;
    take_while;
    drop_while;
    setify;
    setify_sorted;
    union_set;
    inter_set;
    replace_assoc;
    assoc_all;
    rank;
    filter_map;
    filter_opt;
    find_map;
    flatten_map;
    remove;
    removeq;
    prefixes;
    suffixes;
    range;
    filter_out;
    intersperse;
    intercalate;
    inv_assoc;
    inv_mem_assoc;
  ]
