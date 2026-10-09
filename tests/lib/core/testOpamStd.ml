(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Set = struct
  let test_to_string ~ctxt () =
    let fun_name = "OpamStd.Set.to_string" in
    let test ~test_name ~elements ~expected =
      let set = OpamStd.IntSet.of_list elements in
      let got = OpamStd.IntSet.to_string set in
      if String.equal got expected then
        OpamUnit.success ~ctxt ~fun_name ~test_name ()
      else
        OpamUnit.failure ~ctxt ~fun_name ~test_name ~expected ~got ()
    in
    List.iter
      (fun (test_name, elements, expected) ->
         test ~test_name ~elements ~expected)
      [ "Empty", [], "{}"
      ; "Singleton", [1312], "{ 1312 }"
      ; "Few", [1; 2; 3; 4; 5; 6; 7], "{ 1, 2, 3, 4, 5, 6, 7 }"
      ; "Too many", List.init 101 (fun i -> i), "101 elements"
      ]

  let test ~ctxt () =
    test_to_string ~ctxt ()
end

let test ~ctxt () =
  Set.test ~ctxt ()
