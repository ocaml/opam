(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type test = {
  fun_name : string;
  name : string;
}

let pp_test fmt {fun_name; name} =
  Format.fprintf fmt "%s.%s" fun_name name

type result =
  | Failure of {expected : string; got : string}
  | Skip of {reason : string}
  | Success

type test_result = {
  test : test;
  result : result;
}

type ctxt = {
  mutable results : test_result list;
  mutable failures : int;
}

let failure ~ctxt ~fun_name ~test_name ~expected ~got () =
  let test = {fun_name; name = test_name} in
  ctxt.results <- {test; result = Failure {expected; got}}::ctxt.results;
  ctxt.failures <- ctxt.failures + 1

let success ~ctxt ~fun_name ~test_name () =
  let test = {fun_name; name = test_name} in
  ctxt.results <- {test; result = Success}::ctxt.results

let skip ~ctxt ~fun_name ~test_name ~reason () =
  let test = {fun_name; name = test_name} in
  ctxt.results <- {test; result = Skip {reason}}::ctxt.results

let print_failure ~test ~expected ~got =
  Format.printf "[31m[FAILED][0m  %a: expected %s but got %s\n"
    pp_test test expected got

let print_skip ~test ~reason =
  Format.printf "[33m[SKIPPED][0m %a: %s\n" pp_test test reason

let print_success ~test =
  Format.printf "[32m[OK][0m      %a\n" pp_test test

let print_report ~show_failures ~show_skipped ~show_success ctxt =
  List.iter
    (function
      | {test; result = Failure {expected; got}} ->
        if show_failures then print_failure ~test ~expected ~got
      | {test; result = Skip {reason}} ->
        if show_skipped then print_skip ~test ~reason
      | {test; result = Success} ->
        if show_success then print_success ~test)
    ctxt.results

module Args = struct
  let show_skipped = ref false
  let show_success= ref false
  let hide_failures = ref false

  let specs = [
    ("--show-skipped", Arg.Set show_skipped , "Show skipped tests in report");
    ("--show-success", Arg.Set show_success , "Show succesful tests in report");
    ("--hide-failure", Arg.Set hide_failures, "Do not show failed tests in report");
  ]

  let usage_msg =
    let exe_name = Filename.basename Sys.executable_name in
    Printf.sprintf "%s [options]" exe_name

  let handle_pos_arg s =
    raise (Arg.Bad (Printf.sprintf "No positional arguments accepted: %s" s))

  let parse () = Arg.parse specs handle_pos_arg usage_msg
end

let run_tests f =
  Args.parse ();
  let ctxt = {failures = 0; results = []} in
  f ctxt;
  ctxt.results <- List.rev ctxt.results;
  print_report
    ~show_failures:(not !Args.hide_failures)
    ~show_skipped:!Args.show_skipped
    ~show_success:!Args.show_success
    ctxt;
  match ctxt.failures with
  | 0 -> exit 0
  | _ -> exit 1
