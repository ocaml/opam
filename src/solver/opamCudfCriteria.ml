(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type filter =
  | Installed
  | Solution
  | Changed
  | Removed
  | New
  | Upgraded
  | Downgraded
  | Requested

type property = string option

type sign = Plus | Minus

type criterion = sign * filter * property

let criterion_of_string (s,params) =
  let sign = match s.[0] with
    | '+' -> Plus
    | '-' -> Minus
    | c -> failwith (Printf.sprintf "criteria_of_string sign=%c" c)
    | exception Invalid_argument _ ->
      failwith "criteria_of_string sign=EOF"
  in
  let s = String.sub s 1 (String.length s - 1) in
  let subset_of_string = function
    | "new" -> New
    | "removed" -> Removed
    | "changed" -> Changed
    | "up" -> Upgraded
    | "down" -> Downgraded
    | "installed" -> Installed
    | "solution" -> Solution
    | "request" -> Requested
    | s -> failwith ("criteria_of_string subset="^s)
  in
  match s, params with
  | "count", [field; subset] ->
    sign, subset_of_string subset, Some field
  | s, [] -> sign, subset_of_string s, None
  | s, _ -> failwith ("criteria_of_string s="^s)

let criterion_to_string (sign, filter, property: criterion) =
  Printf.sprintf "%c%s%s"
    (match sign with Plus -> '+' | Minus -> '-')
    (match filter with
     | Installed -> "installed"
     | Solution -> "solution"
     | Changed -> "changed"
     | Removed -> "removed"
     | New -> "new"
     | Upgraded -> "up"
     | Downgraded -> "down"
     | Requested -> "request")
    (match property with None -> "" | Some p -> "["^p^"]")

let of_string s =
  let start = ref 0 in
  let crits = ref [] in
  let params = ref None in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ',' ->
      let sub = String.sub s !start (i - !start) in
      start := i + 1;
      if sub <> "" then
        (match !params with
         | None -> crits := (sub, []) :: !crits
         | Some (name, ps) -> params := Some (name, sub :: ps))
    | '[' ->
      let sub = String.sub s !start (i - !start) in
      start := i + 1;
      if !params <> None then failwith "criteria_of_string";
      params := Some (sub, [])
    | ']' ->
      let sub = String.sub s !start (i - !start) in
      start := i + 1;
      (match !params with
       | None -> failwith "criteria_of_string"
       | Some (name, ps) ->
         params := None;
         crits := (name, List.rev (sub::ps)) :: !crits)
    | _ -> ()
  done;
  if !start < String.length s then
    crits := (String.sub s !start (String.length s - !start), []) :: !crits;
  if !params <> None then failwith "criteria_of_string";
  let r = List.rev_map criterion_of_string !crits in
  r
