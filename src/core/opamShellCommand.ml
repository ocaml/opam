(**************************************************************************)
(*                                                                        *)
(*    Copyright 2025 Kate Deplaix                                         *)
(*    Copyright 2015 The bos programmers                                  *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(* NOTE: Inspired from @dbuenzli's astring library
   https://erratique.ch/software/astring *)
module String = struct
  module Sub = struct
    type t = {
      str : string;
      start_pos : int;
      end_pos : int;
    }

    let start_pos {start_pos; _} = start_pos

    let is_empty {start_pos; end_pos; _} = end_pos - start_pos = 0

    let tail ({start_pos; end_pos; _} as sub) =
      if (start_pos : int) = (end_pos : int) then
        sub
      else
        {sub with start_pos = start_pos + 1}

    let head {str; start_pos; end_pos} =
      if (start_pos : int) = (end_pos : int) then
        None
      else
        Some str.[start_pos]

    let span ~sat {str; start_pos; end_pos} =
      let rec loop i str start_pos end_pos =
        if i < end_pos && sat str.[i] then
          loop (i + 1) str start_pos end_pos
        else
          ({str; start_pos; end_pos = i},
           {str; start_pos = i; end_pos})
      in
      loop start_pos str start_pos end_pos

    let concat l =
      let to_string {str; start_pos; end_pos} =
        String.sub str start_pos (end_pos - start_pos)
      in
      List.fold_left (fun acc x -> acc ^ to_string x) "" l

    let extend ~max {str; start_pos; end_pos} =
      let rec loop i str start_pos str_len =
        if i < str_len then
          loop (i + 1) str start_pos str_len
        else
          {str; start_pos; end_pos = i}
      in
      loop start_pos str start_pos
        (min (end_pos + max : int) (String.length str))
  end

  let sub str = {Sub.str; start_pos = 0; end_pos = String.length str}

  let of_char c = String.make 1 c

  let dump fmt s =
    let escape_digit = function
      | 0 -> '0'
      | 1 -> '1'
      | 2 -> '2'
      | 3 -> '3'
      | 4 -> '4'
      | 5 -> '5'
      | 6 -> '6'
      | 7 -> '7'
      | 8 -> '8'
      | 9 -> '9'
      | 10 -> 'A'
      | 11 -> 'B'
      | 12 -> 'C'
      | 13 -> 'D'
      | 14 -> 'E'
      | 15 -> 'F'
      | _ -> assert false
    in
    let dump_escaped_str fmt s i len =
      if i < len then begin
        match s.[i] with
        | '\b' -> Format.pp_print_string fmt "\\b"
        | '\t' -> Format.pp_print_string fmt "\\t"
        | '\n' -> Format.pp_print_string fmt "\\n"
        | '\r' -> Format.pp_print_string fmt "\\r"
        | '\"' -> Format.pp_print_string fmt "\\\""
        | '\\' -> Format.pp_print_string fmt "\\\\"
        | ' '..'~' as c -> Format.pp_print_char fmt c
        | c ->
          let code = Char.code c in
          Format.fprintf fmt "\\x%c%c"
            (escape_digit (code / 16)) (escape_digit (code mod 16))
      end
    in
    Format.pp_print_char fmt '"';
    dump_escaped_str fmt s 0 (String.length s);
    Format.pp_print_char fmt '"';
end

(* NOTE: Modified version from @dbuenzli's bos library (module Bos_cmd)
   https://erratique.ch/software/bos *)
let parse_cmdline s =
  try
    let err_unclosed kind s =
      failwith
        (Format.sprintf "%d: unclosed %s quote delimited string"
           (String.Sub.start_pos s) kind)
    in
    let parse_squoted s =
      let sat = function '\'' -> false | _ -> true in
      let tok, rem = String.Sub.span ~sat (String.Sub.tail s) in
      if not (String.Sub.is_empty rem) then
        (tok, String.Sub.tail rem)
      else
        err_unclosed "single" s
    in
    let parse_dquoted acc s =
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' -> (data :: acc, String.Sub.tail rem)
        | Some '\\' ->
          let rem = String.Sub.tail rem in
          begin match String.Sub.head rem with
          | Some ('"' | '\\' | '$' | '`' as c) ->
            let acc = String.sub (String.of_char c) :: data :: acc in
            loop acc (String.Sub.tail rem)
          | Some '\n' -> loop (data :: acc) (String.Sub.tail rem)
          | Some _c ->
            let acc = String.Sub.extend ~max:2 data :: acc in
            loop acc (String.Sub.tail rem)
          | None ->
            err_unclosed "double" s
          end
        | None -> err_unclosed "double" s
        | Some _ -> assert false
      in
      loop acc (String.Sub.tail s)
    in
    let parse_token s =
      let rec loop acc s =
        match String.Sub.head s with
        | None -> (acc, s)
        | Some c when OpamStd.Char.is_whitespace c -> (acc, s)
        | Some '\'' ->
          let tok, rem = parse_squoted s in
          loop (tok :: acc) rem
        | Some '\"' ->
          let acc, rem = parse_dquoted acc s in
          loop acc rem
        | Some _c ->
          let sat = function
            | '\'' | '\"' -> false
            | c -> not (OpamStd.Char.is_whitespace c)
          in
          let tok, rem = String.Sub.span ~sat s in
          loop (tok :: acc) rem
      in
      loop [] s
    in
    let rec loop acc s =
      match String.Sub.head s with
      | None when acc = [] -> failwith "empty command"
      | None -> acc
      | Some c when OpamStd.Char.is_whitespace c ->
        loop acc (String.Sub.tail s)
      | Some _ ->
        let token, s = parse_token s in
        loop (String.Sub.concat (List.rev token) :: acc) s
    in
    Ok (loop [] (String.sub s))
  with Failure err ->
    Error (Format.asprintf "command line %a:%s" String.dump s err)

let of_string s =
  match parse_cmdline s with
  | Ok x -> List.rev x
  | Error msg -> OpamConsole.error_and_exit `Bad_arguments "%s" msg
