module String = Astring.String
module Char = Astring.Char

(* NOTE: Copied from @dbuenzli's bos library (module Bos_cmd) *)
(*---------------------------------------------------------------------------
   Copyright (c) 2015 The bos programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
let parse_cmdline s =
  try
    let err_unclosed kind s =
      failwith @@
      Format.sprintf "%d: unclosed %s quote delimited string"
        (String.Sub.start_pos s) kind
    in
    let skip_white s = String.Sub.drop ~sat:Char.Ascii.is_white s in
    let tok_sep c = c = '\'' || c = '\"' || Char.Ascii.is_white c in
    let tok_char c = not (tok_sep c) in
    let not_squote c = c <> '\'' in
    let parse_squoted s =
      let tok, rem = String.Sub.span ~sat:not_squote (String.Sub.tail s) in
      if not (String.Sub.is_empty rem) then tok, String.Sub.tail rem else
      err_unclosed "single" s
    in
    let parse_dquoted acc s =
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' -> (data :: acc), (String.Sub.tail rem)
        | Some '\\' ->
            let rem = String.Sub.tail rem in
            begin match String.Sub.head rem with
            | Some ('"' | '\\' | '$' | '`' as c) ->
                let acc = String.(sub (of_char c)) :: data :: acc in
                loop acc (String.Sub.tail rem)
            | Some ('\n') -> loop (data :: acc) (String.Sub.tail rem)
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
      let ret acc s = String.Sub.(to_string @@ concat (List.rev acc)), s in
      let rec loop acc s = match String.Sub.head s with
      | None -> ret acc s
      | Some c when Char.Ascii.is_white c -> ret acc s
      | Some '\'' ->
          let tok, rem = parse_squoted s in loop (tok :: acc) rem
      | Some '\"' ->
          let acc, rem = parse_dquoted acc s in loop acc rem
      | Some _c ->
          let sat = tok_char in
          let tok, rem = String.Sub.span ~sat s in loop (tok :: acc) rem
      in
      loop [] s
    in
    let rec loop acc s =
      if String.Sub.is_empty s then acc else
      let token, s = parse_token s in
      loop (token :: acc) (skip_white s)
    in
    Ok (loop [] (skip_white (String.sub s)))
  with Failure err ->
    Error (Format.asprintf "command line %a:%s" String.dump s err)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The bos programmers
   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

let of_string s =
  match parse_cmdline s with
  | Ok x -> List.rev x
  | Error msg -> OpamConsole.error_and_exit `Bad_arguments "%s" msg
