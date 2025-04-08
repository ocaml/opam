(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Int = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let min x y : Stdlib.Int.t = if x <= y then x else y

  (** NOTE: OCaml >= 4.13 *)
  let max x y : Stdlib.Int.t = if x >= y then x else y

  include Stdlib.Int
end

module Uchar = struct
  [@@@warning "-32"]

  let rep = 0xFFFD
  external unsafe_of_int : int -> Stdlib.Uchar.t = "%identity"
  external to_int : Stdlib.Uchar.t -> int = "%identity"
  let decode_bits = 24
  let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111
  let[@inline] utf_decode_uchar d = unsafe_of_int (d land 0xFFFFFF)
  let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor (to_int u)
  let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor rep

  (** NOTE: OCaml >= 5.4.0 *)
  let utf_8_decode_length_of_byte = function
    | '\x00' .. '\x7F' -> 1
    | '\x80' .. '\xC1' -> 0
    | '\xC2' .. '\xDF' -> 2
    | '\xE0' .. '\xEF' -> 3
    | '\xF0' .. '\xF4' -> 4
    | _ -> 0

  include Stdlib.Uchar
end

module Bytes = struct
  [@@@warning "-32"]

  external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"

  let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)

  let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10

  let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101

  let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100

  let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b

  let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8

  let[@inline] utf_8_uchar_2 b0 b1 =
    ((b0 land 0x1F) lsl 6) lor
    ((b1 land 0x3F))

  let[@inline] utf_8_uchar_3 b0 b1 b2 =
    ((b0 land 0x0F) lsl 12) lor
    ((b1 land 0x3F) lsl 6) lor
    ((b2 land 0x3F))

  let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18) lor
    ((b1 land 0x3F) lsl 12) lor
    ((b2 land 0x3F) lsl 6) lor
    ((b3 land 0x3F))
  let dec_invalid = Uchar.utf_decode_invalid

  (** NOTE: OCaml >= 4.14 *)
  let get_utf_8_uchar b i =
    let b0 = Bytes.get_uint8 b i in (* raises if [i] is not a valid index. *)
    let get = unsafe_get_uint8 in
    let max = Bytes.length b - 1 in
    match Char.unsafe_chr b0 with (* See The Unicode Standard, Table 3.7 *)
    | '\x00' .. '\x7F' -> dec_ret 1 b0
    | '\xC2' .. '\xDF' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
          dec_ret 2 (utf_8_uchar_2 b0 b1)
    | '\xE0' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_invalid 1 else
          let i = i + 1 in if i > max then dec_invalid 2 else
            let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
              dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
          let i = i + 1 in if i > max then dec_invalid 2 else
            let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
              dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xED' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_x9F b1 then dec_invalid 1 else
          let i = i + 1 in if i > max then dec_invalid 2 else
            let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
              dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xF0' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x90_to_xBF b1 then dec_invalid 1 else
          let i = i + 1 in if i > max then dec_invalid 2 else
            let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
              let i = i + 1 in if i > max then dec_invalid 3 else
                let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
                  dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF1' .. '\xF3' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
          let i = i + 1 in if i > max then dec_invalid 2 else
            let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
              let i = i + 1 in if i > max then dec_invalid 3 else
                let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
                  dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF4' ->
      let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_x8F b1 then dec_invalid 1 else
          let i = i + 1 in if i > max then dec_invalid 2 else
            let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
              let i = i + 1 in if i > max then dec_invalid 3 else
                let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
                  dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | _ -> dec_invalid 1

  include Stdlib.Bytes
end

module String = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let exists p s =
    let n = String.length s in
    let rec loop i =
      if i = n then false
      else if p (String.unsafe_get s i) then true
      else loop (succ i) in
    loop 0

  let utf_8_uchar_length s =
    let slen = String.length s in
    let i = ref 0 and ulen = ref 0 in
    while (!i < slen) do
      let dec_len = Uchar.utf_8_decode_length_of_byte (String.unsafe_get s !i) in
      i := (!i + if dec_len = 0 then 1 (* count one Uchar.rep *) else dec_len);
      incr ulen;
    done;
    !ulen

  let get_utf_8_uchar s i = Bytes.get_utf_8_uchar (Bytes.unsafe_of_string s) i

  let uchar_array_of_utf_8_string s =
    let slen = String.length s in (* is an upper bound on Uchar.t count *)
    let uchars = Array.make slen Uchar.max in
    let k = ref 0 and i = ref 0 in
    while (!i < slen) do
      let dec = get_utf_8_uchar s !i in
      i := !i + Uchar.utf_decode_length dec;
      uchars.(!k) <- Uchar.utf_decode_uchar dec;
      incr k;
    done;
    uchars, !k

  let edit_distance' ?(limit = Int.max_int) s (s0, len0) s1 =
    if limit <= 1 then (if String.equal s s1 then 0 else limit) else
      let[@inline] minimum a b c = Int.min a (Int.min b c) in
      let s1, len1 = uchar_array_of_utf_8_string s1 in
      let limit = Int.min (Int.max len0 len1) limit in
      if Int.abs (len1 - len0) >= limit then limit else
        let s0, s1 = if len0 > len1 then s0, s1 else s1, s0 in
        let len0, len1 = if len0 > len1 then len0, len1 else len1, len0 in
        let rec loop row_minus2 row_minus1 row i len0 limit s0 s1 =
          if i > len0 then row_minus1.(Array.length row_minus1 - 1) else
            let len1 = Array.length row - 1 in
            let row_min = ref Int.max_int in
            row.(0) <- i;
            let jmax =
              let jmax = Int.min len1 (i + limit - 1) in
              if jmax < 0 then (* overflow *) len1 else jmax
            in
            for j = Int.max 1 (i - limit) to jmax do
              let cost = if Uchar.equal s0.(i-1) s1.(j-1) then 0 else 1 in
              let min = minimum
                  (row_minus1.(j-1) + cost) (* substitute *)
                  (row_minus1.(j) + 1)      (* delete *)
                  (row.(j-1) + 1)           (* insert *)
                  (* Note when j = i - limit, the latter [row] read makes a bogus read
                     on the value that was in the matrix at d.(i-2).(i - limit - 1).
                     Since by induction for all i,j, d.(i).(j) >= abs (i - j),
                     (row.(j-1) + 1) is greater or equal to [limit] and thus does
                     not affect adversely the minimum computation. *)
              in
              let min =
                if (i > 1 && j > 1 &&
                    Uchar.equal s0.(i-1) s1.(j-2) &&
                    Uchar.equal s0.(i-2) s1.(j-1))
                then Int.min min (row_minus2.(j-2) + cost) (* transpose *)
                else min
              in
              row.(j) <- min;
              row_min := Int.min !row_min min;
            done;
            if !row_min >= limit then (* can no longer decrease *) limit else
              loop row_minus1 row row_minus2 (i + 1) len0 limit s0 s1
        in
        let ignore =
          (* Value used to make the values around the diagonal stripe ignored
             by the min computations when we have a limit. *)
          limit + 1
        in
        let row_minus2 = Array.make (len1 + 1) ignore in
        let row_minus1 = Array.init (len1 + 1) (fun x -> x) in
        let row = Array.make (len1 + 1) ignore in
        let d = loop row_minus2 row_minus1 row 1 len0 limit s0 s1 in
        if d > limit then limit else d

  let edit_distance ?limit s0 s1 =
    let us0 = uchar_array_of_utf_8_string s0 in
    edit_distance' ?limit s0 us0 s1
  let default_max_dist s = match utf_8_uchar_length s with
    | 0 | 1 | 2 -> 0
    | 3 | 4 -> 1
    | _ -> 2

  let spellcheck ?(max_dist = default_max_dist) iter_dict s =
    let min = ref (max_dist s) in
    let acc = ref [] in
    let select_words s us word =
      let d = edit_distance' ~limit:(!min + 1) s us word in
      if d = !min then (acc := word :: !acc) else
      if d < !min then (min := d; acc := [word]) else ()
    in
    let us = uchar_array_of_utf_8_string s in
    iter_dict (select_words s us);
    List.rev !acc

  include Stdlib.String
end

module Seq = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.14 *)
  let rec find_map f xs =
    match xs() with
    | Seq.Nil ->
      None
    | Seq.Cons (x, xs) ->
      match f x with
      | None ->
          find_map f xs
      | Some _ as result ->
          result

  include Seq
end

module Either = struct
  (** NOTE: OCaml >= 4.12 *)
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end

module Unix = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let realpath s =
    let getchdir s =
      let p =
        try Sys.getcwd ()
        with Sys_error _ -> Filename.get_temp_dir_name ()
      in
      Unix.chdir s;
      p
    in
    try getchdir (getchdir s) with Unix.Unix_error _ -> s

  include Unix
end

module Lazy = struct
  [@@@warning "-32"]

  (** NOTE: OCaml >= 4.13 *)
  let map f x =
    lazy (f (Lazy.force x))

  include Stdlib.Lazy
end

module Filename = struct
  [@@@warning "-32"]

  let quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    let rec loop i =
      if i = l then Buffer.add_char b '\"' else
      match s.[i] with
      | '\"' -> loop_bs 0 i;
      | '\\' -> loop_bs 0 i;
      | c    -> Buffer.add_char b c; loop (i+1);
    and loop_bs n i =
      if i = l then begin
        Buffer.add_char b '\"';
        add_bs n;
      end else begin
        match s.[i] with
        | '\"' -> add_bs (2*n+1); Buffer.add_char b '\"'; loop (i+1);
        | '\\' -> loop_bs (n+1) (i+1);
        | _    -> add_bs n; loop i
      end
    and add_bs n = for _j = 1 to n do Buffer.add_char b '\\'; done
    in
    loop 0;
    Buffer.contents b

(*
Quoting commands for execution by cmd.exe is difficult.
1- Each argument is first quoted using the "quote" function above, to
   protect it against the processing performed by the C runtime system,
   then cmd.exe's special characters are escaped with '^', using
   the "quote_cmd" function below.  For more details, see
   https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23
2- The command and the redirection files, if any, must be double-quoted
   in case they contain spaces.  This quoting is interpreted by cmd.exe,
   not by the C runtime system, hence the "quote" function above
   cannot be used.  The two characters we don't know how to quote
   inside a double-quoted cmd.exe string are double-quote and percent.
   We just fail if the command name or the redirection file names
   contain a double quote (not allowed in Windows file names, anyway)
   or a percent.  See function "quote_cmd_filename" below.
3- The whole string passed to Sys.command is then enclosed in double
   quotes, which are immediately stripped by cmd.exe.  Otherwise,
   some of the double quotes from step 2 above can be misparsed.
   See e.g. https://stackoverflow.com/a/9965141
*)
  let quote_cmd s =
    let b = Buffer.create (String.length s + 20) in
    String.iter
      (fun c ->
         match c with
         | '(' | ')' | '!' | '^' | '%' | '\"' | '<' | '>' | '&' | '|' ->
           Buffer.add_char b '^'; Buffer.add_char b c
         | _ ->
           Buffer.add_char b c)
      s;
    Buffer.contents b

  let quote_cmd_filename f =
    if String.contains f '\"' || String.contains f '%' then
      failwith ("Filename.quote_command: bad file name " ^ f)
    else if String.contains f ' ' then
      "\"" ^ f ^ "\""
    else
      f
  (* Redirections in cmd.exe: see https://ss64.com/nt/syntax-redirection.html
     and https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-xp/bb490982(v=technet.10)
  *)

  (** NOTE: OCaml >= 4.10 *)
  let quote_command cmd ?stdin ?stdout ?stderr args =
    String.concat "" [
      "\"";
      quote_cmd_filename cmd;
      " ";
      quote_cmd (String.concat " " (List.map quote args));
      (match stdin  with None -> "" | Some f -> " <" ^ quote_cmd_filename f);
      (match stdout with None -> "" | Some f -> " >" ^ quote_cmd_filename f);
      (match stderr with None -> "" | Some f ->
          if stderr = stdout
          then " 2>&1"
          else " 2>" ^ quote_cmd_filename f);
      "\""
    ]

  include Stdlib.Filename
end
