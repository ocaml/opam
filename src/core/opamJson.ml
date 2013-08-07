module Uutf = struct

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   uutf release 0.9.2
  ---------------------------------------------------------------------------*)

let io_buffer_size = 65536                           (* IO_BUFFER_SIZE 4.0.0 *)

let pp = Format.fprintf
let invalid_encode () = invalid_arg "expected `Await encode"
let invalid_bounds j l =
  invalid_arg (Printf.sprintf "invalid bounds (index %d, length %d)" j l)

(* Unsafe string byte manipulations. If you don't believe the author's
   invariants, replacing with safe versions makes everything safe in
   the module. He won't be upset. *)

let unsafe_chr = Char.unsafe_chr
let unsafe_blit = String.unsafe_blit
let unsafe_array_get = Array.unsafe_get
let unsafe_byte s j = Char.code (String.unsafe_get s j)
let unsafe_set_byte s j byte = String.unsafe_set s j (Char.unsafe_chr byte)

(* Unicode characters *)

type uchar = int
let u_bom = 0xFEFF                                                   (* BOM. *)
let u_rep = 0xFFFD                                 (* replacement character. *)
let is_uchar cp =
  (0x0000 <= cp && cp <= 0xD7FF) || (0xE000 <= cp && cp <= 0x10FFFF)

let pp_cp ppf cp =
  if cp < 0 || cp > 0x10FFFF then pp ppf "U+Invalid(%X)" cp else
  if cp <= 0xFFFF then pp ppf "U+%04X" cp else
  pp ppf "U+%X" cp

let cp_to_string cp =                                    (* NOT thread safe. *)
  pp Format.str_formatter "%a" pp_cp cp; Format.flush_str_formatter ()

(* Unicode encoding schemes *)

type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]
type decoder_encoding = [ encoding | `US_ASCII | `ISO_8859_1 ]

let encoding_of_string s = match String.uppercase s with      (* IANA names. *)
| "UTF-8" -> Some `UTF_8
| "UTF-16" -> Some `UTF_16
| "UTF-16LE" -> Some `UTF_16LE
| "UTF-16BE" -> Some `UTF_16BE
| "ANSI_X3.4-1968" | "ISO-IR-6" | "ANSI_X3.4-1986" | "ISO_646.IRV:1991"
| "ASCII" | "ISO646-US" | "US-ASCII" | "US" | "IBM367" | "CP367" | "CSASCII" ->
    Some `US_ASCII
| "ISO_8859-1:1987" | "ISO-IR-100" | "ISO_8859-1" | "ISO-8859-1"
| "LATIN1" | "L1" | "IBM819" | "CP819" | "CSISOLATIN1" ->
    Some `ISO_8859_1
| _ -> None

let encoding_to_string = function
| `UTF_8 -> "UTF-8" | `UTF_16 -> "UTF-16" | `UTF_16BE -> "UTF-16BE"
| `UTF_16LE -> "UTF-16LE" | `US_ASCII -> "US-ASCII"
| `ISO_8859_1 -> "ISO-8859-1"

(* Base character decoders. They assume enough data. *)

let malformed s j l = `Malformed (String.sub s j l)
let malformed_pair be hi s j l =    (* missing or half low surrogate at eoi. *)
  let bs1 = String.sub s j l in
  let bs0 = String.create 2 in
  let j0, j1 = if be then (0, 1) else (1, 0) in
  unsafe_set_byte bs0 j0 (hi lsr 8);
  unsafe_set_byte bs0 j1 (hi land 0xFF);
  `Malformed (bs0 ^ bs1)

let r_us_ascii s j =
  (* assert (0 <= j && j < String.length s); *)
  let b0 = unsafe_byte s j in
  if b0 <= 127 then `Uchar b0 else malformed s j 1

let r_iso_8859_1 s j =
  (* assert (0 <= j && j < String.length s); *)
  `Uchar (unsafe_byte s j)

let utf_8_len = [| (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

let r_utf_8 s j l =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  match l with
  | 1 -> `Uchar (unsafe_byte s j)
  | 2 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      if b1 lsr 6 != 0b10 then malformed s j l else
      `Uchar (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F))
  | 3 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      let b2 = unsafe_byte s (j + 2) in
      let c = `Uchar (((b0 land 0x0F) lsl 12) lor
                      ((b1 land 0x3F) lsl 6) lor
		      (b2 land 0x3F))
      in
      if b2 lsr 6 != 0b10 then malformed s j l else
      begin match b0 with
      | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then malformed s j l else c
      | 0xED -> if b1 < 0x80 || 0x9F < b1 then malformed s j l else c
      | _ -> if b1 lsr 6 != 0b10 then malformed s j l else c
      end
  | 4 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      let b2 = unsafe_byte s (j + 2) in let b3 = unsafe_byte s (j + 3) in
      let c = `Uchar (((b0 land 0x07) lsl 18) lor
                      ((b1 land 0x3F) lsl 12) lor
		      ((b2 land 0x3F) lsl 6) lor
                      (b3 land 0x3F))
      in
      if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then malformed s j l else
      begin match b0 with
      | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then malformed s j l else c
      | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then malformed s j l else c
      | _ -> if b1 lsr 6 != 0b10 then malformed s j l else c
      end
  | _ -> assert false

let r_utf_16 s j0 j1 =                       (* May return a high surrogate. *)
  (* assert (0 <= j0 && 0 <= j1 && max j0 j1 < String.length s); *)
  let b0 = unsafe_byte s j0 in let b1 = unsafe_byte s j1 in
  let u = (b0 lsl 8) lor b1 in
  if u < 0xD800 || u > 0xDFFF then `Uchar u else
  if u > 0xDBFF then malformed s (min j0 j1) 2 else `Hi u

let r_utf_16_lo hi s j0 j1 =          (* Combines [hi] with a low surrogate. *)
  (* assert (0 <= j0 && 0 <= j1 && max j0 j1 < String.length s); *)
  let b0 = unsafe_byte s j0 in
  let b1 = unsafe_byte s j1 in
  let lo = (b0 lsl 8) lor b1 in
  if lo < 0xDC00 || lo > 0xDFFF
  then malformed_pair (j0 < j1 (* true => be *)) hi s (min j0 j1) 2
  else `Uchar ((((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000)

let r_encoding s j l =                  (* guess encoding with max. 3 bytes. *)
  (* assert (0 <= j && 0 <= l && j + l <= String.length s) *)
  let some i = if i < l then Some (unsafe_byte s (j + i)) else None in
  match (some 0), (some 1), (some 2) with
  | Some 0xEF, Some 0xBB, Some 0xBF                 -> `UTF_8 `BOM
  | Some 0xFE, Some 0xFF, _                         -> `UTF_16BE `BOM
  | Some 0xFF, Some 0xFE, _                         -> `UTF_16LE `BOM
  | Some 0x00, Some    p, _              when p > 0 -> `UTF_16BE (`ASCII p)
  | Some    p, Some 0x00, _              when p > 0 -> `UTF_16LE (`ASCII p)
  | Some    u,         _, _ when utf_8_len.(u) <> 0 -> `UTF_8 `Decode
  | Some    _, Some    _, _                         -> `UTF_16BE `Decode
  | Some    _, None     , None                      -> `UTF_8 `Decode
  | None     , None     , None                      -> `UTF_8 `End
  | None     , Some    _, _                         -> assert false
  | Some    _, None     , Some _                    -> assert false
  | None     , None     , Some _                    -> assert false

(* Decode *)

type src = [ `Channel of in_channel | `String of string | `Manual ]
type nln = [ `ASCII of uchar | `NLF of uchar | `Readline of uchar ]
type decode = [ `Await | `End | `Malformed of string | `Uchar of uchar]

let pp_decode ppf = function
| `Uchar u -> pp ppf "@[`Uchar %a@]" pp_cp u
| `End -> pp ppf "`End"
| `Await -> pp ppf "`Await"
| `Malformed bs ->
    let l = String.length bs in
    pp ppf "@[`Malformed @[(";
    if l > 0 then pp ppf "%02X" (Char.code (bs.[0]));
    for i = 1 to l - 1 do pp ppf " %02X" (Char.code (bs.[i])) done;
    pp ppf ")@]@]"

type decoder =
  { src : src;                                              (* input source. *)
    mutable encoding : decoder_encoding;                (* decoded encoding. *)
    nln : nln option;                     (* newline normalization (if any). *)
    nl : int;                            (* newline normalization character. *)
    mutable i : string;                              (* current input chunk. *)
    mutable i_pos : int;                          (* input current position. *)
    mutable i_max : int;                          (* input maximal position. *)
    t : string;        (* four bytes temporary buffer for overlapping reads. *)
    mutable t_len : int;                      (* current byte length of [t]. *)
    mutable t_need : int;                  (* number of bytes needed in [t]. *)
    mutable removed_bom : bool;     (* [true] if an initial BOM was removed. *)
    mutable last_cr : bool;                   (* [true] if last char was CR. *)
    mutable line : int;                                      (* line number. *)
    mutable col : int;                                     (* column number. *)
    mutable count : int;                                      (* char count. *)
    mutable pp :        (* decoder post-processor for BOM, position and nln. *)
      decoder -> [ `Malformed of string | `Uchar of uchar ] -> decode;
    mutable k : decoder -> decode }                 (* decoder continuation. *)

(* On decodes that overlap two (or more) [d.i] buffers, we use [t_fill] to copy
   the input data to [d.t] and decode from there. If the [d.i] buffers are not
   too small this is faster than continuation based byte per byte writes.

   End of input (eoi) is signalled by [d.i_pos = 0] and [d.i_max = min_int]
   which implies that [i_rem d < 0] is [true]. *)

let i_rem d = d.i_max - d.i_pos + 1     (* remaining bytes to read in [d.i]. *)
let eoi d = d.i <- ""; d.i_pos <- 0; d.i_max <- min_int   (* set eoi in [d]. *)
let src d s j l =                                     (* set [d.i] with [s]. *)
  if (j < 0 || l < 0 || j + l > String.length s) then invalid_bounds j l else
  if (l = 0) then eoi d else
  (d.i <- s; d.i_pos <- j; d.i_max <- j + l - 1)

let refill k d = match d.src with  (* get new input in [d.i] and [k]ontinue. *)
| `Manual -> d.k <- k; `Await
| `String _ -> eoi d; k d
| `Channel ic ->
    let rc = input ic d.i 0 (String.length d.i) in
    (src d d.i 0 rc; k d)

let t_need d need = d.t_len <- 0; d.t_need <- need
let rec t_fill k d =      (* get [d.t_need] bytes (or less if eoi) in [i.t]. *)
  let blit d l =
    unsafe_blit d.i d.i_pos d.t d.t_len (* write pos. *) l;
    d.i_pos <- d.i_pos + l; d.t_len <- d.t_len + l;
  in
  let rem = i_rem d in
  if rem < 0 (* eoi *) then k d else
  let need = d.t_need - d.t_len in
  if rem < need then (blit d rem; refill (t_fill k) d) else (blit d need; k d)

let ret k v d = d.k <- k; d.pp d v             (* return post-processed [v]. *)

(* Decoders. *)

let rec decode_us_ascii d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_us_ascii d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1; ret decode_us_ascii (r_us_ascii d.i j) d

let rec decode_iso_8859_1 d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_iso_8859_1 d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1; ret decode_iso_8859_1 (r_iso_8859_1 d.i j) d

(* UTF-8 decoder *)

let rec t_decode_utf_8 d =                             (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_8 (malformed d.t 0 d.t_len) d
  else ret decode_utf_8 (r_utf_8 d.t 0 d.t_len) d

and decode_utf_8 d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_utf_8 d) else
  let need = unsafe_array_get utf_8_len (unsafe_byte d.i d.i_pos) in
  if rem < need then (t_need d need; t_fill t_decode_utf_8 d) else
  let j = d.i_pos in
  if need = 0
  then (d.i_pos <- d.i_pos + 1; ret decode_utf_8 (malformed d.i j 1) d)
  else (d.i_pos <- d.i_pos + need; ret decode_utf_8 (r_utf_8 d.i j need) d)

(* UTF-16BE decoder *)

let rec t_decode_utf_16be_lo hi d =                    (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_16be (malformed_pair true hi d.t 0 d.t_len) d
  else ret decode_utf_16be (r_utf_16_lo hi d.t 0 1) d

and t_decode_utf_16be d =                              (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_16be (malformed d.t 0 d.t_len) d
  else decode_utf_16be_lo (r_utf_16 d.t 0 1) d

and decode_utf_16be_lo v d = match v with
| `Uchar _ | `Malformed _ as v -> ret decode_utf_16be v d
| `Hi hi ->
    let rem = i_rem d in
    if rem < 2 then (t_need d 2; t_fill (t_decode_utf_16be_lo hi) d) else
    let j = d.i_pos in
    d.i_pos <- d.i_pos + 2; ret decode_utf_16be (r_utf_16_lo hi d.i j (j + 1)) d

and decode_utf_16be d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_utf_16be d) else
  if rem < 2 then (t_need d 2; t_fill t_decode_utf_16be d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 2; decode_utf_16be_lo (r_utf_16 d.i j (j + 1)) d

(* UTF-16LE decoder, same as UTF-16BE with byte swapped. *)

let rec t_decode_utf_16le_lo hi d =                    (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_16le (malformed_pair false hi d.t 0 d.t_len) d
  else ret decode_utf_16le (r_utf_16_lo hi d.t 1 0) d

and t_decode_utf_16le d =                              (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_16le (malformed d.t 0 d.t_len) d
  else decode_utf_16le_lo (r_utf_16 d.t 1 0) d

and decode_utf_16le_lo v d = match v with
| `Uchar _ | `Malformed _ as v -> ret decode_utf_16le v d
| `Hi hi ->
    let rem = i_rem d in
    if rem < 2 then (t_need d 2; t_fill (t_decode_utf_16le_lo hi) d) else
    let j = d.i_pos in
    d.i_pos <- d.i_pos + 2; ret decode_utf_16le (r_utf_16_lo hi d.i (j + 1) j) d

and decode_utf_16le d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_utf_16le d) else
  if rem < 2 then (t_need d 2; t_fill t_decode_utf_16le d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 2; decode_utf_16le_lo (r_utf_16 d.i (j + 1) j) d

(* Encoding guessing. The guess is simple but starting the decoder
   after is tedious, uutf's decoders are not designed to put bytes
   back in the stream. *)

let guessed_utf_8 d =                   (* start decoder after `UTF_8 guess. *)
  let b3 d =                                 (* handles the third read byte. *)
    let b3 = unsafe_byte d.t 2 in
    match utf_8_len.(b3) with
    | 0 -> ret decode_utf_8 (malformed d.t 2 1) d
    | n ->
        d.t_need <- n; d.t_len <- 1; unsafe_set_byte d.t 0 b3;
        t_fill t_decode_utf_8 d
  in
  let b2 d =                                     (* handle second read byte. *)
    let b2 = unsafe_byte d.t 1 in
    let b3 = if d.t_len > 2 then b3 else decode_utf_8 (* decodes `End *) in
    match utf_8_len.(b2) with
    | 0 -> ret b3 (malformed d.t 1 1) d
    | 1 -> ret b3 (r_utf_8 d.t 1 1) d
    | n ->                         (* copy d.t.(1-2) to d.t.(0-1) and decode *)
        d.t_need <- n;
        unsafe_set_byte d.t 0 b2;
        if (d.t_len < 3) then d.t_len <- 1 else
        (d.t_len <- 2; unsafe_set_byte d.t 1 (unsafe_byte d.t 2); );
        t_fill t_decode_utf_8 d
  in
  let b1 = unsafe_byte d.t 0 in                   (* handle first read byte. *)
  let b2 = if d.t_len > 1 then b2 else decode_utf_8 (* decodes `End *) in
  match utf_8_len.(b1) with
  | 0 -> ret b2 (malformed d.t 0 1) d
  | 1 -> ret b2 (r_utf_8 d.t 0 1) d
  | 2 ->
      if d.t_len < 2 then ret decode_utf_8 (malformed d.t 0 1) d else
      if d.t_len < 3 then ret decode_utf_8 (r_utf_8 d.t 0 2) d else
      ret b2 (r_utf_8 d.t 0 2) d
  | 3 ->
      if d.t_len < 3 then ret decode_utf_8 (malformed d.t 0 d.t_len) d else
      ret decode_utf_8 (r_utf_8 d.t 0 3) d
  | 4 ->
      if d.t_len < 3 then ret decode_utf_8 (malformed d.t 0 d.t_len) d else
      (d.t_need <- 4; t_fill t_decode_utf_8 d)
  | _ -> assert false

let guessed_utf_16 d be v =     (* start decoder after `UTF_16{BE,LE} guess. *)
  let decode_utf_16, t_decode_utf_16, t_decode_utf_16_lo, j0, j1 =
    if be then decode_utf_16be, t_decode_utf_16be, t_decode_utf_16be_lo, 0, 1
    else decode_utf_16le, t_decode_utf_16le, t_decode_utf_16le_lo, 1, 0
  in
  let b3 k d =
    if d.t_len < 3 then decode_utf_16 d (* decodes `End *) else
    begin                             (* copy d.t.(2) to d.t.(0) and decode. *)
      d.t_need <- 2; d.t_len <- 1;
      unsafe_set_byte d.t 0 (unsafe_byte d.t 2);
      t_fill k d
    end
  in
  match v with
  | `BOM -> ret (b3 t_decode_utf_16) (`Uchar u_bom) d
  | `ASCII u -> ret (b3 t_decode_utf_16) (`Uchar u) d
  | `Decode ->
      match r_utf_16 d.t j0 j1 with
      | `Malformed _ | `Uchar _ as v -> ret (b3 t_decode_utf_16) v d
      | `Hi hi ->
        if d.t_len < 3 then ret decode_utf_16 (malformed_pair be hi "" 0 0) d
        else (b3 (t_decode_utf_16_lo hi)) d

let guess_encoding d =                  (* guess encoding and start decoder. *)
  let setup d = match r_encoding d.t 0 d.t_len with
  | `UTF_8 r ->
      d.encoding <- `UTF_8; d.k <- decode_utf_8;
      begin match r with
      | `BOM -> ret decode_utf_8 (`Uchar u_bom) d
      | `Decode -> guessed_utf_8 d
      | `End -> `End
      end
  | `UTF_16BE r ->
      d.encoding <- `UTF_16BE; d.k <- decode_utf_16be; guessed_utf_16 d true r
  | `UTF_16LE r ->
      d.encoding <- `UTF_16LE; d.k <- decode_utf_16le; guessed_utf_16 d false r

  in
  (t_need d 3; t_fill setup d)

(* Character post-processors. Used for BOM handling, newline
   normalization and position tracking. The [pp_remove_bom] is only
   used for the first character to remove a possible initial BOM and
   handle UTF-16 endianness recognition. *)

let nline d = d.col <- 0; d.line <- d.line + 1                   (* inlined. *)
let ncol d = d.col <- d.col + 1                                  (* inlined. *)
let ncount d = d.count <- d.count + 1                            (* inlined. *)
let cr d b = d.last_cr <- b                                      (* inlined. *)

let pp_remove_bom utf16 pp d = function(* removes init. BOM, handles UTF-16. *)
| `Uchar 0xFEFF (* BOM *) ->
    if utf16 then (d.encoding <- `UTF_16BE; d.k <- decode_utf_16be);
    d.removed_bom <- true; d.pp <- pp; d.k d
| `Uchar 0xFFFE (* BOM reversed from decode_utf_16be *) when utf16 ->
    d.encoding <- `UTF_16LE; d.k <- decode_utf_16le;
    d.removed_bom <- true; d.pp <- pp; d.k d
| `Malformed _ | `Uchar _ as v ->
    d.removed_bom <- false; d.pp <- pp; d.pp d v

let pp_nln_none d = function
| `Uchar 0x000A (* LF *) as v ->
    let last_cr = d.last_cr in
    cr d false; ncount d; if last_cr then v else (nline d; v)
| `Uchar 0x000D (* CR *) as v -> cr d true; ncount d; nline d; v
| `Uchar (0x0085 | 0x000C | 0x2028 | 0x2029) (* NEL | FF | LS | PS *) as v ->
    cr d false; ncount d; nline d; v
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let pp_nln_readline d = function
| `Uchar 0x000A (* LF *) ->
    let last_cr = d.last_cr in
    cr d false; if last_cr then d.k d else (ncount d; nline d; `Uchar d.nl)
| `Uchar 0x000D (* CR *) -> cr d true; ncount d; nline d; `Uchar d.nl
| `Uchar (0x0085 | 0x000C | 0x2028 | 0x2029) (* NEL | FF | LS | PS *) ->
    cr d false; ncount d; nline d; `Uchar d.nl
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let pp_nln_nlf d = function
| `Uchar 0x000A (* LF *) ->
    let last_cr = d.last_cr in
    cr d false; if last_cr then d.k d else (ncount d; nline d; `Uchar d.nl)
| `Uchar 0x000D (* CR *) -> cr d true; ncount d; nline d; `Uchar d.nl
| `Uchar 0x0085 (* NEL *) -> cr d false; ncount d; nline d; `Uchar d.nl
| `Uchar (0x000C | 0x2028 | 0x2029) as v (* FF | LS | PS *) ->
    cr d false; ncount d; nline d; v
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let pp_nln_ascii d = function
| `Uchar 0x000A (* LF *) ->
    let last_cr = d.last_cr in
    cr d false; if last_cr then d.k d else (ncount d; nline d; `Uchar d.nl)
| `Uchar 0x000D (* CR *) -> cr d true; ncount d; nline d; `Uchar d.nl
| `Uchar (0x0085 | 0x000C | 0x2028 | 0x2029) as v (* NEL | FF | LS | PS *) ->
    cr d false; ncount d; nline d; v
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let decode_fun = function
| `UTF_8 -> decode_utf_8
| `UTF_16 -> decode_utf_16be                         (* see [pp_remove_bom]. *)
| `UTF_16BE -> decode_utf_16be
| `UTF_16LE -> decode_utf_16le
| `US_ASCII -> decode_us_ascii
| `ISO_8859_1 -> decode_iso_8859_1

let decoder ?nln ?encoding src =
  let pp, nl = match nln with
  | None -> pp_nln_none, 0x000A (* not used. *)
  | Some (`ASCII nl) -> pp_nln_ascii, nl
  | Some (`NLF nl) -> pp_nln_nlf, nl
  | Some (`Readline nl) -> pp_nln_readline, nl
  in
  let encoding, k = match encoding with
  | None -> `UTF_8, guess_encoding
  | Some e -> (e :> decoder_encoding), decode_fun e
  in
  let i, i_pos, i_max = match src with
  | `Manual -> "", 1, 0                            (* implies src_rem d = 0. *)
  | `Channel _ -> String.create io_buffer_size, 1, 0                (* idem. *)
  | `String s -> s, 0, String.length s - 1
  in
  { src = (src :> src); encoding; nln = (nln :> nln option); nl;
    i; i_pos; i_max; t = String.create 4; t_len = 0; t_need = 0;
    removed_bom = false; last_cr = false; line = 1; col = 0; count = 0;
    pp = pp_remove_bom (encoding = `UTF_16) pp; k }

let decode d = d.k d
let decoder_line d = d.line
let decoder_col d = d.col
let decoder_count d = d.count
let decoder_removed_bom d = d.removed_bom
let decoder_src d = d.src
let decoder_nln d = d.nln
let decoder_encoding d = d.encoding
let set_decoder_encoding d e =
  d.encoding <- (e :> decoder_encoding); d.k <- decode_fun e

(* Encode *)

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
type encode = [ `Await | `End | `Uchar of uchar ]
type encoder =
  { dst : dst;                                        (* output destination. *)
    encoding : encoding;                                (* encoded encoding. *)
    mutable o : string;                             (* current output chunk. *)
    mutable o_pos : int;                   (* next output position to write. *)
    mutable o_max : int;                (* maximal output position to write. *)
    t : string;                 (* four bytes buffer for overlapping writes. *)
    mutable t_pos : int;                    (* next position to read in [t]. *)
    mutable t_max : int;                 (* maximal position to read in [t]. *)
    mutable k :                                     (* encoder continuation. *)
      encoder -> encode -> [ `Ok | `Partial ] }

(* On encodes that overlap two (or more) [e.o] buffers, we encode the
   character to the temporary buffer [o.t] and continue with
   [tmp_flush] to write this data on the different [e.o] buffers. If
   the [e.o] buffers are not too small this is faster than
   continuation based byte per byte writes. *)

let o_rem e = e.o_max - e.o_pos + 1    (* remaining bytes to write in [e.o]. *)
let dst e s j l =                                     (* set [e.o] with [s]. *)
  if (j < 0 || l < 0 || j + l > String.length s) then invalid_bounds j l;
  e.o <- s; e.o_pos <- j; e.o_max <- j + l - 1

let partial k e = function `Await -> k e | `Uchar _ | `End -> invalid_encode ()
let flush k e = match e.dst with(* get free storage in [d.o] and [k]ontinue. *)
| `Manual -> e.k <- partial k; `Partial
| `Buffer b -> Buffer.add_substring b e.o 0 e.o_pos; e.o_pos <- 0; k e
| `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0; k e

let t_range e max = e.t_pos <- 0; e.t_max <- max
let rec t_flush k e =               (* flush [d.t] up to [d.t_max] in [d.i]. *)
  let blit e l =
    unsafe_blit e.t e.t_pos e.o e.o_pos l;
    e.o_pos <- e.o_pos + l; e.t_pos <- e.t_pos + l
  in
  let rem = o_rem e in
  let len = e.t_max - e.t_pos + 1 in
  if rem < len then (blit e rem; flush (t_flush k) e) else (blit e len; k e)

(* Encoders. *)

let rec encode_utf_8 e v =
  let k e = e.k <- encode_utf_8; `Ok in
  match v with
  | `Await -> k e
  | `End -> flush k e
  | `Uchar u as v ->
      let rem = o_rem e in
      if u <= 0x007F then
      if rem < 1 then flush (fun e -> encode_utf_8 e v) e else
      (unsafe_set_byte e.o e.o_pos u; e.o_pos <- e.o_pos + 1; k e)
      else if u <= 0x07FF then
      begin
        let s, j, k =
          if rem < 2 then (t_range e 1; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 2; e.o, j, k)
        in
        unsafe_set_byte s j (0xC0 lor (u lsr 6));
        unsafe_set_byte s (j + 1) (0x80 lor (u land 0x3F));
        k e
      end
      else if u <= 0xFFFF then
      begin
        let s, j, k =
          if rem < 3 then (t_range e 2; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 3; e.o, j, k)
        in
        unsafe_set_byte s j (0xE0 lor (u lsr 12));
        unsafe_set_byte s (j + 1) (0x80 lor ((u lsr 6) land 0x3F));
        unsafe_set_byte s (j + 2) (0x80 lor (u land 0x3F));
        k e
      end
      else
      begin
        let s, j, k =
          if rem < 4 then (t_range e 3; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 4; e.o, j, k)
        in
        unsafe_set_byte s j (0xF0 lor (u lsr 18));
        unsafe_set_byte s (j + 1) (0x80 lor ((u lsr 12) land 0x3F));
        unsafe_set_byte s (j + 2) (0x80 lor ((u lsr 6) land 0x3F));
        unsafe_set_byte s (j + 3) (0x80 lor (u land 0x3F));
        k e
      end

let rec encode_utf_16be e v =
  let k e = e.k <- encode_utf_16be; `Ok in
  match v with
  | `Await -> k e
  | `End -> flush k e
  | `Uchar u ->
      let rem = o_rem e in
      if u < 0x10000 then
      begin
        let s, j, k =
          if rem < 2 then (t_range e 1; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 2; e.o, j, k)
        in
        unsafe_set_byte s j (u lsr 8);
        unsafe_set_byte s (j + 1) (u land 0xFF);
        k e
      end else begin
        let s, j, k =
          if rem < 4 then (t_range e 3; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 4; e.o, j, k)
        in
        let u' = u - 0x10000 in
        let hi = (0xD800 lor (u' lsr 10)) in
        let lo = (0xDC00 lor (u' land 0x3FF)) in
        unsafe_set_byte s j (hi lsr 8);
        unsafe_set_byte s (j + 1) (hi land 0xFF);
        unsafe_set_byte s (j + 2) (lo lsr 8);
        unsafe_set_byte s (j + 3) (lo land 0xFF);
        k e
      end

let rec encode_utf_16le e v =         (* encode_uft_16be with bytes swapped. *)
  let k e = e.k <- encode_utf_16le; `Ok in
  match v with
  | `Await -> k e
  | `End -> flush k e
  | `Uchar u ->
    let rem = o_rem e in
    if u < 0x10000 then
    begin
      let s, j, k =
        if rem < 2 then (t_range e 1; e.t, 0, t_flush k) else
        let j = e.o_pos in (e.o_pos <- e.o_pos + 2; e.o, j, k)
      in
      unsafe_set_byte s j (u land 0xFF);
      unsafe_set_byte s (j + 1) (u lsr 8);
      k e
    end
    else
    begin
      let s, j, k =
        if rem < 4 then (t_range e 3; e.t, 0, t_flush k) else
        let j = e.o_pos in (e.o_pos <- e.o_pos + 4; e.o, j, k)
      in
      let u' = u - 0x10000 in
      let hi = (0xD800 lor (u' lsr 10)) in
      let lo = (0xDC00 lor (u' land 0x3FF)) in
      unsafe_set_byte s j (hi land 0xFF);
      unsafe_set_byte s (j + 1) (hi lsr 8);
      unsafe_set_byte s (j + 2) (lo land 0xFF);
      unsafe_set_byte s (j + 3) (lo lsr 8);
      k e
    end

let encode_fun = function
| `UTF_8 -> encode_utf_8
| `UTF_16 -> encode_utf_16be
| `UTF_16BE -> encode_utf_16be
| `UTF_16LE -> encode_utf_16le

let encoder encoding dst =
  let o, o_pos, o_max = match dst with
  | `Manual -> "", 1, 0                              (* implies o_rem e = 0. *)
  | `Buffer _
  | `Channel _ -> String.create io_buffer_size, 0, io_buffer_size - 1
  in
  { dst = (dst :> dst); encoding = (encoding :> encoding); o; o_pos; o_max;
    t = String.create 4; t_pos = 1; t_max = 0; k = encode_fun encoding}

let encode e v = e.k e (v :> encode)
let encoder_encoding e = e.encoding
let encoder_dst e = e.dst

(* Manual sources and destinations. *)

module Manual = struct
  let src = src
  let dst = dst
  let dst_rem = o_rem
end

(* Strings folders and Buffer encoders *)

module String = struct
  let encoding_guess s = match r_encoding s 0 (max (String.length s) 3) with
  | `UTF_8 d -> `UTF_8, (d = `BOM)
  | `UTF_16BE d -> `UTF_16BE, (d = `BOM)
  | `UTF_16LE d -> `UTF_16LE, (d = `BOM)

  let fold_utf_8 f acc s =
    let rec loop acc f s i l =
      if i = l then acc else
      let need = unsafe_array_get utf_8_len (unsafe_byte s i) in
      if need = 0 then loop (f acc i (malformed s i 1)) f s (i + 1) l else
      let rem = l - i in
      if rem < need then f acc i (malformed s i rem) else
      loop (f acc i (r_utf_8 s i need)) f s (i + need) l
    in
    loop acc f s 0 (String.length s)

  let fold_utf_16be f acc s =
    let rec loop acc f s i l =
      if i = l then acc else
      let rem = l - i in
      if rem < 2 then f acc i (malformed s i 1) else
      match r_utf_16 s i (i + 1) with
      | `Uchar _ | `Malformed _ as v -> loop (f acc i v) f s (i + 2) l
      | `Hi hi ->
          if rem < 4 then f acc i (malformed s i rem)  else
          loop (f acc i (r_utf_16_lo hi s (i + 2) (i + 3))) f s (i + 4) l
    in
    loop acc f s 0 (String.length s)

  let fold_utf_16le f acc s =             (* [fold_utf_16be], bytes swapped. *)
    let rec loop acc f s i l =
      if i = l then acc else
      let rem = l - i in
      if rem < 2 then f acc i (malformed s i 1) else
      match r_utf_16 s (i + 1) i with
      | `Uchar _ | `Malformed _ as v -> loop (f acc i v) f s (i + 2) l
      | `Hi hi ->
          if rem < 4 then f acc i (malformed s i rem)  else
          loop (f acc i (r_utf_16_lo hi s (i + 3) (i + 2))) f s (i + 4) l
    in
    loop acc f s 0 (String.length s)
end

module Buffer = struct
  let add_utf_8 b u =
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u <= 0x007F then
    (w u)
    else if u <= 0x07FF then
    (w (0xC0 lor (u lsr 6));
     w (0x80 lor (u land 0x3F)))
    else if u <= 0xFFFF then
    (w (0xE0 lor (u lsr 12));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))
    else
    (w (0xF0 lor (u lsr 18));
     w (0x80 lor ((u lsr 12) land 0x3F));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))

  let add_utf_16be b u =
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u < 0x10000 then (w (u lsr 8); w (u land 0xFF)) else
    let u' = u - 0x10000 in
    let hi = (0xD800 lor (u' lsr 10)) in
    let lo = (0xDC00 lor (u' land 0x3FF)) in
    w (hi lsr 8); w (hi land 0xFF);
    w (lo lsr 8); w (lo land 0xFF)

  let add_utf_16le b u =                            (* swapped add_utf_16be. *)
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u < 0x10000 then (w (u land 0xFF); w (u lsr 8)) else
    let u' = u - 0x10000 in
    let hi = (0xD800 lor (u' lsr 10)) in
    let lo = (0xDC00 lor (u' land 0x3FF)) in
    w (hi land 0xFF); w (hi lsr 8);
    w (lo land 0xFF); w (lo lsr 8)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

end

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   jsonm release 0.9.1
  ---------------------------------------------------------------------------*)

(* Braced non-terminals in comments refer to RFC 4627 non-terminals. *)

let io_buffer_size = 65536                          (* IO_BUFFER_SIZE 4.0.0 *)
let pp = Format.fprintf

(* Unsafe string byte manipulations. If you don't believe the authors's
   invariants, replacing with safe versions makes everything safe in the
   module. He won't be upset. *)

let unsafe_blit = String.unsafe_blit
let unsafe_set_byte s j byte = String.unsafe_set s j (Char.unsafe_chr byte)
let unsafe_byte s j = Char.code (String.unsafe_get s j)

(* Characters and their classes *)

let ux_eoi = max_int                 (* End of input, outside unicode range. *)
let ux_soi = max_int - 1           (* Start of input, outside unicode range. *)
let u_nl     = 0x0A (* \n *)
let u_sp     = 0x20 (*   *)
let u_quot   = 0x22 (* '' *)
let u_lbrack = 0x5B (* [ *)
let u_rbrack = 0x5D (* ] *)
let u_lbrace = 0x7B (* { *)
let u_rbrace = 0x7D (* } *)
let u_colon  = 0x3A (* : *)
let u_dot    = 0x2E (* . *)
let u_comma  = 0x2C (* , *)
let u_minus  = 0x2D (* - *)
let u_slash  = 0x2F (* / *)
let u_bslash = 0x5C (* \ *)
let u_times  = 0x2A (* * *)

let must_escape u = u <= 0x1F || u = 0x22 || u = 0x5C
let is_digit u = 0x30 <= u && u <= 0x39
let is_hex_digit u =
  0x30 <= u && u <= 0x39 || 0x41 <= u && u <= 0x46 || 0x61 <= u && u <= 0x66

let is_white = function            (* N.B. Uutf normalizes U+000D to U+000A. *)
| 0x20 | 0x09 | 0x0A -> true | _ -> false

let is_val_sep = function          (* N.B. Uutf normalizes U+000D to U+000A. *)
| 0x20 | 0x09 | 0x0A | 0x2C | 0x5D | 0x7D -> true | _ -> false

(* Data model *)

type lexeme = [
| `Null | `Bool of bool | `String of string | `Float of float
| `Name of string | `As | `Ae | `Os | `Oe ]

let pp_lexeme ppf = function
| `Null -> pp ppf "`Null"
| `Bool b -> pp ppf "@[`Bool %b@]" b
| `String s -> pp ppf "@[`String %S@]" s
| `Name s -> pp ppf "@[`Name %S@]" s
| `Float f -> pp ppf "@[`Float %s@]" (string_of_float f)
| `As -> pp ppf "`As"
| `Ae -> pp ppf "`Ae"
| `Os -> pp ppf "`Os"
| `Oe -> pp ppf "`Oe"

(* Decode *)

type error = [
| `Illegal_BOM
| `Illegal_escape of
    [ `Not_hex_uchar of int
    | `Not_esc_uchar of int
    | `Not_lo_surrogate of int
    | `Lone_lo_surrogate of int
    | `Lone_hi_surrogate of int ]
| `Illegal_string_uchar of int
| `Illegal_bytes of string
| `Illegal_literal of string
| `Illegal_number of string
| `Unclosed of [ `As | `Os | `String | `Comment ]
| `Expected of
    [ `Comment | `Value | `Name | `Name_sep | `Json | `Eoi
    | `Aval of bool (* [true] if first array value  *)
    | `Omem of bool (* [true] if first object member *) ]]

let err_bom = `Error (`Illegal_BOM)
let err_not_hex u = `Error (`Illegal_escape (`Not_hex_uchar u))
let err_not_esc u = `Error (`Illegal_escape (`Not_esc_uchar u))
let err_not_lo p = `Error (`Illegal_escape (`Not_lo_surrogate p))
let err_lone_lo p = `Error (`Illegal_escape (`Lone_lo_surrogate p))
let err_lone_hi p = `Error (`Illegal_escape (`Lone_hi_surrogate p))
let err_str_char u = `Error (`Illegal_string_uchar u)
let err_bytes bs = `Error (`Illegal_bytes bs)
let err_unclosed_comment = `Error (`Unclosed `Comment)
let err_unclosed_string = `Error (`Unclosed `String)
let err_unclosed_arr = `Error (`Unclosed `As)
let err_unclosed_obj = `Error (`Unclosed `Os)
let err_number s = `Error (`Illegal_number s)
let err_literal s = `Error (`Illegal_literal s)
let err_exp_comment = `Error (`Expected `Comment)
let err_exp_value = `Error (`Expected `Value)
let err_exp_name = `Error (`Expected `Name)
let err_exp_nsep = `Error (`Expected `Name_sep)
let err_exp_arr_fst = `Error (`Expected (`Aval true))
let err_exp_arr_nxt = `Error (`Expected (`Aval false))
let err_exp_obj_fst = `Error (`Expected (`Omem true))
let err_exp_obj_nxt = `Error (`Expected (`Omem false))
let err_exp_json = `Error (`Expected `Json)
let err_exp_eoi = `Error (`Expected `Eoi)

let pp_uchar ppf u =
  if u <= 0x1F (* most control chars *) then Uutf.pp_cp ppf u else
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b u;
  pp ppf "'%s' (%a)" (Buffer.contents b) Uutf.pp_cp u

let pp_error ppf = function
| `Illegal_BOM -> pp ppf "@[illegal@ initial@ BOM@ in@ character@ stream@]"
| `Illegal_escape r ->
    pp ppf "@[illegal@ escape,@ ";
    begin match r with
    | `Not_hex_uchar u -> pp ppf "%a@ not@ a@ hex@ digit@]" pp_uchar u
    | `Not_esc_uchar u -> pp ppf "%a@ not@ an@ escaped@ character@]" pp_uchar u
    | `Lone_lo_surrogate p -> pp ppf "%a@ lone@ low@ surrogate@]" Uutf.pp_cp p
    | `Lone_hi_surrogate p -> pp ppf "%a@ lone@ high@ surrogate@]" Uutf.pp_cp p
    | `Not_lo_surrogate p -> pp ppf "%a@ not@ a@ low@ surrogate@]" Uutf.pp_cp p
    end
| `Illegal_string_uchar u ->
    pp ppf "@[illegal@ character@ in@ JSON@ string@ (%a)@]" pp_uchar u
| `Illegal_bytes bs ->
    let l = String.length bs in
    pp ppf "@[illegal@ bytes@ in@ character@ stream@ (";
    if l > 0 then pp ppf "%02X" (Char.code (bs.[0]));
    for i = 1 to l - 1 do pp ppf " %02X" (Char.code (bs.[i])) done;
    pp ppf ")@]"
| `Illegal_number n -> pp ppf "@[illegal@ number@ (%s)@]" n
| `Illegal_literal l -> pp ppf "@[illegal@ literal@ (%s)@]" l
| `Unclosed r ->
    pp ppf "@[unclosed@ ";
    begin match r with
    | `As -> pp ppf "array@]";
    | `Os -> pp ppf "object@]";
    | `String -> pp ppf "string@]";
    | `Comment -> pp ppf "comment@]"
    end
| `Expected r ->
    pp ppf "@[expected@ ";
    begin match r with
    | `Comment -> pp ppf "JavaScript@ comment@]"
    | `Value -> pp ppf "JSON@ value@]"
    | `Name -> pp ppf "member@ name@]"
    | `Name_sep -> pp ppf "name@ separator@ (':')@]"
    | `Aval true -> pp ppf "value@ or@ array@ end@ (value@ or@ ']')@]"
    | `Aval false -> pp ppf "value@ separator@ or@ array@ end@ (','@ or@ ']')@]"
    | `Omem true -> pp ppf "member@ name@ or@ object@ end@ ('\"'@ or@ '}')@]"
    | `Omem false ->pp ppf "value@ separator@ or@ object@ end@ (','@ or@ '}')@]"
    | `Json -> pp ppf "JSON@ text@ ('{'@ or@ '[')@]"
    | `Eoi -> pp ppf "end@ of@ input@]"
    end

type pos = int * int
type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]
type src = [ `Channel of in_channel | `String of string | `Manual ]
type decode = [ `Await | `End | `Lexeme of lexeme | `Error of error ]
type uncut = [ `Comment of [ `M | `S ] * string | `White of string ]

let pp_decode ppf = function
| `Lexeme l -> pp ppf "@[`Lexeme @[(%a)@]@]" pp_lexeme l
| `Await -> pp ppf "`Await"
| `End -> pp ppf "`End"
| `Error e -> pp ppf "@[`Error @[(%a)@]@]" pp_error e
| `White s -> pp ppf "@[`White @[%S@]@]" s
| `Comment (style, s) ->
    let pr_style ppf = function `M -> pp ppf "`M" | `S -> pp ppf "`S" in
    pp ppf "@[`Comment @[(%a, %S)@]@]" pr_style style s

type decoder =
  { u : Uutf.decoder;                          (* Unicode character decoder. *)
    buf : Buffer.t;                           (* string accumulation buffer. *)
    mutable uncut : bool;   (* [true] to bufferize comments and white space. *)
    mutable s_line : int;                          (* last saved start line. *)
    mutable s_col : int;                         (* last saved start column. *)
    mutable e_line : int;                            (* last saved end line. *)
    mutable e_col : int;                           (* last saved end column. *)
    mutable c : Uutf.uchar;                          (* character lookahead. *)
    mutable stack :                     (* stack of open arrays and objects. *)
      [ `As of pos | `Os of pos ] list;
    mutable next_name : bool;    (* [true] if next decode should be [`Name]. *)
    mutable last_start : bool;      (* [true] if last lexeme was `As or `Os. *)
    mutable k :                                     (* decoder continuation. *)
      decoder -> [ decode | uncut ] }

let baddc d c = Uutf.Buffer.add_utf_8 d.buf c
let badd d = Uutf.Buffer.add_utf_8 d.buf d.c
let buf d = let t = Buffer.contents d.buf in (Buffer.clear d.buf; t)
let dpos d = Uutf.decoder_line d.u, Uutf.decoder_col d.u
let spos d = d.s_line <- Uutf.decoder_line d.u; d.s_col <- Uutf.decoder_col d.u
let epos d = d.e_line <- Uutf.decoder_line d.u; d.e_col <- Uutf.decoder_col d.u
let stack_range d = match d.stack with [] -> assert false
| `As (l,c) :: _ | `Os (l,c) :: _ -> d.s_line <- l; d.s_col <- c; epos d

let dpop d = match (spos d; epos d; d.stack) with
| _ :: (`Os _ :: _ as ss) -> d.next_name <- true; d.stack <- ss
| _ :: (`As _ :: _ as ss) -> d.next_name <- false; d.stack <- ss
| _ :: [] -> d.next_name <- false; d.stack <- []
| [] -> assert false

let ret_eoi _ = `End
let ret (v : [< decode | uncut]) k d = d.k <- k; v
let rec readc k d = match Uutf.decode d.u with
| `Uchar u -> d.c <- u; k d
| `End -> d.c <- ux_eoi; k d
| `Await -> ret `Await (readc k) d
| `Malformed bs -> d.c <- Uutf.u_rep; epos d; ret (err_bytes bs) k d

let rec r_scomment k d =               (* single line comment. // was eaten. *)
  if (d.c <> u_nl && d.c <> ux_eoi) then (badd d; readc (r_scomment k) d) else
  (epos d; ret (`Comment (`S, buf d)) (readc k) d)

let rec r_mcomment closing k d =         (* multiline comment. /* was eaten. *)
  if (d.c = ux_eoi) then (epos d; ret err_unclosed_comment ret_eoi d) else
  if closing then begin
    if (d.c = u_slash) then (epos d; ret (`Comment (`M, buf d)) (readc k) d)else
    if (d.c = u_times) then (badd d; readc (r_mcomment true k) d) else
    (baddc d u_times; badd d; readc (r_mcomment false k) d)
  end else begin
    if (d.c = u_times) then readc (r_mcomment true k) d else
    (badd d; readc (r_mcomment false k) d)
  end

let r_comment k d =                                 (* comment, / was eaten. *)
  if d.c = u_slash then readc (r_scomment k) d else
  if d.c = u_times then readc (r_mcomment false k) d else
  (epos d; ret err_exp_comment k d)

let rec r_ws_uncut k d =
  if (is_white d.c) then (epos d; badd d; readc (r_ws_uncut k) d) else
  ret (`White (buf d)) k d

let rec r_white_uncut k d =                                (* {ws} / comment *)
  if (is_white d.c) then (spos d; r_ws_uncut (r_white_uncut k) d) else
  if (d.c = u_slash) then (spos d; readc (r_comment (r_white_uncut k)) d) else
  k d

let rec r_ws k d = if (is_white d.c) then readc (r_ws k) d else k d  (* {ws} *)
let r_white k d = if d.uncut then r_white_uncut k d else r_ws k d

let rec r_u_escape hi u count k d =                      (* unicode escapes. *)
  let error err k d = baddc d Uutf.u_rep; ret err k d in
  if count > 0 then
    if not (is_hex_digit d.c) then (epos d; error (err_not_hex d.c) (readc k) d)
    else
    let u = u * 16 + (if d.c <= 0x39 (* 9 *) then d.c - 0x30 else
                      if d.c <= 0x46 (* F *) then d.c - 0x37 else d.c - 0x57)
    in
    (epos d; readc (r_u_escape hi u (count - 1) k) d)
  else match hi with
  | Some hi ->          (* combine high and low surrogate into scalar value. *)
      if u < 0xDC00 || u > 0xDFFF then error (err_not_lo u) k d else
      let u = ((((hi land 0x3FF) lsl 10) lor (u land 0x3FF)) + 0x10000) in
      (baddc d u; k d)
  | None ->
      if u < 0xD800 || u > 0xDFFF then (baddc d u; k d) else
      if u > 0xDBFF then error (err_lone_lo u) k d else
      if d.c <> u_bslash then error (err_lone_hi u) k d else
      readc (fun d ->
        if d.c <> 0x75 (* u *) then error (err_lone_hi u) (r_escape k) d else
        readc (r_u_escape (Some u) 0 4 k) d) d

and r_escape k d = match d.c with
| 0x22 (* '' *)-> baddc d u_quot; readc k d
| 0x5C (* \ *) -> baddc d u_bslash; readc k d
| 0x2F (* / *) -> baddc d u_slash; readc k d
| 0x62 (* b *) -> baddc d 0x08; readc k d
| 0x66 (* f *) -> baddc d 0x0C; readc k d
| 0x6E (* n *) -> baddc d u_nl; readc k d
| 0x72 (* r *) -> baddc d 0x0D; readc k d
| 0x74 (* t *) -> baddc d 0x09; readc k d
| 0x75 (* u *) -> readc (r_u_escape None 0 4 k) d
| c -> epos d; baddc d Uutf.u_rep; ret (err_not_esc c) (readc k) d

let rec r_string k d =                                (* {string}, '' eaten. *)
  if d.c = ux_eoi then (epos d; ret err_unclosed_string ret_eoi d) else
  if not (must_escape d.c) then (badd d; readc (r_string k) d) else
  if d.c = u_quot then (epos d; readc k d) else
  if d.c = u_bslash then readc (r_escape (r_string k)) d else
  (epos d; baddc d Uutf.u_rep; ret (err_str_char d.c) (readc (r_string k)) d)

let rec r_float k d =                                            (* {number} *)
  if not (is_val_sep d.c) && d.c <> ux_eoi
  then (epos d; badd d; readc (r_float k) d) else
  let s = buf d in
  try ret (`Lexeme (`Float (float_of_string s))) k d with
  | Failure _ -> ret (err_number s) k d

let rec r_literal k d =                         (* {true} / {false} / {null} *)
  if not (is_val_sep d.c) && d.c <> ux_eoi
  then (epos d; badd d; readc (r_literal k) d) else
  match buf d with
  | "true" -> ret (`Lexeme (`Bool true)) k d
  | "false" -> ret (`Lexeme (`Bool false)) k d
  | "null" -> ret (`Lexeme `Null) k d
  | s -> ret (err_literal s) k d

let r_value err k d = match d.c with                          (* {value} *)
| 0x5B (* [ *) ->                                           (* {begin-array} *)
    spos d; epos d; d.last_start <- true;
    d.stack <- `As (dpos d) :: d.stack;
    ret (`Lexeme `As) (readc k) d
| 0x7B (* { *) ->                                          (* {begin-object} *)
    spos d; epos d; d.last_start <- true; d.next_name <- true;
    d.stack <- `Os (dpos d) :: d.stack;
    ret (`Lexeme `Os) (readc k) d
| 0x22 (* '' *) ->
    let lstring k d = ret (`Lexeme (`String (buf d))) k d in
    spos d; readc (r_string (lstring k)) d
| 0x66 (* f *) | 0x6E (* n *) |  0x74 (* t *) ->
    spos d; r_literal k d
| u when is_digit u || u = u_minus -> spos d; r_float k d
| _ -> err k d

let rec discard_to c1 c2 err k d =
  if d.c = c1 || d.c = c2 || d.c = ux_eoi then ret err k d else
  (epos d; readc (discard_to c1 c2 err k) d)

let r_arr_val k d =          (* [{value-separator}] {value} / {end-array} *)
  let nxval err k d = spos d; discard_to u_comma u_rbrack err k d in
  let last_start = d.last_start in
  d.last_start <- false;
  if d.c = ux_eoi then (stack_range d; ret err_unclosed_arr ret_eoi d) else
  if d.c = u_rbrack then (dpop d; ret (`Lexeme `Ae) (readc k) d) else
  if last_start then r_value (nxval err_exp_arr_fst) k d else
  if d.c = u_comma then readc (r_white (r_value (nxval err_exp_value) k)) d
  else nxval err_exp_arr_nxt k d

let nxmem err k d =
  spos d; d.next_name <- true; discard_to u_comma u_rbrace err k d

let r_obj_value k d =                        (* {name-separator} {value} *)
  d.next_name <- true;
  if d.c = u_colon then readc (r_white (r_value (nxmem err_exp_value) k)) d
  else nxmem err_exp_nsep k d

let r_obj_name k d =          (* [{value-separator}] string / end-object *)
  let r_name err k d =
    let ln k d = ret (`Lexeme (`Name (buf d))) k d in
    if d.c <> u_quot then nxmem err k d else (spos d; readc (r_string (ln k)) d)
  in
  let last_start = d.last_start in
  d.last_start <- false; d.next_name <- false;
  if d.c = ux_eoi then (stack_range d; ret err_unclosed_obj ret_eoi d) else
  if d.c = u_rbrace then (dpop d; ret (`Lexeme `Oe) (readc k) d) else
  if last_start then r_name err_exp_obj_fst k d else
  if d.c = u_comma then readc (r_white (r_name err_exp_name k)) d else
  nxmem err_exp_obj_nxt k d

let r_end _ d =                                              (* end of input *)
  if d.c = ux_eoi then ret `End ret_eoi d else
  let drain k d = spos d; discard_to ux_eoi ux_eoi err_exp_eoi k d in
  drain ret_eoi d

let rec r_lexeme d = match d.stack with
| `As _ :: _ -> r_white (r_arr_val r_lexeme) d
| `Os _ :: _ ->
    if d.next_name then r_white (r_obj_name r_lexeme) d else
    r_white (r_obj_value r_lexeme) d
| [] -> r_white (r_end r_lexeme) d

let rec r_json k d =                       (* {begin-array} / {begin-object} *)
  let err k d = spos d; discard_to u_lbrack u_lbrace err_exp_json (r_json k)d in
  if d.c = u_lbrack || d.c = u_lbrace then r_value err k d else err k d

let r_start d =                                            (* start of input *)
  let bom k d = if Uutf.decoder_removed_bom d.u then ret err_bom k d else k d in
  readc (bom (r_white (r_json r_lexeme))) d

let decoder ?encoding src =
  let u = Uutf.decoder ?encoding ~nln:(`ASCII 0x000A) src in
  { u; buf = Buffer.create 1024; uncut = false;
    s_line = 1; s_col = 0; e_line = 1; e_col = 0;
    c = ux_soi; next_name = false; last_start = false; stack = [];
    k = r_start }

let decode_uncut d = d.uncut <- true; d.k d
let decode d = match (d.uncut <- false; d.k d) with
| #decode as v -> (v :> [> decode])
| `Comment _ | `White _ -> assert false

let decoder_src d = Uutf.decoder_src d.u
let decoded_range d = (d.s_line, d.s_col), (d.e_line, d.e_col)
let decoder_encoding d = match Uutf.decoder_encoding d.u with
| #encoding as enc -> enc
| `US_ASCII | `ISO_8859_1 -> assert false

(* Encode *)

let invalid_arg fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); invalid_arg (Buffer.contents b) in
  Format.kfprintf k ppf fmt

let invalid_bounds j l = invalid_arg "invalid bounds (index %d, length %d)" j l
let expect e v = invalid_arg "%a encoded but expected %s" pp_decode v e
let expect_await v = expect "`Await" v
let expect_end l = expect "`End" (`Lexeme l)
let expect_mem_value l = expect "any `Lexeme but `Name, `Oe or `Ae" (`Lexeme l)
let expect_arr_value_ae l = expect "any `Lexeme but `Name or `Oe" (`Lexeme l)
let expect_name_or_oe l = expect "`Lexeme (`Name _ | `Oe)" (`Lexeme l)
let expect_json v = expect "`Lexeme (`As | `Os)" v
let expect_lend lstart v  =
  expect (if lstart = `As then "`Lexeme `Ae" else "`Lexeme `Oe") v

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
type encode = [ `Await | `End | `Lexeme of lexeme ]
type encoder =
  { dst : dst;                                        (* output destination. *)
    minify : bool;                             (* [true] for compact output. *)
    mutable o : string;                             (* current output chunk. *)
    mutable o_pos : int;                   (* next output position to write. *)
    mutable o_max : int;                (* maximal output position to write. *)
    buf : Buffer.t;                              (* buffer to format floats. *)
    mutable stack : [`As | `Os ] list;  (* stack of open arrays and objects. *)
    mutable nest : int;              (* nesting level (String.length stack). *)
    mutable next_name : bool;         (* [true] if next encode should `Name. *)
    mutable last_start : bool;     (* [true] if last encode was [`As | `Os]. *)
    mutable k :                                     (* decoder continuation. *)
      encoder -> [ encode | uncut ] -> [ `Ok | `Partial ] }

let o_rem e = e.o_max - e.o_pos + 1    (* remaining bytes to write in [e.o]. *)
let dst e s j l =                                     (* set [e.o] with [s]. *)
  if (j < 0 || l < 0 || j + l > String.length s) then invalid_bounds j l;
  e.o <- s; e.o_pos <- j; e.o_max <- j + l - 1

let partial k e = function `Await -> k e | v -> expect_await v
let flush k e = match e.dst with  (* get free space in [d.o] and [k]ontinue. *)
| `Manual -> e.k <- partial k; `Partial
| `Buffer b -> Buffer.add_substring b e.o 0 e.o_pos; e.o_pos <- 0; k e
| `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0; k e

let rec writeb b k e =                     (* write byte [b] and [k]ontinue. *)
  if e.o_pos > e.o_max then flush (writeb b k) e else
  (unsafe_set_byte e.o e.o_pos b; e.o_pos <- e.o_pos + 1; k e)

let rec writes s j l k e =      (* write [l] bytes from [s] starting at [j]. *)
  let rem = o_rem e in
  if rem >= l then (unsafe_blit s j e.o e.o_pos l; e.o_pos <- e.o_pos + l; k e)
  else begin
    unsafe_blit s j e.o e.o_pos rem; e.o_pos <- e.o_pos + rem;
    flush (writes s (j + rem) (l - rem) k) e
  end

let rec writebuf j l k e =  (* write [l] bytes from [e.buf] starting at [j]. *)
  let rem = o_rem e in
  if rem >= l
  then (Buffer.blit e.buf j e.o e.o_pos l; e.o_pos <- e.o_pos + l; k e)
  else begin
    Buffer.blit e.buf j e.o e.o_pos rem; e.o_pos <- e.o_pos + rem;
    flush (writebuf (j + rem) (l - rem) k) e
  end

let w_indent k e =
  let rec loop indent k e =
    let spaces e indent =
      let max = e.o_pos + indent - 1 in
      for j = e.o_pos to max do unsafe_set_byte e.o j u_sp done;
      e.o_pos <- max + 1
    in
    let rem = o_rem e in
    if rem < indent then (spaces e rem; flush (loop (indent - rem) k) e) else
    (spaces e indent; k e)
  in
  loop (e.nest * 2) k e

let w_json_string s k e =        (* escapes as mandated by the standard. *)
  let rec loop s j pos max k e =
    if pos > max then (if j > max then k e else writes s j (pos - j) k e) else
    let next = pos + 1 in
    let escape esc =                     (* assert (String.length esc = 2 ). *)
      writes s j (pos - j) (writes esc 0 2 (loop s next next max k)) e
    in
    match unsafe_byte s pos with
    | 0x22 -> escape "\\\""
    | 0x5C -> escape "\\\\"
    | 0x0A -> escape "\\n"
    | c when c <= 0x1F ->
        let hex d = (if d < 10 then 0x30 + d else 0x41 + (d - 10)) in
        writes s j (pos - j)
          (writes "\\u00" 0 4
             (writeb (hex (c lsr 4))
                (writeb (hex (c land 0xF))
                   (loop s next next max k)))) e
    | _ -> loop s j next max k e
  in
  writeb u_quot (loop s 0 0 (String.length s - 1) (writeb u_quot k)) e

let w_name n k e =
  e.last_start <- false; e.next_name <- false;
  w_json_string n (writeb u_colon k) e

let w_value ~in_obj l k e = match l with
| `String s ->
    e.last_start <- false; e.next_name <- in_obj;
    w_json_string s k e
| `Bool b ->
    e.last_start <- false; e.next_name <- in_obj;
    if b then writes "true" 0 4 k e else writes "false" 0 5 k e
| `Float f ->
    e.last_start <- false; e.next_name <- in_obj;
    Buffer.clear e.buf; Printf.bprintf e.buf "%.16g" f;
    writebuf 0 (Buffer.length e.buf) k e
| `Os ->
    e.last_start <- true; e.next_name <- true;
    e.nest <- e.nest + 1; e.stack <- `Os :: e.stack;
    writeb u_lbrace k e
| `As ->
    e.last_start <- true; e.next_name <- false;
    e.nest <- e.nest + 1; e.stack <- `As :: e.stack;
    writeb u_lbrack k e
| `Null ->
    e.last_start <- false; e.next_name <- in_obj;
    writes "null" 0 4 k e
| `Oe | `Ae | `Name _ as l ->
    if in_obj then expect_mem_value l else expect_arr_value_ae l

let w_lexeme k e l =
  let epop e =
    e.last_start <- false;
    e.nest <- e.nest - 1;  e.stack <- List.tl e.stack;
    match e.stack with
    | `Os :: _ -> e.next_name <- true;
    | _ -> e.next_name <- false
  in
  match List.hd e.stack with
  | `Os ->                                                 (* inside object. *)
      if not e.next_name then w_value ~in_obj:true l k e else
      begin match l with
      | `Name n ->
          let name n k e =
            if e.minify then w_name n k e else
            writeb u_nl (w_indent (w_name n (writeb u_sp k))) e
          in
          if e.last_start then name n k e else
          writeb u_comma (name n k) e
      | `Oe ->
          if e.minify || e.last_start then (epop e; writeb u_rbrace k e) else
          (epop e; writeb u_nl (w_indent (writeb u_rbrace k)) e)
      | _ -> expect_name_or_oe l
      end
  | `As ->                                                  (* inside array. *)
      begin match l with
      | `Ae ->
          if e.minify || e.last_start then (epop e; writeb u_rbrack k e) else
          (epop e; writeb u_nl (w_indent (writeb u_rbrack k)) e)
      | l ->
          let value l k e =
            if e.minify then w_value ~in_obj:false l k e else
            writeb u_nl (w_indent (w_value ~in_obj:false l k)) e
          in
          if e.last_start then value l k e else
          writeb u_comma (value l k) e
      end

let encode_ k e = function
| `Lexeme l ->
    if e.stack = [] then expect_end l else w_lexeme k e l
| `End as v ->
    if e.stack = [] then flush k e else expect_lend (List.hd e.stack) v
| `White w ->
    writes w 0 (String.length w) k e
| `Comment (`S, c) ->
    writes "//" 0 2 (writes c 0 (String.length c) (writeb u_nl k)) e
| `Comment (`M, c) ->
    writes "/*" 0 2 (writes c 0 (String.length c) (writes "*/" 0 2 k)) e
| `Await -> `Ok

let rec encode_loop e = e.k <- encode_ encode_loop; `Ok
let rec encode_json e = function  (* first [k] to start with [`Os] or [`As]. *)
| `Lexeme (`Os | `As as l) -> w_value ~in_obj:true (* irrelevant *) l encode_loop e
| `End | `Lexeme _ as v -> expect_json v
| `White _ | `Comment _ as v -> encode_ (fun e -> e.k <- encode_json; `Ok) e v
| `Await -> `Ok

let encoder ?(minify = true) dst =
  let o, o_pos, o_max = match dst with
  | `Manual -> "", 1, 0                            (* implies [o_rem e = 0]. *)
  | `Buffer _
  | `Channel _ -> String.create io_buffer_size, 0, io_buffer_size - 1
  in
  { dst = (dst :> dst); minify; o; o_pos; o_max; buf = Buffer.create 30;
    stack = []; nest = 0; next_name = false; last_start = false;
    k = encode_json }

let encode e v = e.k e (v :> [ encode | uncut ])
let encoder_dst e = e.dst
let encoder_minify e = e.minify

(* Manual *)

module Manual = struct
  let src d = Uutf.Manual.src d.u
  let dst = dst
  let dst_rem = o_rem
end

(* Uncut *)

module Uncut = struct
  let decode = decode_uncut
  let pp_decode = pp_decode
  let encode e v = e.k e (v :> [ encode | uncut])
end

(* String conversion *)
exception Escape of ((int * int) * (int * int)) * error
type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]

let json_of_src ?encoding src =
  let dec d = match decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d  | `As -> arr [] k d
    | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

let of_string str: t =
  match json_of_src (`String str) with
  | `JSON j  -> j
  | `Error _ -> failwith "json_of_string"

let json_to_dst ~minify dst (json:t) =
  let enc e l = ignore (encode e (`Lexeme l)) in
  let rec value v k e = match v with
    | `A vs -> arr vs k e
    | `O ms -> obj ms k e
    | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with
    | v :: vs' -> value v (arr_vs vs' k) e
    | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with
    | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
    | [] -> enc e `Oe; k e
  in
  let e = encoder ~minify dst in
  let finish e = ignore (encode e `End) in
  match json with
  | `A _ | `O _ as json -> value json finish e
  | _ -> invalid_arg "invalid json text"

let to_string (json:t) =
  let buf = Buffer.create 1024 in
  json_to_dst ~minify:false (`Buffer buf) json;
  Buffer.contents buf

let json_output = ref None

let json_buffer = ref []

let add json =
  json_buffer := json :: !json_buffer

let set_output write =
  json_output := Some write

let verbose () =
  !json_output <> None

let output () =
  match !json_output with
  | None      -> ()
  | Some write ->
    let json = `A (List.rev !json_buffer) in
    write (to_string json)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
