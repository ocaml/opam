(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   jsonm release 0.9.1
  ---------------------------------------------------------------------------*)

(** Non-blocking streaming JSON codec.

    [Jsonm] is a non-blocking streaming codec to
    {{!section:decode}decode} and {{!section:encode}encode} the
    {{:http://tools.ietf.org/html/rfc4627}JSON} data format. It can
    process JSON text without blocking on IO and without a complete
    in-memory representation of the data.

    The {{!Uncut}uncut codec} also processes whitespace and
    (non-standard) JSON with JavaScript comments.

    Consult the {{!datamodel}data model}, {{!limitations}limitations}
    and {{!examples}examples} of use.

    {e Version 0.9.1 - Daniel Bünzli <daniel.buenzli at erratique.ch> }
    {3 References}
    {ul
    {- D. Crockford
    {e {{:http://tools.ietf.org/html/rfc4627}JSON The application/json Media
    Type for JavaScript Object Notation (JSON)}}, 2006.}} *)

(** {1:datamodel JSON data model} *)

type lexeme = [
| `Null
| `Bool of bool
| `String of string
| `Float of float
| `Name of string
| `As
| `Ae
| `Os
| `Oe ]
(** The type for JSON lexemes. [`As] and [`Ae]
    start and end arrays and [`Os] and [`Oe] start
    and end objects. [`Name] is for the member names of objects.

    A {e well-formed} sequence of lexemes belongs to the language of
    the [json] grammar:
{[
  json = object / array
object = `Os *member `Oe
member = (`Name s) value
 array = `As *value `Ae
 value = `Null / `Bool b / `Float f / `String s / object / array
]}
    A {{!section:decode}decoder} returns only well-formed sequences of
    lexemes or [`Error]s are returned. The
    {{:http://tools.ietf.org/html/rfc3629}UTF-8},
    {{:http://tools.ietf.org/html/rfc2781}UTF-16}, UTF-16LE and
    UTF-16BE encoding schemes are supported.  The strings of decoded
    [`Name] and [`String] lexemes are however always UTF-8 encoded. In
    these strings, characters originally escaped in the input are in
    their unescaped representation.

    An {{!section:encode}encoder} accepts only well-formed sequences
    of lexemes or [Invalid_argument] is raised. Only the UTF-8
    encoding scheme is supported. The strings of encoded [`Name] and
    [`String] lexemes are assumed to be immutable and must be UTF-8
    encoded, this is {b not} checked by the module. In these strings,
    the delimiter characters [U+0022] and [U+005C] (['"'], ['\'])
    aswell as the control characters [U+0000-U+001F] are automatically
    escaped by the encoders, as mandated by the standard. *)

val pp_lexeme : Format.formatter -> [< lexeme] -> unit
(** [pp_lexeme ppf l] prints a unspecified non-JSON representation of [l]
    on [ppf]. *)

(** {1:decode Decode} *)

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

(** The type for decoding errors. *)

val pp_error : Format.formatter -> [< error] -> unit
(** [pp_error e] prints an unspecified UTF-8 representation of [e] on [ppf]. *)

type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]
(** The type for Unicode encoding schemes. *)

type src = [ `Channel of in_channel | `String of string | `Manual ]
(** The type for input sources. With a [`Manual] source the client
    must provide input with {!Manual.src}. *)

type decoder
(** The type for JSON decoders. *)

val decoder :?encoding:[< encoding] -> [< src] -> decoder
(** [decoder encoding src] is a JSON decoder that inputs from [src].
    [encoding] specifies the character encoding of the data. If unspecified
    the encoding is guessed as
    {{:http://tools.ietf.org/html/rfc4627#section-3}suggested} by
    the standard. *)

val decode : decoder -> [> `Await | `Lexeme of lexeme | `End | `Error of error ]
(** [decode d] is:
    {ul
    {- [`Await] if [d] has a [`Manual] source and awaits for more input.
       The client must use {!Manual.src} to provide it.}
    {- [`Lexeme l] if a lexeme [l] was decoded.}
    {- [`End] if the end of input was reached.}
    {- [`Error e] if a decoding error occured. If the client is interested
       in a best-effort decoding it can still continue to decode
       after an error (see {!errorrecovery}) although the resulting sequence
       of [`Lexeme]s is undefined and may not be well-formed.}}

    The {!Uncut.pp_decode} function can be used to inspect decode results.

    {b Note.} Repeated invocation always eventually returns [`End], even
    in case of errors. *)

val decoded_range : decoder -> (int * int) * (int * int)
(** [decoded_range d] is the range of characters spanning the last
    [`Lexeme] or [`Error] (or [`White] or [`Comment] for an
    {!Uncut.decode}) decoded by [d].  A pair of line and column numbers
    respectively one and zero based. *)

val decoder_encoding : decoder -> encoding
(** [decoder_encoding d] is [d]'s encoding.

    {b Warning.} If the decoder guesses the encoding, rely on this
    value only after the first [`Lexeme] was decoded. *)

val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s input source. *)

(** {1:encode Encode} *)

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
(** The type for output destinations. With a [`Manual] destination the
    client must provide output storage with {!Manual.dst}. *)

type encoder
(** The type for JSON encoders. *)

val encoder : ?minify:bool -> [< dst] -> encoder
(** [encoder minify dst] is an encoder that outputs to [dst]. If
    [minify] is [true] (default) the output is made as compact as
    possible, otherwise the output is indented. If you want better
    control on whitespace use [minify = true] and {!Uncut.encode}. *)

val encode : encoder -> [< `Await | `End | `Lexeme of lexeme ] ->
  [`Ok | `Partial]
(** [encode e v] is:
    {ul
    {- [`Partial] iff [e] has a [`Manual] destination and needs more
       output storage. The client must use {!Manual.dst} to provide
       a new buffer and then call {!encode} with [`Await] until [`Ok]
       is returned.}
    {- [`Ok] when the encoder is ready to encode a new [`Lexeme]
       or [`End].}}
    For [`Manual] destinations, encoding [`End] always returns [`Partial],
    the client should as usual use {!Manual.dst} and continue with [`Await]
    until [`Ok] is returned at which point {!Manual.dst_rem} [e] is guaranteed
    to be the size of the last provided buffer (i.e. nothing was written).

    {b Raises.} [Invalid_argument] if a non {{!datamodel}well-formed}
    sequence of lexemes is encoded or if [`Lexeme] or [`End] is
    encoded after a [`Partial] encode. *)

val encoder_dst : encoder -> dst
(** [encoder_dst e] is [e]'s output destination. *)

val encoder_minify : encoder -> bool
(** [encoder_minify e] is [true] if [e]'s output is minified. *)

(** {1:manual Manual sources and destinations} *)

(** Manual input sources and output destinations.

    {b Warning.} Use only with [`Manual] decoders and encoders. *)
module Manual : sig

  val src : decoder -> string -> int -> int -> unit
  (** [src d s j l] provides [d] with [l] bytes to read, starting
      at [j] in [s]. This byte range is read by calls to {!decode} until
      [`Await] is returned. To signal the end of input call the function
      with [l = 0]. *)

  val dst : encoder -> string -> int -> int -> unit
  (** [dst e s j l] provides [e] with [l] bytes to write, starting
      at [j] in [s]. This byte rang is written by calls to {!encode} with [e]
      until [`Partial] is returned. Use {!dst_rem} to know the remaining
      number of non-written free bytes in [s]. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is the remaining number of non-written, free bytes
      in the last buffer provided with {!dst}. *)
end

(** {1:uncut Uncut codec} *)

(** Codec with comments and whitespace.

    The uncut codec also processes whitespace and JavaScript
    comments. The latter is non-standard JSON, fail on [`Comment]
    decoding if you want to process whitespace but stick to the standard.

    The uncut codec preserves as much of the original input as
    possible. Perfect round-trip with [Jsonm] is however impossible for
    the following reasons:
    {ul
    {- Escapes unescaped by the decoder may not be escaped or escaped
       differently by the encoder.}
    {- The encoder automatically inserts name separator [':'] and
       value separators [","]. If you just reencode the sequence of
       decodes, whitespace and comments may (harmlessly, but significantly)
       commute with these separators.}
    {- Internally the encoder uses [U+000A] (['\n']) for newlines.}
    {- [`Float] lexemes may be rewritten differently by the encoder.}}
*)
module Uncut : sig

  (** {1:uncutdatamodel Uncut data model}

      The uncut data model is the same as the regular
      {{!datamodel}data model}, except that before or after any lexeme
      you may decode/encode one or more:
      {ul
      {- [`White w], representing JSON whitespace [w]. On input
         the sequence CR ([U+000D]) and CRLF (<[U+000A], [U+000A]>)
         are normalized to [U+000A]. The string [w] must be
         a sequence of [U+0020], [U+0009], [U+000A] or [U+000D]
         characters ([' '], ['\t'], ['\n'], ['\r']).}
      {- [`Comment (`S, c)], representing a JavaScript single line
      comment [c]. [c] is the comment's content without the starting
      [//] and the ending newline. The string [c] must not contain any newline.
      }
      {- [`Comment (`M, c)], representing a JavaScript multi-line
      comment [c]. [c] is the comment's content without the starting
      [/*] and the ending [*/]. The string [c] must not contain the
      sequence [*/]. }}

      {b Warning.} {!Uncut.encode} does not check the above constraints on
      [w] and [c]. *)

  (** {1 Decode} *)

  val decode : decoder ->
    [ `Await | `Lexeme of lexeme | `White of string
    | `Comment of [ `S | `M ] * string
    | `End | `Error of error ]
  (** [decode d] is like {!Jsonm.decode} but for the
      {{!uncutdatamodel}uncut data model}. *)

  val pp_decode : Format.formatter ->
    [< `Await | `Lexeme of lexeme | `White of string
    | `Comment of [ `S | `M ] * string
    | `End | `Error of error ] -> unit
  (** [pp_decode ppf v] prints an unspecified representation of [v]
      on [ppf]. *)

  (** {1 Encode} *)

  val encode : encoder ->
    [< `Await | `Lexeme of lexeme | `White of string
    | `Comment of [`S | `M] * string |  `End ] -> [`Ok | `Partial]
  (** [encode] is like {!Jsonm.encode} but for the {{!uncutdatamodel}
      uncut data model}.

      {b IMPORTANT.} Never encode [`Comment] for the web, it is
      non-standard and breaks interoperability. *)
end

(** {1:limitations Limitations}

    {2 Decode}

    Decoders parse valid JSON with the following limitations:
    {ul
    {- JSON numbers are represented with OCaml [float] values.
       This means that it can only represent integers exactly
       in the in the interval \[-2{^53};2{^53}\]. This is equivalent
       to the contraints JavaScript has.}
    {- A superset of JSON numbers is parsed. After having seen a minus
       or a digit, including zero, {!Pervasives.float_of_string}, is
       used. In particular this parses number with leading zeros, which are
       specifically prohibited by the standard.}
    {- Strings returned by [`String], [`Name], [`White] and [`Comment]
       are limited by {!Sys.max_string_length}.  There is no built-in
       protection against the fact that the internal OCaml [Buffer.t]
       value may raise [Failure] on {!Jsonm.decode}. This should
       however only be a problem on 32-bits platforms if your
       strings are greater than 16Mo.}}

    Position tracking assumes that each decoded Unicode scalar value
    has a column width of 1. The same assumption may not be made by
    the display program (e.g. for [emacs]' compilation mode you need
    to set [compilation-error-screen-columns] to [nil]).

    The newlines LF ([U+000A]), CR ([U+000D]), and CRLF are all normalized
    to LF internally. This may have an impact in some corner [`Error]
    cases. For example the invalid escape sequence [<U+005C,U+000D>] in
    a string will be reported as being [`Illegal_escape (`Not_esc_uchar
    0x000A)].

    {2 Encode}

    Encoders produce valid JSON provided the {e client} ensures that
    the following holds.
    {ul
    {- All the strings given to the encoder must be valid UTF-8 and immutable.
       Characters that need to be escaped are automatically escaped by [Jsonm].}
    {- [`Float] lexemes must not be, {!Pervasives.nan},
       {!Pervasives.infinity} or {!Pervasives.neg_infinity}. They
       are encoded with the format string ["%.16g"], this allows
       to roundtrip all the integers that can be precisely represented
       in OCaml [float] values, i.e. the integers in the interval
       \[-2{^53};2{^53}\]. This is equivalent to the constraints
       JavaScript has.}
    {- If the {{!Uncut}uncut} codec is used [`White] must be made
       of {{!Uncut.uncutdatamodel}JSON whitespace} and [`Comment]
       must never be encoded.}}
*)

(** {1:errorrecovery Error recovery}

    After a decoding error, if best-effort decoding is performed. The following
    happens before continuing:
    {ul
    {- [`Illegal_BOM], the initial
       {{:http://unicode.org/glossary/#byte_order_mark}BOM} is skipped.}
    {- [`Illegal_bytes], [`Illegal_escape], [`Illegal_string_uchar], a
       Unicode
       {{:http://unicode.org/glossary/#replacement_character}replacement
       character} ([U+FFFD]) is substituted to the illegal sequence.}
    {- [`Illegal_literal], [`Illegal_number] the corresponding
       [`Lexeme] is skipped.}
    {- [`Expected r], input is discarded until a synchronyzing lexeme
       that depends on [r] is found.}
    {- [`Unclosed], the end of input is reached, further decodes will be
       [`End]}} *)

(** {1:examples Examples}

    {2:filter Trip}

    The result of [trip src dst] has the JSON from [src] written on [dst].
{[
let trip ?encoding ?minify
    (src : [`Channel of in_channel | `String of string])
    (dst : [`Channel of out_channel | `Buffer of Buffer.t])
  =
  let rec loop d e = match Jsonm.decode d with
  | `Lexeme _ as v -> ignore (Jsonm.encode e v); loop d e
  | `End -> ignore (Jsonm.encode e `End); `Ok
  | `Error err -> `Error (Jsonm.decoded_range d, err)
  | `Await -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  let e = Jsonm.encoder ?minify dst in
  loop d e
]}
    Using the [`Manual] interface, [trip_fd] does the same but between Unix
    file descriptors.
{[
let trip_fd ?encoding ?minify
    (fdi : Unix.file_descr)
    (fdo : Unix.file_descr)
  =
  let rec encode fd s e v = match Jsonm.encode e v with `Ok -> ()
  | `Partial ->
      let rec unix_write fd s j l =
        let rec write fd s j l = try Unix.single_write fd s j l with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l
        in
        let wc = write fd s j l in
        if wc < l then unix_write fd s (j + wc) (l - wc) else ()
      in
      unix_write fd s 0 (String.length s - Jsonm.Manual.dst_rem e);
      Jsonm.Manual.dst e s 0 (String.length s);
      encode fd s e `Await
  in
  let rec loop fdi fdo ds es d e = match Jsonm.decode d with
  | `Lexeme _ as v -> encode fdo es e v; loop fdi fdo ds es d e
  | `End -> encode fdo es e `End; `Ok
  | `Error err -> `Error (Jsonm.decoded_range d, err)
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l
      in
      let rc = unix_read fdi ds 0 (String.length ds) in
      Jsonm.Manual.src d ds 0 rc; loop fdi fdo ds es d e
  in
  let ds = String.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let es = String.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let d = Jsonm.decoder ?encoding `Manual in
  let e = Jsonm.encoder ?minify `Manual in
  Jsonm.Manual.dst e es 0 (String.length es);
  loop fdi fdo ds es d e
]}
    {2:memsel Member selection}

    The result of [memsel names src] is the list of string values of
    members of [src] that have their name in [names]. In this example,
    decoding errors are silently ignored.
{[
let memsel ?encoding names
    (src : [`Channel of in_channel | `String of string])
  =
  let rec loop acc names d = match Jsonm.decode d with
  | `Lexeme (`Name n) when List.mem n names ->
      begin match Jsonm.decode d with
      | `Lexeme (`String s) -> loop (s :: acc) names d
      | _ -> loop acc names d
      end
  | `Lexeme _ | `Error _ -> loop acc names d
  | `End -> List.rev acc
  | `Await -> assert false
  in
  loop [] names (Jsonm.decoder ?encoding src)
]}

    {2:tree Generic JSON representation}

    A generic OCaml representation of JSON text is the following one.
{[
type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]
]}
    The result of [json_of_src src] is the JSON text from [src] in this
    representation. The function is tail recursive.
{[
exception Escape of ((int * int) * (int * int)) * Jsonm.error

let json_of_src ?encoding
    (src : [`Channel of in_channel | `String of string])
  =
  let dec d = match Jsonm.decode d with
  | `Lexeme l -> l
  | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
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
  let d = Jsonm.decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)
]}
    The result of [json_to_dst dst json] has the JSON text [json] written
    on [dst]. The function is tail recursive.
{[
let json_to_dst ~minify
    (dst : [`Channel of out_channel | `Buffer of Buffer.t ])
    (json : json)
  =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
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
  let e = Jsonm.encoder ~minify dst in
  let finish e = ignore (Jsonm.encode e `End) in
  match json with `A _ | `O _ as json -> value json finish e
  | _ -> invalid_arg "invalid json text"
]}
*)

type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]

val to_string: json -> string

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
