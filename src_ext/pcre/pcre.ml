type regexp = Re.re

let regexp ?(flags = []) pat =
  let opts = List.map (function
    | `CASELESS -> `Caseless
  ) flags in
  Re_perl.compile_pat ~opts pat

let extract ~rex s =
  Re.get_all (Re.exec rex s)

let exec ~rex ?pos s =
  Re.exec rex ?pos s

let get_substring s i =
  Re.get s i

let get_substring_ofs s i =
  Re.get_ofs s i

let pmatch ~rex s =
  Re.execp rex s

let substitute ~rex ~subst str =
  let b = Buffer.create 1024 in
  let rec loop pos =
    if pos >= String.length str then
      Buffer.contents b
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.get_ofs ss 0 in
      let pat = Re.get ss 0 in
      Buffer.add_substring b str pos (start - pos);
      Buffer.add_string b (subst pat);
      loop fin
    ) else (
      Buffer.add_substring b str pos (String.length str - pos);
      loop (String.length str)
    )
  in
  loop 0

let split ~rex str =
  let rec loop accu pos =
    if pos >= String.length str then
      List.rev accu
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.get_ofs ss 0 in
      let s = String.sub str pos (start - pos) in
      loop (s :: accu) fin
    ) else (
      let s = String.sub str pos (String.length str - pos) in
      loop (s :: accu) (String.length str)
    ) in
  loop [] 0

(* From PCRE *)
let string_unsafe_sub s ofs len =
  let r = String.create len in
  String.unsafe_blit s ofs r 0 len;
  r

let quote s =
  let len = String.length s in
  let buf = String.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      String.unsafe_set buf !pos '\\';
      incr pos;
      String.unsafe_set buf !pos c; incr pos
    | c -> String.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub buf 0 !pos
