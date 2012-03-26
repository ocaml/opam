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

(* XXX: quick hack, untested *)
let qreplace ~pat ~templ str =
  let r = Re_perl.compile_pat pat in
  let ss = Re.exec r str in
  let ofs = Re.get_all_ofs ss in
  let b = Buffer.create (String.length str) in
  let prev = ref 0 in
  for i = 0 to Array.length ofs - 1 do
    let orig, off = ofs.(i) in
    Buffer.add_substring b str !prev (orig - !prev);
    Buffer.add_string b templ;
    prev := orig + off;
  done;
  if !prev <> String.length str then
    Buffer.add_substring b str !prev (String.length str - !prev);
  Buffer.contents b
