(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open String

(* Character predicates *)
let for_all f s =
  let res = ref true in
  for i = 0 to length s - 1 do
    res := !res && f s.[i]
  done;
  !res

let exists f s =
  let res = ref false in
  for i = 0 to length s - 1 do
    res := !res || f s.[i]
  done;
  !res

(* The substring before [pos] (the character at position [pos] is discarded) *)
let before s pos =
  sub s 0 pos

(* The substring after [pos] (the character at position [pos] is discarded) *)
let after s pos =
  sub s (pos + 1) (length s - pos - 1)

let rec strneql s1 s2 len =
  len = 0 || (
    let len = len - 1 in
    s1.[len] = s2.[len] && strneql s1 s2 len)

let starts_with s ~prefix =
  let len1 = length s in
  let len2 = length prefix in
  len2 <= len1 && strneql s prefix len2

let ends_with s ~suffix =
  let rec aux offset len =
    len = 0 || (
      let len = len - 1 in
      s.[offset+len] = suffix.[len] && aux offset len) in
  let len1 = length s in
  let len2 = length suffix in
  len2 <= len1 && aux (len1 - len2) len2

(* Cut the string at position [pos] (the character at position [pos]
   is kept if [keep]) *)
let cut ?(keep=false) s pos =
  try
    let npos = if keep then pos - 1 else pos in
    before s pos, after s npos
  with _ -> s, ""

let cuts str =
  let r = ref [] in
  for i = 0 to String.length str - 1 do
    r := cut ~keep:true str i :: !r
  done;
  List.rev ((str,"") :: !r)

let cut_at ?(keep=false) s c =
  try
    let pos = index s c in
    cut ~keep s pos
  with _ -> s, ""

let rcut_at ?(keep=false) s c =
  try
    let pos = rindex s c in
    cut ~keep s pos
  with _ -> s, ""

let is_ws = function
  | '\r' | ' ' | '\t' -> true
  | _ -> false

let is_ws_or_nl = function
  | '\r' | ' ' | '\t' | '\n'-> true
  | _ -> false

let strip_aux ?(fn=is_ws) dir s =
  if s = "" then
    s
  else begin
    let n = length s - 1 in
    let i = ref 0 in
    let j = ref n in
    begin match dir with
      | `Left | `All ->
        let break = ref false in
        while !i <= n && not !break do
          if fn s.[!i] then incr i else break := true
        done
      | `Right -> ()
    end;
    begin match dir with
      | `All | `Right ->
        let break = ref false in
        while !j >= 0 && not !break do
          if fn s.[!j] then decr j else break := true
        done
      | `Left -> ()
    end;
    if !i <> !j && (!i = n || !j = 0) then
      ""
    else if !i = 0 && !j = n then
      s
    else if !j - !i + 1 <= 0 then
        ""
      else
        sub s !i (!j - !i + 1)
    end

let lstrip ?fn = strip_aux `Left ?fn
let rstrip ?fn = strip_aux `Right ?fn
let strip ?fn = strip_aux `All ?fn

(* [n] is the max number of splits; [dir] specifies the direction in
   which we start cutting the string into pieces (ie. left or
   right).  [sep] is a pair of open/close separators, where the
   splitting is not done : ie. [split ~sep=("(",")") "f(1 2) 3" ' ']
   should return ["f(1 2)"; "3"]. *)
let split_aux dir ?(n=max_int) s ch =
  let x = ref [] in
  let index = match dir with
    | `Left  -> index
    | `Right -> rindex in
  let cut s pos = match dir with
    | `Left  -> before s pos, after s pos
    | `Right -> after s pos , before s pos in
  let return () = match dir with
    | `Left  -> List.rev !x
    | `Right -> !x in
  let add = function
    | "" -> ()
    | s  -> x := s :: !x in
  let rec aux s =
    if List.length !x + 1 = n then
      add (strip_aux dir ~fn:((=)ch) s)
    else begin
      try
        let pos = index s ch in
        let current, next = cut s pos in
        add current;
        aux next
      with Not_found ->
        add s
    end in
  aux s;
  return ()

let splitn n s ch = split_aux `Left ~n s ch
let rsplitn n s ch = split_aux `Right ~n s ch
let rsplit s ch = split_aux `Right s ch

exception Out_of_bounds
(* Returns the pair (n, j) such that : lines.(n).[j] = (lines.(0) ^
   ... ^ lines.(m)).[i] It throws [Out_of_bounds] if the indice is
   too large. *)
let indexes lines =
  let nlines = Array.length lines - 1 in
  if nlines > 0 then begin
    let sizes = Array.map (fun l -> length l) lines in
    let csizes = Array.copy sizes in
    for i = 1 to nlines do
      csizes.(i) <- csizes.(i-1) + sizes.(i)
    done;
    (fun i ->
      if i >= csizes.(nlines) then
        raise Out_of_bounds
      else begin
        let n = ref 0 in
        let j = ref i in
        while !n <= nlines && csizes.(!n) <= i do
          j := !j - sizes.(!n);
          incr n;
        done;
        !n, !j
      end)
  end else
    (fun _ -> raise Out_of_bounds)

let first_word str =
  match splitn 2 str ' ' with
    | []        -> raise Not_found
    | name :: _ -> name

let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let _ =
  assert (split "" 'o' = [""]);
  assert (split "toto" 'o' = ["t"; "t"; ""]);
  assert (split "ototo" 'o' = [""; "t"; "t"; ""]);
  assert (split "tot" 'o' = ["t"; "t"]);
  ()

let split_simplify s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev to_rev else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) to_rev else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let _ =
  assert (split_simplify "" 'o' = []);
  assert (split_simplify "toto" 'o' = ["t"; "t"]);
  assert (split_simplify "ototo" 'o' = ["t"; "t"]);
  assert (split_simplify "tot" 'o' = ["t"; "t"]);
  ()



(* remove spaces and upper case *)
let compact str =
  let l = split (lowercase str) ' ' in
  concat "-" l

let index2_from str pos c1 c2 =
  let n = length str in
  let rec aux p =
    if p + 1 < n && str.[p + 1] = c2
    then p (* ":{" *)
    else aux (index_from str (p + 1) c1) in
  aux (index_from str pos c1)

let index2 str c1 c2 = index2_from str 0 c1 c2

let prefixes str =
  let n = String.length str in
  let l = OcpList.range 1 n in
  List.map (fun n -> String.sub str 0 n) l

let suffixes str =
  let n = String.length str in
  let l = OcpList.range 0 (n-1) in
  List.map (fun k -> String.sub str k (n-k)) l

let substrings str =
  OcpList.flatten_map suffixes (prefixes str)

let split_chars str cs =
  let l =  ref [] in
  let next = ref 0 in
  let current = ref 0 in
  let continue = ref true in
  let n = String.length str in

  while !continue do

    while !current < n && (List.mem str.[!current] cs) do
      incr current
    done;

    next := !current;
    while !next < n && not (List.mem str.[!next] cs) do
      incr next
    done;

    if !current < n then begin
      l :=  String.sub str !current (!next - !current) :: !l;
      current := !next;
    end else
      continue := false

  done;
  List.rev !l

let rec sub_match s pos sub subpos len =
  if subpos = len then true else
    if s.[pos] = sub.[subpos] then
      sub_match s (pos+1) sub (subpos+1) len
    else false

let rec find_from_aux s sub pos len =
  if pos = len then raise Not_found;
  if s.[pos] = sub.[0] && sub_match s (pos+1) sub 1 (String.length sub) then pos
  else find_from_aux s sub (pos+1) len

let find sub s =
  let len = String.length s in
  let sublen = String.length sub in
  if len < sublen then raise Not_found;
  find_from_aux s sub 0 (len - sublen + 1)

let find_from sub s pos =
  let len = String.length s in
  let sublen = String.length sub in
  if len < pos+sublen then raise Not_found;
  find_from_aux s sub pos (len - sublen + 1)

let _ =
  assert (try ignore (find "xyz" "abcdefgh"); false with Not_found -> true);
  assert (find "abc" "abcdef" = 0);
  assert (find "cd" "abcdef" = 2);
  assert (find "def" "abcdef" = 3);

  assert (find_from "abc" "abcdefabc" 0 = 0);
  assert (find_from "abc" "abcdefabc" 1 = 6);
  assert (find_from "abc" "abcdefabc" 2 = 6);
  assert (find_from "abc" "abcdefabc" 3 = 6);
  assert (find_from "abc" "abcdefabc" 4 = 6);
  assert (find_from "abc" "abcdefabc" 5 = 6);
  assert (find_from "abc" "abcdefabc" 6 = 6);
  ()



let unspace line =
  let len = String.length line in
  let line = String.copy line in
  let rec iter line spaced src dst len =
    if src = len then
      String.sub line 0 dst
    else
      let c = line.[src] in
      match c with
          ' ' | '\t' | '\n' ->
            if spaced < 0 then
              iter line (-1) (src+1) dst len
            else
              iter line 1 (src+1) dst len
        | _ ->
          let dst =
            if spaced > 0 then begin
              line.[dst] <- ' ';
              dst + 1
            end else dst
          in
          line.[dst] <- c;
          iter line 0 (src+1) (dst+1) len
  in
  iter line (-1) 0 0 len

(*
let unspace line =
(*  Printf.fprintf stderr "unspace [%s]\n%!" line; *)
  let len = String.length line in
  if len = 0 then line else
  let rec find_first_non_space line pos len =
    if pos = len then len
    else
      match line.[pos] with
          ' ' | '\t' -> find_first_non_space line (pos+1) len
        | _ -> pos
  in
  let pos0 = find_first_non_space line 0 len in
(*  Printf.fprintf stderr "pos0 = %d/%d\n%!" pos0 len;*)
  if pos0 = len then "" else
    let rec find_last_non_space line pos pos0 =
      if pos = pos0 then String.make 1 line.[pos]
      else
        match line.[pos] with
            ' ' | '\t' ->
              find_last_non_space line (pos-1) pos0
          | _ ->
(*            Printf.fprintf stderr "pos = %d\n%!" pos; *)
            let s = String.sub line pos0 (pos - pos0 + 1) in
(*            Printf.fprintf stderr "sub [%s] %d %d = [%s]\n%!"
              line pos0 (pos - pos0 + 1) s; *)
            s
    in
    find_last_non_space line (len-1) pos0
*)

let _ =
  assert (unspace "a b c" = "a b c");
  assert (unspace "a b c " = "a b c");
  assert (unspace " a b c " = "a b c");
  assert (unspace " a b c" = "a b c");
  assert (unspace "  a  b  c  " = "a b c");
  ()

module CharMap = Map.Make(Char)

let replace_chars s list =

  let set = ref CharMap.empty in
  List.iter (fun (c, s) -> set := CharMap.add c s !set) list;
  let set = !set in

  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c = s.[i] in
    try
      Buffer.add_string b (CharMap.find c set)
    with Not_found -> Buffer.add_char b c
  done;
  Buffer.contents b

let _ =
  assert (replace_chars "toto" [ 'o', "--" ] = "t--t--");
  assert (replace_chars "toto" [ 't', "--" ] = "--o--o");
  ()
