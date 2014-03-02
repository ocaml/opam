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


type t = {
  mutable sections : section StringMap.t;
}

and section = {
  mutable section_name : string;
  mutable section_options : string StringMap.t;
}



exception DuplicateSectionError of t * string
exception BadSectionLine of string
exception BadOptionLine of string
exception SectionNotFound of string


let default_section_name = "DEFAULT"

let create () = { sections = StringMap.empty }

let defaults t =
  try
    let section = StringMap.find default_section_name t.sections in
    section.section_options
  with Not_found ->
    StringMap.empty

let has_section t s = StringMap.mem s t.sections
let add_new_section t s =
(*  Printf.fprintf stderr "add_new_section [%s]\n%!" s; *)
  t.sections <- StringMap.add s
    { section_name = s;
      section_options = StringMap.empty } t.sections

let add_section t s =
(*  Printf.fprintf stderr "add_section [%s]\n%!" s; *)
  if not (has_section t s) then add_new_section t s
let add_new_section t s =
  if has_section t s then raise (DuplicateSectionError (t,s));
  add_new_section t s

let has_option t s o =
  try
    let s = StringMap.find s t.sections in
    StringMap.mem o s.section_options
  with Not_found -> false

let options t s =
  try
    let s = StringMap.find s t.sections in
    s.section_options
  with Not_found -> StringMap.empty

let sections t =
  let list = ref [] in
  StringMap.iter (fun s _ ->
    if s <> default_section_name then
      list := s :: !list
  ) t.sections;
  !list

let find_section t section =
  try
    StringMap.find section t.sections
  with Not_found ->
    raise (SectionNotFound section)

let add_option t section o v =
(*  Printf.fprintf stderr "add_option [%s.%s] = [%s]\n%!" section o v; *)
  let s =
    try
      StringMap.find section t.sections
    with Not_found ->
      let s = { section_name = section; section_options = StringMap.empty }
      in
      t.sections <- StringMap.add section s t.sections;
      s
  in
  s.section_options <- StringMap.add o v s.section_options

let remove_section t section =
  t.sections <- StringMap.remove section t.sections

let remove_option t section o =
  let s = find_section t section in
  s.section_options <- StringMap.remove o s.section_options

let set t s o v =
  let s = find_section t s in
  s.section_options <- StringMap.add o v s.section_options

let set_bool t s o v = set t s o (string_of_bool v)
let set_int t s o v =  set t s o (string_of_int v)
let set_float t s o v =  set t s o (string_of_float v)

let items t s =
  StringMap.to_list (find_section t s).section_options

let get t s o =
  StringMap.find o (find_section t s).section_options
let get_int t s o = int_of_string (get t s o)
let get_float t s o = float_of_string (get t s o)
let bool_of_generic_string s =
  match String.lowercase s with
      "true" | "yes" | "y" | "o" -> true
    | "false" | "no" | "n" | "non" -> false
    | _ -> failwith "int_of_generic_bool"
let get_bool t s o = bool_of_generic_string (get t s o)



























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

let unspace line =
  let line = unspace line in
(*  Printf.fprintf stderr " => [%s]\n%!" line; *)
  line

let add_option_lines t section lines =
  let line = String.concat " " (List.rev lines) in
  let rec cut line pos len =
    if pos >= len-1 then raise (BadOptionLine line);
    match line.[pos] with
        ':' | '=' ->
          if pos = 0 then raise (BadOptionLine line);
          unspace (String.sub line 0 pos), unspace (String.sub line (pos+1) (len-pos-1))
      | _ -> cut line (pos+1) len
  in
  let (o, v) = cut line 0 (String.length line) in
  add_section t section;
  add_option t section o v

let read filename =
(*  Printf.fprintf stderr "ConfigParser.read\n%!"; *)
  let lines = File.X.read_lines filename in
  let rec iter t section lines =
    match lines with
        [] -> ()
      | line :: lines ->
(*        Printf.fprintf stderr "line [%s]\n%!" line; *)
        let line = unspace line in
        if String.length line = 0 then
          iter t section lines
        else
          match line.[0] with
              ';' | '#' ->
                iter t section lines
            | '[' ->
              let section =
                try
                  let pos = String.index line ']' in
                  String.sub line 1 (pos-1)
                with Not_found ->
                  raise (BadSectionLine line)
              in
(*              Printf.fprintf stderr "should add section [%s]\n%!" section; *)
              add_section t section;
              iter t section lines
            | _ ->
(*              Printf.fprintf stderr "iters [%s]\n%!" line; *)
              iter2 t section [line] lines

  and iter2 t section prev_lines lines =
    match lines with
        [] ->
          add_option_lines t section prev_lines
      | line :: next_lines ->
        let len = String.length line in
        if len = 0 then begin
          add_option_lines t section prev_lines;
          iter t section next_lines
        end else
          match line.[0] with
            | ' ' | '\t' ->
                iter2 t section (unspace line :: prev_lines) next_lines
            | ';' | '#' ->
              iter2 t section prev_lines next_lines
            | _ ->
              add_option_lines t section prev_lines;
              iter t section lines

  in
  let t = {
    sections = StringMap.empty;
  } in
  iter t default_section_name lines;
  t

let write_section b t section =
  let options = options t section in
  if section <> default_section_name then
    Printf.bprintf b "[%s]\n" section;
  StringMap.iter (fun o v ->
    Printf.bprintf b "%s = %s\n" o v
  ) options

let write file t =
  let b = Buffer.create 1000 in
  write_section b t default_section_name;
  List.iter (write_section b t) (sections t);
  File.X.write_of_string file (Buffer.contents b)

