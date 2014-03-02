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

open OcpLang

let cut_last_extension ~basename =
  try
    let pos = String.rindex basename '.' in
    String.before basename pos,
    String.lowercase (String.after basename pos)
  with Not_found -> (basename, "")

let string_of_channel =
  let size_s = 16000 in
  let s = String.create size_s in
  fun ~ic ->
    let b = Buffer.create 1000 in
    let rec iter () =
      let nread = input ic s 0 size_s in
      if nread > 0 then begin
          Buffer.add_substring b s 0 nread;
          iter ()
        end
    in
    iter ();
    Buffer.contents b

let string_of_file ~filename =
  let ic = open_in filename in
  try
    let s = string_of_channel ic in
    close_in ic;
    s
  with e ->
      close_in ic;
      raise e

let output_line ~oc ~str =
  output_string oc (str ^ "\n")

(* [line_break] tells whether or not the '\n' characters need to be kept. *)
let lines_of_file ?(line_break=false) ?(discard = fun _ -> false) filename =
  let chan = open_in filename in
  let x = ref [] in
  begin try
    while true do
      let line = input_line chan in
      if not (discard line) then begin
        let l = if line_break then line ^ "\n" else line in
        x := l :: !x
      end;
    done
    with End_of_file -> ()
  end;
  close_in chan;
  List.rev !x

let file_of_lines ~filename lines =
  File.file_of_lines filename lines

let file_of_string ~filename ~str =
  File.file_of_string filename str


(* We want to keep the exact same lines as in the file (for error
   location) but then we want to discard some lines before using the
   lexer. *)
(*
let genlex_of_file ~keywords ?discard ~filename =
  let lines = lines_of_file ~line_break:true filename in
  Genlex.of_lines keywords filename ?discard lines
*)
