(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

{

type token =
  | WORD of string
  | NEWLINE
  | EOF

let word = Buffer.create 57

}

let normalchar = [^' ' '\t' '\n' '\\']

rule main = parse
| '\n'         { Lexing.new_line lexbuf; NEWLINE }
| [' ' '\t']+  { main lexbuf }
| (normalchar* as w) '\\'
               { Buffer.reset word ; Buffer.add_string word w; escaped lexbuf }
| (normalchar* as w)
               { WORD w }
| eof          { EOF }

and escaped = parse
| (_ normalchar*) as w '\\'
               { Buffer.add_string word w; escaped lexbuf }
| (_ normalchar*) as w
               { Buffer.add_string word w; WORD (Buffer.contents word) }

{

let main lexbuf =
  let rec aux lines words =
    match main lexbuf with
    | WORD "" -> aux lines words
    | WORD s -> aux lines (s::words)
    | NEWLINE ->
      let lines = if words = [] then lines else List.rev words::lines in
      aux lines []
    | EOF ->
      let lines = if words = [] then lines else List.rev words::lines in
      List.rev lines
  in
  aux [] []

}
