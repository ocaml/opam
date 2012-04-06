{
(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  TypeRex is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open Parser

let newline lexbuf = Lexing.new_line lexbuf
}

let space = [' ' '\t' '\r' '\n']
let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = alpha (alpha | digit)*
let number = '-'? ('.'['0'-'9']+ | ['0'-'9']+('.'['0'-'9']*)? )

rule token = parse
| space  { token lexbuf }
| "\n"   { newline lexbuf; token lexbuf }
| "@"    { AT }
| "="    { EQUAL }
| "{"    { LBRACE }
| "}"    { RBRACE }
| "["    { LBRACKET }
| "]"    { RBRACKET }
| ";"    { SEMI }
| '"'    { let s = string1 "" lexbuf in
           STRING s }
| '#'    { let s = string2 "" lexbuf in
           STRING s }
| "(*"   { comment 1 lexbuf; token lexbuf }
| number { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ident  { IDENT (Lexing.lexeme lexbuf) }
| eof     { EOF }
| _       { let token = Lexing.lexeme lexbuf in
            Globals.error_and_exit "lexer error: '%s' is not a valid tokenm" token }

(* XXX: not optimal at all *)
and string1 s = parse
| '"'  { s }
| "\n" { newline lexbuf;
         string1 (s ^ Lexing.lexeme lexbuf) lexbuf }
| "\\\"" { string1 (s ^ "\"") lexbuf }
| "\\\\" { string1 (s ^ "\\") lexbuf }
| "\\" [^ '"' '\\']+
       { string1 (s ^ Lexing.lexeme lexbuf) lexbuf }
| eof  { s }
| _    { string1 (s ^ Lexing.lexeme lexbuf) lexbuf }

(* XXX: not optimal at all *)
and string2 s = parse
| '#'  { s }
| "\n" { newline lexbuf;
         string2 (s ^ Lexing.lexeme lexbuf) lexbuf }
| "\\#" { string2 (s ^ "#") lexbuf }
| "\\\\" { string2 (s ^ "\\") lexbuf }
| "\\" [^ '#' '\\']+
       { string2 (s ^ Lexing.lexeme lexbuf) lexbuf }
| eof  { s }
| _    { string2 (s ^ Lexing.lexeme lexbuf) lexbuf }

and comment n = parse
| "*)" { if n > 1 then comment (n-1) lexbuf }
| "(*" { comment (n+1)lexbuf }
| eof  { }
| "\n" { newline lexbuf; comment n lexbuf }
| _    { comment n lexbuf }




















