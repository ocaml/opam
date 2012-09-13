{
(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
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
let char  = ['-']
let ident = alpha (alpha | digit | char)*
let symbol = ['=' '<' '>' '!' '+' '|']+
let number = '-'? ('.'['0'-'9']+ | ['0'-'9']+('.'['0'-'9']*)? )

rule token = parse
| space  { token lexbuf }
| "\n"   { newline lexbuf; token lexbuf }
| ":"    { COLON }
| "{"    { LBRACE }
| "}"    { RBRACE }
| "["    { LBRACKET }
| "]"    { RBRACKET }
| "("    { LPAR }
| ")"    { RPAR }
| '"'    { let s = string "" lexbuf in
           STRING s }
| "(*"   { comment 1 lexbuf; token lexbuf }
| "true" { BOOL true }
| "false"{ BOOL false }
| "\n"   { Lexing.new_line lexbuf; token lexbuf }
| digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ident  { IDENT (Lexing.lexeme lexbuf) }
| symbol { SYMBOL (Lexing.lexeme lexbuf) }
| eof    { EOF }
| _      { let token = Lexing.lexeme lexbuf in
           Globals.error_and_exit "lexer error: '%s' is not a valid token" token }

(* XXX: not optimal at all *)
and string s = parse
| '"'    { s }
| "\n"   { newline lexbuf;
           string (s ^ Lexing.lexeme lexbuf) lexbuf }
| "\\\"" { string (s ^ "\"") lexbuf }
| "\\\\" { string (s ^ "\\") lexbuf }
| "\\"   [^ '"' '\\']+
          { string (s ^ Lexing.lexeme lexbuf) lexbuf }
| eof  { s }
| _    { string (s ^ Lexing.lexeme lexbuf) lexbuf }

and comment n = parse
| "*)" { if n > 1 then comment (n-1) lexbuf }
| "(*" { comment (n+1)lexbuf }
| eof  { }
| "\n" { newline lexbuf; comment n lexbuf }
| _    { comment n lexbuf }
