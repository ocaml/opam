%{
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

open File_format

%}

%token <string> STRING IDENT SYMBOL
%token <bool> BOOL
%token EOF
%token LBRACKET RBRACKET
%token LPAR RPAR
%token LBRACE RBRACE
%token COLON
%token <int> INT
%left LPAR RPAR
%left LBRACE RBRACE
%left LBRACKET RBRACKET
%left STRING IDENT BOOL SYMBOL
%left COLON

%start main value
%type <string -> File_format.file> main
%type <File_format.value> value

%%

main:
| items EOF { fun filename -> { contents = $1; filename } }
;

items:
| item items { $1 :: $2 }
|            { [] }
;

item:
| IDENT COLON value                { Variable ($1, $3) }
| IDENT STRING LBRACE items RBRACE { Section {kind=$1; name=$2; items= $4} }
;

value:
| BOOL                       { Bool $1 }
| INT                        { Int $1 }
| STRING                     { String $1 }
| SYMBOL                     { Symbol $1 }
| IDENT                      { Ident $1 }
| LPAR values RPAR           { Group $2 }
| LBRACKET values RBRACKET   { List $2 }
| value LBRACE values RBRACE { Option ($1, $3) }
;

values:
|              { [] }
| value values { $1 :: $2 }
;

%%

exception Error of int * int * string

let lexer_error lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let tok = Lexing.lexeme lexbuf in
  raise (Error (line,cnum,tok))

open Lexing

let main t l f =
  try main t l f
  with _ ->
    lexer_error l
