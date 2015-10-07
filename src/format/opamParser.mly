/**************************************************************************/
/*                                                                        */
/*    Copyright 2012-2015 OCamlPro                                        */
/*    Copyright 2012 INRIA                                                */
/*                                                                        */
/*  All rights reserved.This file is distributed under the terms of the   */
/*  GNU Lesser General Public License version 3.0 with linking            */
/*  exception.                                                            */
/*                                                                        */
/*  OPAM is distributed in the hope that it will be useful, but WITHOUT   */
/*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    */
/*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        */
/*  License for more details.                                             */
/*                                                                        */
/**************************************************************************/

%{

open OpamTypes
open OpamTypesBase

let get_pos n =
  let pos = Parsing.rhs_start_pos n in
  Lexing.(OpamFilename.of_string pos.pos_fname,
          pos.pos_lnum,
          pos.pos_cnum - pos.pos_bol)

%}

%token <string> STRING IDENT
%token <bool> BOOL
%token EOF
%token LBRACKET RBRACKET
%token LPAR RPAR
%token LBRACE RBRACE
%token COLON
%token <int> INT
%token <OpamTypes.relop> RELOP
%token <OpamTypes.logop> LOGOP
%token <OpamTypes.pfxop> PFXOP
%token <string> ENVOP

%left COLON
%left ATOM
%left LOGOP
%nonassoc ENVOP
%nonassoc PFXOP
%left LBRACE RBRACE
%nonassoc RELOP
%nonassoc URELOP

%start main value
%type <string -> OpamTypes.opamfile> main
%type <OpamTypes.value> value

%%

main:
| items EOF { fun file_name ->
        { file_contents = $1; file_name } }
;

items:
| item items { $1 :: $2 }
|            { [] }
;

item:
| IDENT COLON value                { Variable (get_pos 1, $1, $3) }
| IDENT STRING LBRACE items RBRACE {
  Section (get_pos 1,
           {section_kind=$1; section_name=$2; section_items= $4})
}
;

value:
| atom            %prec ATOM { $1 }
| LPAR values RPAR           { Group (get_pos 1,$2) }
| LBRACKET values RBRACKET   { List (get_pos 1,$2) }
| value LBRACE values RBRACE { Option (get_pos 2,$1, $3) }
| value LOGOP value          { Logop (get_pos 2,$2,$1,$3) }
| atom RELOP atom            { Relop (get_pos 2,$2,$1,$3) }
| atom ENVOP atom            { Env_binding (get_pos 1,$2,$1,$3) }
| PFXOP value                { Pfxop (get_pos 1,$1,$2) }
| RELOP atom                 { Prefix_relop (get_pos 1,$1,$2) }
;

values:
|                            { [] }
| value values               { $1 :: $2 }
;

atom:
| IDENT                      { Ident (get_pos 1,$1) }
| BOOL                       { Bool (get_pos 1,$1) }
| INT                        { Int (get_pos 1,$1) }
| STRING                     { String (get_pos 1,$1) }
;

%%

let error lexbuf exn msg =
  let curr = lexbuf.Lexing.lex_curr_p in
  let start = lexbuf.Lexing.lex_start_p in
  OpamConsole.error
      "File %S, line %d, character %d-%d: %s."
      curr.Lexing.pos_fname
      start.Lexing.pos_lnum
      (start.Lexing.pos_cnum - start.Lexing.pos_bol)
      (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
      msg;
  raise exn

let main t l f =
  try
    let r = main t l f in
    Parsing.clear_parser ();
    r
  with
  | Lexer_error msg     as e ->
    Parsing.clear_parser ();
    error l e msg
  | Parsing.Parse_error as e ->
    Parsing.clear_parser ();
    error l e "parse error"
