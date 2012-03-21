%{
  open File_format
%}

%token <string> STRING IDENT
%token <int> INT
%token EOF
%token AT
%token SEMI
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EQUAL

%start main
%type <File_format.file> main

%%

main:
| version statements EOF { {version = $1; statements = $2} }
;

version:
| AT INT { $2 }
|        { Globals.api_version  }
;

statements:
| statement statements { $1 :: $2 }
|                      { [] }
;

statement:
| IDENT STRING                        { {kind=$1; name=$2; contents= []} }
| IDENT STRING LBRACE contents RBRACE { {kind=$1; name=$2; contents= $4} }
;

contents:
|                                    { [] }
| IDENT EQUAL content contents       { ($1, $3) :: $4 }
| IDENT EQUAL content SEMI contents  { ($1, $3) :: $5 }
;

content:
| STRING                        { String $1 }
| LBRACKET contentlist RBRACKET { List $2 }
;

contentlist:
|                          { [] }
| content                  { [$1] }
| content SEMI contentlist { $1 :: $3 }
;

%%

exception Error of int * int * string

let lexer_error lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let tok = Lexing.lexeme lexbuf in
  raise (Error (line,cnum,tok))

let main t l =
  try main t l
  with _ ->
    lexer_error l
