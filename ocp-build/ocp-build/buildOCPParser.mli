type token =
  | STRING of (string)
  | INT of (int)
  | EOF
  | FLOAT of (float)
  | CHAR of (char)
  | SEMI
  | BEGIN
  | END
  | IDENT of (string)
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | PLUSEQUAL
  | MINUSEQUAL
  | TRUE
  | FALSE
  | INCLUDE
  | INCLUDED of (BuildOCPTree.statement list)
  | OBJECTS
  | LIBRARY
  | SYNTAX
  | PROGRAM
  | CONFIG
  | RULES
  | BANG
  | EQUAL
  | LPAREN
  | RPAREN
  | TYPE
  | USE
  | PACK
  | IF
  | THEN
  | ELSE
  | NOT
  | COND_OR
  | COND_AND
  | SYNTAXES
  | CAMLP4
  | CAMLP5
  | TEST
  | PERCENT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> BuildOCPTree.statement list
