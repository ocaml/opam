{
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
| '"'    { let s = string "" lexbuf in
           STRING s }
| "(*"   { comment 1 lexbuf; token lexbuf }
| number { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ident  { IDENT (Lexing.lexeme lexbuf) }
| eof     { EOF }
| _       { let token = Lexing.lexeme lexbuf in
            Globals.error_and_exit "lexer error: '%s' is not a valid tokenm" token }

(* XXX: not optimal at all *)
and string s = parse
| '"'  { s }
| "\n" { newline lexbuf;
         string (s ^ Lexing.lexeme lexbuf) lexbuf }
| "\\" [^ '"' '\\']+
       { string (s ^ Lexing.lexeme lexbuf) lexbuf }
| eof  { s }
| _    { string (s ^ Lexing.lexeme lexbuf) lexbuf }

and comment n = parse
| "*)" { if n > 1 then comment (n-1) lexbuf }
| "(*" { comment (n+1)lexbuf }
| eof  { }
| "\n" { newline lexbuf; comment n lexbuf }
| _    { comment n lexbuf }




















