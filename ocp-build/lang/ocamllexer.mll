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

{

open Lexing

(* open Misc *)

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
;;

exception Error of error * int * int

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
;;

module Make(S : sig

end) = struct

(* The table of keywords *)
let keyword_table = Hashtbl.create 149

let add_keyword s = Hashtbl.add keyword_table s (Kwd s)
let find_keyword lexbuf =
  let s = Lexing.lexeme lexbuf in
  try
    Hashtbl.find keyword_table s
  with Not_found -> Ident s

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr(c land 0xFF)

(* To store the position of the beginning of a string and comment *)
let string_start_pos = ref 0;;
let comment_start_pos = ref [];;

(* Error report *)

}

let blank = [' ' '\010' '\013' '\009' '\012']
let firstidentchar = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' 'A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolchar2 =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' '<' '=' '>' '?' '@' '^' '|' '~']
(*  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *)
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    blank +
      { token lexbuf }
  | firstidentchar identchar*  { Some (find_keyword lexbuf) }
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { Some (Int (int_of_string(Lexing.lexeme lexbuf))) }
  | float_literal
      { Some (Float (float_of_string (Lexing.lexeme lexbuf))) }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        Some (String (get_stored_string())) }
  | "'" [^ '\\' '\''] "'"
      { Some (Char(Lexing.lexeme_char lexbuf 1)) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { Some (Char(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { Some (Char(char_for_decimal_code lexbuf 2)) }
  | "(*"
      { comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf;
        token lexbuf }
  | "#"
  | "&"
  | "&&"
  | "`"
  | "'"
  | "("
  | ")"
  | "*"
  | ","
  | "?"
  | "??"
  | "->"
  | "."
  | ".."
  | ":"
  | "::"
  | ":="
  | ":>"
  | ";"
  | ";;"
  | "<"
  | "<-"
  | "="
  | "["
  | "[|"
  | "[<"
  | "]"
  | "{"
  | "{<"
  | "|"
  | "||"
  | "|]"
  | ">"
  | ">]"
  | "}"
  | ">}"

  | "!="
  | "-"
  | "-."

  | ['!' '~'] symbolchar *

  | '?' symbolchar2 *

  | ['=' '<' '>' '|' '&' '$'] symbolchar *

  | ['@' '^'] symbolchar *

  | ['+' '-'] symbolchar *

  | "**" symbolchar *

  | ['*' '/' '%'] symbolchar *
            { Some (find_keyword  lexbuf) }
  | eof { None }
  | _
      { raise (Error(Illegal_character ((Lexing.lexeme lexbuf).[0]),
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_start_pos := Lexing.lexeme_start lexbuf :: !comment_start_pos;
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_pos with
        | [] -> assert false
        | [x] -> ()
        | _ :: l -> comment_start_pos := l;
                    comment lexbuf;
       }
  | "\""
      { reset_string_buffer();
        string_start_pos := Lexing.lexeme_start lexbuf;
        begin try string lexbuf
        with Error (Unterminated_string, _, _) ->
          let st = List.hd !comment_start_pos in
          raise (Error (Unterminated_string_in_comment, st, st + 2))
        end;
        string_buff := initial_string_buffer;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { let st = List.hd !comment_start_pos in
        raise (Error (Unterminated_comment, st, st + 2));
      }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Error (Unterminated_string,
                      !string_start_pos, !string_start_pos+1)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

{
 end

let make_lexer list =
  let module M = Make(struct end) in
  List.iter (fun kwd -> M.add_keyword kwd) list;
  M.token

}
