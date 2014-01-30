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

(* open SafeCaml *)
open Genlex
open BuildMisc
open BuildOCPVariable


(* open BuildBase *)
open Ocamllexer
open BuildOCPParser
open BuildOCPTree

  let lexer = Ocamllexer.make_lexer
    [ "begin"; "end"; "true"; "false";
      "library"; "syntax"; "program"; "objects"; "config"; "include"; "type";
(*      "files"; "requires";  "tests"; *)
      "use"; "pack"; "test"; "rules";
      "if"; "then"; "else";
      "["; "]"; ";"; "("; ")"; "{"; "}"; "="; "+="; "!";
      "not"; "&&"; "||"; "%"
    ]

exception ParseError

let read_ocamlconf filename content =
  let lexbuf = Lexing.from_string content in
  let token_of_token token_opt =
    match token_opt with
      None -> EOF
    | Some token ->
      match token with
      | String s -> STRING s
      | Float f -> FLOAT f
      | Int i -> INT i
      | Char c -> CHAR c
      | Kwd ";" -> SEMI
      | Kwd "%" -> PERCENT
      | Kwd "[" -> LBRACKET
      | Kwd "]" -> RBRACKET
      | Kwd "(" -> LPAREN
      | Kwd ")" -> RPAREN
      | Kwd "{" -> LBRACE
      | Kwd "}" -> RBRACE
      | Kwd "!" -> BANG
      | Kwd "begin" -> BEGIN
      | Kwd "end" -> END
      | Kwd "objects" -> OBJECTS
      | Kwd "library" -> LIBRARY
      | Kwd "test" -> TEST
      (*	  | Kwd "tests" -> TESTS *)
      | Kwd "syntax" -> SYNTAX
      | Kwd "config" -> CONFIG
      | Kwd "use" -> USE
      | Kwd "program" -> PROGRAM
      | Kwd "type" -> TYPE
      | Kwd "include" -> INCLUDE
      | Kwd "rules" -> RULES
      | Kwd "=" -> EQUAL
      | Kwd "+=" -> PLUSEQUAL
      | Kwd "-=" -> MINUSEQUAL
      | Kwd "true" -> TRUE
      | Kwd "false" -> FALSE
      | Kwd "pack" -> PACK
      | Kwd "if" -> IF
      | Kwd "then" -> THEN
      | Kwd "else" -> ELSE
      | Kwd "not" -> NOT
      | Kwd "&&" -> COND_AND
      | Kwd "||" -> COND_OR
      | Kwd "syntaxes" -> SYNTAXES
      (*          | Kwd "camlp4" -> CAMLP4 *)
      (*          | Kwd "camlp5" -> CAMLP5 *)
      | Ident s -> IDENT s
      | Kwd s ->

        Printf.eprintf "Internal error: %S should not be a keyword\n%!" s;
        IDENT s
  in

(*
  let trap_include lexbuf =
    try
    match token_of_token (lexer lexbuf) with
    | INCLUDE ->
        let next_token = token_of_token (lexer lexbuf) in
        begin
          match next_token with
          | STRING inc_filename ->
            let inc_filename = if Filename.is_implicit inc_filename then
                Filename.concat dir inc_filename
              else
                inc_filename
            in
            if not (Sys.file_exists inc_filename) then begin
              Logger.warning "Warning: file %S does not exist.\n\t(included from %S)\n" inc_filename filename;
              INCLUDED []
            end else
              INCLUDED (read_ocamlconf inc_filename)
          | _ -> raise Parsing.Parse_error
        end
    | token -> token
    with Ocamllexer.Error (error, n, m) ->
      Printf.eprintf "File %S, line 1, characters %d-%d:\n"
        filename n m;
      Ocamllexer.report_error Format.err_formatter error;
      Format.fprintf Format.err_formatter "@.";
      raise Exit
  in
*)

  let lexer lexbuf =
    try
      token_of_token (lexer lexbuf)
    with Ocamllexer.Error (error, n, m) ->
      Printf.eprintf "File %S, line 1, characters %d-%d:\n"
        filename n m;
      Ocamllexer.report_error Format.err_formatter error;
      Format.fprintf Format.err_formatter "@.";
      raise ParseError
  in

  let ast =
    try
      BuildOCPParser.main lexer lexbuf
    with Parsing.Parse_error ->
      BuildMisc.print_loc filename (Lexing.lexeme_start lexbuf);
      Printf.eprintf "Parse error\n%!";
      raise ParseError
  in
  ast
