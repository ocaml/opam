/******************************************************************************/
/*                                                                            */
/*                          TypeRex OCaml Tools                               */
/*                                                                            */
/*                               OCamlPro                                     */
/*                                                                            */
/*    Copyright 2011-2012 OCamlPro                                            */
/*    All rights reserved.  See accompanying files for the terms under        */
/*    which this file is distributed. In doubt, contact us at                 */
/*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         */
/*                                                                            */
/******************************************************************************/

%{

open BuildOCPTree
  (* TODO: location of the type in ocamlyacc is erroneous, for example here token "main"
   type is located in the .mli/.ml file instead of the .mly file. *)

let true_value = ExprList [ ExprApply (ExprString "", [ OptionVariableSet ("type", ExprString "%bool") ])]

%}

%token <string> STRING
%token <int> INT
%token EOF
%token <float> FLOAT
%token <char> CHAR
%token SEMI
%token BEGIN
%token END
%token <string> IDENT
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token PLUSEQUAL
%token MINUSEQUAL
%token TRUE
%token FALSE
%token INCLUDE
%token <BuildOCPTree.statement list> INCLUDED
%token OBJECTS
%token LIBRARY
%token SYNTAX
%token PROGRAM
%token CONFIG
%token RULES
%token BANG
%token EQUAL
%token LPAREN
%token RPAREN
%token TYPE
%token USE
%token PACK
%token IF
%token THEN
%token ELSE
%token NOT
%token COND_OR
%token COND_AND
%token SYNTAXES
%token CAMLP4
%token CAMLP5
%token TEST
%token PERCENT
/* %token TESTS */

%start main
%type <BuildOCPTree.statement list> main

%%

main:
toplevel_statements EOF { $1 }
;

toplevel_statements:
  { [] }
| toplevel_statement toplevel_statements { $1 :: $2 }
| SEMI toplevel_statements { $2 }
;

package_type:
  PROGRAM { ProgramPackage }
| LIBRARY { LibraryPackage }
| TEST    { TestPackage }
| OBJECTS { ObjectsPackage }
| SYNTAX  { SyntaxPackage }
| RULES   { RulesPackage }
;

toplevel_statement:
| BEGIN CONFIG string_expression list_of_set_options END { StmtDefineConfig ($3, $4) }
| BEGIN package_type string_expression statements END { StmtDefinePackage ($2, $3, $4) }
| BEGIN toplevel_statements END { StmtBlock $2 }
| IF INCLUDE string_expression THEN
     one_toplevel_statement maybe_else_one_toplevel_statement { StmtInclude($3,$5,$6) }
| IF condition THEN one_toplevel_statement maybe_else_one_toplevel_statement { StmtIfThenElse($2,$4,$5) }
| simple_statement { $1 }

/* for backward compatibility
| BEGIN STRING TYPE EQUAL package_type statements END { StmtDefinePackage ($5, $2, $6) }
*/
;

one_toplevel_statement:
| toplevel_statement { [$1] }
| LBRACE toplevel_statements RBRACE { $2 }
;

statements:
| statement statements { $1 :: $2 }
| { [] }
| SEMI statements { $2 }
;

statement:
| BEGIN statements END { StmtBlock $2 }
| IF condition THEN one_statement maybe_else_one_statement { StmtIfThenElse($2, $4, $5) }
| simple_statement { $1 }
;

one_statement:
| statement { [$1] }
| LBRACE statements RBRACE { $2 }
;

simple_statement:
| set_option { StmtOption $1 }
;

maybe_else_one_statement:
| { None }
| ELSE one_statement { Some $2 }
;

maybe_else_one_toplevel_statement:
| { None }
| ELSE one_toplevel_statement { Some $2 }
;

condition:
condition2 { $1 }
;

condition2:
| condition1 COND_OR condition2 { OrConditions ($1, $3) }
| condition1 { $1 }
;

condition1:
| condition0 COND_AND condition1 { AndConditions ($1, $3) }
| condition0 { $1 }
;

condition0:
| NOT condition0 { NotCondition $2 }
| LPAREN condition RPAREN { $2 }
| expression EQUAL expression { IsEqual($1, $3) }
| expression { IsNonFalse $1 }
;

list_of_expressions:
| { [] }
| SEMI list_of_expressions { $2 }
| expression list_of_expressions { $1 :: $2 }
;

expression:
  string_expression { $1 }
| INT      { ExprString (string_of_int  $1) }
| FALSE     { ExprList [] }
| TRUE     { true_value }
| LBRACE list_of_expressions RBRACE {
    ExprList [ ExprApply (ExprString "", [
                   OptionVariableSet ("type", ExprString "%list");
                   OptionVariableSet ("value", ExprList $2);
                 ])] }
| LBRACKET list_of_expressions RBRACKET { ExprList $2 }
| expression set_option_list { ExprApply ($1, $2) }

/* OBSOLETE SYNTAXES */
| packer maybe_set_option_list LBRACKET list_of_expressions RBRACKET {
  let packname = $1 in
  let pack_options = $2 in
  let packname = modname_of_fullname packname in
    ExprPrimitive ("pack",
      [
       OptionVariableSet ("to_module", ExprApply(ExprString packname, pack_options));
       OptionVariableSet ("files", ExprList $4)
      ])
}
| BEGIN set_option_list list_of_expressions END {
  ExprApply (ExprList $3, $2)
}
;

string_expression:
| PERCENT ident set_option_list { ExprPrimitive ($2, $3) }
| STRING { ExprString   $1 }
| IDENT { ExprVariable $1 }
;

set_option_list:
| LPAREN list_of_set_options RPAREN { $2 }
;

maybe_set_option_list:
|   { [] }
| set_option_list { $1 }
;

list_of_set_options:
| { [] }
| SEMI list_of_set_options { $2 }
| set_option list_of_set_options { $1 :: $2 }
;

set_option:
| USE string_expression { OptionConfigUse $2 }
| ident set_ident { $2 $1 }
| IF condition THEN one_set_option maybe_else_one_set_option { OptionIfThenElse($2, $4, $5) }
;

set_ident:
| EQUAL expression { fun id -> OptionVariableSet (id, $2) }
| PLUSEQUAL expression { fun id -> OptionVariableAppend (id, $2) }
|  { fun id -> OptionVariableSet (id, true_value) }
;

ident:
| STRING { $1 }
| IDENT  { $1 }
| SYNTAX { "syntax" }
| RULES  { "rules" }
| PACK   { "pack" }
;

maybe_else_one_set_option:
| { None }
| ELSE one_set_option { Some $2 }
;

one_set_option:
| set_option { $1  }
| LBRACE list_of_set_options RBRACE { OptionBlock $2 }
;

packer:
| PACK STRING { $2 }
| PACK IDENT  { let s = $2 in s.[0] <- Char.lowercase s.[0]; s ^ ".ml" }
;

%%
