%{
open AST
open Parsing

let p nterm = rhs_start_pos nterm
%}

%token <float> NUM
%token <bool> BOOL
%token <string> ID
%token <string> OP
%token LPAREN RPAREN ARROW COLON COMMA DOT
%token LAMBDA LET IF
%token NUM_TYPE BOOL_TYPE
%token EOF ERR

%type <AST.expr> main
%start main

%%

main:
  | expr EOF                           { $1 }
  | error                              { raise (Exceptions.parse_error "Invalid program" 1) }
;

expr:
  | BOOL                               { (Bool $1, (p 1, Untyped)) }
  | NUM                                { (Num $1, (p 1, Untyped)) }
  | ID                                 { (Id $1, (p 1, Untyped)) }
  | LPAREN LAMBDA LPAREN lambda_args RPAREN expr RPAREN
                                       { (Lambda ($4, $6), (p 1, Untyped)) }
  | LPAREN LET ID expr expr RPAREN     { (Let ($3, $4, $5), (p 1, Untyped)) }
  | LPAREN IF expr expr expr RPAREN    { (If ($3, $4, $5), (p 1, Untyped)) }
  | LPAREN OP exprs RPAREN             { (Op ($2, $3), (p 1, Untyped)) }
  | LPAREN expr exprs RPAREN           { (App ($2, $3), (p 1, Untyped)) }
  | error                              { raise (Exceptions.parse_error "Invalid expression" 1) }
;

lambda_args:
  | empty                              { [] }
  | ID COLON type_c                    { [($1, $3)] }
  | ID COLON type_c COMMA lambda_args  { ($1, $3) :: $5 }
  | error                              { raise (Exceptions.parse_error "Invalid lambda expression arguments" 1) }
;

type_c:
  | NUM_TYPE                           { NumType }
  | BOOL_TYPE                          { BoolType }
  | LPAREN type_cs RPAREN ARROW type_c { FunctionType ($2, $5) }
  | error                              { raise (Exceptions.parse_error "Invalid type" 1) }
;

type_cs:
  | empty                              { [] }
  | type_c                             { [$1] }
  | type_c COMMA type_cs               { $1 :: $3 }
;

exprs:
  | empty                              { [] }
  | expr exprs                         { $1 :: $2 }
;

empty: { } ;
