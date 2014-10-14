%{
open AST
open Parsing
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
  | BOOL                               { Bool $1 }
  | NUM                                { Num $1 }
  | ID                                 { Id $1 }
  | LPAREN LAMBDA LPAREN lambda_args RPAREN expr RPAREN
                                       { Lambda ($4, $6) }
  | LPAREN LET LPAREN ID expr RPAREN expr RPAREN
                                       { Let ($4, $5, $7) }
  | LPAREN IF expr expr expr RPAREN    { If ($3, $4, $5) }
  | LPAREN OP exprs RPAREN             { Op ($2, $3) }
  | LPAREN expr exprs RPAREN           { App ($2, $3) }
  | error                              { raise (Exceptions.parse_error "Invalid expression" 1) }
;

lambda_args:
  | empty                              { [] }
  | lambda_args_2                      { $1 }
  | error                              { raise (Exceptions.parse_error "Invalid lambda expression" 1) }

lambda_args_2:
  | ID COLON type_c                      { [($1, $3)] }
  | ID COLON type_c COMMA lambda_args_2  { ($1, $3) :: $5 }
;

type_c:
  | NUM_TYPE                           { NumType }
  | BOOL_TYPE                          { BoolType }
  | LPAREN type_c ARROW type_c RPAREN  { CompoundType ($2, $4) }
  | error                              { raise (Exceptions.parse_error "Invalid type" 1) }
;

exprs:
  | empty                              { [] }
  | expr exprs                         { $1 :: $2 }
;

empty: { } ;
