%{
open AST_0_U
open Parsing

let p nterm = rhs_start_pos nterm
%}

%token <float> NUM
%token <bool> BOOL
%token <string> ID
%token <string> OP
%token <string> PRIM
%token <string> TYPED_PRIM
%token LPAREN RPAREN LSQUARE RSQUARE
%token ARROW COLON COMMA DOT
%token LAMBDA LET IF COND ELSE
%token EMPTY_LIST LIST
%token ASSUME OBSERVE PREDICT
%token NUM_TYPE BOOL_TYPE LIST_TYPE
%token EOF ERR

%type <AST_0_U.prog> main
%start main

%%

main:
  | stmts EOF                          { $1 }
  | error                              { raise (Exceptions.parse_error "Invalid program" 1) }
;

expr:
  | BOOL                               { (Bool $1, p 1) }
  | NUM                                { (Num $1, p 1) }
  | ID                                 { (Id $1, p 1) }
  | LPAREN LAMBDA LPAREN lambda_args RPAREN ARROW type_c expr RPAREN
                                       { (Lambda ($4, $7, $8), p 1) }
  | LPAREN LET ID expr expr RPAREN     { (Let ($3, $4, $5), p 1) }
  | LPAREN IF expr expr expr RPAREN    { (If ($3, $4, $5), p 1) }
  | LPAREN COND cond_expr RPAREN       { $3 }
  | LPAREN PRIM exprs RPAREN           { (Prim ($2, $3), p 1) }
  | LPAREN TYPED_PRIM type_c exprs RPAREN
                                       { (TypedPrim ($2, $3, $4), p 1) }
  | LPAREN expr exprs RPAREN           { (App ($2, $3), p 1) }
  | EMPTY_LIST                         { (Prim ("empty", []), p 1) }
  | LPAREN LIST exprs RPAREN           { (List.fold_right (fun f r -> (Prim ("cons", [f; r]), p 1)) $3 (Prim ("empty", []), p 1)) }
  | error                              { raise (Exceptions.parse_error "Invalid expression" 1) }
;

cond_expr:
  | LPAREN ELSE expr RPAREN            { $3 }
  | LPAREN expr expr RPAREN cond_expr  { (If ($2, $3, $5), p 1) }
  | error                              { raise (Exceptions.parse_error "Invalid cond expression" 1) }
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
  | LIST_TYPE                          { ListType }
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

stmt:
  | LSQUARE ASSUME ID expr RSQUARE     { (Assume ($3, $4), p 1) }
  | LSQUARE OBSERVE expr expr RSQUARE  { (Observe ($3, $4), p 1) }
  | LSQUARE PREDICT expr RSQUARE       { (Predict $3, p 1) }
;

stmts:
  | empty                              { [] }
  | stmt stmts                         { $1 :: $2 }
;

empty: { } ;
