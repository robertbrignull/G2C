{
open Lexing
open Parser        (* The type 'token' is defined in parser.mli *)

let make_hash n ps =
  let t = Hashtbl.create n in
  List.iter (fun (k, v) -> Hashtbl.add t k v) ps;
  t

(* A little table to recognize keywords *)
let kwtable = 
  make_hash 64
    [ ("->", ARROW);
      ("+", OP "plus"); ("-", OP "minus");
      ("*", OP "times"); ("/", OP "divide");
      ("=", OP "eq"); ("!=", OP "neq");
      ("<", OP "lt"); (">", OP "gt");
      ("<=", OP "leq"); (">=", OP "geq"); 
      ("and", OP "and"); ("or", OP "or"); ("not", OP "not");
      ("true", BOOL true); ("false", BOOL false);
      ("num", NUM_TYPE); ("bool", BOOL_TYPE);
      ("lambda", LAMBDA); ("let", LET); ("if", IF);

      ("assume", ASSUME); ("observe", OBSERVE); ("predict", PREDICT);

      ("beta", PRIM "beta"); ("binomial", PRIM "binomial");
      ("exponential", PRIM "exponential"); ("flip", PRIM "flip");
      ("gamma", PRIM "gamma"); ("invgamma", PRIM "invgamma");
      ("normal", PRIM "normal"); ("poisson", PRIM "poisson");
      ("uniform-continuous", PRIM "uniform-continuous");
      ("uniform-discrete", PRIM "uniform-discrete") ]

let lookup s = try Hashtbl.find kwtable s with Not_found -> ID s
}

rule token = parse
  [' ''\t']                   { token lexbuf }                    (* skip blanks *)
  | '\n'                      { new_line lexbuf; token lexbuf; }  (* count line numbers *)

  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | "["                       { LSQUARE }
  | "]"                       { RSQUARE }
  | ","                       { COMMA }
  | ":"                       { COLON }

  | '-'?['0'-'9']+('.'['0'-'9']+)?|'-'?'.'['0'-'9']+ as s
                              { NUM (float_of_string s) }

  | ['a'-'z''A'-'Z''*''/''<''=''>''!''?'':''$''%''_''&''~''^']?['a'-'z''A'-'Z''0'-'9''+''-''.''*''/''<''=''>''!''?'':''$''%''_''&''~''^']* as s    { lookup (String.lowercase s) }

  | _                         { raise (Exceptions.lex_error lexbuf) }
  | eof                       { EOF }
