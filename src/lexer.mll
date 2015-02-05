{
open Lexing
open Parser        (* The type 'token' is defined in parser.mli *)

let make_hash n ps =
  let t = Hashtbl.create n in
  List.iter (fun (k, v) -> Hashtbl.add t k v) ps;
  t

(* A little table to recognize keywords *)
let kwtable = 
  make_hash 256
    [ ("->", ARROW);
      ("true", BOOL true); ("false", BOOL false);
      ("Num", NUM_TYPE); ("Bool", BOOL_TYPE); ("List", LIST_TYPE);
      ("lambda", LAMBDA); ("let", LET); ("if", IF);
      ("cond", COND); ("else", ELSE);
      ("mem", MEM);

      ("assume", ASSUME); ("observe", OBSERVE); ("predict", PREDICT);

      ("+", PRIM "plus"); ("-", PRIM "minus");
      ("*", PRIM "times"); ("/", PRIM "divide");
      ("=", PRIM "eq"); ("!=", PRIM "neq");
      ("<", PRIM "lt"); (">", PRIM "gt");
      ("<=", PRIM "leq"); (">=", PRIM "geq"); 
      ("and", PRIM "and"); ("or", PRIM "or"); ("not", PRIM "not");

      ("cons", PRIM "cons"); ("list", LIST);
      ("first", TYPED_PRIM "first"); ("second", TYPED_PRIM "second"); 
      ("nth", TYPED_PRIM "nth");
      ("rest", PRIM "rest"); 
      ("empty?", PRIM "empty?"); ("count", PRIM "count");
      
      ("log", PRIM "log"); ("log10", PRIM "log10"); ("exp", PRIM "exp");
      ("pow", PRIM "pow"); ("sqrt", PRIM "sqrt"); ("cbrt", PRIM "cbrt");
      ("floor", PRIM "floor"); ("ceil", PRIM "ceil");
      ("round", PRIM "round"); ("rint", PRIM "rint");
      ("abs", PRIM "abs"); ("signum", PRIM "signum");
      ("sin", PRIM "sin"); ("cos", PRIM "cos"); ("tan", PRIM "tan");
      ("asin", PRIM "asin"); ("acos", PRIM "acos"); ("atan", PRIM "atan");
      ("sinh", PRIM "sinh"); ("cosh", PRIM "cosh"); ("tanh", PRIM "tanh");
      ("inc", PRIM "inc"); ("dec", PRIM "dec"); ("mod", PRIM "mod");

      ("beta", PRIM "beta"); ("binomial", PRIM "binomial");
      ("exponential", PRIM "exponential"); ("flip", PRIM "flip");
      ("gamma", PRIM "gamma"); ("invgamma", PRIM "invgamma");
      ("normal", PRIM "normal"); ("poisson", PRIM "poisson");
      ("geometric", PRIM "geometric");
      ("uniform-continuous", PRIM "uniform-continuous");
      ("uniform-discrete", PRIM "uniform-discrete");
      ("discrete", PRIM "discrete");
      ("categorical", TYPED_PRIM "categorical") ]

let lookup s = try Hashtbl.find kwtable s with Not_found -> ID s
}

rule token = parse
    [' ''\t']                 { token lexbuf }                    (* skip blanks *)
  | '\n'                      { new_line lexbuf; token lexbuf; }  (* count line numbers *)
  | ';'[^'\n']*               { token lexbuf }

  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | "["                       { LSQUARE }
  | "]"                       { RSQUARE }
  | ","                       { COMMA }
  | ":"                       { COLON }

  | '-'?['0'-'9']+('.'['0'-'9']+)?|'-'?'.'['0'-'9']+ as s
                              { NUM (float_of_string s) }

  | ['a'-'z''A'-'Z''*''/''<''=''>''!''?'':''$''%''_''&''~''^']?['a'-'z''A'-'Z''0'-'9''+''-''.''*''/''<''=''>''!''?'':''$''%''_''&''~''^']* as s    { lookup s }
  | "[]"                      { EMPTY_LIST }

  | _                         { raise (Exceptions.lex_error lexbuf) }
  | eof                       { EOF }
