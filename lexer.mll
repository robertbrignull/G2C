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
    [ ("true", BOOL true); ("false", BOOL false); ("lambda", LAMBDA); ("let", LET); ("if", IF); ("num", NUM_TYPE); ("bool", BOOL_TYPE) ]

let lookup s = try Hashtbl.find kwtable s with Not_found -> ID s
}

rule token = parse
    [' ''\t']                 { token lexbuf }                    (* skip blanks *)
  | '\n'                      { new_line lexbuf; token lexbuf; }  (* count line numbers *)

  | ['+''-']?['0'-'9']+('.'['0'-'9']+)? as s
                              { NUM (float_of_string s) }

  | ['a'-'z''A'-'Z']+ as s    { lookup (String.lowercase s) }

  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | ","                       { COMMA }
  | ":"                       { COLON }
  | "->"                      { ARROW }
  | "+"                       { OP "plus" }
  | "-"                       { OP "minus" }
  | "*"                       { OP "times" }
  | "/"                       { OP "divide" }
  | "="                       { OP "eq" }
  | "!="                      { OP "neq" }
  | "<"                       { OP "lt" }
  | ">"                       { OP "gt" }
  | "<="                      { OP "leq" }
  | ">="                      { OP "geq" }

  | _                         { raise (Exceptions.lex_error lexbuf) }
  | eof                       { EOF }
