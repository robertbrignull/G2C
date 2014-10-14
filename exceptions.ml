open Lexing
open Parsing

exception LexErr of string
exception ParseErr of string

let error msg pos =
  Printf.sprintf "%s: line %d: char %d" msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let lex_error lexbuf = 
  LexErr (error ("Unrecognised characters: " ^ (lexeme lexbuf)) (lexeme_start_p lexbuf))

let parse_error msg nterm =
  ParseErr (error msg (rhs_start_pos nterm))
