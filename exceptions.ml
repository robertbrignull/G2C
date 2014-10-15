open Lexing
open Parsing

exception LexErr of string
exception ParseErr of string
exception TypingErr of string

let error msg pos =
  Printf.sprintf "%s: line %d: char %d" msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let lex_error lexbuf = 
  LexErr (error ("Unrecognised characters: " ^ (lexeme lexbuf)) (lexeme_start_p lexbuf))

let parse_error msg nterm =
  ParseErr (error msg (rhs_start_pos nterm))

let typing_error msg pos =
  TypingErr (error msg pos)
