exception LexErr of string
exception ParseErr of string

val error : string -> Lexing.position -> string

val lex_error : Lexing.lexbuf -> exn
val parse_error : string -> int -> exn
