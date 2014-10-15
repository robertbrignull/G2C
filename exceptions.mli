exception LexErr of string
exception ParseErr of string
exception TypingErr of string

val error : string -> Lexing.position -> string

val lex_error : Lexing.lexbuf -> exn
val parse_error : string -> int -> exn
val typing_error : string -> Lexing.position -> exn
