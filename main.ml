let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let lexed_prog = Parser.main Lexer.token lexbuf in
    let typed_porg = Infer_types.infer_types lexed_prog in

    (* Print the AST *)
    print_endline (Printing.pretty_print_expr typed_porg);

  with
    | Exceptions.LexErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.ParseErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.TypingErr msg ->
        print_endline ("Typing error: " ^ msg)

let _ = main ()
