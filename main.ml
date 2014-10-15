let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let lexed_prog = Parser.main Lexer.token lexbuf in
    let typed_porg = Infer_types.infer_types lexed_prog in

    (* Print the AST *)
    print_string (Printing.pretty_print_expr typed_porg);
    print_newline ()

  with
    | Exceptions.LexErr msg ->
        print_string ("Syntax error: " ^ msg);
        print_newline ()
    | Exceptions.ParseErr msg ->
        print_string ("Syntax error: " ^ msg);
        print_newline ()
    | Exceptions.TypingErr msg ->
        print_string ("Typing error: " ^ msg);
        print_newline ()

let _ = main ()
