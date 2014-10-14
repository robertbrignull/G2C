let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.main Lexer.token lexbuf in

    (* Print the AST *)
    print_string (Printing.pretty_print_expr prog);
    print_newline ()

  with
    | Exceptions.LexErr msg ->
        print_string ("Syntax error: " ^ msg);
        print_newline ()
    | Exceptions.ParseErr msg ->
        print_string ("Syntax error: " ^ msg);
        print_newline ()

let _ = main ()
