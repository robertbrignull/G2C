let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let prog_0_U = Parser.main Lexer.token lexbuf in
    let prog_1_F = Trans_0_infer_types.infer_types prog_0_U in

    (* Print the AST *)
    print_endline (Printing_1_F.pretty_print_expr prog_1_F);

  with
    | Exceptions.LexErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.ParseErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.TypingErr msg ->
        print_endline ("Typing error: " ^ msg)

let _ = main ()
