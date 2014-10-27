let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let prog_0_U = Parser.main Lexer.token lexbuf in
    print_newline ();

    let prog_1_F = Trans_0_infer_types.infer_types prog_0_U in
    print_endline "----- F AST -----";
    print_endline (Printf.sprintf "size: %d" (AST_1_F.size prog_1_F));
    print_endline (Printing_1_F.pretty_print_expr prog_1_F);
    print_newline ();

    let prog_1_F_unique = Opt_1_unique_ids.make_ids_unique prog_1_F in
    print_endline "----- F AST unique ids -----";
    print_endline (Printf.sprintf "size: %d" (AST_1_F.size prog_1_F_unique));
    print_endline (Printing_1_F.pretty_print_expr prog_1_F_unique);
    print_newline ();

    let prog_2_K = Trans_1_F_to_K.transform prog_1_F_unique in
    print_endline "----- K AST -----";
    print_endline (Printf.sprintf "size: %d" (AST_2_K.size prog_2_K));
    print_endline (Printing_2_K.pretty_print_expr prog_2_K);
    print_newline ();

    let prog_2_K_opt = Opt_2_K.optimise prog_2_K in
    print_endline "----- K AST optimised -----";
    print_endline (Printf.sprintf "size: %d" (AST_2_K.size prog_2_K_opt));
    print_endline (Printing_2_K.pretty_print_expr prog_2_K_opt);
    print_newline ();

    let prog_3_C = Trans_2_K_to_C.transform prog_2_K_opt in
    print_endline "----- C AST -----";
    print_endline (Printf.sprintf "size: %d" (AST_3_C.size prog_3_C));
    print_endline (Printing_3_C.pretty_print_expr prog_3_C);
    print_newline ();

  with
    | Exceptions.LexErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.ParseErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.TypingErr msg ->
        print_endline ("Typing error: " ^ msg)

let _ = main ()
