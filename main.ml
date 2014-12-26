let input_filename = ref ""
let output_filename = ref "out.c"
let verbose = ref false

let set_input_filename filename = input_filename := filename
let set_output_filename filename = output_filename := filename

let main () =
  let speclist = [
    ("-i", Arg.String (set_input_filename), "Sets the input file, by default reads from stdin.");
    ("-o", Arg.String (set_output_filename), "Sets the output file.");
    ("-v", Arg.Set verbose, "Be verbose and print out intermediate syntax trees.")
  ] in
  let usage_msg = "g2c converts the probabilistic programming language G to probabilistic C." in

  Arg.parse speclist set_input_filename usage_msg;

  try
    let lexbuf =
      if !input_filename = "" then
        Lexing.from_channel stdin
      else
        Lexing.from_channel (open_in !input_filename)
    in
    let prog_0_U = Parser.main Lexer.token lexbuf in

    let prog_1_F = Trans_0_infer_types.infer_types prog_0_U in
    if !verbose then begin
      print_endline "----- F AST -----";
      print_endline (Printing_1_F.pretty_print_prog prog_1_F);
      print_newline ()
    end;

    let prog_1_F_unique = Opt_1_unique_ids.make_ids_unique prog_1_F in
    if !verbose then begin
      print_endline "----- F AST unique ids -----";
      print_endline (Printing_1_F.pretty_print_prog prog_1_F_unique);
      print_newline ();
    end;

    let prog_2_K = Trans_1_F_to_K.transform prog_1_F_unique in
    if !verbose then begin
      print_endline "----- K AST -----";
      print_endline (Printing_2_K.pretty_print_expr prog_2_K);
      print_newline ();
    end;

    let prog_2_K_opt = Opt_2_K.optimise prog_2_K in
    if !verbose then begin
      print_endline "----- K AST optimised -----";
      print_endline (Printing_2_K.pretty_print_expr prog_2_K_opt);
      print_newline ();
    end;

    let prog_3_H = Trans_2_K_to_H.transform prog_2_K_opt in
    if !verbose then begin
      print_endline "----- H AST -----";
      print_endline (Printing_3_H.pretty_print_prog prog_3_H);
      print_newline ();
    end;

    let prog_4_C = Trans_3_H_to_C.transform prog_3_H in
    let c_output = Printing_4_C.pretty_print_prog prog_4_C in
    if !verbose then begin
      print_endline "----- C -----";
      print_endline c_output;
      print_newline ();
    end;

    output_string (open_out !output_filename) c_output;

  with
    | Exceptions.LexErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.ParseErr msg ->
        print_endline ("Syntax error: " ^ msg)
    | Exceptions.TypingErr msg ->
        print_endline ("Typing error: " ^ msg)
    | Exceptions.TransformErr msg ->
        print_endline ("Transform error: " ^ msg)

let _ = main ()
