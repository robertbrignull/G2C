let input_filename = ref ""
let output_filename = ref "out.c"
let verbose = ref false
let optimisation_level = ref 2

let set_input_filename filename = input_filename := filename
let set_output_filename filename = output_filename := filename
let set_optimisation_level level = optimisation_level := level

let main () =
  let speclist = [
    ("-i", Arg.String (set_input_filename), "Sets the input file, by default reads from stdin.");
    ("-o", Arg.String (set_output_filename), "Sets the output file.");
    ("-v", Arg.Set verbose, "Be verbose and print out intermediate syntax trees.");
    ("-O", Arg.Int (set_optimisation_level), "Optimisation level. 0 is no optimisation, 1 is only non-probabilistic optimisations, 2 is all optimisations and is the default.")
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
    let prog_U = Parser.main Lexer.token lexbuf in

    let prog_F = Trans_infer_types.infer_types prog_U in
    if !verbose then begin
      print_endline "----- F AST -----";
      print_endline (Printing_F.pretty_print_prog prog_F);
      print_newline ()
    end;

    let prog_F_unique = Opt_unique_ids.make_ids_unique prog_F in
    if !verbose then begin
      print_endline "----- F AST unique ids -----";
      print_endline (Printing_F.pretty_print_prog prog_F_unique);
      print_newline ();
    end;

    let prog_K = Trans_F_to_K.transform prog_F_unique in
    if !verbose then begin
      print_endline "----- K AST -----";
      print_endline (Printing_K.pretty_print_expr prog_K);
      print_newline ();
    end;

    let prog_K_opt = Opt_K.optimise !optimisation_level prog_K in
    if !verbose then begin
      print_endline "----- K AST optimised -----";
      print_endline (Printing_K.pretty_print_expr prog_K_opt);
      print_newline ();
    end;

    let prog_H = Trans_K_to_H.transform prog_K_opt in
    if !verbose then begin
      print_endline "----- H AST -----";
      print_endline (Printing_H.pretty_print_prog prog_H);
      print_newline ();
    end;

    let prog_C = Trans_H_to_C.transform prog_H in
    let c_output = Printing_C.pretty_print_prog prog_C in
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
