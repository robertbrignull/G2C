open AST_C
open Common

let indent i = String.make i ' '
let il = 4



(* print_type :: type_c -> string *)
let rec print_type = function
  | NumType -> "double"
  | BoolType -> "int"
  | ListType -> "list_node*"
  | DataType id -> id
  | BundleType id -> id
  | FunctionType args -> raise (Exceptions.transform_error "Cannot have function type here")

(* print_id :: id -> string *)
and print_id (id, type_c) =
  (print_type type_c) ^ " " ^ id

(* Print a declaration of a bundle struct *)
(* print_bundle_decl :: bundle_struct -> string *)
and print_bundle_decl ((id, _), _) =
  "typedef struct " ^ id ^ " " ^ id ^ ";\n"

(* Print a declaration of a data struct *)
(* print_data_decl :: data_struct -> string *)
and print_data_decl ((id, _), _) =
  "typedef struct " ^ id ^ " " ^ id ^ ";\n"

(* Print a definition of a bundle struct *)
(* print_bundle_struct :: bundle_struct -> string *)
and print_bundle_struct ((id, _), args) =
  "typedef struct " ^ id ^ " {\n" ^
  (indent il) ^ "void (*func)(void *data" ^
  (String.concat ", " ("" :: (List.map print_id args))) ^
  ");\n" ^
  (indent il) ^ "void *data;\n" ^
  "} " ^ id ^ ";\n"

(* Print a definition of a data struct *)
(* print_data_struct :: data_struct -> string *)
and print_data_struct ((id, _), bundle) =
  "typedef struct " ^ id ^ " {\n" ^
  (String.concat "" (List.map (fun b -> (indent il) ^ (print_id b) ^ ";\n") bundle)) ^
  "} " ^ id ^ ";\n"

(* Print an op that takes one argument, hence it is prefixed.
   Basically only '-' and '!' use this. *)
(* print_unary_op :: string -> args -> string *)
and print_unary_op op = function
  | [x] ->
      op ^ (fst x)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 1 (List.length args)))

(* Print an op that takes two arguments and hence is infix.
   Eg: mod *)
(* print_binary_op :: string -> args -> string *)
and print_binary_op op = function
  | [x; y] ->
      (fst x) ^ " " ^ op ^ " " ^ (fst y)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 2 (List.length args)))

(* Print an op that can take any number of arguments.
   Eg: '+', '&&' *)
(* print_vararg_op :: string -> args -> string *)
and print_vararg_op op args =
  map_and_concat fst (" " ^ op ^ " ") args

(* Print an op that in scheme can take any number of arguments
   but in C is binary.
   Eg: '<', '!=' *)
(* print_anded_binary_op :: string -> args -> string *)
and print_anded_binary_op op = function
  | [] -> "1"
  | [x] -> "1"
  | x :: y :: args ->
      (print_binary_op op [x; y]) ^
      " && " ^
      (print_anded_binary_op op (y :: args))

(* Print a prim as a function with that name *)
(* print_func_app :: string -> args -> string *)
and print_func_app prim args =
  prim ^ "(" ^
  (map_and_concat fst ", " args) ^
  ")"

(* Print any prim application correctly.
   This delegates to one of the above functions in most cases. *)
(* print_prim_app :: string -> args -> string *)
and print_prim_app prim args =
  let num_args = List.length args in
  match prim with
  | "plus" -> print_vararg_op "+" args
  | "minus" -> if num_args == 1 then print_unary_op "-" args else print_vararg_op "-" args
  | "times" -> print_vararg_op "*" args
  | "divide" -> if num_args == 1 then "(1.0 / " ^ (fst (List.hd args)) ^ ")" else print_vararg_op "/" args
  | "eq" -> print_anded_binary_op "==" args
  | "neq" -> print_anded_binary_op "!=" args
  | "lt" -> print_anded_binary_op "<" args
  | "gt" -> print_anded_binary_op ">" args
  | "leq" -> print_anded_binary_op "<=" args
  | "geq" -> print_anded_binary_op ">=" args
  | "and" -> print_vararg_op "&&" args
  | "or" -> print_vararg_op "||" args
  | "not" -> print_unary_op "!" args

  | "cbrt" -> "pow(" ^ (fst (List.hd args)) ^ ", 1.0/3)"
  | "rint" -> print_func_app "round" args
  | "abs" -> print_func_app "fabs" args
  | "inc" -> (fst (List.hd args)) ^ " + 1"
  | "dec" -> (fst (List.hd args)) ^ " - 1"
  | "mod" -> print_binary_op "%" args

  | "empty" -> print_func_app "create_empty_list" args
  | "cons" ->
      (match snd (List.hd args) with
      | NumType -> print_func_app "create_list_node_num" args
      | BoolType -> print_func_app "create_list_node_bool" args
      | ListType -> print_func_app "create_list_node_list" args
      | BundleType id ->
          "create_list_node_bundle(*((abstract_bundle*) &" ^
          (fst (List.hd args)) ^
          "), " ^
          (fst (List.hd (List.tl args))) ^
          ")"
      | _ -> raise (Exceptions.transform_error "Trying to cons invalid type to list"))
  | "first_num" -> print_func_app "first_num" args
  | "first_bool" -> print_func_app "first_bool" args
  | "first_list" -> print_func_app "first_list" args
  | "rest" -> print_func_app "rest" args
  | "empty?" -> print_func_app "is_empty_list" args
  | "count" -> print_func_app "count_list" args
  | "nth_num" -> print_func_app "nth_num" args
  | "nth_bool" -> print_func_app "nth_bool" args
  | "nth_list" -> print_func_app "nth_list" args

  | "beta" -> print_func_app "beta_rng" args
  | "flip" -> print_func_app "flip_rng" args
  | "gamma" -> print_func_app "gamma_rng" args
  | "normal" -> print_func_app "normal_rng" args
  | "poisson" -> print_func_app "poisson_rng" args
  | "geometric" -> print_func_app "geometric_rng" args
  | "exponential" -> print_func_app "exponential_rng" args
  | "uniform-continuous" -> print_func_app "uniform_rng" args
  | "uniform-discrete" ->
      let l = fst (List.hd args) in
      let u = fst (List.hd (List.tl args)) in
      l ^ " + uniform_discrete_rng(" ^ u ^ " - " ^ l ^ ")"
  | "discrete" -> print_func_app "discrete_rng_wrapper" args

  | _ -> print_func_app prim args

(* The same for a typed prim *)
(* print_typed_prim_app :: string -> type_c -> args -> string *)
and print_typed_prim_app prim type_c args =
  match prim, type_c with
  | "first", NumType -> print_func_app "first_num" args
  | "first", BoolType -> print_func_app "first_bool" args
  | "first", ListType -> print_func_app "first_list" args
  | "first", BundleType id -> "(" ^ id ^ ") " ^ print_func_app "first_bundle" args

  | "second", NumType -> "first_num(rest(" ^ (fst (List.hd args)) ^ "))"
  | "second", BoolType -> "first_bool(rest(" ^ (fst (List.hd args)) ^ "))"
  | "second", ListType -> "first_list(rest(" ^ (fst (List.hd args)) ^ "))"
  | "second", BundleType id -> "(" ^ id ^ ") first_bundle(rest(" ^ (fst (List.hd args)) ^ "))"

  | "nth", NumType -> print_func_app "nth_num" args
  | "nth", BoolType -> print_func_app "nth_bool" args
  | "nth", ListType -> print_func_app "nth_list" args

  | "categorical", NumType -> print_func_app "categorical_num_rng_wrapper" args
  | "categorical", BoolType -> print_func_app "categorical_bool_rng_wrapper" args
  | "categorical", ListType -> print_func_app "categorical_list_rng_wrapper" args
  | "categorical", BundleType id -> print_func_app "categorical_bundle_rng_wrapper" args

  | _, _ -> print_func_app prim args

(* Print a observe, for most they have the same name so you just
   add on '_lnp'.
   For some however (eg: discrete, categorical) we must convert
   the list to an array first using a wrapper. *)
(* print_prim_observe :: string -> args -> string *)
and print_prim_observe prim args =
  match prim with
  | "discrete" -> print_func_app ("discrete_lnp_wrapper") args
  | prim -> print_func_app (prim ^ "_lnp") args

(* Print any value *)
(* print_value :: value -> string *)
and print_value = function
  | Bool b -> if b then "1" else "0"
  | Num x -> string_of_float x
  | Id (id, type_c) -> id
  | Prim (prim, args) -> print_prim_app prim args
  | TypedPrim (prim, type_c, args) -> print_typed_prim_app prim type_c args

(* Print any statement *)
(* print_stmt :: int -> stmt -> string *)
and print_stmt i = function
  | Seq stmts -> map_and_concat (print_stmt i) "" stmts

  | Assign (id, TypedPrim ("nth", BundleType bundle_id, args)) ->
      let abs_id = new_id () in
      (indent i) ^ "abstract_bundle " ^ abs_id ^ " = " ^
      (print_func_app "nth_bundle" args) ^
      ";\n" ^
      (indent i) ^ (print_id id) ^
      " = *((" ^  bundle_id ^ "*) &" ^ abs_id ^ ");\n"

  | Assign (id, value) ->
      (indent i) ^
      (print_id id) ^
      " = " ^
      (print_value value) ^
      ";\n"

  | If ((test_id, _), then_stmt, else_stmt) ->
      (indent i) ^
      "if (" ^
      test_id ^
      ") {\n" ^
      (print_stmt (i + il) then_stmt) ^
      (indent i) ^
      "}\n" ^
      (indent i) ^
      "else {\n" ^
      (print_stmt (i + il) else_stmt) ^
      (indent i) ^
      "}\n"

  | BundleApp ((bundle_id, _), args) ->
      (indent i) ^
      bundle_id ^
      ".func(" ^
      bundle_id ^
      ".data" ^
      (String.concat ", " ("" :: (List.map fst args))) ^
      ");\n"

  | RecursiveApp ((function_id, _), args) ->
      (indent i) ^
      function_id ^
      "(data" ^
      (String.concat ", " ("" :: (List.map fst args))) ^
      ");\n"

  | AllocateBundle ((bundle_id, bundle_type), (proc_id, _), (data_id, _)) ->
      (indent i) ^
      (print_type bundle_type) ^ " " ^ bundle_id ^ ";\n" ^
      (indent i) ^ bundle_id ^ ".func = " ^ proc_id ^ ";\n" ^
      (indent i) ^ bundle_id ^ ".data = (void*) malloc(sizeof(" ^ data_id ^ "));\n" ^
      (indent i) ^ "((int*) " ^ bundle_id ^ ".data)[0] = 1;\n"

  | AllocateRecursiveBundle ((bundle_id, bundle_type), (proc_id, _)) ->
      (indent i) ^
      (print_type bundle_type) ^ " " ^ bundle_id ^ ";\n" ^
      (indent i) ^ bundle_id ^ ".func = " ^ proc_id ^ ";\n" ^
      (indent i) ^ bundle_id ^ ".data = data;\n"

  | PackBundleItem ((bundle_id, _), (data_id, _), (arg_id, _)) ->
      (indent i) ^
      "((" ^ data_id ^ "*) " ^ bundle_id ^ ".data)->" ^
      arg_id ^ " = " ^ arg_id ^ ";\n"

  | PackMemBundle ((bundle_id, _), (data_id, _), (proc_id, _)) ->
      (indent i) ^
      "((" ^ data_id ^ "*) " ^ bundle_id ^ ".data)->bundle = " ^ proc_id ^ ";\n"

  | UnpackBundleItem ((data_id, _), (arg_id, arg_type)) ->
      (indent i) ^
      (print_id (arg_id, arg_type)) ^ " = " ^
      "((" ^ data_id ^ "*) data)->" ^ arg_id ^ ";\n"

  | Observe (prim, args, value) ->
      (indent i) ^
      "observe(" ^
      (print_prim_observe prim (value :: args)) ^
      ");\n"

  | UnvaluedObserve (prim, args) ->
      (indent i) ^
      "observe(" ^
      (print_prim_observe prim args) ^
      ");\n"

  | Predict (label, (id, type_c)) ->
      (indent i) ^
      (match type_c with
      | NumType -> "predict(\"%s,%f\\n\", \"" ^ label ^ "\", " ^ id ^ ");\n"
      | BoolType -> "predict(\"%s,%s\\n\", \"" ^ label ^ "\", (" ^ id ^ ")?\"true\":\"false\");\n"
      | ListType -> "predict(\"%s,%s\\n\", \"" ^ label ^ "\", " ^ print_func_app "print_list" [(id, type_c)] ^ ");\n"
      | _ -> raise (Exceptions.transform_error "Can only predict a number or boolean type"))

  | Halt -> ""

(* Print the declaration of a procedure *)
(* print_proc_decl :: proc -> string *)
and print_proc_decl = function
  | Proc ((id, _), args, _) ->
      "void " ^ id ^ "(void *data" ^
      (String.concat ", " ("" :: (List.map print_id args))) ^
      ");\n"

  | MemProc ((id, _), (_, _), args, (_, _), _) ->
      "void " ^ id ^ "(void *data" ^
      (String.concat ", " ("" :: (List.map print_id args))) ^
      ");\n"

(* Print the definition of a procedure *)
(* print_proc :: proc -> string *)
and print_proc = function
  | Proc ((proc_id, _), args, stmt) ->
      "void " ^ proc_id ^ "(void *data" ^
      (String.concat ", " ("" :: (List.map print_id args))) ^
      ") {\n" ^
      (print_stmt il stmt) ^
      "}\n"

  | MemProc ((proc_id, proc_type), (data_id, _), args, (cont_bundle_id, _), cont_args) ->
      let cont = last args in
      let args_without_cont = remove_last args in
      let false_cont_id = proc_id ^ "_false_cont" in
      let false_cont_data_id = proc_id ^ "_false_cont_data" in
      let cache_struct_id = proc_id ^ "_cache_struct" in
      let cache_array_id = proc_id ^ "_cache" in
      let cache_array_length_id = proc_id ^ "_cache_length" in
      let cache_array_count_id = proc_id ^ "_cache_count" in

      "typedef struct " ^ false_cont_data_id ^ "{\n" ^
      (indent il) ^ cont_bundle_id ^ " next;\n" ^
      (map_and_concat (fun id -> (indent il) ^ (print_id id) ^ ";\n") "" args_without_cont) ^
      "} " ^ false_cont_data_id ^ ";\n" ^
      "\n" ^

      "typedef struct " ^ cache_struct_id ^ " {\n" ^
      (map_and_concat (fun id -> (indent il) ^ (print_id id) ^ ";\n") "" (List.append args_without_cont cont_args)) ^
      "} " ^ cache_struct_id ^ ";\n" ^
      "\n" ^

      "int " ^ cache_array_length_id ^ " = 10;\n" ^
      "int " ^ cache_array_count_id ^ " = 0;\n" ^
      cache_struct_id ^ " *" ^ cache_array_id ^ " = NULL;\n" ^
      "\n" ^

      "void " ^ false_cont_id ^ "(void *data" ^
      (String.concat ", " ("" :: List.map print_id cont_args)) ^
      ") {\n" ^
      (indent il) ^ cont_bundle_id ^ " next = ((" ^ false_cont_data_id ^"*) data)->next;\n" ^
      (map_and_concat (fun id -> (indent il) ^ (print_id id) ^ " = ((" ^ false_cont_data_id ^ "*) data)->" ^ (fst id) ^ ";\n") "" args_without_cont) ^
      "\n" ^
      (map_and_concat (fun id -> (indent il) ^ cache_array_id ^ "[" ^ cache_array_count_id ^ "]." ^ (fst id) ^ " = " ^ (fst id) ^ ";\n") "" (List.append args_without_cont cont_args)) ^
      (indent il) ^ cache_array_count_id ^ "++;\n" ^
      "\n" ^
      (indent il) ^ "if (" ^ cache_array_count_id ^ " == " ^ cache_array_length_id ^ ") {\n" ^
      (indent (il*2)) ^ cache_array_length_id ^ " *= 2;\n" ^
      (indent (il*2)) ^ cache_array_id ^ " = (" ^ cache_struct_id ^ "*) realloc(" ^ cache_array_id ^ ", sizeof(" ^ cache_struct_id ^ ") * " ^ cache_array_length_id ^ ");\n" ^
      (indent il) ^ "}\n" ^
      "\n" ^
      (indent il) ^ "next.func(next.data" ^
      (String.concat ", " ("" :: List.map fst cont_args)) ^
      ");\n" ^
      "}\n" ^
      "\n" ^

      "void " ^ proc_id ^ "(void *data" ^
      (String.concat ", " ("" :: List.map print_id args)) ^
      ") {\n" ^
      (indent il) ^ data_id ^ " *cast_data = (" ^ data_id ^ "*) data;\n" ^
      "\n" ^
      (indent il) ^ "if (" ^ cache_array_id ^ " == NULL) {\n" ^
      (indent (il*2)) ^ cache_array_id ^ " = (" ^ cache_struct_id ^ "*) malloc(sizeof(" ^ cache_struct_id ^ ") * " ^ cache_array_length_id ^ ");\n" ^
      (indent il) ^ "}\n" ^
      "\n" ^
      (indent il) ^ "int i = 0;\n" ^
      (indent il) ^ "while (i < " ^ cache_array_count_id ^ " && " ^
      (if List.length args_without_cont = 0
       then "0"
       else map_and_concat (fun id -> (fst id) ^ " != " ^ cache_array_id ^ "[i]." ^ (fst id)) " && " args_without_cont) ^ 
      ") {\n" ^
      (indent (il*2)) ^ "i++;\n" ^
      (indent il) ^ "}\n" ^
      "\n" ^ 
      (indent il) ^ "if (i == " ^ cache_array_count_id ^ ") {\n" ^
      (indent (il*2)) ^ cont_bundle_id ^ " false_cont;\n" ^
      (indent (il*2)) ^ "false_cont.func = " ^ false_cont_id ^ ";\n" ^
      (indent (il*2)) ^ "false_cont.data = malloc(sizeof(" ^ false_cont_data_id ^ "));\n" ^
      (indent (il*2)) ^ "((" ^ false_cont_data_id ^ "*) false_cont.data)->next = " ^ (fst cont) ^ ";\n" ^
      (map_and_concat (fun id -> (indent (il*2)) ^ "((" ^ false_cont_data_id ^ "*) false_cont.data)->" ^ (fst id) ^ " = " ^ (fst id) ^ ";\n") "" args_without_cont) ^ 
      "\n" ^
      (indent (il*2)) ^ "cast_data->bundle.func(cast_data->bundle.data" ^
      (String.concat ", " ("" :: List.map fst args_without_cont)) ^
      ", false_cont);\n" ^
      (indent il) ^ "}\n" ^
      (indent il) ^ "else {\n" ^
      (indent (il*2)) ^ (fst cont) ^ ".func(" ^ (fst cont) ^ ".data" ^
      (String.concat ", " ("" :: List.map (fun id -> cache_array_id ^ "[i]." ^ (fst id)) cont_args)) ^
      ");\n" ^
      (indent il) ^ "}\n" ^
      "}\n"

(* Print the C main function *)
(* print_main :: stmt -> string *)
and print_main stmt =
  "int main(int argc, char **argv) {\n" ^
  (print_stmt il stmt) ^
  (String.make il ' ') ^
  "return 0;\n" ^
  "}\n"

(* The next four functions determine whether the program uses various
   features. This is so that only the headers that are needed are included *)

(* Returns true iff the statement uses the given prim somewhere *)
(* uses_prim_stmt :: string -> stmt -> bool *)
let rec uses_prim_stmt prim = function
  | Seq stmts -> List.fold_right (||) (List.map (uses_prim_stmt prim) stmts) false
  | Assign (_, Prim (prim2, _)) when prim = prim2 -> true
  | Assign (_, TypedPrim (prim2, _, _)) when prim = prim2 -> true
  | If (_, then_stmt, else_stmt) -> uses_prim_stmt prim then_stmt || uses_prim_stmt prim else_stmt
  | Observe (prim2, _, _) when prim = prim2 -> true
  | UnvaluedObserve (prim2, _) when prim = prim2 -> true
  | _ -> false

(* Returns true iff the proc uses the given prim somewhere *)
(* uses_prim_proc :: string -> proc -> bool *)
let uses_prim_proc prim = function
  | Proc (_, _, stmt) -> uses_prim_stmt prim stmt
  | MemProc (_, _, _, _, _) -> false

(* Returns true iff the program uses the given prim somewhere *)
(* uses_prim :: string -> prog -> bool *)
let uses_prim prim (bundle_structs, data_structs, procs, main) =
     List.fold_right (||) (List.map (fun proc -> uses_prim_proc prim proc) procs) false
  || uses_prim_stmt prim main

(* Returns true iff the statement uses lists somewhere *)
(* uses_lists :: prog -> bool *)
let uses_lists prog =
  let prims = ["cons"; "rest"; "empty"; "count"; "fist"; "second"; "nth"] in
  List.fold_right (||) (List.map (fun prim -> uses_prim prim prog) prims) false

(* Reads an entire file and outputs it as a string *)
(* read_file :: string -> string *)
let read_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

(* Pretty prints an entire program *)
(* pretty_print_prog :: prog -> bool *)
let pretty_print_prog prog =
  let (bundle_structs, data_structs, procs, main) = prog in
  String.concat "\n" (List.concat [
    [read_file "src/c_headers/default.c"];
    if uses_prim "signum" prog then [read_file "src/c_headers/signum.c"] else [];
    if uses_lists prog then [read_file "src/c_headers/lists.c"] else [];
    if uses_prim "discrete" prog then [read_file "src/c_headers/discrete.c"] else [];
    if uses_prim "categorical" prog then [read_file "src/c_headers/categorical.c"] else [];
    if uses_prim "beta_geometric" prog then [read_file "src/c_headers/beta_geometric.c"] else [];
    List.map print_bundle_decl bundle_structs;
    List.map print_data_decl data_structs;
    List.map print_proc_decl procs;
    List.map print_bundle_struct bundle_structs;
    List.map print_data_struct data_structs;
    List.map print_proc procs
  ]) ^
  "\n" ^
  (print_main main)
