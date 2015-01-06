open AST_4_C
open Common

let indent i = String.make i ' '
let il = 4



let rec print_type = function
  | NumType -> "double"
  | BoolType -> "int"
  | DataType id -> id
  | BundleType id -> id
  | FunctionType args -> raise (Exceptions.transform_error "Cannot have function type here")

and print_id (id, type_c) =
  (print_type type_c) ^ " " ^ id

and print_bundle_decl ((id, _), _) =
  "typedef struct " ^ id ^ " " ^ id ^ ";\n"

and print_data_decl ((id, _), _) =
  "typedef struct " ^ id ^ " " ^ id ^ ";\n"

and print_bundle_struct ((id, _), args) =
  "typedef struct " ^ id ^ " {\n" ^
  (indent il) ^ "void (*func)(void *data" ^
  (String.concat ", " ("" :: (List.map print_id args))) ^
  ");\n" ^
  (indent il) ^ "void *data;\n" ^
  "} " ^ id ^ ";\n"

and print_data_struct ((id, _), bundle) =
  "typedef struct " ^ id ^ " {\n" ^
  (indent il) ^ "int references;\n" ^
  (String.concat "" (List.map (fun b -> (indent il) ^ (print_id b) ^ ";\n") bundle)) ^
  "} " ^ id ^ ";\n"

and print_unary_op op = function
  | [x] ->
      op ^ (fst x)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 1 (List.length args)))

and print_binary_op op = function
  | [x; y] ->
      (fst x) ^ " " ^ op ^ " " ^ (fst y)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 2 (List.length args)))

and print_func_app prim args =
  prim ^ "(" ^
  (map_and_concat fst ", " args) ^
  ")"

and print_prim = function
  | "plus" -> "+"
  | "minus" -> "-"
  | "times" -> "*"
  | "divide" -> "/"
  | "eq" -> "="
  | "neq" -> "!="
  | "lt" -> "<"
  | "gt" -> ">"
  | "leq" -> "<="
  | "geq" -> ">="
  | "and" -> "&&"
  | "or" -> "||"
  | "not" -> "!"

  | "uniform-continuous" -> "uniform"
  | "uniform-discrete" -> "uniform_discrete"

  | x -> x

and print_prim_app prim args =
  match prim with
  | "plus" -> print_binary_op "+" args
  | "minus" -> print_binary_op "-" args
  | "times" -> print_binary_op "*" args
  | "divide" -> print_binary_op "/" args
  | "eq" -> print_binary_op "==" args
  | "neq" -> print_binary_op "!=" args
  | "lt" -> print_binary_op "<" args
  | "gt" -> print_binary_op ">" args
  | "leq" -> print_binary_op "<=" args
  | "geq" -> print_binary_op ">=" args
  | "and" -> print_binary_op "&&" args
  | "or" -> print_binary_op "||" args
  | "not" -> print_unary_op "!" args

  | "beta" -> print_func_app "beta_rng" args
  | "flip" -> print_func_app "flip_rng" args
  | "gamma" -> print_func_app "gamma_rng" args
  | "normal" -> print_func_app "normal_rng" args
  | "poisson" -> print_func_app "poisson_rng" args
  | "uniform-continuous" -> print_func_app "uniform_rng" args
  | "uniform-discrete" -> print_func_app "uniform_discrete_rng" args

  | x -> print_func_app (print_prim prim) args

and print_value = function
  | Bool b -> if b then "1" else "0"
  | Num x -> string_of_float x
  | Id (id, type_c) -> id
  | Prim (prim, args) -> print_prim_app prim args

and print_stmt i = function
  | Seq stmts -> map_and_concat (print_stmt i) "" stmts

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

  | PackBundleItem ((bundle_id, _), (data_id, _), (arg_id, _)) ->
      (indent i) ^
      "((" ^ data_id ^ "*) " ^ bundle_id ^ ".data)->" ^
      arg_id ^ " = " ^ arg_id ^ ";\n"

  | AllocateBundle ((bundle_id, bundle_type), (proc_id, _), (data_id, _)) ->
      (indent i) ^
      (print_type bundle_type) ^ " " ^ bundle_id ^ ";\n" ^
      (indent i) ^ bundle_id ^
      ".func = " ^ proc_id ^ ";\n" ^
      (indent i) ^
      bundle_id ^ ".data = (void*) malloc(sizeof(" ^ data_id ^ "));\n" ^
      (indent i) ^
      "((int*) " ^ bundle_id ^ ".data)[0] = 1;\n"

  | UnpackBundleItem ((data_id, _), (arg_id, arg_type)) ->
      (indent i) ^
      (print_id (arg_id, arg_type)) ^ " = " ^
      "((" ^ data_id ^ "*) data)->" ^ arg_id ^ ";\n"

  | DeallocateBundle ->
      (indent i) ^
      "if (--((int*) data)[0] == 0) { free(data); }\n"

  | IncrementRefCount (id, _) ->
      (indent i) ^
      "((int*) " ^ id ^ ".data)[0]++;\n"

  | DecrementRefCount (id, _) ->
      (indent i) ^
      "if (--((int*) " ^ id ^ ".data)[0] == 0) { free(" ^ id ^ ".data); }\n"

  | Observe (prim, args, value) ->
      (indent i) ^
      "observe(" ^
      (print_prim prim) ^
      "_lnp(" ^
      (fst value) ^
      ", " ^
      (map_and_concat fst ", " args) ^
      "));\n"

  | Predict (label, (id, type_c)) ->
      (indent i) ^
      (match type_c with
      | NumType -> "predict(\"%s,%f\\n\", \"" ^ label ^ "\", " ^ id ^ ");\n"
      | BoolType -> "predict(\"%s,%s\\n\", \"" ^ label ^ "\", (" ^ id ^ ")?\"true\":\"false\");\n"
      | _ -> raise (Exceptions.transform_error "Can only predict a number or boolean type"))

  | Halt -> ""

and print_proc_decl ((id, type_c), args, stmt) =
  "void " ^ id ^ "(void *data" ^
  (String.concat ", " ("" :: (List.map print_id args))) ^
  ");\n"

and print_proc ((id, type_c), args, stmt) =
  "void " ^ id ^ "(void *data" ^
  (String.concat ", " ("" :: (List.map print_id args))) ^
  ") {\n" ^
  (print_stmt il stmt) ^
  "}\n"

and print_main stmt =
  "int main(int argc, char **argv) {\n" ^
  (print_stmt il stmt) ^
  (String.make il ' ') ^
  "return 0;\n" ^
  "}\n"

let pretty_print_prog (bundle_structs, data_structs, procs, main) =
  "#include <stdlib.h>\n" ^
  "#include <stdio.h>\n\n" ^
  "#include \"probabilistic.h\"\n\n" ^
  String.concat "\n" (List.concat [
    List.map print_bundle_decl bundle_structs;
    List.map print_data_decl data_structs;
    List.map print_proc_decl procs;
    List.map print_bundle_struct bundle_structs;
    List.map print_data_struct data_structs;
    List.map print_proc procs
  ]) ^
  "\n" ^
  (print_main main)
