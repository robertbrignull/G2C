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
  (String.concat "" (List.map (fun b -> (indent il) ^ (print_id b) ^ ";\n") bundle)) ^
  "} " ^ id ^ ";\n"

and print_op op args =
  match op with
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
  | op -> raise (Exceptions.transform_error ("Unrecognised op: " ^ op))

and print_unary_op op = function
  | [x] ->
      op ^ (fst x)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 1 (List.length args)))

and print_binary_op op = function
  | [x; y] ->
      (fst x) ^ " " ^ op ^ " " ^ (fst y)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 2 (List.length args)))

and print_prim = function
  | "beta" -> "beta"
  | "flip" -> "flip"
  | "gamma" -> "gamma"
  | "normal" -> "normal"
  | "poisson" -> "poisson"
  | "uniform-continuous" -> "uniform"
  | "uniform-discrete" -> "uniform_discrete"
  | prim -> raise (Exceptions.transform_error ("Unrecognised prim: " ^ prim))

and print_value = function
  | Bool b -> if b then "1" else "0"
  | Num x -> string_of_float x
  | Id (id, type_c) -> id
  | Op (op, args) -> print_op op args
  | Prim (prim, args) ->
      (print_prim prim) ^
      "_rng(" ^
      (map_and_concat fst ", " args) ^
      ")"

and print_stmt i = function
  | Seq stmts ->
      (String.concat "" (List.map (print_stmt i) stmts))

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

  | PackBundle ((bundle_id, bundle_type), (proc_id, _), (data_id, _), bundle) ->
      let pack_one (arg_id, _) =
        (indent i) ^
        "((" ^ data_id ^ "*) " ^ bundle_id ^ ".data)->" ^
        arg_id ^ " = " ^ arg_id ^ ";\n"
      in
      (indent i) ^
      (print_type bundle_type) ^ " " ^ bundle_id ^ ";\n" ^
      (indent i) ^ bundle_id ^
      ".func = " ^ proc_id ^ ";\n" ^
      (indent i) ^ bundle_id ^
      ".data = (void*) malloc(sizeof(" ^ data_id ^ "));\n" ^
      (String.concat "" (List.map pack_one bundle))

  | UnpackBundle ((data_id, _), bundle) ->
      let unpack_one (id, type_c) =
        (indent i) ^
        (print_id (id, type_c)) ^ " = " ^
        "((" ^ data_id ^ "*) data)->" ^ id ^ ";\n"
      in
      (String.concat "" (List.map unpack_one bundle)) ^
      (indent i) ^
      "//free(data);\n"

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
