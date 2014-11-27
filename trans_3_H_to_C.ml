open AST_3_H
open Common

let id_to_bundle id =
  "bundle_" ^ (id_index id)

let id_to_data id =
  "data_" ^ (id_index id)

let rec transform_id (id, type_c) =
  match type_c with
  | NumType -> "double " ^ id
  | BoolType -> "int " ^ id
  | FunctionType args -> (id_to_bundle id) ^ " " ^ id

and pack_closure let_id bundle =
  let pack_one = fun (id, type_c) ->
    Printf.sprintf "((%s*) %s.data)->%s = %s;\n"
      (id_to_data let_id)
      (id_to_bundle let_id)
      id
      id
  in
  map_and_concat pack_one "" bundle

and unpack_closure func_id bundle =
  let unpack_one = fun (id, type_c) ->
    (transform_id (id, type_c)) ^
    " = ((" ^
    (id_to_data func_id) ^
    "*) data)->" ^
    id ^ ";\n"
  in
  map_and_concat unpack_one "" bundle

and transform_op = function
  | "plus" -> "+"
  | "minus" -> "-"
  | "times" -> "*"
  | "divide" -> "/"
  | "eq" -> "=="
  | "neq" -> "!="
  | "lt" -> "<"
  | "gt" -> ">"
  | "leq" -> "<="
  | "geq" -> ">="
  | op -> raise (Exceptions.transform_error ("Unrecognised op: " ^ op))

and transform_assignment (let_id, let_type) = function
  | Bool b ->
      (transform_id (let_id, let_type)) ^ " = " ^
      (if b then "1" else "0") ^ ";\n"

  | Num x ->
      (transform_id (let_id, let_type)) ^ " = " ^
      (string_of_float x) ^ ";\n"

  | Id (id, type_c) ->
      (transform_id (let_id, let_type)) ^ " = " ^
      id ^ ";\n"

  | ProcInstance ((id, type_c), bundle) ->
      (transform_id (let_id, let_type)) ^ ";\n" ^
      (Printf.sprintf "%s.func = %s;\n"
        (id_to_bundle let_id)
         id) ^
      (Printf.sprintf "%s.data = (void*) malloc(sizeof(%s));\n"
        (id_to_bundle let_id)
        (id_to_data let_id)) ^
      (pack_closure let_id bundle)

  | Op (op, args) ->
      (transform_id (let_id, let_type)) ^ " = " ^
      (binary_op op args) ^ ";\n"

and binary_op op = function
  | [x; y] ->
      (fst x) ^ " " ^
      (transform_op op) ^
      " " ^ (fst y)

  | args -> raise (Exceptions.transform_error (Printf.sprintf "Wrong number of arguments to '%s', %d expected, %d received" op 2 (List.length args)))

and transform_expr = function
  | Let (id, value, expr) ->
      (transform_assignment id value) ^
      (transform_expr expr)

  | If ((test, type_c), then_expr, else_expr) ->
      "if (" ^
      test ^ ") {\n" ^
      (transform_expr then_expr) ^
      "}\nelse {\n" ^
      (transform_expr then_expr) ^
      "}"

  | App ((id, type_c), args) ->
      id ^ ".func(" ^
      (String.concat ", " ((id ^ ".data") :: (List.map fst args))) ^
      ");"

  | Halt (id, type_c) ->
      match type_c with
    | NumType -> "printf(\"%f\", " ^ id ^ ");"
    | BoolType -> "printf(\"%s\", (" ^ id ^ ")?\"true\":\"false\");"
    | FunctionType args -> raise (Exceptions.transform_error "Cannot halt on a function type")

and transform_proc ((id, _), bundle, args, expr) =
  "void " ^ id ^ "(" ^
  (String.concat ", " ("void *data" :: (List.map transform_id args))) ^
  ") {\n" ^
  (unpack_closure id bundle) ^
  "free(data);\n" ^
  (transform_expr expr) ^
  "\n}\n"

let transform (procs, expr) =
  "#include <stdlib.h>\n" ^
  "#include <stdio.h>\n" ^
  "\n" ^
  (map_and_concat transform_proc
                  "\n"
                  procs) ^
  "\n" ^
  "int main(int argc, char **argv) {\n" ^
  (transform_expr expr) ^ "\n" ^
  "}"
