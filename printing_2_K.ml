open AST_2_K
open Common

(* let rec print_type = function
  | NumType            -> "num"
  | BoolType           -> "bool"
  | FunctionType args  -> "lambda (" ^ (map_and_concat print_type ", " args) ^ ")"

let print_bool b = "bool " ^ if b then "true" else "false"

let print_num x = "num " ^ (string_of_float x)

let print_id (id, type_c) =
  "id " ^
  id ^
  " : " ^
  (print_type type_c)

let print_op op = "op " ^ op

let rec print_value i = function
  | Bool b -> print_bool b

  | Num x -> print_num x

  | Id id -> print_id id

  | Lambda (args, expr) ->
      "lambda" ^
      (indent (i + 2)) ^
      "(" ^
      (map_and_concat print_id
                      (indent (i + 3))
                      args) ^
      ")" ^
      (indent (i + 2)) ^
      (print_expr (i + 2) expr)

  | Op (op, args) ->
      print_op op ^
      (indent (i + 2)) ^
      (map_and_concat print_id
                      (indent (i + 2))
                      args)

and print_expr i = function
  | Let (id, value, expr) ->
      "let " ^
      (print_id id) ^
      (indent (i + 2)) ^
      (print_value (i + 2) value) ^
      (indent (i + 2)) ^
      (print_expr (i + 2) expr)

  | If (test_expr, then_expr, else_expr) ->
      "if" ^
      (indent (i + 2)) ^
      (print_id test_expr) ^
      (indent (i + 2)) ^
      (print_expr (i + 2) then_expr) ^
      (indent (i + 2)) ^
      (print_expr (i + 2) else_expr)

  | App (expr, args) ->
      "app" ^
      (indent (i + 2)) ^
      (map_and_concat print_id
                      (indent (i + 2))
                      (expr :: args))

  | Halt value ->
      "halt" ^
      (indent (i + 2)) ^
      (print_id value) *)

let pretty_print_expr expr = raise (Failure "printing_2_K not implemented")
  (* print_expr 0 expr *)
