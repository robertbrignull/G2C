open AST_4_H
open Common

let rec print_type = function
  | NumType            -> "num"
  | BoolType           -> "bool"
  | FunctionType (closure, args) ->
      "lambda (" ^
      (map_and_concat print_type ", " closure) ^
      ") (" ^
      (map_and_concat print_type ", " args) ^
      ")"

and print_bool b = "bool " ^ if b then "true" else "false"

and print_num x = "num " ^ (string_of_float x)

and print_id id = "id " ^ id

and print_op op = "op " ^ op

and print_arg (id, type_c) =
  (print_id id) ^ " : " ^ (print_type type_c)

and print_value i (value_guts, type_c) =
  "<value, " ^
  (print_type type_c) ^
  ", " ^
  (match value_guts with
  | Bool b -> print_bool b

  | Num x -> print_num x

  | Id id -> print_id id
  ) ^
  ">"

and print_decl i = function
  | Value value -> print_value i value

  | Op (op, args) ->
      print_op op ^
      (indent (i + 2)) ^
      (map_and_concat (print_value (i + 2))
                      (indent (i + 2))
                      args)

and print_expr i expr =
  "<expr, " ^
  (match expr with
  | Let (id, decl, expr) ->
      "let " ^
      (print_id id) ^
      (indent (i + 2)) ^
      (print_decl (i + 2) decl) ^
      (indent (i + 2)) ^
      (print_expr (i + 2) expr)

  | If (test, then_expr, else_expr) ->
      "if" ^
      (indent (i + 2)) ^
      (print_value (i + 2) test) ^
      (indent (i + 2)) ^
      (print_expr (i + 2) then_expr) ^
      (indent (i + 2)) ^
      (print_expr (i + 2) else_expr)

  | App (expr, args) ->
      "app" ^
      (indent (i + 2)) ^
      (map_and_concat (print_value (i + 2))
                      (indent (i + 2))
                      (expr :: args))

  | Halt value ->
      "halt" ^
      (indent (i + 2)) ^
      (print_value (i + 2) value)
  ) ^
  ">"

and print_proc i (id, closure, args, expr) =
  "<proc, " ^
  (print_id id) ^
  (indent (i + 2)) ^
  "(" ^
  (map_and_concat print_arg
                  (indent (i + 3))
                  closure) ^
  ")" ^
  (indent (i + 2)) ^
  "(" ^
  (map_and_concat print_arg
                  (indent (i + 3))
                  args) ^
  ")" ^
  (indent (i + 2)) ^
  (print_expr (i + 2) expr) ^
  ">"

and print_prog i (procs, expr) =
  (map_and_concat (print_proc i)
                  (indent i)
                  procs) ^
  (indent i) ^
  (print_expr i expr)

let pretty_print_prog prog = print_prog 0 prog
