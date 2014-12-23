open AST_1_F
open Common

let rec print_type = function
  | NumType                   -> "num"
  | BoolType                  -> "bool"
  | FunctionType (args, res)  -> (print_type_list args) ^
                                 " -> " ^
                                 (print_type res)

and print_type_list ts = "(" ^ (map_and_concat print_type ", " ts) ^ ")"

let print_info type_c =
  "<" ^
  (print_type type_c) ^
  ", "

let print_bool b = "bool " ^ if b then "true" else "false"

let print_num x = "num " ^ (string_of_float x)

let print_id id = "id " ^ id

let print_op op = "op " ^ op

let print_def (id, type_c) =
  (print_id id) ^ " : " ^ (print_type type_c)

let rec print_expr i (expr_guts, expr_info) = 
  print_info expr_info ^
  (match expr_guts with
  | Bool b                 -> print_bool b
  | Num x                  -> print_num x
  | Id id                  -> print_id id

  | Lambda (args, expr)    -> "lambda" ^
                              (indent (i + 2)) ^
                              "(" ^
                              (map_and_concat print_def
                                              (indent (i + 3))
                                              args) ^
                              ")" ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) expr)

  | Let (id, value, expr)  -> "let " ^
                              (print_id id) ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) value) ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) expr)

  | If (test, then_expr, else_expr) ->
                              "if" ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) test) ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) then_expr) ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) else_expr)

  | Op (op, args)          -> print_op op ^
                              (indent (i + 2)) ^
                              (map_and_concat (print_expr (i + 2))
                                              (indent (i + 2))
                                              args)

  | App (expr, args)       -> "app" ^
                              (indent (i + 2)) ^
                              (map_and_concat (print_expr (i + 2))
                                              (indent (i + 2))
                                              (expr :: args))
  ) ^
  ">"

and print_stmt i (stmt_guts, stmt_info) =
  print_info stmt_info ^
  (match stmt_guts with
  | Assume (id, expr)      -> "assume " ^
                              (print_id id) ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) expr)

  | Observe (expr, value)  -> "observe " ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) expr) ^
                              (indent (i + 2)) ^
                              (print_expr (i + 2) value)

  | Predict id             -> "predict " ^
                              (print_id id)
  ) ^
  ">" 

and print_stmts i stmts =
  map_and_concat (print_stmt i)
                 (indent 0)
                 stmts

let pretty_print_prog prog = print_stmts 0 prog
