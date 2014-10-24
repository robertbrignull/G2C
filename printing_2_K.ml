open AST_2_K

let map_and_concat f sep xs = String.concat sep (List.map f xs)

let indent_level = 2
let indent n = "\n" ^ String.make n ' '

let rec print_type = function
  | NumType            -> "num"
  | BoolType           -> "bool"
  | FunctionType args  -> "lambda (" ^ (map_and_concat print_type ", " args) ^ ")"

let print_bool b = "bool " ^ if b then "true" else "false"

let print_num x = "num " ^ (string_of_float x)

let print_id id = "id " ^ id

let print_op op = "op " ^ op

let print_def (id, type_c) =
  (print_id id) ^ " : " ^ (print_type type_c)

let rec print_value i (value_guts, type_c) =
  "<value, " ^
  (print_type type_c) ^
  ", " ^
  (match value_guts with
  | Bool b -> print_bool b

  | Num x -> print_num x

  | Id id -> print_id id

  | Lambda (args, expr) ->
      "lambda" ^
      (indent (i + 2)) ^
      "(" ^
      (map_and_concat print_def
                      (indent (i + 3))
                      args) ^
      ")" ^
      (indent (i + 2)) ^
      (print_expr (i + 2) expr)
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

let pretty_print_expr expr = print_expr 0 expr
