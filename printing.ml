open AST

let map_and_concat f sep xs = String.concat sep (List.map f xs)

let indent_level = 2
let indent n = "\n" ^ String.make n ' '

let print_id id = "_" ^ id

let print_op op = op

let rec print_type = function
  | NumType                -> "Num"
  | BoolType               -> "Bool"
  | CompoundType (l, r)    -> "(" ^
                              (print_type l) ^
                              " -> " ^
                              (print_type r) ^
                              ")"

let print_lambda_arg (id, type_c) =
  (print_id id) ^ " : " ^ (print_type type_c)

let rec print_expr i = function
  | Bool b                 -> if b then "#t" else "#f"
  | Num x                  -> string_of_float x
  | Id id                  -> print_id id

  | Lambda (args, expr)    -> "(lambda (" ^
                              (map_and_concat print_lambda_arg
                                              (indent (i + 9))
                                              args) ^
                              ") " ^
                              (indent (i + 8)) ^
                              (print_expr (i + 8) expr) ^
                              ")"

  | Let (id, value, expr)  -> let id_text = print_id id in
                              let l = String.length id_text in
                              "(let (" ^
                              id_text ^
                              " " ^
                              (print_expr (i + l + 7) value) ^
                              ") " ^
                              (print_expr (i + 5) expr) ^
                              ")"

  | If (test, then_expr, else_expr) ->
                              "(if " ^
                              (print_expr (i + 4) test) ^
                              (indent (i + 4)) ^
                              (print_expr (i + 4) then_expr) ^
                              (indent (i + 4)) ^
                              (print_expr (i + 4) else_expr) ^
                              ")"

  | Op (op, args)          -> let op_text = print_op op in
                              let l = String.length op_text in
                              "(" ^
                              op_text ^
                              " " ^
                              (map_and_concat (print_expr (i + l + 2))
                                              (indent (i + l + 2))
                                              args) ^
                              ")"

  | App (expr, args)       -> let expr_text = print_expr (i + 1) expr in
                              let was_lambda = String.contains expr_text '\n' in
                              let j =
                                if was_lambda then
                                  i + 11
                                else
                                  i + String.length expr_text + 2 in
                              "(" ^
                              expr_text ^
                              (if was_lambda then indent j else " ") ^
                              (map_and_concat (print_expr j)
                                              (indent j)
                                              args) ^
                              ")"

let pretty_print_expr expr = print_expr 0 expr
