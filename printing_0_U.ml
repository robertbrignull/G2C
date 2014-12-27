open AST_0_U
open Common

let rec print_type = function
  | NumType                   -> "num"
  | BoolType                  -> "bool"
  | FunctionType (args, res)  -> (print_type_list args) ^
                                 " -> " ^
                                 (print_type res)

and print_type_list ts = "(" ^ (map_and_concat print_type ", " ts) ^ ")"

let print_bool b = if b then "true" else "false"

let print_num x = string_of_float x

let print_id id = id

let print_op op = op

let get_op_symbol = function
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
  | "and" -> "and"
  | "or" -> "or"
  | "not" -> "not"
  | op -> raise (Exceptions.transform_error "op not recognised")
  
let print_prim prim = prim

let print_def (id, type_c) =
  (print_id id) ^ " : " ^ (print_type type_c)

let rec print_inline_expr (expr_guts, expr_info) =
  match expr_guts with
  | Bool b -> if b then "true" else "false"
  | Num x -> string_of_float x
  | Id id -> id

  | Lambda (args, expr) -> 
      "(lambda (" ^
      (map_and_concat print_def ", " args) ^
      ") " ^ (print_inline_expr expr) ^ ")"

  | Let (id, value, expr) ->
      "(let " ^ (print_id id) ^ " " ^
      (print_inline_expr value) ^ " " ^
      (print_inline_expr expr) ^ ")"

  | If (test, then_expr, else_expr) ->
      "(if " ^
      (print_inline_expr test) ^ " " ^
      (print_inline_expr then_expr) ^ " " ^
      (print_inline_expr else_expr) ^ ")"

  | Op (op, args) ->
      "(" ^ op ^ " " ^
      (map_and_concat print_inline_expr " " args) ^ ")"

  | Prim (prim, args) ->
      "(" ^ print_prim prim ^ " " ^
      (map_and_concat print_inline_expr " " args) ^ ")"

  | App (expr, args) ->
      "(" ^ (map_and_concat print_inline_expr " " (expr :: args)) ^ ")"
