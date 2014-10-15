open Lexing
open Parsing

open AST

let map_and_concat f sep xs = String.concat sep (List.map f xs)

let indent_level = 2
let indent n = "\n" ^ String.make n ' '

let print_pos pos =
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let rec print_type = function
  | NumType                -> "num"
  | BoolType               -> "bool"
  | CompoundType (l, r)    -> "(" ^
                              (print_type l) ^
                              " -> " ^
                              (print_type r) ^
                              ")"
  | NoType                 -> "notype"

let print_info (pos, type_c) =
  "<" ^
  (print_pos pos) ^
  ", " ^
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

let pretty_print_expr expr = print_expr 0 expr
