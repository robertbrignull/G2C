open AST_K
open Common

let rec print_type = function
  | NumType            -> "Num"
  | BoolType           -> "Bool"
  | ListType           -> "List"
  | FunctionType args  -> "lambda (" ^ (map_and_concat print_type ", " args) ^ ")"

let print_bool b = "bool " ^ if b then "true" else "false"

let print_num x = "num " ^ (string_of_float x)

let print_id (id, type_c) =
  "id " ^
  id ^
  " : " ^
  (print_type type_c)

let print_prim prim = "prim " ^ prim

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

  | Prim (prim, args) ->
      print_prim prim ^
      (indent (i + 2)) ^
      (map_and_concat print_id
                      (indent (i + 2))
                      args)

  | TypedPrim (prim, type_c, args) ->
      print_prim prim ^
      (indent (i + 2)) ^
      (print_type type_c) ^
      (indent (i + 2)) ^
      (map_and_concat print_id
                      (indent (i + 2))
                      args)

  | Mem id ->
      "mem" ^
      (print_id id)

and print_observable i = function
  | ValuedObserve (prim, args, value) ->
      prim ^
      "(" ^
      (map_and_concat print_id ", " args) ^
      ") " ^
      (print_id value)

  | UnvaluedObserve (prim, args) ->
      prim ^
      "(" ^
      (map_and_concat print_id ", " args) ^
      ")"

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

  | SingleValuedObserve (prim, args, value, next) ->
      "observe " ^
      (indent (i + 2)) ^
      prim ^
      "(" ^
      (map_and_concat print_id ", " args) ^
      ")" ^
      (indent (i + 2)) ^
      (print_id value) ^
      (indent i) ^
      (print_expr i next)

  | SingleUnvaluedObserve (prim, args, next) ->
      "unvalued observe " ^
      (indent (i + 2)) ^
      prim ^
      "(" ^
      (map_and_concat print_id ", " args) ^
      ")" ^
      (indent i) ^
      (print_expr i next)

  | MultiObserve (observables, next) ->
      "multi observe" ^
      (String.concat
        (indent (i + 2))
        ("" ::
          (List.map
            (print_observable (i + 2))
            observables))) ^
      (indent i) ^
      (print_expr i next)

  | Predict (label, id, next) ->
      "predict " ^
      (indent (i + 2)) ^
      label ^
      (indent (i + 2)) ^
      (print_id id) ^
      (indent i) ^
      (print_expr i next)

  | Halt ->
      "halt"

let pretty_print_expr expr =
  print_expr 0 expr
