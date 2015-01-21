open AST_H
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

  | ProcInstance (id, bundle) ->
      "proc instance " ^
      (print_id id) ^
      (indent (i + 2)) ^
      "(" ^
      (map_and_concat print_id
                      (indent (i + 3))
                      bundle) ^
      ")"

  | RecursiveProcInstance id ->
      "recursive proc instance " ^
      (print_id id)

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

  | Mem (mem_id, proc_id) ->
      "mem" ^
      (indent (i + 2)) ^
      (print_id mem_id) ^
      (indent (i + 2)) ^
      (print_id proc_id)

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

  | Observe (prim, args, value, next) ->
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

  | Predict (label, id, next) ->
      "predict " ^
      (indent (i + 2)) ^
      label ^
      (indent (i + 2)) ^
      (print_id id) ^
      (indent i) ^
      (print_expr i next)

  | Halt -> "halt"

and print_proc i = function
  | Proc (id, closure, args, expr) ->
      "proc " ^
      (print_id id) ^
      (indent (i + 2)) ^
      "(" ^
      (map_and_concat print_id
                      (indent (i + 3))
                      closure) ^
      ")" ^
      (indent (i + 2)) ^
      "(" ^
      (map_and_concat print_id
                      (indent (i + 3))
                      args) ^
      ")" ^
      (indent (i + 2)) ^
      (print_expr (i + 2) expr)

  | MemProc (mem_id, args) ->
      "mem proc " ^
      (print_id mem_id) ^
      (indent (i + 2)) ^
      "(" ^
      (map_and_concat print_id
                      (indent (i + 3))
                      args) ^
      ")"



let pretty_print_prog (procs, expr) =
  (map_and_concat (print_proc 0)
                  ((indent 0) ^ (indent 0))
                  procs) ^
  (indent 0) ^ (indent 0) ^
  print_expr 0 expr