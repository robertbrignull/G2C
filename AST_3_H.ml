type type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list

and id = (string * type_c)

and bundle = id list
and args = id list

and value =
  | Bool of bool
  | Num of float
  | Id of id
  | ProcInstance of id * bundle
  | Op of string * args

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | Halt of id

and proc = id * bundle * args * expr

and prog = proc list * expr



let size (procs, expr) =
  let rec proc_size (id, closure, args, expr) =
    1 + expr_size expr
  
  and expr_size = function
    | Let (id, value, expr) ->
        1 + value_size value + expr_size expr
    | If (test, then_expr, else_expr) ->
        1 + expr_size then_expr + expr_size else_expr
    | App (expr, args) -> 1
    | Halt value -> 1

  and value_size value = 1 

  in List.fold_right (+) (List.map proc_size procs) 0 +
     expr_size expr
