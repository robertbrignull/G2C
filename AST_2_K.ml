type type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list

and id = (string * type_c)

and args = id list

and value =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of id list * expr
  | Op of string * args

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | Halt of id



let size expr =
  let rec expr_size = function
    | Let (id, value, expr) ->
        1 + value_size value + expr_size expr
    | If (test, then_expr, else_expr) ->
        1 + expr_size then_expr + expr_size else_expr
    | App (expr, args) -> 1
    | Halt value -> 1

  and value_size = function
    | Bool b -> 1
    | Num x -> 1
    | Id id -> 1
    | Lambda (args, expr) -> 1 + expr_size expr
    | Op (op, args) -> 1

  in expr_size expr
