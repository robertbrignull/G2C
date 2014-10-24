type id = string

and type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list

and value_guts =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of (id * type_c) list * expr

and value = value_guts * type_c

and decl =
  | Value of value
  | Op of id * value list

and expr =
  | Let of id * decl * expr
  | If of value * expr * expr
  | App of value * value list
  | Halt of value



let size expr =
  let rec expr_size = function
    | Let (id, decl, expr) ->
        1 + decl_size decl + expr_size expr
    | If (test, then_expr, else_expr) ->
        1 + value_size test + expr_size then_expr + expr_size else_expr
    | App (expr, args) ->
        1 + value_size expr + values_size args
    | Halt value ->
        1 + value_size value

  and decl_size = function
    | Value value ->
        value_size value
    | Op (op, args) ->
        values_size args

  and value_size = function
    | (Bool b, type_c) -> 1
    | (Num x, type_c) -> 1
    | (Id id, type_c) -> 1
    | (Lambda (args, expr), type_c) -> 1 + expr_size expr

  and values_size values =
    List.fold_right (fun arg total -> value_size arg + total) values 0

  in expr_size expr