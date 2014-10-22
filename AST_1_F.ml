type id = string

and type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list * type_c

and expr_guts =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of (id * type_c) list * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Op of id * expr list
  | App of expr * expr list

and expr = expr_guts * type_c



let rec size (expr_guts, type_c) =
  match expr_guts with
  | Bool b -> 1
  | Num x -> 1
  | Id id -> 1
  | Lambda (args, expr) -> 1 + size expr
  | Let (id, value, expr) -> 1 + size value + size expr
  | If (test, then_expr, else_expr) -> 1 + size test + size then_expr + size else_expr
  | Op (op, args) -> 1 + List.fold_right (fun arg total -> size arg + total) args 0
  | App (expr, args) -> 1 + size expr + List.fold_right (fun arg total -> size arg + total) args 0
