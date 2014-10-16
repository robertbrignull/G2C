type id = string

and type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list * type_c
  | VoidType
  | Untyped

and expr_guts =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of ((id * type_c) list) * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Op of id * expr list
  | App of expr * expr list

and expr_info = Lexing.position * type_c

and expr = expr_guts * expr_info
