type id = string

and type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list
  | ContType of type_c

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



val size : expr -> int
