type id = string

and type_c =
  | NumType
  | BoolType
  | ListType
  | FunctionType of type_c list * type_c

and expr_guts =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of (id * type_c) list * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Prim of string * expr list
  | App of expr * expr list

and expr = expr_guts * type_c

and stmt_guts =
  | Assume of id * expr
  | Observe of string * expr list * expr
  | Predict of string * expr

and stmt = stmt_guts * type_c

and prog = stmt list
