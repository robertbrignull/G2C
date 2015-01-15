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
  | Lambda of (id * type_c) list * type_c * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Prim of id * expr list
  | App of expr * expr list

and expr = expr_guts * Lexing.position

and stmt_guts =
  | Assume of id * expr
  | Observe of expr * expr
  | Predict of expr

and stmt = stmt_guts * Lexing.position

and prog = stmt list
