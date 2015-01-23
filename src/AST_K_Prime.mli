type type_c =
  | NumType
  | BoolType
  | ListType
  | FunctionType of type_c list

and id = string * type_c * value

and args = id list

and value =
  | Unknown
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of args * expr
  | Prim of string * args
  | TypedPrim of string * type_c * args
  | Mem of id

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | Observe of string * args * id * expr
  | Predict of string * id * expr
  | Halt

and prog = expr



val id_name : id -> string

val transform_K : AST_K.prog -> prog
val rebuild_values : prog -> prog

val transform_K_Prime : prog -> AST_K.prog
