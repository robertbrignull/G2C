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
  | Prim of string * args

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | Observe of string * args * id * expr
  | Predict of id * expr
  | Halt

and prog = expr
