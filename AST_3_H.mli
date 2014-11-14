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



val size : prog -> int
