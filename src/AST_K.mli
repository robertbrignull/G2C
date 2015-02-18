type type_c =
  | NumType
  | BoolType
  | ListType
  | FunctionType of type_c list

and id = string * type_c

and args = id list

and value =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of args * expr
  | Prim of string * args
  | TypedPrim of string * type_c * args
  | Mem of id

and observable =
  | ValuedObserve of string * args * id
  | UnvaluedObserve of string * args

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | SingleValuedObserve of string * args * id * expr
  | SingleUnvaluedObserve of string * args * expr
  | MultiObserve of observable list * expr
  | Predict of string * id * expr
  | Halt

and prog = expr
