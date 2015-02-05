type type_c =
  | NumType
  | BoolType
  | ListType
  | FunctionType of type_c list

and id = string * type_c

and bundle = id list
and args = id list

and value =
  | Bool of bool
  | Num of float
  | Id of id
  | ProcInstance of id * bundle
  | RecursiveProcInstance of id
  | Prim of string * args
  | TypedPrim of string * type_c * args
  | Mem of id * id

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | Observe of string * args * id * expr
  | UnvaluedObserve of string * args * expr
  | Predict of string * id * expr
  | Halt

and proc =
  | Proc of id * bundle * args * expr
  | MemProc of id * args

and prog = proc list * expr
