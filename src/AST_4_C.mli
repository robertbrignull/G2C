type type_c =
  | NumType
  | BoolType
  | ListType
  | DataType of string
  | BundleType of string
  | FunctionType of type_c list

and id = (string * type_c)

and bundle = id list
and args = id list

and value =
  | Bool of bool
  | Num of float
  | Id of id
  | Prim of string * args
  | TypedPrim of string * type_c * args

and stmt =
  | Seq of stmt list
  | Assign of id * value
  | If of id * stmt * stmt
  | BundleApp of id * args
  | RecursiveApp of id * args
  | AllocateBundle of id * id * id
  | PackBundleItem of id * id * id
  | UnpackBundleItem of id * id
  | DeallocateBundle
  | IncrementDataRefCount of id
  | DecrementDataRefCount of id
  | DeleteList of id
  | Observe of string * args * id
  | Predict of string * id
  | Halt

and bundle_struct = id * args
and data_struct = id * bundle

and proc = id * args * stmt

and prog = bundle_struct list *
           data_struct list *
           proc list *
           stmt
