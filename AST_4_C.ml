type type_c =
  | NumType
  | BoolType
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
  | Op of string * args

and stmt =
  | Seq of stmt list
  | Assign of id * value
  | If of id * stmt * stmt
  | BundleApp of id * args
  | PackBundle of id * id * id * bundle
  | UnpackBundle of id * bundle
  | Halt of id

and bundle_struct = id * args
and data_struct = id * bundle

and proc = id * args * stmt

and prog = bundle_struct list *
           data_struct list *
           proc list *
           stmt
