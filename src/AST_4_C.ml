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
  | Assign of id * value              (* id, value *)
  | If of id * stmt * stmt            (* test, then_expr, else_expr *)
  | BundleApp of id * args            (* bundle_id, args *)
  | RecursiveApp of id * args         (* function_id, args *)
  | AllocateBundle of id * id * id    (* bundle_id, proc_id, data_id *)
  | PackBundleItem of id * id * id    (* bundle_id, data_id, arg_id *)
  | PackMemBundle of id * id * id     (* bundle_id, data_id, proc_id *)
  | UnpackBundleItem of id * id       (* data_id, arg_id *)
  | DeallocateBundle
  | IncrementDataRefCount of id       (* id *)
  | DecrementDataRefCount of id       (* id *)
  | DeleteList of id                  (* id *)
  | Observe of string * args * id     (* label, args, value *)
  | Predict of string * id            (* label, value *)
  | Halt

and bundle_struct = id * args
and data_struct = id * bundle

and proc =
  | Proc of id * args * stmt          (* proc_id, args, body *)
  | MemProc of id * id * args         (* id, data_id, args *)

and prog = bundle_struct list *
           data_struct list *
           proc list *
           stmt
