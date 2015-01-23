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
  | Mem of id * id                        (* mem_proc_id, proc_id *)     

and expr =
  | Let of id * value * expr              (* id, value, expr *)
  | If of id * expr * expr                (* test, then_expr, else_expr *)
  | App of id * args                      (* function_id, args *)
  | Observe of string * args * id * expr  (* label, args, value, next_expr *)
  | Predict of string * id * expr         (* label, value, next_expr *)
  | Halt

and proc =
  | Proc of id * bundle * args * expr     (* id, bundle, args, expr *)
  | MemProc of id * args                  (* id, args *)

and prog = proc list * expr
