(* This is the third AST and here we have transformed into
   continuation passing style. So functions no longer return
   anything but instead call a continuation.
   All lambdas will have had an extra argument added for the
   continuation.
   Also at this point we make the distinction between expressions
   and values, and we perform only one prim at a time.
   Note as well that assume no longer exists, it is now just a let.
   Also from here on types are only associated with ids, not every
   value or expr*)

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

and expr =
  | Let of id * value * expr              (* id, value, expr *)
  | If of id * expr * expr                (* test, then_expr, else_expr *)
  | App of id * args                      (* function_id, args *)
  | Observe of string * args * id * expr  (* label, args, value, next_expr *)
  | Predict of string * id * expr         (* label, value, next_expr *)
  | Halt

and prog = expr
