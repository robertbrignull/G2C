type id = string

and type_c =
  | NumType
  | BoolType
  | ListType
  | FunctionType of type_c list * type_c

and expr_guts =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of (id * type_c) list * type_c * expr    (* args, ret_type, expr *)
  | Let of id * expr * expr                         (* id, value, expr *)
  | If of expr * expr * expr                        (* test, then_expr, else_expr *)
  | Prim of id * expr list                          (* prim_name, args *)
  | TypedPrim of string * type_c * expr list        (* prim_name, type, args *)
  | Mem of expr                                     (* expr *)
  | App of expr * expr list                         (* function, args *)

and expr = expr_guts * Lexing.position

and stmt_guts =
  | Assume of id * expr                             (* id, value *)
  | Observe of expr * expr                          (* expr, value *)
  | Predict of expr                                 (* value *)

and stmt = stmt_guts * Lexing.position

and prog = stmt list
