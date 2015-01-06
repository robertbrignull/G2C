type id = string

and type_c =
  | NumType
  | BoolType
  | FunctionType of type_c list * type_c

and expr_guts =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of (id * type_c) list * expr     (* args, expr *)
  | Let of id * expr * expr                 (* id, value, expr *)
  | If of expr * expr * expr                (* test, then_expr, else_expr *)
  | Prim of string * expr list              (* prim_name, args *)
  | App of expr * expr list                 (* function, args *)

and expr = expr_guts * type_c

and stmt_guts =
  | Assume of id * expr                     (* id, value *)
  | Observe of string * expr list * expr    (* label, args, value *)
  | Predict of string * expr                (* label, value *)

and stmt = stmt_guts * type_c

and prog = stmt list
