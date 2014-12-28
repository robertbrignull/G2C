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
  | Op of id * expr list                    (* op_name, args *)
  | Prim of id * expr list                  (* prim_name, args *)
  | App of expr * expr list                 (* function, args *)

and expr = expr_guts * Lexing.position

and stmt_guts =
  | Assume of id * expr                     (* id, value *)
  | Observe of expr * expr                  (* expr, value *)
  | Predict of expr                         (* value *)

and stmt = stmt_guts * Lexing.position

and prog = stmt list
