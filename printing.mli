val map_and_concat : ('a -> string) -> string -> 'a list -> string

val print_type : AST.type_c -> string
val print_type_list : AST.type_c list -> string

val print_def : (AST.id * AST.type_c) -> string

val pretty_print_expr : AST.expr -> string
