val print_type : AST_1_F.type_c -> string
val print_type_list : AST_1_F.type_c list -> string

val print_def : (AST_1_F.id * AST_1_F.type_c) -> string

val pretty_print_expr : AST_1_F.expr -> string
