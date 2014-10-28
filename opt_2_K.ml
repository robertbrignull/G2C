open AST_2_K

let replace_id source target expr =
  let rec expr_replace_id = function
    | Let (id, decl, expr) ->
        Let (id,
             decl_replace_id decl,
             expr_replace_id expr)
    | If (test, then_expr, else_expr) ->
        If (value_replace_id test,
            expr_replace_id then_expr,
            expr_replace_id else_expr)
    | App (expr, args) ->
        App (value_replace_id expr,
             List.map value_replace_id args)
    | Halt value ->
        Halt (value_replace_id value)

  and decl_replace_id = function
    | Value value ->
        Value (value_replace_id value)
    | Op (op, args) ->
        Op (op, List.map value_replace_id args)

  and value_replace_id = function
    | (Bool b, type_c) ->
        (Bool b, type_c)
    | (Num x, type_c) ->
        (Num x, type_c)
    | (Id id, type_c) ->
        if id = source then (target, type_c)
        else (Id id, type_c)
    | (Lambda (args, expr), type_c) ->
        (Lambda (args, expr_replace_id expr), type_c)

  in expr_replace_id expr

let count_id target expr =
  let rec expr_count_id = function
    | Let (id, decl, expr) ->
        if id = target then
          0
        else
          decl_count_id decl +
          expr_count_id expr
    | If (test, then_expr, else_expr) ->
        value_count_id test +
        expr_count_id then_expr +
        expr_count_id else_expr
    | App (expr, args) ->
        value_count_id expr +
        List.fold_left (+) 0 (List.map value_count_id args)
    | Halt value ->
        value_count_id value

  and decl_count_id = function
    | Value value ->
        value_count_id value
    | Op (op, args) ->
        List.fold_left (+) 0 (List.map value_count_id args)

  and value_count_id = function
    | (Bool b, type_c) ->  0
    | (Num x, type_c) -> 0
    | (Id id, type_c) ->
        if id = target then 1
        else 0
    | (Lambda (args, expr), type_c) ->
        expr_count_id expr

  in expr_count_id expr

let rec optimise expr_in =
  let expr_top = ref expr_in in
  let changed = ref true in

  let rec expr_optimise = function
    (* If an id is assigned to a number, remove the id
       and replae by the number throughout the code *)
    | Let (id, Value (Bool b, type_c), expr) -> begin
        changed := true;
        replace_id id (Bool b) expr
      end

    (* If an id is assigned to a boolean, remove the id
       and replae by the boolean throughout the code *)
    | Let (id, Value (Num x, type_c), expr) -> begin
        changed := true;
        replace_id id (Num x) expr
      end

    (* If an id is assigned to another id, remove the first id
       and replae by the second id throughout the code *)
    | Let (id_1, Value (Id id_2, type_c), expr) -> begin
        changed := true;
        replace_id id_1 (Id id_2) expr
      end

    (* Remove inline lambdas, replace them by lets *)
    | App ((Lambda (l_args, l_expr), type_c), f_args) -> begin
        changed := true;
        let args = List.combine l_args f_args in
        List.fold_right (fun ((arg_name, arg_type), arg_value) rest -> Let (arg_name, Value arg_value, rest)) args l_expr
      end

    (* Remove lets where the value is never used *)
    | Let (id, value, expr) when count_id id expr = 0 -> begin
        changed := true;
        expr
      end

    (* End of optimisations, just recurse *)
    | Let (id, decl, expr) ->
        Let (id, decl_optimise decl, expr_optimise expr)
    | If (test, then_expr, else_expr) ->
        If (value_optimise test, expr_optimise then_expr, expr_optimise else_expr)
    | App (expr, args) ->
        App (value_optimise expr, List.map value_optimise args)
    | Halt value ->
        Halt (value_optimise value)

  and decl_optimise = function
    (* End of optimisations, just recurse *)
    | Value value ->
        Value (value_optimise value)
    | Op (op, args) ->
        Op (op, List.map value_optimise args)

  and value_optimise = function
    (* Remove trivial continuations that are equivalent
       to some other named function *)
    | (Lambda ([(l_arg, _)], App (f, [(Id f_arg, _)])), type_c)
      when l_arg = f_arg ->
        f

    (* End of optimisations, just recurse *)
    | (Bool b, type_c) ->
        (Bool b, type_c)
    | (Num x, type_c) ->
        (Num x, type_c)
    | (Id id, type_c) ->
        (Id id, type_c)
    | (Lambda (args, expr), type_c) ->
        (Lambda (args, expr_optimise expr), type_c)

  (* Keep applying optimisations until we can do no more *)
  in begin
    expr_top := expr_in;
    while !changed do
      changed := false;
      expr_top := expr_optimise !expr_top;
    done;
    !expr_top
  end
