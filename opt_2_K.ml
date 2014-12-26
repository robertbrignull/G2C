open AST_2_K

let replace_id source target expr =
  let rec expr_replace_id = function
    | Let (id, value, expr) ->
        Let (id,
             value_replace_id value,
             expr_replace_id expr)

    | If (test, then_expr, else_expr) ->
        If (id_replace_id test,
            expr_replace_id then_expr,
            expr_replace_id else_expr)

    | App (expr, args) ->
        App (id_replace_id expr,
             List.map id_replace_id args)

    | Observe (prim, args, value, next) ->
        Observe (prim,
                 List.map id_replace_id args,
                 id_replace_id value,
                 expr_replace_id next)

    | Predict (id, next) ->
        Predict (id_replace_id id,
                 expr_replace_id next)

    | Halt -> Halt

  and value_replace_id = function
    | Bool b -> Bool b

    | Num x -> Num x

    | Id id -> Id (id_replace_id id)

    | Lambda (args, expr) ->
        Lambda (args, expr_replace_id expr)

    | Op (op, args) ->
        Op (op, List.map id_replace_id args)

    | Prim (prim, args) ->
        Prim (prim, List.map id_replace_id args)

  and id_replace_id id =
    if id = source then target
    else id

  in expr_replace_id expr

let count_id target expr =
  let rec expr_count_id = function
    | Let (id, value, expr) ->
        if id = target then 0
        else
          value_count_id value +
          expr_count_id expr

    | If (test, then_expr, else_expr) ->
        id_count_id test +
        expr_count_id then_expr +
        expr_count_id else_expr

    | App (expr, args) ->
        List.fold_left (+) 0 (List.map id_count_id (expr :: args))

    | Observe (prim, args, value, next) ->
        List.fold_left (+) 0 (List.map id_count_id (value :: args)) +
        expr_count_id next

    | Predict (id, next) ->
        id_count_id id +
        expr_count_id next

    | Halt -> 0

  and value_count_id = function
    | Bool b -> 0

    | Num x -> 0

    | Id id -> id_count_id id

    | Lambda (args, expr) ->
        expr_count_id expr

    | Op (op, args) ->
        List.fold_left (+) 0 (List.map id_count_id args)

    | Prim (prim, args) ->
        List.fold_left (+) 0 (List.map id_count_id args)

  and id_count_id id =
    if id = target then 1 else 0

  in expr_count_id expr

let rec optimise expr_in =
  let expr_top = ref expr_in in
  let changed = ref true in

  let rec expr_optimise = function
    (* If an id is assigned to another id, remove the second id
       and replace by the first id throughout the code *)
    | Let (id_1, Id id_2, expr) -> begin
        changed := true;
        replace_id id_1 id_2 expr
      end

    (* Remove lets where the value is never used *)
    | Let (id, value, expr) when count_id id expr = 0 -> begin
        changed := true;
        expr
      end

    (* End of optimisations, just recurse *)
    | Let (id, value, expr) ->
        Let (id, value_optimise value, expr_optimise expr)

    | If (test, then_expr, else_expr) ->
        If (test, expr_optimise then_expr, expr_optimise else_expr)

    | App (expr, args) ->
        App (expr, args)

    | Observe (prim, args, value, next) ->
        Observe (prim, args, value, expr_optimise next)

    | Predict (id, next) ->
        Predict (id, expr_optimise next)

    | Halt -> Halt

  and value_optimise = function
    (* Remove trivial continuations that are equivalent
       to some other named function *)
    | Lambda ([l_arg], App (f, [f_arg]))
      when l_arg = f_arg ->
        Id f

    (* End of optimisations, just recurse *)
    | Bool b ->
        Bool b

    | Num x ->
        Num x

    | Id (id, type_c) ->
        Id (id, type_c)

    | Lambda (args, expr) ->
        Lambda (args, expr_optimise expr)

    | Op (op, args) ->
        Op (op, args)
        
    | Prim (prim, args) ->
        Prim (prim, args)

  (* Keep applying optimisations until we can do no more *)
  in begin
    expr_top := expr_in;
    while !changed do
      changed := false;
      expr_top := expr_optimise !expr_top;
    done;
    !expr_top
  end
