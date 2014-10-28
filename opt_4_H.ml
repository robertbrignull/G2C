open AST_4_H

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

  in expr_replace_id expr

let rec optimise prog_in =
  let prog_top = ref prog_in in
  let changed = ref true in

  let rec prog_optimise (procs, expr) =
    (List.map proc_optimise procs,
     expr_optimise expr)

  and proc_optimise (id, closure, args, expr) =
    (id, closure, args, expr_optimise expr)

  and expr_optimise = function
    (* If an id is assigned to another id, remove the first id
       and replae by the second id throughout the code *)
    | Let (id_1, Value (Id id_2, type_c), expr) -> begin
        changed := true;
        replace_id id_1 (Id id_2) expr
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
    (* End of optimisations, just recurse *)
    | (Bool b, type_c) ->
        (Bool b, type_c)
    | (Num x, type_c) ->
        (Num x, type_c)
    | (Id id, type_c) ->
        (Id id, type_c)

  (* Keep applying optimisations until we can do no more *)
  in begin
    prog_top := prog_in;
    while !changed do
      changed := false;
      prog_top := prog_optimise !prog_top;
    done;
    !prog_top
  end
