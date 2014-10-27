module K = AST_2_K;;
module C = AST_3_C;;

let rec remove_dups = function
  | [] -> []
  | x :: xs ->
      x :: (remove_dups (List.filter (fun y -> x <> y) xs))

let rec transform_type = function
  | K.NumType -> C.NumType

  | K.BoolType -> C.BoolType

  | K.FunctionType args ->
      C.FunctionType ([], List.map transform_type args)

let find_free_vars env expr =
  let rec find_free_vars_value env (value_guts, type_c) =
    let type_c = transform_type type_c in
    match value_guts with
    | K.Bool b -> []

    | K.Num x -> []

    | K.Id id ->
        (try let _ = List.find (fun x -> x = id) env in []
        with Not_found -> [(id, type_c)])

    | K.Lambda (args, expr) ->
        find_free_vars_expr (List.append env (List.map fst args)) expr

  and find_free_vars_decl env = function
    | K.Value value ->
        find_free_vars_value env value

    | K.Op (op, args) ->
        List.concat (List.map (find_free_vars_value env) args)

  and find_free_vars_expr env = function
    | K.Let (id, decl, expr) ->
        let env = id :: env in
        List.concat [
          find_free_vars_decl env decl;
          find_free_vars_expr env expr
        ]

    | K.If (test, then_expr, else_expr) ->
        List.concat [
          find_free_vars_value env test;
          find_free_vars_expr env then_expr;
          find_free_vars_expr env else_expr
        ]

    | K.App (expr, args) ->
        List.concat [
          find_free_vars_value env expr;
          List.concat (List.map (find_free_vars_value env) args)
        ]

    | K.Halt value ->
        find_free_vars_value env value

  in remove_dups (find_free_vars_expr env expr)

let rec transform_def (id, type_c) =
  (id, transform_type type_c)

and transform_value (value_guts, type_c) =
  let type_c = transform_type type_c in
  match value_guts with
  | K.Bool b -> (C.Bool b, type_c)

  | K.Num x -> (C.Num x, type_c)

  | K.Id id -> (C.Id id, type_c)

  | K.Lambda (args, expr) ->
      (C.Lambda (find_free_vars (List.map fst args) expr,
                 List.map transform_def args,
                 transform_expr expr),
       type_c)

and transform_decl = function
  | K.Value value ->
      C.Value (transform_value value)

  | K.Op (op, args) ->
      C.Op (op, List.map transform_value args)

and transform_expr = function
  | K.Let (id, decl, expr) ->
      C.Let (id,
             transform_decl decl,
             transform_expr expr)

  | K.If (test, then_expr, else_expr) ->
      C.If (transform_value test,
            transform_expr then_expr,
            transform_expr else_expr)

  | K.App (expr, args) ->
      C.App (transform_value expr,
             List.map transform_value args)

  | K.Halt value ->
      C.Halt (transform_value value)

let transform expr = transform_expr expr
