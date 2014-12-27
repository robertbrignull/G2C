module K = AST_2_K
module H = AST_3_H
open Common

let rec transform_type = function
  | K.NumType -> H.NumType

  | K.BoolType -> H.BoolType

  | K.FunctionType args ->
      H.FunctionType (List.map transform_type args)

and find_free_vars env expr =
  let rec find_free_vars_id env id =
    (try let _ = List.find (fun x -> x = id) env in []
      with Not_found -> [transform_id id])

  and find_free_vars_value env = function
    | K.Bool b -> []

    | K.Num x -> []

    | K.Id id -> find_free_vars_id env id

    | K.Lambda (args, expr) ->
        find_free_vars_expr (List.append env args) expr

    | K.Op (op, args) ->
        List.concat (List.map (find_free_vars_id env) args)

    | K.Prim (prim, args) ->
        List.concat (List.map (find_free_vars_id env) args)

  and find_free_vars_expr env = function
    | K.Let (id, value, expr) ->
        let env = id :: env in
        List.concat [
          find_free_vars_value env value;
          find_free_vars_expr env expr
        ]

    | K.If (test, then_expr, else_expr) ->
        List.concat [
          find_free_vars_id env test;
          find_free_vars_expr env then_expr;
          find_free_vars_expr env else_expr
        ]

    | K.App (expr, args) ->
        List.concat [
          find_free_vars_id env expr;
          List.concat (List.map (find_free_vars_id env) args)
        ]

    | K.Observe (prim, args, value, next) ->
        List.concat [
          List.concat (List.map (find_free_vars_id env) args);
          find_free_vars_id env value;
          find_free_vars_expr env next
        ]

    | K.Predict (label, id, next) ->
        List.concat [
          find_free_vars_id env id;
          find_free_vars_expr env next
        ]

    | K.Halt -> []

  in remove_dups (find_free_vars_expr env expr)

and transform_id (id, type_c) =
  (id, transform_type type_c)

and transform_value = function
  | K.Bool b -> ([], H.Bool b)

  | K.Num x -> ([], H.Num x)

  | K.Id id -> ([], H.Id (transform_id id))

  | K.Lambda (args, expr) ->
      let free_vars = find_free_vars args expr in
      let args = List.map transform_id args in
      let (procs_1, expr) = transform_expr expr in
      let proc_type = H.FunctionType (List.map snd args) in
      let proc_id = (new_id (), proc_type) in
      let new_proc = (proc_id, free_vars, args, expr) in
      let proc_instance = H.ProcInstance (proc_id, free_vars) in
      (new_proc :: procs_1, proc_instance)

  | K.Op (op, args) ->
      ([], H.Op (op, List.map transform_id args))

  | K.Prim (prim, args) ->
      ([], H.Prim (prim, List.map transform_id args))

and transform_expr = function
  | K.Let (id, value, expr) ->
      let id = transform_id id in
      let (procs_1, value) = transform_value value in
      let (procs_2, expr) = transform_expr expr in
      (List.append procs_1 procs_2,
       H.Let (id, value, expr))

  | K.If (test_id, then_expr, else_expr) ->
      let test_id = transform_id test_id in
      let (procs_1, then_expr) = transform_expr then_expr in
      let (procs_2, else_expr) = transform_expr else_expr in
      (List.append procs_1 procs_2,
       H.If (test_id, then_expr, else_expr))

  | K.App (expr, args) ->
      let expr = transform_id expr in
      let args = List.map transform_id args in
      ([], H.App (expr, args))

  | K.Observe (prim, args, value, next) ->
      let args = List.map transform_id args in
      let value = transform_id value in
      let (procs, next) = transform_expr next in
      (procs, H.Observe (prim, args, value, next))

  | K.Predict (label, id, next) ->
      let id = transform_id id in
      let (procs, next) = transform_expr next in
      (procs, H.Predict (label, id, next))

  | K.Halt -> ([], H.Halt)

let transform expr = transform_expr expr
