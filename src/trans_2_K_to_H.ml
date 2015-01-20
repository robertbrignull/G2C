module K = AST_2_K
module H = AST_3_H
open Common

let rec transform_type = function
  | K.NumType -> H.NumType
  | K.BoolType -> H.BoolType
  | K.ListType -> H.ListType
  | K.FunctionType args ->
      H.FunctionType (List.map transform_type args)

and get_arg_types = function
  | H.FunctionType args -> args
  | _ -> raise (Exceptions.transform_error "Tried to get argument types of a non-function type")

and find_free_vars env expr =
  let rec find_free_vars_id env id =
    (try let _ = List.find (fun x -> x = id) env in []
      with Not_found -> [transform_id id])

  and find_free_vars_value env = function
    | K.Bool b -> []

    | K.Num x -> []

    | K.Id id -> find_free_vars_id env id

    | K.Lambda (args, expr) ->
        find_free_vars_expr (List.append args env) expr

    | K.Prim (prim, args) ->
        List.concat (List.map (find_free_vars_id env) args)

    | K.TypedPrim (prim, type_c, args) ->
        List.concat (List.map (find_free_vars_id env) args)

    | K.Mem id ->
        find_free_vars_id env id

  and find_free_vars_expr env = function
    | K.Let (id, K.Lambda (args, body), expr) ->
        let env = id :: env in
        List.concat [
          find_free_vars_expr (List.append args env) body;
          find_free_vars_expr env expr
        ]

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

and replace_id source target expr =
  let rec replace_id_id id =
    if fst id == fst source then target else id

  and replace_id_value = function
    | K.Id id -> K.Id (replace_id_id id)
    | K.Lambda (args, expr) -> K.Lambda (args, replace_id_expr expr)
    | K.Prim (prim, args) -> K.Prim (prim, List.map replace_id_id args)
    | K.TypedPrim (prim, type_c, args) -> K.TypedPrim (prim, type_c, List.map replace_id_id args)
    | K.Mem (mem_id) -> K.Mem (replace_id_id mem_id)
    | x -> x

  and replace_id_expr = function
    | K.Let (id, value, expr) ->
        K.Let (id, replace_id_value value, replace_id_expr expr)

    | K.If (test, then_expr, else_expr) ->
        K.If (replace_id_id test,
              replace_id_expr then_expr,
              replace_id_expr else_expr)

    | K.App (proc, args) ->
        K.App (replace_id_id proc,
               List.map replace_id_id args)

    | K.Observe (label, args, value, next) ->
        K.Observe (label,
                   List.map replace_id_id args,
                   replace_id_id value,
                   replace_id_expr next)

    | K.Predict (label, value, next) ->
        K.Predict (label,
                   replace_id_id value,
                   replace_id_expr next)

    | K.Halt -> K.Halt

  in replace_id_expr expr

and transform_id (id, type_c) =
  (id, transform_type type_c)

and transform_value = function
  | K.Bool b -> ([], H.Bool b)

  | K.Num x -> ([], H.Num x)

  | K.Id id -> ([], H.Id (transform_id id))

  | K.Lambda (args, body) ->
      let free_vars = find_free_vars args body in
      let args = List.map transform_id args in
      let (procs_1, body) = transform_expr body in
      let proc_type = H.FunctionType (List.map snd args) in
      let proc_id = (new_id (), proc_type) in
      let new_proc = H.Proc (proc_id, free_vars, args, body) in
      let proc_instance = H.ProcInstance (proc_id, free_vars) in
      (new_proc :: procs_1, proc_instance)

  | K.Prim (prim, args) ->
      ([], H.Prim (prim, List.map transform_id args))

  | K.TypedPrim (prim, type_c, args) ->
      let type_c = transform_type type_c in
      ([], H.TypedPrim (prim, type_c, List.map transform_id args))

  | K.Mem proc_id ->
      let proc_id = transform_id proc_id in
      let mem_id = (new_id (), snd proc_id) in
      let args = List.map (fun type_c -> (new_id (), type_c)) (get_arg_types (snd proc_id)) in
      let mem_proc = H.MemProc (mem_id, args) in
      ([mem_proc], H.Mem (mem_id, proc_id))

and transform_expr = function
  | K.Let (let_id, K.Lambda (args, body), expr) ->
      let free_vars = find_free_vars (let_id :: args) body in
      let let_id = transform_id let_id in
      let proc_id = (new_id (), K.FunctionType (List.map snd args)) in
      let recursive_proc_instance_id = (new_id (), snd proc_id) in
      let body = replace_id let_id recursive_proc_instance_id body in
      let proc_id = transform_id proc_id in
      let recursive_proc_instance_id = transform_id recursive_proc_instance_id in
      let (procs_1, body) = transform_expr body in
      let args = List.map transform_id args in
      let body = H.Let (recursive_proc_instance_id, H.RecursiveProcInstance proc_id, body) in
      let new_proc = H.Proc (proc_id, free_vars, args, body) in
      let proc_instance = H.ProcInstance (proc_id, free_vars) in
      let (procs_2, expr) = transform_expr expr in
      (new_proc :: (List.append procs_1 procs_2),
       H.Let (let_id,
              proc_instance,
              expr))

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
