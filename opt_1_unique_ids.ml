open AST_1_F
open Common

let rec make_ids_unique_expr env (expr_guts, type_c) =
  match expr_guts with
  | Bool b -> ((Bool b, type_c), env)

  | Num x -> ((Num x, type_c), env)

  | Id id ->
      (try
        ((Id (List.assoc id env), type_c), env)
      with Not_found ->
        raise (Exceptions.transform_error (Printf.sprintf "Id %s not defined" id)))

  | Lambda (args, expr) ->
      let ids = List.map fst args in
      let types = List.map snd args in
      let new_ids = List.map (fun _ -> new_id ()) ids in
      let env = List.append (List.combine ids new_ids) env in
      ((Lambda (List.combine new_ids types,
                fst (make_ids_unique_expr env expr)),
       type_c), env)

  | Let (id, value, expr) ->
      let id_2 = new_id () in
      let env = (id, id_2) :: env in
      ((Let (id_2,
             fst (make_ids_unique_expr env value),
             fst (make_ids_unique_expr env expr)),
       type_c), env)

  | If (test, then_expr, else_expr) ->
      ((If (fst (make_ids_unique_expr env test),
            fst (make_ids_unique_expr env then_expr),
            fst (make_ids_unique_expr env else_expr)),
       type_c), env)

  | Op (op, args) ->
      ((Op (op,
            List.map fst (List.map (make_ids_unique_expr env) args)),
       type_c), env)

  | Prim (prim, args) ->
      ((Prim (prim,
              List.map fst (List.map (make_ids_unique_expr env) args)),
       type_c), env)

  | App (expr, args) ->
      ((App (fst (make_ids_unique_expr env expr),
             List.map fst (List.map (make_ids_unique_expr env) args)),
       type_c), env)

and make_ids_unique_stmt env (stmt_guts, type_c) =
  match stmt_guts with
  | Assume (id, expr) ->
      let id_2 = new_id () in
      let env = (id, id_2) :: env in
      ((Assume (id_2, fst (make_ids_unique_expr env expr)),
       type_c), env)

  | Observe (prim, args, value) ->
      ((Observe (prim,
                 List.map fst (List.map (make_ids_unique_expr env) args),
                 fst (make_ids_unique_expr env value)),
       type_c), env)

  | Predict (label, expr) ->
      ((Predict (label, fst (make_ids_unique_expr env expr)),
       type_c), env)


and make_ids_unique_stmts env = function
  | [] -> ([], env)
  | stmt :: stmts ->
      let (stmt, env) = make_ids_unique_stmt env stmt in
      let (stmts, env) = make_ids_unique_stmts env stmts in
      (stmt :: stmts, env)

let make_ids_unique prog =
  fst (make_ids_unique_stmts [] prog)