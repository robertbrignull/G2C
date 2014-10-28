open AST_1_F
open Common

let make_ids_unique expr =
  let rec make_ids_unique_impl env (expr_guts, type_c) =
    let expr_guts_2 = (match expr_guts with
    | Bool b -> Bool b

    | Num x -> Num x

    | Id id ->
        (try
          Id (List.assoc id env)
        with Not_found ->
          raise (Exceptions.transform_error (Printf.sprintf "Id %s not defined" id)))

    | Lambda (args, expr) ->
        let ids = List.map fst args in
        let types = List.map snd args in
        let new_ids = List.map (fun _ -> new_id ()) ids in
        let env_2 = List.append (List.combine ids new_ids) env in
        Lambda (List.combine new_ids types,
                make_ids_unique_impl env_2 expr)

    | Let (id, value, expr) ->
        let id_2 = new_id () in
        let env_2 = (id, id_2) :: env in
        Let (id_2,
             make_ids_unique_impl env_2 value,
             make_ids_unique_impl env_2 expr)

    | If (test, then_expr, else_expr) ->
        If (make_ids_unique_impl env test,
            make_ids_unique_impl env then_expr,
            make_ids_unique_impl env else_expr)

    | Op (op, args) ->
        Op (op,
            List.map (make_ids_unique_impl env) args)

    | App (expr, args) ->
        App (make_ids_unique_impl env expr,
             List.map (make_ids_unique_impl env) args)
    )
    in (expr_guts_2, type_c)

  in make_ids_unique_impl [] expr
