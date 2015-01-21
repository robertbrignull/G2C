module F = AST_F
module K = AST_K
open Common



let replace_id source target expr =
  let rec replace_id_id id =
    if id == source then target else id

  and replace_id_expr (expr_guts, expr_info) =
    let expr_guts =
    (match expr_guts with
    | F.Bool b -> F.Bool b

    | F.Num x -> F.Num x

    | F.Id id -> F.Id (replace_id_id id)

    | F.Lambda (args, expr) ->
        F.Lambda (args, replace_id_expr expr)

    | F.Let (id, value, expr) ->
        F.Let (replace_id_id id,
               replace_id_expr value,
               replace_id_expr expr)

    | F.If (test, then_expr, else_expr) ->
        F.If (replace_id_expr test,
              replace_id_expr then_expr,
              replace_id_expr else_expr)

    | F.Prim (prim, args) ->
        F.Prim (prim, List.map replace_id_expr args)

    | F.TypedPrim (prim, type_c, args) ->
        F.TypedPrim (prim, type_c, List.map replace_id_expr args)

    | F.Mem expr ->
        F.Mem (replace_id_expr expr)

    | F.App (proc, args) ->
        F.App (replace_id_expr proc,
               List.map replace_id_expr args)
    ) in
    (expr_guts, expr_info)

  in replace_id_expr expr



let rec transform_type = function
  | F.NumType -> K.NumType
  | F.BoolType -> K.BoolType
  | F.ListType -> K.ListType
  | F.FunctionType (args, res) ->
      let args = List.map transform_type args in
      let res = transform_type res in
      K.FunctionType (List.append args [K.FunctionType [res]])

and get_cont_type = function
  | K.FunctionType args -> (List.hd (List.rev args))
  | _ -> raise (Exceptions.transform_error "Trying to get return type of non-function type")

and get_id_type (id, type_c) = type_c

and transform_args args =
  List.map (fun (id, type_c) -> (id, transform_type type_c)) args

and gen_expr (expr_guts, type_c) cont_gen =
  let type_c = transform_type type_c in
  match expr_guts with
  | F.Bool b ->
      let out_id = (new_id (), type_c) in
      K.Let (out_id, K.Bool b, cont_gen out_id)

  | F.Num x ->
      let out_id = (new_id (), type_c) in
      K.Let (out_id, K.Num x, cont_gen out_id)

  | F.Id id ->
      cont_gen (id, type_c)

  | F.Lambda (args, expr) ->
      let cont_id = (new_id (), get_cont_type type_c) in
      let args = List.append (transform_args args) [cont_id] in
      let expr = gen_expr expr (fun out_id -> K.App (cont_id, [out_id])) in
      let out_id = (new_id (), type_c) in
      K.Let (out_id,
             K.Lambda (args, expr),
             cont_gen out_id)

  | F.Let (let_id, value, expr) ->
      gen_expr value (fun value_id ->
        K.Let ((let_id, get_id_type value_id),
               K.Id value_id,
               gen_expr expr cont_gen))

  | F.If (test_expr, then_expr, else_expr) ->
      let expr_type = transform_type (snd then_expr) in
      let cont_id = (new_id (), K.FunctionType [expr_type]) in
      let cont_arg_id = (new_id (), expr_type) in
      let new_cont_gen out_id = K.App (cont_id, [out_id]) in
      K.Let (cont_id,
             K.Lambda ([cont_arg_id],
                       cont_gen cont_arg_id),
             gen_expr test_expr (fun test_id ->
               K.If (test_id,
                     gen_expr then_expr new_cont_gen,
                     gen_expr else_expr new_cont_gen)))

  | F.Prim (prim, args) ->
      gen_args args (fun arg_ids ->
        let out_id = (new_id (), type_c) in
        K.Let (out_id,
               K.Prim (prim, arg_ids),
               cont_gen out_id))

  | F.TypedPrim (prim, prim_type, args) ->
      let prim_type = transform_type prim_type in
      gen_args args (fun arg_ids ->
        let out_id = (new_id (), type_c) in
        K.Let (out_id,
               K.TypedPrim (prim, prim_type, arg_ids),
               cont_gen out_id))

  | F.Mem expr ->
      let out_id = (new_id (), type_c) in
      gen_expr expr (fun expr_id ->
        K.Let (out_id,
               K.Mem expr_id,
               cont_gen out_id))

  | F.App (expr, args) ->
      gen_expr expr (fun expr_id ->
        gen_args args (fun arg_ids ->
          let cont_type = K.FunctionType [type_c] in
          let cont_id = (new_id (), cont_type) in
          let cont_arg_id = (new_id (), type_c) in
          let cont = K.Lambda ([cont_arg_id], cont_gen cont_arg_id) in
          K.Let (cont_id,
                 cont,
                 K.App (expr_id,
                        List.append arg_ids [cont_id]))))

and gen_args args cont_gen =
  let rec gen_args_impl cont_gen ress = function
    | [] -> cont_gen (List.rev ress)

    | arg :: args ->
        gen_expr arg (fun out_id ->
          gen_args_impl cont_gen (out_id :: ress) args)

  in gen_args_impl cont_gen [] args

and gen_stmt (stmt_guts, type_c) cont =
  match stmt_guts with
  | F.Assume (assume_id, (F.Lambda (args, expr), _)) ->
      let type_c = transform_type type_c in
      let cont_id = (new_id (), get_cont_type type_c) in
      let args = List.append (transform_args args) [cont_id] in
      let expr = gen_expr expr (fun out_id -> K.App (cont_id, [out_id])) in
      K.Let ((assume_id, type_c),
             K.Lambda (args, expr),
             cont)

  | F.Assume (assume_id, (F.Mem (F.Lambda (args, expr), _), _)) ->
      let type_c = transform_type type_c in
      let cont_id = (new_id (), get_cont_type type_c) in
      let lambda_id = new_id () in
      let args = List.append (transform_args args) [cont_id] in
      let expr = replace_id assume_id lambda_id expr in
      let expr = gen_expr expr (fun out_id -> K.App (cont_id, [out_id])) in
      K.Let ((lambda_id, type_c),
             K.Lambda (args, expr),
             K.Let ((assume_id, type_c),
                    K.Mem (lambda_id, type_c),
                    cont))

  | F.Assume (assume_id, value) ->
      gen_expr value (fun value_id ->
        K.Let ((assume_id, get_id_type value_id),
               K.Id value_id,
               cont))

  | F.Observe (prim, args, value) ->
      gen_args args (fun arg_ids ->
        gen_expr value (fun value_id ->
          K.Observe (prim, arg_ids, value_id, cont)))

  | F.Predict (label, expr) ->
      gen_expr expr (fun expr_id ->
        K.Predict (label, expr_id, cont))

and gen_stmts stmts cont =
  match stmts with
  | [] -> cont
  | stmt :: stmts ->
      gen_stmt stmt (gen_stmts stmts cont)

let transform prog =
  gen_stmts prog K.Halt
