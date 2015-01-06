module F = AST_1_F
module K = AST_2_K
open Common

let rec transform_type = function
  | F.NumType -> K.NumType

  | F.BoolType -> K.BoolType

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
      gen_expr test_expr (fun test_id ->
        K.If (test_id,
              gen_expr then_expr cont_gen,
              gen_expr else_expr cont_gen))

  | F.Prim (prim, args) ->
      gen_args args (fun arg_ids ->
        let out_id = (new_id (), type_c) in
        K.Let (out_id,
               K.Prim (prim, arg_ids),
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
  | F.Assume (assume_id, (F.Lambda (args, expr), lambda_type)) ->
      let type_c = transform_type type_c in
      let lambda_type = transform_type lambda_type in
      let cont_id = (new_id (), get_cont_type lambda_type) in
      let args = List.append (transform_args args) [cont_id] in
      let expr = gen_expr expr (fun out_id -> K.App (cont_id, [out_id])) in
      K.Let ((assume_id, type_c),
             K.Lambda (args, expr),
             cont)

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
