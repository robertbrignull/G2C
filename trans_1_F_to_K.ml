module F = AST_1_F;;
module K = AST_2_K;;

let last_id = ref 0
let new_id () =
  last_id := !last_id + 1;
  "genid_" ^ (string_of_int !last_id)

let get_expr_type = snd

let rec transform_type = function
  | F.NumType -> K.NumType

  | F.BoolType -> K.BoolType

  | F.FunctionType (args, res) ->
      let args = List.map transform_type args in
      let res = transform_type res in
      K.FunctionType (List.append args [K.FunctionType [res]])

let rec last = function
  | [] -> raise (Failure "Cannot get last element of empty list")
  | [x] -> x
  | x::xs -> last xs

let get_arg_types = function
  | K.FunctionType args -> args
  | _ -> raise (Exceptions.transform_error "Not a FunctionType")

let get_cont_type = function
  | K.FunctionType args -> List.hd (get_arg_types (last args))
  | _ -> raise (Exceptions.transform_error "Trying to get return type of non-function type")

let transform_args args =
  List.map (fun (x, y) -> (x, transform_type y)) args

let could_be_value = function
  | F.Bool _ -> true
  | F.Num _ -> true
  | F.Id _ -> true
  | F.Lambda _ -> true
  | _ -> false



let rec calc_expr (expr_guts, type_c) cont_gen =
  let let_id = new_id () in
  let let_type = transform_type type_c in
  match expr_guts with
  | F.Bool b ->
      K.Let (let_id,
             K.Value (K.Bool b, let_type),
             cont_gen (let_id, let_type))

  | F.Num x ->
      K.Let (let_id,
             K.Value (K.Num x, let_type),
             cont_gen (let_id, let_type))

  | F.Id id ->
      K.Let (let_id,
             K.Value (K.Id id, let_type),
             cont_gen (let_id, let_type))

  | F.Lambda (args, expr) ->
      let cont_id = new_id () in
      let arg_id = new_id () in
      let cont_type = get_cont_type let_type in
      let cont = (K.Lambda ([(arg_id, cont_type)],
                            K.App ((K.Id cont_id, K.FunctionType [cont_type]),
                                   [(K.Id arg_id, cont_type)])),
                  cont_type) in
      let args = List.append (transform_args args)
                             [(cont_id, K.FunctionType [cont_type])] in
      K.Let (let_id,
             K.Value (K.Lambda (args,
                                gen_expr expr cont),
                      let_type),
             cont_gen (let_id, let_type))

  | F.Let (id, value, expr) ->
      calc_expr value (fun (value_id, value_type) ->
        K.Let (id,
              K.Value (K.Id value_id, value_type),
              calc_expr expr cont_gen))

  | F.If (test, then_expr, else_expr) ->
      let cont_id = new_id () in
      let res_type = transform_type (get_expr_type then_expr) in
      let cont = (K.Lambda ([(let_id, res_type)],
                            cont_gen (let_id, res_type)),
                  K.FunctionType [res_type]) in
      let cont_gen_2 = (fun (res_id, res_type) ->
        K.App ((K.Id cont_id, K.FunctionType [res_type]),
               [(K.Id res_id, res_type)])) in
      K.Let (cont_id,
             K.Value cont,
             calc_expr test (fun (if_id, if_type) ->
               K.If ((K.Id if_id, if_type),
                     calc_expr then_expr cont_gen_2,
                     calc_expr else_expr cont_gen_2)))

  | F.Op (op, args) ->
      calc_exprs args (fun ids ->
        let op_args = List.map (fun (id, type_c) -> (K.Id id, type_c)) ids in
        K.Let (let_id,
               K.Op (op, op_args),
               cont_gen (let_id, let_type)))

  | F.App (expr, args) ->
      calc_expr expr (fun (expr_id, expr_type) ->
        calc_exprs args (fun ids ->
          let app_args = List.map (fun (id, type_c) -> (K.Id id, type_c)) ids in
          let cont = (K.Lambda ([(let_id, let_type)],
                                cont_gen (let_id, let_type)),
                      K.FunctionType [let_type]) in
          K.App ((K.Id expr_id, expr_type),
                 cont :: app_args)))



and calc_exprs exprs cont_gen =
  let rec calc_exprs_impl exprs cont_gen ress =
    (match exprs with
    | [] -> cont_gen ress

    | expr :: exprs ->
        calc_expr expr (fun res ->
          calc_exprs_impl exprs cont_gen (List.append ress [res]))
    )

  in calc_exprs_impl exprs cont_gen []



and gen_expr (expr_guts, type_c) cont =
  let type_c = transform_type type_c in
  match expr_guts with
  | F.Bool b ->
      K.App (cont, [(K.Bool b, type_c)])

  | F.Num x ->
      K.App (cont, [(K.Num x, type_c)])

  | F.Id id ->
      K.App (cont, [(K.Id id, type_c)])

  | F.Lambda (args, expr) ->
      raise (Exceptions.transform_error "Invalid top level expression: lambda")

  | F.Let (id, value, expr) ->
      let expr = gen_expr expr cont in
      calc_expr value (fun (value_id, value_type) ->
        K.Let (id,
               K.Value (K.Id value_id, value_type),
               expr))

  | F.If (test, then_expr, else_expr) ->
      let then_expr = gen_expr then_expr cont in
      let else_expr = gen_expr else_expr cont in
      calc_expr test (fun (test_id, test_type) ->
        K.If ((K.Id test_id, test_type),
              then_expr,
              else_expr))

  | F.Op (op, args) ->
      calc_exprs args (fun ids ->
        let op_args = List.map (fun (id, type_c) -> (K.Id id, type_c)) ids in
        let res_id = new_id () in
        K.Let (res_id,
               K.Op (op, op_args),
               K.App (cont, [(K.Id res_id, type_c)])))

  | F.App (expr, args) ->
      calc_expr expr (fun (expr_id, expr_type) ->
        calc_exprs args (fun ids ->
          let app_args = List.map (fun (id, type_c) -> (K.Id id, type_c)) ids in
          K.App ((K.Id expr_id, expr_type),
                 List.append app_args [cont])))



let gen_prog (expr_guts, type_c) =
  let arg_id = new_id () in
  let type_c_t = transform_type type_c in
  let cont = (K.Lambda ([(arg_id, type_c_t)],
                        K.Halt (K.Id arg_id, type_c_t)),
              K.FunctionType [type_c_t]) in
  gen_expr (expr_guts, type_c) cont



let transform expr = gen_prog expr
