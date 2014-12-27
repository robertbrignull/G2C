module U = AST_0_U
module F = AST_1_F
module PF = Printing_1_F

let get_type = snd

let rec equal_types t1 t2 =
  match t1, t2 with
  | F.BoolType, F.BoolType -> true
  | F.NumType, F.NumType -> true
  | F.FunctionType (args1, res1), F.FunctionType (args2, res2) ->
      List.length args1 == List.length args2 &&
      (List.fold_right2 (fun t1 t2 c -> equal_types t1 t2 && c) args1 args2 true) &&
      (equal_types res1 res2)
  | _ -> false

let is_function_type = function
  | F.FunctionType (l, r) -> true
  | _ -> false

let get_arg_types pos = function
  | F.FunctionType (args, _) -> args
  | t -> raise (Exceptions.typing_error (Printf.sprintf "Cannot determine arg types: %s is not a function type" (PF.print_type t)) pos)

let get_result_type pos = function
  | F.FunctionType (_, res) -> res
  | t -> raise (Exceptions.typing_error (Printf.sprintf "Cannot determine return type: %s is not a function type" (PF.print_type t)) pos)

let rec check_app_types expected ts =
  match expected with
  | F.FunctionType (args, _) ->
      List.length args == List.length ts &&
      List.fold_right2 (fun t1 t2 c -> equal_types t1 t2 && c) args ts true
  | _ -> false

let rec convert_type = function
  | U.NumType -> F.NumType
  | U.BoolType -> F.BoolType
  | U.FunctionType (args, res) -> F.FunctionType (List.map convert_type args, convert_type res)

let convert_args args =
  List.map (fun (id, type_c) -> (id, convert_type type_c)) args

let empty_env () = []

let rec env_add env vs = List.append vs env

let rec env_find env id =
  snd (List.find (fun (k, v) -> k = id) env)

let get_op_type op arg_types =
  match op with
  | "plus" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "minus" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "times" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "divide" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "eq" -> F.FunctionType ([List.hd arg_types; List.hd arg_types], F.BoolType)
  | "neq" -> F.FunctionType ([List.hd arg_types; List.hd arg_types], F.BoolType)
  | "lt" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "gt" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "leq" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "geq" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "and" -> F.FunctionType ([F.BoolType; F.BoolType], F.BoolType)
  | "or" -> F.FunctionType ([F.BoolType; F.BoolType], F.BoolType)
  | "not" -> F.FunctionType ([F.BoolType], F.BoolType)
  | op -> raise Not_found

let get_prim_type = function
  | "beta" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "flip" -> F.FunctionType ([F.NumType], F.BoolType)
  | "gamma" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "normal" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "poisson" -> F.FunctionType ([F.NumType], F.NumType)
  | "uniform-continuous" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "uniform-discrete" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | prim -> raise Not_found

let rec infer_types_expr env (expr_guts, pos) =
  match expr_guts with
  | U.Bool b ->
      ((F.Bool b, F.BoolType), env)

  | U.Num x ->
      ((F.Num x, F.NumType), env)

  | U.Id id ->
      (try
        ((F.Id id, env_find env id), env)
      with Not_found ->
        raise (Exceptions.typing_error (Printf.sprintf "Unbound variable %s" id) pos))

  | U.Lambda (args, expr) -> 
      let args = convert_args args in
      let env = env_add env args in
      let arg_types = List.map snd args in
      let (expr, _) = infer_types_expr env expr in
      ((F.Lambda (args, expr), F.FunctionType (arg_types, (get_type expr))), env)

  | U.Let (id, value, expr) ->
      let (value, _) = infer_types_expr env value in
      let env = env_add env [(id, get_type value)] in
      let (expr, _) = infer_types_expr env expr in
      ((F.Let (id, value, expr), get_type expr), env)

  | U.If (test, then_expr, else_expr) ->
      let (test, _) = infer_types_expr env test in
      let (then_expr, _) = infer_types_expr env then_expr in
      let (else_expr, _) = infer_types_expr env else_expr in
      if equal_types (get_type test) F.BoolType then
        (if equal_types (get_type then_expr) (get_type else_expr) then
          ((F.If (test, then_expr, else_expr), get_type then_expr), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "If expression: then expression has type %s but else expression has type %s" (PF.print_type (get_type then_expr)) (PF.print_type (get_type else_expr))) pos))
      else
        raise (Exceptions.typing_error (Printf.sprintf "If expression: expected bool for test but received %s" (PF.print_type (get_type test))) pos)

  | U.Op (op, args) -> 
      (try
        let args = List.map fst (List.map (infer_types_expr env) args) in
        let arg_types = List.map get_type args in
        let op_type = get_op_type op arg_types in
        if check_app_types op_type arg_types then
          ((F.Op (op, args), get_result_type pos op_type), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos op_type)) (PF.print_type_list arg_types)) pos)
      with Not_found ->
        raise (Exceptions.typing_error (Printf.sprintf "Invalid builtin operation '%s' used" op) pos))

  | U.Prim (prim, args) -> 
      (try
        let prim_type = get_prim_type prim in
        let args = List.map fst (List.map (infer_types_expr env) args) in
        let arg_types = List.map get_type args in
        if check_app_types prim_type arg_types then
          ((F.Prim (prim, args), get_result_type pos prim_type), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos prim_type)) (PF.print_type_list arg_types)) pos)
      with Not_found ->
        raise (Exceptions.typing_error (Printf.sprintf "Invalid builtin primitive '%s' used" prim) pos))

  | U.App (expr, args) ->
      let (expr, _) = infer_types_expr env expr in
      let expr_type = get_type expr in
      let args = List.map fst (List.map (infer_types_expr env) args) in
      if is_function_type expr_type then
        (let arg_types = List.map get_type args in
        if check_app_types expr_type arg_types then
          ((F.App (expr, args), get_result_type pos expr_type), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos expr_type)) (PF.print_type_list arg_types)) pos))
      else
        raise (Exceptions.typing_error (Printf.sprintf "Application: %s is not a function type" (PF.print_type expr_type)) pos)

and infer_types_stmt env (stmt_guts, pos) =
  match stmt_guts with
  | U.Assume (id, value) ->
      let (value, _) = infer_types_expr env value in
      let value_type = get_type value in
      let env = env_add env [(id, value_type)] in
      ((F.Assume (id, value), value_type), env)

  | U.Observe (expr, value) ->
      let (expr, _) = infer_types_expr env expr in
      let expr_type = get_type expr in
      let (value, _) = infer_types_expr env value in
      let value_type = get_type value in
      if equal_types expr_type value_type then
        (match expr with
        | (F.Prim (prim, args), _) ->
            ((F.Observe (prim, args, value), value_type), env)
        | _ -> raise (Exceptions.typing_error "Observe: outer expresion must be a probabilistic primitive" pos))
      else
        raise (Exceptions.typing_error (Printf.sprintf "Observe: types %s and %s do not match" (PF.print_type expr_type) (PF.print_type value_type)) pos)

  | U.Predict expr ->
      let label = Printing_0_U.print_inline_expr expr in
      let (expr, _) = infer_types_expr env expr in
      let expr_type = get_type expr in
      if not (is_function_type expr_type) then
        ((F.Predict (label, expr), expr_type), env)
      else
        raise (Exceptions.typing_error "Predict: cannot predict a function type" pos)

and infer_types_stmts env = function
  | [] -> ([], env)
  | stmt :: stmts ->
      let (stmt, env) = infer_types_stmt env stmt in
      let (stmts, env) = infer_types_stmts env stmts in
      (stmt :: stmts, env)

let infer_types prog =
  fst (infer_types_stmts (empty_env ()) prog)