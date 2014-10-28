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

let get_op_type pos = function
  | "plus" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "minus" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "times" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "divide" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
  | "eq" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "neq" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "lt" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "gt" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "leq" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | "geq" -> F.FunctionType ([F.NumType; F.NumType], F.BoolType)
  | op -> raise (Exceptions.ParseErr Exceptions.(error (Printf.sprintf "Invalid builtin operation '%s' used" op) pos))

let infer_types expr =
  let rec infer_types_impl env (expr_guts, pos) =
    match expr_guts with
    | U.Bool b ->
        (F.Bool b, F.BoolType)

    | U.Num x ->
        (F.Num x, F.NumType)

    | U.Id id ->
        (try
          (F.Id id, env_find env id)
        with Not_found ->
          raise (Exceptions.typing_error (Printf.sprintf "Unbound variable %s" id) pos))

    | U.Lambda (args, expr) -> 
        let args = convert_args args in
        let env = env_add env args in
        let arg_types = List.map snd args in
        let expr = infer_types_impl env expr in
        (F.Lambda (args, expr), F.FunctionType (arg_types, (get_type expr)))

    | U.Let (id, value, expr) ->
        let value = infer_types_impl env value in
        let env = env_add env [(id, get_type value)] in
        let expr = infer_types_impl env expr in
        (F.Let (id, value, expr), get_type expr)

    | U.If (test, then_expr, else_expr) ->
        let test = infer_types_impl env test in
        let then_expr = infer_types_impl env then_expr in
        let else_expr = infer_types_impl env else_expr in
        if equal_types (get_type test) F.BoolType then
          (if equal_types (get_type then_expr) (get_type else_expr) then
            (F.If (test, then_expr, else_expr), get_type then_expr)
          else
            raise (Exceptions.typing_error (Printf.sprintf "If expression: then expression has type %s but else expression has type %s" (PF.print_type (get_type then_expr)) (PF.print_type (get_type else_expr))) pos))
        else
          raise (Exceptions.typing_error (Printf.sprintf "If expression: expected bool for test but received %s" (PF.print_type (get_type test))) pos)

    | U.Op (op, args) -> 
        let op_type = get_op_type pos op in
        let args = List.map (infer_types_impl env) args in
        let arg_types = List.map get_type args in
        if check_app_types op_type arg_types then
          (F.Op (op, args), get_result_type pos op_type)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos op_type)) (PF.print_type_list arg_types)) pos)

    | U.App (expr, args) ->
        let expr = infer_types_impl env expr in
        let expr_type = get_type expr in
        let args = List.map (infer_types_impl env) args in
        if is_function_type expr_type then
          (let arg_types = List.map get_type args in
          if check_app_types expr_type arg_types then
            (F.App (expr, args), get_result_type pos expr_type)
          else
            raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos expr_type)) (PF.print_type_list arg_types)) pos))
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: %s is not a function type" (PF.print_type expr_type)) pos)

  in infer_types_impl (empty_env ()) expr