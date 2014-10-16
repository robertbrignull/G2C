open AST

let get_pos (_, (pos, _)) = pos
let get_type (_, (_, type_c)) = type_c

let rec equal_types t1 t2 =
  match t1, t2 with
  | BoolType, BoolType -> true
  | NumType, NumType -> true
  | FunctionType (args1, res1), FunctionType (args2, res2) ->
      List.length args1 == List.length args2 &&
      (List.fold_right2 (fun t1 t2 c -> equal_types t1 t2 && c) args1 args2 true) &&
      (equal_types res1 res2)
  | _ -> false

let is_function_type = function
  | FunctionType (l, r) -> true
  | _ -> false

let get_arg_types = function
  | FunctionType (args, _) -> args
  | _ -> []

let get_result_type = function
  | FunctionType (_, res) -> res
  | _ -> Untyped

let rec check_app_types expected ts =
  match expected with
  | FunctionType (args, _) ->
      List.length args == List.length ts &&
      List.fold_right2 (fun t1 t2 c -> equal_types t1 t2 && c) args ts true
  | _ -> false

let empty_env () = []

let rec env_add env vs = List.append vs env

let rec env_find env id =
  snd (List.find (fun (k, v) -> k = id) env)

let get_op_type pos = function
  | "eq" -> FunctionType ([NumType; NumType], BoolType)
  | "times" -> FunctionType ([NumType; NumType], NumType)
  | op -> raise (Exceptions.ParseErr Exceptions.(error (Printf.sprintf "Invalid builtin operation '%s' used" op) pos))

let infer_types expr =
  let rec infer_types_impl env (expr_guts, (pos, type_c)) =
    match expr_guts with
    | Bool b                 -> (Bool b, (pos, BoolType))

    | Num x                  -> (Num x, (pos, NumType))

    | Id id                  -> (try
                                  (Id id, (pos, env_find env id))
                                with Not_found ->
                                  raise (Exceptions.typing_error (Printf.sprintf "Unbound variable %s" id) pos))

    | Lambda (args, expr)    -> let env = env_add env args in
                                let arg_types = List.map snd args in
                                let expr = infer_types_impl env expr in
                                (Lambda (args, expr), (pos, FunctionType (arg_types, (get_type expr))))

    | Let (id, value, expr)  -> let value = infer_types_impl env value in
                                let env = env_add env [(id, get_type value)] in
                                let expr = infer_types_impl env expr in
                                (Let (id, value, expr), (pos, get_type expr))

    | If (test, then_expr, else_expr) ->
                                let test = infer_types_impl env test in
                                let then_expr = infer_types_impl env then_expr in
                                let else_expr = infer_types_impl env else_expr in
                                if equal_types (get_type test) BoolType then
                                  (if equal_types (get_type then_expr) (get_type else_expr) then
                                    (If (test, then_expr, else_expr), (pos, get_type then_expr))
                                  else
                                    raise (Exceptions.typing_error (Printf.sprintf "If expression: then expression has type %s but else expression has type %s" (Printing.print_type (get_type then_expr)) (Printing.print_type (get_type else_expr))) pos))
                                else
                                  raise (Exceptions.typing_error (Printf.sprintf "If expression: expected bool for test but received %s" (Printing.print_type (get_type test))) pos)

    | Op (op, args)          -> let op_type = get_op_type pos op in
                                let args = List.map (infer_types_impl env) args in
                                let arg_types = List.map get_type args in
                                if check_app_types op_type arg_types then
                                  (Op (op, args), (pos, get_result_type op_type))
                                else
                                  raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (Printing.print_type_list (get_arg_types op_type)) (Printing.print_type_list arg_types)) pos)

    | App (expr, args)       -> let expr = infer_types_impl env expr in
                                let expr_type = get_type expr in
                                let args = List.map (infer_types_impl env) args in
                                if is_function_type expr_type then
                                  (let arg_types = List.map get_type args in
                                  if check_app_types expr_type arg_types then
                                    (App (expr, args), (pos, get_result_type expr_type))
                                  else
                                    raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (Printing.print_type_list (get_arg_types expr_type)) (Printing.print_type_list arg_types)) pos))
                                else
                                  raise (Exceptions.typing_error (Printf.sprintf "Application: %s is not a function type" (Printing.print_type expr_type)) pos)

  in infer_types_impl (empty_env ()) expr