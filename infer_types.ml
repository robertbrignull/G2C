open AST

let get_pos (_, (pos, _)) = pos
let get_type (_, (_, type_c)) = type_c

let rec equal_types t1 t2 =
  match t1, t2 with
  | BoolType, BoolType -> true
  | NumType, NumType -> true
  | CompoundType (l1, r1), CompoundType (l2, r2) ->
      (equal_types l1 l2) && (equal_types r1 r2)
  | _ -> false

let rec check_app_types expr_type args_types =
  match expr_type, args_types with
  | CompoundType (l, r), [t] -> equal_types l t
  | CompoundType (l, r), t :: ts -> equal_types l t && check_app_types r ts
  | _ -> false

let rec get_result_type = function
  | CompoundType (l, r) -> get_result_type r
  | t -> t

let rec extract_types pos = function
  | [(_, t)] -> t
  | (_, t) :: ts -> CompoundType (t, extract_types pos ts)
  | [] -> raise (Exceptions.typing_error "Invalid lambda expression arguments" pos)

let rec add_result_type res = function
  | CompoundType (l, r) -> CompoundType (l, add_result_type res r)
  | t -> CompoundType (t, res)

let empty_env () = []

let rec env_add env vs = List.append vs env

let rec env_find env id =
  snd (List.find (fun (k, v) -> k = id) env)

let get_op_type = function
  | "eq" -> CompoundType (NumType, CompoundType (NumType, BoolType))
  | "times" -> CompoundType (NumType, CompoundType (NumType, NumType))
  | _ -> NoType

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
                                let expr = infer_types_impl env expr in
                                (Lambda (args, expr), (pos, add_result_type (get_type expr) (extract_types pos args)))

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

    | Op (op, args)          -> let op_type = get_op_type op in
                                let args = List.map (infer_types_impl env) args in
                                if check_app_types op_type (List.map get_type args) then
                                  (Op (op, args), (pos, get_result_type op_type))
                                else
                                  raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (Printing.print_type (get_type expr)) (Printing.map_and_concat (fun arg -> Printing.print_type (get_type arg)) ", " args)) pos)

    | App (expr, args)       -> let expr = infer_types_impl env expr in
                                let args = List.map (infer_types_impl env) args in
                                if check_app_types (get_type expr) (List.map get_type args) then
                                  (App (expr, args), (pos, get_result_type (get_type expr)))
                                else
                                  raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (Printing.print_type (get_type expr)) (Printing.map_and_concat (fun arg -> Printing.print_type (get_type arg)) ", " args)) pos)

  in infer_types_impl (empty_env ()) expr