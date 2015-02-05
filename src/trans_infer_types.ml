open List

module U = AST_U
module F = AST_F
module PF = Printing_F
open Common

(* In this file we transform U to F by assigning a type to every
   expression and statement.
   Each transformation is fairly obvious. *)

(* is_function_type :: F.type_c -> bool *) 
let rec is_function_type = function
  | F.FunctionType (_, _) -> true
  | _ -> false

(* get_arg_types :: F.type_c -> F.type_c *) 
let get_arg_types pos = function
  | F.FunctionType (args, _) -> args
  | t -> raise (Exceptions.typing_error (Printf.sprintf "Cannot determine arg types: %s is not a function type" (PF.print_type t)) pos)

(* get_result_type :: F.type_c -> F.type_c *) 
let get_result_type pos = function
  | F.FunctionType (_, res) -> res
  | t -> raise (Exceptions.typing_error (Printf.sprintf "Cannot determine return type: %s is not a function type" (PF.print_type t)) pos)

(* Returns true iff ts matches the arguments of func *)
(* check_app_types :: F.type_c -> F.type_c list -> bool *)
let rec check_app_types func ts =
  match func with
  | F.FunctionType (args, _) ->
      length args == length ts &&
      fold_right (&&) (map2 (=) args ts) true
  | _ -> false

(* Converts a type from U to F *)
(* convert_type :: U.type_c -> F.type_c *)
let rec convert_type = function
  | U.NumType -> F.NumType
  | U.BoolType -> F.BoolType
  | U.ListType -> F.ListType
  | U.FunctionType (args, res) -> F.FunctionType (map convert_type args, convert_type res)

(* convert_args :: U.args -> F.args *)
let convert_args args =
  map (fun (id, type_c) -> (id, convert_type type_c)) args



(* Implement an simple environment to map ids to types *)
(* empty_env :: unit -> (F.id * F.type_c) list *)
let empty_env () = []

(* env_add :: env -> (F.id * F.type_c) list -> env *)
let rec env_add env vs = append vs env

(* env_find :: env -> F.id -> F.type_c *)
(* can throw Not_found *)
let rec env_find env id =
  snd (find (fun (k, v) -> k = id) env)



(* Returns the type of a prim, or Not_found *)
(* get_prim_type :: string -> F.type_c list -> F.type_c *)
(* can throw Not_found *)
let get_prim_type prim arg_types =
  let num_args = length arg_types in
  if num_args == 0 then
    match prim with
    | "empty" -> F.FunctionType([], F.ListType)
    | prim -> raise Not_found
    
  else
    let first_arg_type = hd arg_types in
    match prim with
    | "plus" -> F.FunctionType (duplicate num_args F.NumType, F.NumType)
    | "minus" -> F.FunctionType (duplicate num_args F.NumType, F.NumType)
    | "times" -> F.FunctionType (duplicate num_args F.NumType, F.NumType)
    | "divide" -> F.FunctionType (duplicate num_args F.NumType, F.NumType)
    | "eq" -> F.FunctionType (duplicate num_args first_arg_type, F.BoolType)
    | "neq" -> F.FunctionType (duplicate num_args first_arg_type, F.BoolType)
    | "lt" -> F.FunctionType (duplicate num_args F.NumType, F.BoolType)
    | "gt" -> F.FunctionType (duplicate num_args F.NumType, F.BoolType)
    | "leq" -> F.FunctionType (duplicate num_args F.NumType, F.BoolType)
    | "geq" -> F.FunctionType (duplicate num_args F.NumType, F.BoolType)
    | "and" -> F.FunctionType (duplicate num_args F.BoolType, F.BoolType)
    | "or" -> F.FunctionType (duplicate num_args F.BoolType, F.BoolType)
    | "not" -> F.FunctionType ([F.BoolType], F.BoolType)

    | "cons" -> F.FunctionType([first_arg_type; F.ListType], F.ListType)
    | "rest"  -> F.FunctionType([F.ListType], F.ListType)
    | "empty?" -> F.FunctionType([F.ListType], F.BoolType)
    | "count" -> F.FunctionType([F.ListType], F.NumType)

    | "log" -> F.FunctionType ([F.NumType], F.NumType)
    | "log10" -> F.FunctionType ([F.NumType], F.NumType)
    | "exp" -> F.FunctionType ([F.NumType], F.NumType)
    | "pow" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
    | "sqrt" -> F.FunctionType ([F.NumType], F.NumType)
    | "cbrt" -> F.FunctionType ([F.NumType], F.NumType)
    | "floor" -> F.FunctionType ([F.NumType], F.NumType)
    | "ceil" -> F.FunctionType ([F.NumType], F.NumType)
    | "round" -> F.FunctionType ([F.NumType], F.NumType)
    | "rint" -> F.FunctionType ([F.NumType], F.NumType)
    | "abs" -> F.FunctionType ([F.NumType], F.NumType)
    | "signum" -> F.FunctionType ([F.NumType], F.NumType)
    | "sin" -> F.FunctionType ([F.NumType], F.NumType)
    | "cos" -> F.FunctionType ([F.NumType], F.NumType)
    | "tan" -> F.FunctionType ([F.NumType], F.NumType)
    | "asin" -> F.FunctionType ([F.NumType], F.NumType)
    | "acos" -> F.FunctionType ([F.NumType], F.NumType)
    | "atan" -> F.FunctionType ([F.NumType], F.NumType)
    | "sinh" -> F.FunctionType ([F.NumType], F.NumType)
    | "cosh" -> F.FunctionType ([F.NumType], F.NumType)
    | "tanh" -> F.FunctionType ([F.NumType], F.NumType)
    | "inc" -> F.FunctionType ([F.NumType], F.NumType)
    | "dec" -> F.FunctionType ([F.NumType], F.NumType)
    | "mod" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)

    | "beta" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
    | "flip" -> F.FunctionType ([F.NumType], F.BoolType)
    | "gamma" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
    | "normal" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
    | "poisson" -> F.FunctionType ([F.NumType], F.NumType)
    | "geometric" -> F.FunctionType([F.NumType], F.NumType)
    | "exponential" -> F.FunctionType ([F.NumType], F.NumType)
    | "uniform-continuous" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
    | "uniform-discrete" -> F.FunctionType ([F.NumType; F.NumType], F.NumType)
    | "discrete" -> F.FunctionType([F.ListType], F.NumType)

    | _ -> raise Not_found

(* Returns the type of a typed prim, or Not_found *)
(* get_prim_type :: string -> F.type_c -> F.type_c list -> F.type_c *)
(* can throw Not_found *)
and get_typed_prim_type prim type_c arg_types =
  match prim, type_c with
  | "first", type_c -> F.FunctionType ([F.ListType], type_c)
  | "second", type_c -> F.FunctionType ([F.ListType], type_c)
  | "nth", type_c -> F.FunctionType ([F.ListType; F.NumType], type_c)

  | "categorical", type_c -> F.FunctionType ([F.ListType], type_c)

  | _, _ -> raise Not_found

(* Infers types recursively for an expr *)
(* infer_types_expr :: env -> U.expr -> F.expr *)
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

  | U.Let (id, (U.Lambda (args, ret_type, body), pos), expr) ->
      let args = convert_args args in
      let ret_type = convert_type ret_type in
      let lambda_type = F.FunctionType (map snd args, ret_type) in
      let env = env_add env [(id, lambda_type)] in
      let env2 = env_add env args in
      let (body, _) = infer_types_expr env2 body in
      if (snd body) = ret_type then
        let (expr, _) = infer_types_expr env expr in
        ((F.Let (id,
                 (F.Lambda (args, body), lambda_type),
                 expr),
          lambda_type),
         env)
      else
        raise (Exceptions.typing_error (Printf.sprintf "Lambda expression: inferred type '%s' does not match specified type '%s'" (PF.print_type (snd body)) (PF.print_type ret_type)) pos)

  | U.Lambda (args, ret_type, body) -> 
      let args = convert_args args in
      let env2 = env_add env args in
      let (body, _) = infer_types_expr env2 body in
      ((F.Lambda (args, body),
        F.FunctionType (map snd args, snd body)),
       env)

  | U.Let (id, value, expr) ->
      let (value, _) = infer_types_expr env value in
      let env = env_add env [(id, snd value)] in
      let (expr, _) = infer_types_expr env expr in
      ((F.Let (id, value, expr), snd expr), env)

  | U.If (test, then_expr, else_expr) ->
      let (test, _) = infer_types_expr env test in
      let (then_expr, _) = infer_types_expr env then_expr in
      let (else_expr, _) = infer_types_expr env else_expr in
      if (snd test) = F.BoolType then
        (if (snd then_expr) = (snd else_expr) then
          ((F.If (test, then_expr, else_expr), snd then_expr), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "If expression: then expression has type %s but else expression has type %s" (PF.print_type (snd then_expr)) (PF.print_type (snd else_expr))) pos))
      else
        raise (Exceptions.typing_error (Printf.sprintf "If expression: expected bool for test but received %s" (PF.print_type (snd test))) pos)

  | U.Prim (prim, args) -> 
      (try
        let args = map fst (map (infer_types_expr env) args) in
        let arg_types = map snd args in
        let prim_type = get_prim_type prim arg_types in
        if check_app_types prim_type arg_types then
          ((F.Prim (prim, args), get_result_type pos prim_type), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos prim_type)) (PF.print_type_list arg_types)) pos)
      with Not_found ->
        raise (Exceptions.typing_error (Printf.sprintf "Invalid builtin primitive '%s' used" prim) pos))

  | U.TypedPrim (prim, type_c, args) -> 
      (try
        let args = map fst (map (infer_types_expr env) args) in
        let arg_types = map snd args in
        let type_c = convert_type type_c in
        let prim_type = get_typed_prim_type prim type_c arg_types in
        if check_app_types prim_type arg_types then
          ((F.TypedPrim (prim, type_c, args), get_result_type pos prim_type), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos prim_type)) (PF.print_type_list arg_types)) pos)
      with Not_found ->
        raise (Exceptions.typing_error (Printf.sprintf "Invalid builtin primitive '%s' used" prim) pos))

  | U.Mem expr ->
      let (expr, _) = infer_types_expr env expr in
      let expr_type = snd expr in
      if is_function_type expr_type then
        ((F.Mem expr, expr_type), env)
      else
        raise (Exceptions.typing_error (Printf.sprintf "Mem: expected a function type but received %s" (PF.print_type expr_type)) pos)

  | U.App (expr, args) ->
      let (expr, _) = infer_types_expr env expr in
      let expr_type = snd expr in
      let args = map fst (map (infer_types_expr env) args) in
      if is_function_type expr_type then
        (let arg_types = map snd args in
        if check_app_types expr_type arg_types then
          ((F.App (expr, args), get_result_type pos expr_type), env)
        else
          raise (Exceptions.typing_error (Printf.sprintf "Application: expected %s but received %s" (PF.print_type_list (get_arg_types pos expr_type)) (PF.print_type_list arg_types)) pos))
      else
        raise (Exceptions.typing_error (Printf.sprintf "Application: %s is not a function type" (PF.print_type expr_type)) pos)

(* infer_types_stmt :: env -> U.stmt -> F.stmt *)
and infer_types_stmt env (stmt_guts, pos) =
  match stmt_guts with
  | U.Assume (id, (U.Lambda (args, ret_type, body), pos)) ->
      let args = convert_args args in
      let ret_type = convert_type ret_type in
      let lambda_type = F.FunctionType (map snd args, ret_type) in
      let env = env_add env [(id, lambda_type)] in
      let env2 = env_add env args in
      let (body, _) = infer_types_expr env2 body in
      if (snd body) = ret_type then
        ((F.Assume (id,
                   (F.Lambda (args, body), lambda_type)),
          lambda_type),
         env)
      else
        raise (Exceptions.typing_error (Printf.sprintf "Lambda expression: inferred type '%s' does not match specified type '%s'" (PF.print_type (snd body)) (PF.print_type ret_type)) pos)

  | U.Assume (id, value) ->
      let (value, _) = infer_types_expr env value in
      let value_type = snd value in
      let env = env_add env [(id, value_type)] in
      ((F.Assume (id, value), value_type), env)

  | U.Observe (expr, value) ->
      let (expr, _) = infer_types_expr env expr in
      let expr_type = snd expr in
      let (value, _) = infer_types_expr env value in
      let value_type = snd value in
      if expr_type = value_type then
        (match expr with
        | (F.Prim (prim, args), _) ->
            if is_probabilistic_prim prim then
              ((F.Observe (prim, args, value), value_type), env)
            else
              raise (Exceptions.typing_error "Observe: outer expresion must be a probabilistic primitive" pos)
        | (F.TypedPrim (prim, type_c, args), _) ->
            if is_probabilistic_prim prim then
              ((F.Observe (prim, args, value), value_type), env)
            else
              raise (Exceptions.typing_error "Observe: outer expresion must be a probabilistic primitive" pos)
        | _ -> raise (Exceptions.typing_error "Observe: outer expresion must be a probabilistic primitive" pos))
      else
        raise (Exceptions.typing_error (Printf.sprintf "Observe: types %s and %s do not match" (PF.print_type expr_type) (PF.print_type value_type)) pos)

  | U.Predict expr ->
      let label = Printing_U.print_inline_expr expr in
      let (expr, _) = infer_types_expr env expr in
      let expr_type = snd expr in
      if not (is_function_type expr_type) then
        ((F.Predict (label, expr), expr_type), env)
      else
        raise (Exceptions.typing_error "Predict: cannot predict a function type" pos)

(* infer_types_stmts :: env -> U.stmts -> F.stmts *)
and infer_types_stmts env = function
  | [] -> ([], env)
  | stmt :: stmts ->
      let (stmt, env) = infer_types_stmt env stmt in
      let (stmts, env) = infer_types_stmts env stmts in
      (stmt :: stmts, env)

(* Does a first pass to infer types of lambdas and mems,
   as these are specified by the user.
   This is to allow recursive mem'd functions. *)
(* infer_function_types :: env -> U.prog -> env *)
and infer_function_types env_in prog =
  let addded_to_env = ref true in

  (* infer_function_types_stmt :: env -> stmt -> env *)
  let infer_function_types_stmt env (stmt_guts, stmt_info) =
    match stmt_guts with
    | U.Assume (assume_id, (U.Lambda (args, ret_type, _), _)) ->
        (try let _ = env_find env assume_id in env
        with Not_found -> begin
          addded_to_env := true;
          let func_type = U.FunctionType (map snd args, ret_type) in
          env_add env [(assume_id, convert_type func_type)]
        end)

    | U.Assume (assume_id, (U.Mem (U.Lambda (args, ret_type, _), _), _)) ->
        (try let _ = env_find env assume_id in env
        with Not_found -> begin
          addded_to_env := true;
          let func_type = U.FunctionType (map snd args, ret_type) in
          env_add env [(assume_id, convert_type func_type)]
        end)

    | U.Assume (assume_id, (U.Mem (U.Id proc_id, _), _)) ->
        (try let _ = env_find env assume_id in env
        with Not_found -> begin
          addded_to_env := true;
          (try env_add env [(assume_id, env_find env proc_id)]
          with Not_found -> env)
        end)

    | _ -> env
  in
  (* infer_function_types_pass :: env -> prog -> env *)
  let rec infer_function_types_pass env = function
    | [] -> env
    | stmt :: stmts ->
        let env = infer_function_types_stmt env stmt in
        infer_function_types_pass env stmts
  in

  (* Loop infer_function_types_pass until it cannot type
     anything more *)
  let env_top = ref env_in in
  while !addded_to_env do
    addded_to_env := false;
    env_top := infer_function_types_pass !env_top prog
  done;
  !env_top

(* infer_types :: U.prog -> F.prog *)
let infer_types prog =
  let env = infer_function_types (empty_env ()) prog in
  fst (infer_types_stmts env prog)