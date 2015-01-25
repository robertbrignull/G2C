module K = AST_K

type type_c =
  | NumType
  | BoolType
  | ListType
  | FunctionType of type_c list

and id = string * type_c * value

and args = id list

and value =
  | Unknown
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of args * expr
  | Prim of string * args
  | TypedPrim of string * type_c * args
  | Mem of id

and expr =
  | Let of id * value * expr
  | If of id * expr * expr
  | App of id * args
  | Observe of string * args * id * expr
  | Predict of string * id * expr
  | Halt

and prog = expr



let id_name (id, _, _) = id
let id_type (_, type_c, _) = type_c
let id_value (_, _, value) = value



let transform_K prog =
  let empty_env () = [] in

  let env_add env id value = (id, value) :: env in

  let rec env_find env id =
    match List.assoc id env with
    | Id (another_id, _, _) -> env_find env another_id
    | x -> x
  in

  let rec transform_K_type_c = function
    | K.NumType -> NumType
    | K.BoolType -> BoolType
    | K.ListType -> ListType
    | K.FunctionType args -> FunctionType (List.map transform_K_type_c args)

  and transform_K_id env (id, type_c) =
    (try
      let value = env_find env id in
      (id, transform_K_type_c type_c, value)
    with Not_found ->
      raise (Exceptions.transform_error (Printf.sprintf "KP: Unbound variable %s" id)))

  and transform_K_lambda_arg (id, type_c) =
    (id, transform_K_type_c type_c, Unknown)

  and transform_K_value env = function
    | K.Bool b -> Bool b
    | K.Num x -> Num x
    | K.Id id -> Id (transform_K_id env id)

    | K.Lambda (args, expr) ->
        let env2 = List.fold_right (fun arg env -> env_add env (fst arg) Unknown) args env in
        Lambda (List.map transform_K_lambda_arg args,
                transform_K_expr env2 expr)

    | K.Prim (prim, args) ->
        Prim (prim,
              List.map (transform_K_id env) args)

    | K.TypedPrim (prim, type_c, args) ->
        TypedPrim (prim,
                   transform_K_type_c type_c,
                   List.map (transform_K_id env) args)

    | K.Mem id -> Mem (transform_K_id env id)

  and transform_K_expr env = function
    | K.Let ((id, type_c), value, expr) ->
        let env2 = env_add env id Unknown in
        let value = transform_K_value env2 value in
        let env3 = env_add env id value in
        Let ((id, transform_K_type_c type_c, value),
             value,
             transform_K_expr env3 expr)

   | K.If (test, then_expr, else_expr) ->
        If (transform_K_id env test,
               transform_K_expr env then_expr,
               transform_K_expr env else_expr)

   | K.App (expr, args) ->
        App (transform_K_id env expr,
             List.map (transform_K_id env) args)

   | K.Observe (label, args, value, next) ->
        Observe (label,
                 List.map (transform_K_id env) args,
                 transform_K_id env value,
                 transform_K_expr env next)

   | K.Predict (label, value, next) ->
        Predict (label,
                 transform_K_id env value,
                 transform_K_expr env next)

   | K.Halt -> Halt

  in
  transform_K_expr (empty_env ()) prog



let rebuild_values prog =
  let empty_env () = [] in

  let env_add env id value = (id, value) :: env in

  let rec env_find env id =
    match List.assoc id env with
    | Id (another_id, _, _) -> env_find env another_id
    | x -> x
  in

  let rec rebuild_values_id env (id, type_c, _) =
    (try
      let value = env_find env id in
      (id, type_c, value)
    with Not_found ->
      raise (Exceptions.transform_error (Printf.sprintf "KP: Unbound variable %s" id)))

  and rebuild_values_value env = function
    | Unknown -> Unknown
    | Bool b -> Bool b
    | Num x -> Num x
    | Id id -> Id (rebuild_values_id env id)

    | Lambda (args, expr) ->
        let env2 = List.fold_right (fun arg env -> env_add env (id_name arg) Unknown) args env in
        Lambda (args,
                rebuild_values_expr env2 expr)

    | Prim (prim, args) ->
        Prim (prim,
              List.map (rebuild_values_id env) args)

    | TypedPrim (prim, type_c, args) ->
        TypedPrim (prim,
                   type_c,
                   List.map (rebuild_values_id env) args)

    | Mem id -> Mem (rebuild_values_id env id)

  and rebuild_values_expr env = function
    | Let ((id, type_c, _), value, expr) ->
        let env2 = env_add env id Unknown in
        let value = rebuild_values_value env2 value in
        let env3 = env_add env id value in
        Let ((id, type_c, value),
             value,
             rebuild_values_expr env3 expr)

   | If (test, then_expr, else_expr) ->
        If (rebuild_values_id env test,
               rebuild_values_expr env then_expr,
               rebuild_values_expr env else_expr)

   | App (expr, args) ->
        App (rebuild_values_id env expr,
             List.map (rebuild_values_id env) args)

   | Observe (label, args, value, next) ->
        Observe (label,
                 List.map (rebuild_values_id env) args,
                 rebuild_values_id env value,
                 rebuild_values_expr env next)

   | Predict (label, value, next) ->
        Predict (label,
                 rebuild_values_id env value,
                 rebuild_values_expr env next)

   | Halt -> Halt

  in
  rebuild_values_expr (empty_env ()) prog



let rec transform_K_Prime prog =
  let rec transform_K_Prime_type_c = function
    | NumType -> K.NumType
    | BoolType -> K.BoolType
    | ListType -> K.ListType
    | FunctionType args -> K.FunctionType (List.map transform_K_Prime_type_c args)

  and transform_K_Prime_id (id, type_c, value) =
    (id, transform_K_Prime_type_c type_c)

  and transform_K_Prime_value = function
    | Unknown -> raise (Failure "Unknown value in K Prime AST")

    | Bool b -> K.Bool b
    | Num x -> K.Num x
    | Id id -> K.Id (transform_K_Prime_id id)

    | Lambda (args, expr) ->
        K.Lambda (List.map transform_K_Prime_id args,
                  transform_K_Prime_expr expr)

    | Prim (prim, args) ->
        K.Prim (prim,
                List.map transform_K_Prime_id args)

    | TypedPrim (prim, type_c, args) ->
        K.TypedPrim (prim,
                     transform_K_Prime_type_c type_c,
                     List.map transform_K_Prime_id args)

    | Mem id -> K.Mem (transform_K_Prime_id id)

  and transform_K_Prime_expr = function
    | Let (id, value, expr) ->
        K.Let (transform_K_Prime_id id,
               transform_K_Prime_value value,
               transform_K_Prime_expr expr)

    | If (test, then_expr, else_expr) ->
        K.If (transform_K_Prime_id test,
              transform_K_Prime_expr then_expr,
              transform_K_Prime_expr else_expr)

    | App (expr, args) ->
        K.App (transform_K_Prime_id expr,
               List.map transform_K_Prime_id args)

    | Observe (label, args, value, next) ->
         K.Observe (label,
                    List.map transform_K_Prime_id args,
                    transform_K_Prime_id value,
                    transform_K_Prime_expr next)

    | Predict (label, value, next) ->
         K.Predict (label,
                    transform_K_Prime_id value,
                    transform_K_Prime_expr next)

    | Halt -> K.Halt

  in
  transform_K_Prime_expr prog
