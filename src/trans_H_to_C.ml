module H = AST_3_H
module C = AST_4_C
open Common

let new_data_id () =
  "data_" ^ (id_index (new_id ()))

let new_bundle_id () =
  "bundle_" ^ (id_index (new_id ()))



let empty_env () = ([], [])

let add_data_to_env (d_env, b_env) proc_id data_id =
  ((proc_id, data_id) :: d_env, b_env)

let get_data_id (d_env, b_env) proc_id =
  List.assoc proc_id d_env

let add_bundle_to_env (d_env, b_env) type_c bundle_id =
  (d_env, (type_c, bundle_id) :: b_env)

let get_bundle_id (d_env, b_env) type_c =
  List.assoc type_c b_env



let rec gen_struct_ids = function
  | [] -> empty_env ()
  | proc :: procs ->
      let env = gen_struct_ids procs in
      let env = gen_data_id env proc in
      gen_bundle_id env proc

and gen_data_id env = function
  | H.Proc ((proc_id, _), _, _, _) ->
      let data_id = new_data_id () in
      add_data_to_env env proc_id (data_id, C.DataType data_id)

  | H.MemProc ((mem_id, _), _) ->
      let data_id = new_data_id () in
      add_data_to_env env mem_id (data_id, C.DataType data_id)

and gen_bundle_id env = function
  | H.Proc ((_, type_c), _, _, _) ->
      (try
        let _ = get_bundle_id env type_c in env
      with Not_found ->
        let bundle_id = new_bundle_id () in
        add_bundle_to_env env type_c (bundle_id, C.BundleType bundle_id))

  | H.MemProc ((_, type_c), _) ->
      (try
        let _ = get_bundle_id env type_c in env
      with Not_found ->
        let bundle_id = new_bundle_id () in
        add_bundle_to_env env type_c (bundle_id, C.BundleType bundle_id))

and gen_data_structs env = function
  | [] -> []

  | H.Proc ((proc_id, _), bundle, _, _) :: procs ->
      let ds = (get_data_id env proc_id,
                List.map (transform_id env) bundle) in
      ds :: (gen_data_structs env procs)

  | H.MemProc ((mem_id, type_c), _) :: procs ->
      let ds = (get_data_id env mem_id,
                [("bundle", snd (get_bundle_id env type_c))]) in
      ds :: (gen_data_structs env procs)

and gen_bundle_structs env = function
  | [] -> []  

  | H.Proc ((_, type_c), _, args, _) :: procs ->
      let bs = (get_bundle_id env type_c,
                List.map (transform_id env) args) in
      let bss = gen_bundle_structs env procs in
      if contains (fun b -> fst b = fst bs) bss then
        bss
      else
        bs :: bss

  | H.MemProc ((_, type_c), args) :: procs ->
      let bs = (get_bundle_id env type_c,
                List.map (transform_id env) args) in
      let bss = gen_bundle_structs env procs in
      if contains (fun b -> fst b = fst bs) bss then
        bss
      else
        bs :: bss

and is_bundle_type (id, type_c) =
  match type_c with
  | C.BundleType id -> true
  | _ -> false

and is_list_type (id, type_c) =
  match type_c with
  | C.ListType -> true
  | _ -> false



and transform_type env = function
  | H.NumType -> C.NumType
  | H.BoolType -> C.BoolType
  | H.ListType -> C.ListType
  | H.FunctionType args ->
      snd (get_bundle_id env (H.FunctionType args))

and get_function_args = function
  | H.FunctionType args -> args
  | _ -> raise (Exceptions.transform_error "Trying to get arguments of a non-function type")

and transform_id env (id, type_c) =
  (id, transform_type env type_c)

and transform_let env let_id = function
  | H.Bool b ->
      C.Assign (transform_id env let_id,
                C.Bool b)

  | H.Num x ->
      C.Assign (transform_id env let_id,
                C.Num x)

  | H.Id id ->
      C.Assign (transform_id env let_id,
                C.Id (transform_id env id))

  | H.ProcInstance (proc_id, bundle) ->
      let bundle_id =
        (try (fst let_id, snd (get_bundle_id env (snd let_id)))
        with Not_found -> raise (Exceptions.transform_error ("Could not find bundle for '" ^ (fst let_id) ^ ": " ^ (Printing_3_H.print_type (snd let_id)) ^ "'"))) in
      let proc_id = transform_id env proc_id in
      let data_id = get_data_id env (fst proc_id) in
      let packItem arg = C.PackBundleItem (bundle_id, data_id, transform_id env arg) in
      C.Seq [
        C.AllocateBundle (bundle_id, proc_id, data_id);
        C.Seq (List.map packItem bundle)
      ];

  | H.RecursiveProcInstance proc_id ->
      let bundle_id =
        (try (fst let_id, snd (get_bundle_id env (snd let_id)))
        with Not_found -> raise (Exceptions.transform_error ("Could not find bundle for '" ^ (fst let_id) ^ ": " ^ (Printing_3_H.print_type (snd let_id)) ^ "'"))) in
      let proc_id = transform_id env proc_id in
      C.AllocateRecursiveBundle (bundle_id, proc_id)

  | H.Prim (prim, args) ->
      C.Assign (transform_id env let_id,
                C.Prim (prim, List.map (transform_id env) args))

  | H.TypedPrim (prim, type_c, args) ->
      let type_c = transform_type env type_c in
      C.Assign (transform_id env let_id,
                C.TypedPrim (prim, type_c, List.map (transform_id env) args))

  | H.Mem (mem_id, proc_id) ->
      let bundle_id =
        (try (fst let_id, snd (get_bundle_id env (snd let_id)))
        with Not_found -> raise (Exceptions.transform_error ("Could not find bundle for '" ^ (fst let_id) ^ ": " ^ (Printing_3_H.print_type (snd let_id)) ^ "'"))) in
      let mem_id = transform_id env mem_id in
      let proc_id = transform_id env proc_id in
      let data_id = get_data_id env (fst mem_id) in
      C.Seq [
        C.AllocateBundle (bundle_id, mem_id, data_id);
        C.PackMemBundle (bundle_id, data_id, proc_id)
      ]

and transform_expr env current_proc_id = function
  | H.Let (id, value, expr) ->
      C.Seq [transform_let env id value;
             transform_expr env current_proc_id expr]

  | H.If (test, then_expr, else_expr) ->
      C.If (transform_id env test,
            transform_expr env current_proc_id then_expr,
            transform_expr env current_proc_id else_expr)

  | H.App (proc_id, args) ->
      let proc_id = transform_id env proc_id in
      let args = List.map (transform_id env) args in
      (match current_proc_id with
      | Some id when fst id = fst proc_id ->
          C.RecursiveApp (proc_id, args)
      | _ -> C.BundleApp (proc_id, args))

  | H.Observe (prim, args, value, next) ->
      let args = List.map (transform_id env) args in
      let value = transform_id env value in
      C.Seq [
        C.Observe (prim, args, value);
        transform_expr env current_proc_id next
      ]

  | H.Predict (label, id, next) ->
      C.Seq [
        C.Predict (label, transform_id env id);
        transform_expr env current_proc_id next
      ]

  | H.Halt -> C.Halt

and transform_proc env = function
  | H.Proc (id, bundle, args, expr) ->
      let id = transform_id env id in
      let bundle = List.map (transform_id env) bundle in
      let args = List.map (transform_id env) args in
      let data_id = get_data_id env (fst id) in
      let unpackItem = fun arg -> C.UnpackBundleItem (data_id, arg) in
      C.Proc (id,
              args,
              C.Seq [
                C.Seq (List.map unpackItem bundle);
                transform_expr env (Some id) expr
              ])

  | H.MemProc (mem_id, args) ->
      let mem_id = transform_id env mem_id in
      let bundle_id = get_bundle_id env (snd (last args)) in
      let data_id = get_data_id env (fst mem_id) in
      C.MemProc (mem_id,
                 data_id,
                 List.map (transform_id env) args,
                 bundle_id,
                 List.map (fun type_c -> (new_id (), transform_type env type_c))
                          (get_function_args (snd (last args))))

let transform (procs, expr) =
  let env = gen_struct_ids procs in
  let bundle_structs = gen_bundle_structs env procs in
  let data_structs = gen_data_structs env procs in
  (bundle_structs,
   data_structs,
   List.map (transform_proc env) procs,
   transform_expr env None expr)
