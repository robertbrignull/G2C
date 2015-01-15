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
      gen_bundle_id_for_proc env proc

and gen_data_id env ((proc_id, _), _, _, _) =
  let data_id = new_data_id () in
  add_data_to_env env proc_id (data_id, C.DataType data_id)

and gen_bundle_id_for_proc env ((_, type_c), _, _, _) =
  try
    let _ = get_bundle_id env type_c in env
  with Not_found ->
    let bundle_id = new_bundle_id () in
    add_bundle_to_env env type_c (bundle_id, C.BundleType bundle_id)

and gen_structs env = function
  | [] -> ([], [])
  | proc :: procs ->
      let (bs1, ds1) = gen_structs env procs in
      let bs2 = gen_bundle_struct_for_proc env proc in
      let ds2 = gen_data_struct env proc in
      ((try
         let _ = List.find (fun b -> fst b = fst bs2) bs1 in bs1
       with Not_found ->
         List.append bs1 [bs2]),
       List.append ds1 [ds2])

and gen_data_struct env ((proc_id, _), bundle, _, _) =
  (get_data_id env proc_id,
   List.map (transform_id env) bundle)

and gen_bundle_struct_for_proc env ((_, type_c), _, args, _) =
  (get_bundle_id env type_c,
   List.map (transform_id env) args)



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
      let packItem =
        fun arg ->
          let arg = transform_id env arg in
          if is_bundle_type arg then
            C.Seq [
              C.PackBundleItem (bundle_id, data_id, arg);
              C.IncrementDataRefCount arg
            ]
          else
            C.PackBundleItem (bundle_id, data_id, arg) in
      C.Seq [
        C.AllocateBundle (bundle_id, proc_id, data_id);
        C.Seq (List.map packItem bundle)
      ];

  | H.Prim (prim, args) ->
      C.Assign (transform_id env let_id,
                C.Prim (prim, List.map (transform_id env) args))

  | H.TypedPrim (prim, type_c, args) ->
      let type_c = transform_type env type_c in
      C.Assign (transform_id env let_id,
                C.TypedPrim (prim, type_c, List.map (transform_id env) args))

and transform_expr env scope current_proc_id = function
  | H.Let (id, value, expr) ->
      let scope = (transform_id env id) :: scope in
      C.Seq [transform_let env id value;
             transform_expr env scope current_proc_id expr]

  | H.If (test, then_expr, else_expr) ->
      C.If (transform_id env test,
            transform_expr env scope current_proc_id then_expr,
            transform_expr env scope current_proc_id else_expr)

  | H.App (proc_id, args) ->
      let proc_id = transform_id env proc_id in
      let args = List.map (transform_id env) args in
      let id_find a = fun b -> fst a == fst b in
      let rec gen_dealocations = function
        | [] -> []
        | id :: ids ->
            let in_args = contains (id_find id) args in
            let rest = gen_dealocations ids in
            if is_bundle_type id && not in_args && not (id_find id proc_id) then
              (C.DecrementDataRefCount id) :: rest
            else if is_list_type id && not in_args then
              (C.DeleteList id) :: rest
            else
              rest
      in
      let bundle_deallocation =
        match current_proc_id with
        | Some id ->
            if fst id = fst proc_id then []
            else [C.DeallocateBundle]
        | _ -> []
      in
      let app =
        match current_proc_id with
        | Some id when fst id = fst proc_id ->
            C.RecursiveApp (proc_id, args)
        | _ -> C.BundleApp (proc_id, args)
      in
      C.Seq [
        C.Seq (gen_dealocations scope);
        C.Seq bundle_deallocation;
        app
      ]

  | H.Observe (prim, args, value, next) ->
      let args = List.map (transform_id env) args in
      let value = transform_id env value in
      C.Seq [
        C.Observe (prim, args, value);
        transform_expr env scope current_proc_id next
      ]

  | H.Predict (label, id, next) ->
      C.Seq [
        C.Predict (label, transform_id env id);
        transform_expr env scope current_proc_id next
      ]

  | H.Halt -> C.Halt

and transform_proc env (id, bundle, args, expr) =
  let id = transform_id env id in
  let bundle = List.map (transform_id env) bundle in
  let args = List.map (transform_id env) args in
  let scope = List.append bundle args in
  let data_id = get_data_id env (fst id) in
  let unpackItem = fun arg -> C.UnpackBundleItem (data_id, arg) in
  (id,
   args,
   C.Seq [
     C.Seq (List.map unpackItem bundle);
     transform_expr env scope (Some id) expr
   ])

let transform (procs, expr) =
  let env = gen_struct_ids procs in
  let (bundle_structs, data_structs) = gen_structs env procs in
  (bundle_structs,
   data_structs,
   List.map (transform_proc env) procs,
   transform_expr env [] None expr)
