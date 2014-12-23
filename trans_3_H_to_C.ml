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



and transform_type env = function
  | H.NumType -> C.NumType
  | H.BoolType -> C.BoolType
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
      let proc_id = transform_id env proc_id in
      let let_id = let_id in
      let bundle_id =
        (try get_bundle_id env (snd let_id)
        with Not_found -> raise (Exceptions.transform_error ("Could not find bundle for '" ^ (fst let_id) ^ ": " ^ (Printing_3_H.print_type (snd let_id)) ^ "'")))
      in
      C.PackBundle ((fst let_id, snd bundle_id),
                    proc_id,
                    get_data_id env (fst proc_id),
                    List.map (transform_id env) bundle)

  | H.Op (op, args) ->
      C.Assign (transform_id env let_id,
                C.Op (op, List.map (transform_id env) args))

and transform_expr env = function
  | H.Let (id, value, expr) ->
      C.Seq [transform_let env id value;
             transform_expr env expr]

  | H.If (test, then_expr, else_expr) ->
      C.If (transform_id env test,
            transform_expr env then_expr,
            transform_expr env else_expr)

  | H.App (id, args) ->
      C.BundleApp (transform_id env id,
                   List.map (transform_id env) args)

  | H.Halt id ->
      C.Halt (transform_id env id)

and transform_proc env (id, bundle, args, expr) =
  let id = transform_id env id in
  let bundle = List.map (transform_id env) bundle in
  let args = List.map (transform_id env) args in
  (id,
   args,
   C.Seq [C.UnpackBundle (get_data_id env (fst id), bundle);
          transform_expr env expr])

let transform (procs, expr) =
  let env = gen_struct_ids procs in
  let (bundle_structs, data_structs) = gen_structs env procs in
  (bundle_structs,
   data_structs,
   List.map (transform_proc env) procs,
   transform_expr env expr)
