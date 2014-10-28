module C = AST_3_C
module H = AST_4_H
open Common

let rec transform_type = function
  | C.NumType -> H.NumType

  | C.BoolType -> H.BoolType

  | C.FunctionType (closure, args) ->
      H.FunctionType (List.map transform_type closure,
                      List.map transform_type args)

and transform_arg (id, type_c) =
  (id, transform_type type_c)

and transform_value (value_guts, type_c) =
  let type_c = transform_type type_c in
  match value_guts with
  | C.Bool b ->
      ([], (H.Bool b, type_c))

  | C.Num x ->
      ([], (H.Num x, type_c))

  | C.Id id ->
      ([], (H.Id id, type_c))

  | C.Lambda (closure, args, expr) ->
      let id = new_id () in
      let (procs, expr) = transform_expr expr in
      let closure = List.map transform_arg closure in
      let args = List.map transform_arg args in
      let proc = (id, closure, args, expr) in
      (proc :: procs,
       (H.Id id, type_c))

and transform_decl = function
  | C.Value value ->
      let (procs_1, value) = transform_value value in
      (procs_1, H.Value value)

  | C.Op (op, args) ->
      let res = List.map transform_value args in
      (List.concat (List.map fst res),
       H.Op (op, List.map snd res))

and transform_expr = function
  | C.Let (id, decl, expr) ->
      let (procs_1, decl) = transform_decl decl in
      let (procs_2, expr) = transform_expr expr in 
      (List.append procs_1 procs_2,
       H.Let (id, decl, expr))

  | C.If (test, then_expr, else_expr) ->
      let (procs_1, test) = transform_value test in
      let (procs_2, then_expr) = transform_expr then_expr in
      let (procs_3, else_expr) = transform_expr else_expr in
      (List.concat [procs_1; procs_2; procs_3],
       H.If (test, then_expr, else_expr))

  | C.App (expr, args) ->
      let (procs_1, expr) = transform_value expr in
      let res = List.map transform_value args in
      (List.concat (procs_1 :: (List.map fst res)),
       H.App (expr, List.map snd res))

  | C.Halt value ->
      let (procs_1, value) = transform_value value in
      (procs_1, H.Halt value)

let transform expr = transform_expr expr
