open AST_K_Prime
module F = AST_F
open Trans_F_to_K
open Common



let rec replace_id_expr source target = function
  | Let (id, value, expr) ->
      Let (id,
           replace_id_value source target value,
           replace_id_expr source target expr)

  | If (test, then_expr, else_expr) ->
      If (replace_id_id source target test,
          replace_id_expr source target then_expr,
          replace_id_expr source target else_expr)

  | App (expr, args) ->
      App (replace_id_id source target expr,
           List.map (replace_id_id source target) args)

  | Observe (prim, args, value, next) ->
      Observe (prim,
               List.map (replace_id_id source target) args,
               replace_id_id source target value,
               replace_id_expr source target next)

  | Predict (label, id, next) ->
      Predict (label,
               replace_id_id source target id,
               replace_id_expr source target next)

  | Halt -> Halt

and replace_id_value source target = function
  | Id id -> Id (replace_id_id source target id)

  | Lambda (args, expr) ->
      Lambda (args, replace_id_expr source target expr)

  | Prim (prim, args) ->
      Prim (prim, List.map (replace_id_id source target) args)

  | TypedPrim (prim, type_c, args) ->
      TypedPrim (prim, type_c, List.map (replace_id_id source target) args)

  | Mem id -> Mem (replace_id_id source target id)

  | x -> x

and replace_id_id source target id =
  if id_name id = id_name source then target
  else id



let rec is_free_in_expr target = function
  | Let (id, value, expr) ->
      if id_name id = id_name target then false
      else
           is_free_in_value target value
        || is_free_in_expr target expr

  | If (test, then_expr, else_expr) ->
         is_free_in_id target test
      || is_free_in_expr target then_expr
      || is_free_in_expr target else_expr

  | App (expr, args) ->
      List.fold_left (||) false (List.map (is_free_in_id target) (expr :: args))

  | Observe (prim, args, value, next) ->
         List.fold_left (||) false (List.map (is_free_in_id target) (value :: args))
      || is_free_in_expr target next

  | Predict (label, id, next) ->
         is_free_in_id target id
      || is_free_in_expr target next

  | Halt -> false

and is_free_in_value target = function
  | Id id -> is_free_in_id target id

  | Lambda (args, expr) -> is_free_in_expr target expr

  | Prim (prim, args) ->
      List.fold_left (||) false (List.map (is_free_in_id target) args)

  | TypedPrim (prim, type_c, args) ->
      List.fold_left (||) false (List.map (is_free_in_id target) args)

  | Mem id -> is_free_in_id target id

  | x -> false

and is_free_in_id target id =
  id_name id = id_name target



let rec is_const (_, _, value) =
  match value with
  | Unknown -> false
  | Bool b -> true
  | Num x -> true
  | Id id -> is_const id
  | Lambda (args, expr) -> false
  | Prim (prim, args) ->
      if is_probabilistic_prim prim then false
      else List.fold_right (&&) (List.map is_const args) true
  | TypedPrim (prim, type_c, args) ->
      if is_probabilistic_prim prim then false
      else List.fold_right (&&) (List.map is_const args) true
  | Mem id -> false



let rec replace_let src_id new_let = function
  | Let (id, value, expr) ->
      if id_name id = src_id then
        new_let expr
      else
        Let (id, value, replace_let src_id new_let expr)

  | If (test, then_expr, else_expr) ->
      If (test,
          replace_let src_id new_let then_expr,
          replace_let src_id new_let else_expr)

  | Observe (prim, args, value, next) ->
      Observe (prim, args, value, replace_let src_id new_let next)

  | Predict (label, id, next) ->
      Predict (label, id, replace_let src_id new_let next)

  | x -> x



let rec replace_observe src_observe new_observe = function
  | Let (id, value, expr) ->
      Let (id, value, replace_observe src_observe new_observe expr)

  | Observe (prim, args, value, next) ->
      if src_observe = (prim, args, value) then
        new_observe next
      else
        Observe (prim, args, value, replace_observe src_observe new_observe next)

  | Predict (label, id, next) ->
      Predict (label, id, replace_observe src_observe new_observe next)

  | x -> x



let local prog =
  let rec local_expr = function
    (* If an id is assigned to another id, remove the second id
       and replace by the first id throughout the code *)
    | Let (id_1, Id id_2, expr) -> begin
        (true, replace_id_expr id_1 id_2 expr)
      end

    (* Remove lets where the value is never used *)
    | Let (id, value, expr) when not (is_free_in_expr id expr) -> begin
        (true, expr)
      end

    (* End of optimisations, just recurse *)
    | Let (id, value, expr) ->
        let (c1, value) = local_value value in
        let (c2, expr) = local_expr expr in
        (c1 || c2, Let (id, value, expr))

    | If (test, then_expr, else_expr) ->
        let (c1, then_expr) = local_expr then_expr in
        let (c2, else_expr) = local_expr else_expr in
        (c1 || c2, If (test, then_expr, else_expr))

    | Observe (prim, args, value, next) ->
        let (c1, next) = local_expr next in
        (c1, Observe (prim, args, value, next))

    | Predict (label, id, next) ->
        let (c1, next) = local_expr next in
        (c1, Predict (label, id, next))

    | x -> (false, x)

  and local_value = function
    (* Remove trivial continuations that are equivalent
       to some other named function *)
    | Lambda ([l_arg], App (f, [f_arg]))
      when l_arg = f_arg ->
        (true, Id f)

    (* End of optimisations, just recurse *)
    | Lambda (args, expr) ->
        let (c, expr) = local_expr expr in
        (c, Lambda (args, expr))
        
    | x -> (false, x)

  in
  local_expr prog



let commute_sample_observe prog =
  let rec commute_sample_observe_expr = function
    | Let (id, value, expr) ->
        commute_sample_observe_expr expr

    | If (test, then_expr, else_expr) ->
        let (c, prog) = commute_sample_observe_expr then_expr in
        if c then
          (c, prog)
        else
          commute_sample_observe_expr else_expr

    | Observe (prim, args, value, next) ->
        (match prim, args, value with
        | "normal", [(m2, NumType, Prim ("normal", [m1; b1])); b2], (_, _, Num value_val) ->
            (match m1, b1, b2 with
            | (m1_id, NumType, Num m1_val), (b1_id, NumType, Num b1_val), (b2_id, NumType, Num b2_val) ->
                let nm1 = (new_id (), NumType, Unknown) in
                let nb1 = (new_id (), NumType, Unknown) in
                let nm2 = (new_id (), NumType, Unknown) in
                let nb2 = (new_id (), NumType, Unknown) in
                let new_let next =
                  let id1 = (new_id (), NumType, Unknown) in
                  let id2 = (new_id (), NumType, Unknown) in
                  let id3 = (new_id (), NumType, Unknown) in
                  let id4 = (new_id (), NumType, Unknown) in
                  let id5 = (new_id (), NumType, Unknown) in
                  let id6 = (new_id (), NumType, Unknown) in
                  let id7 = (new_id (), NumType, Unknown) in
                  let id8 = (new_id (), NumType, Unknown) in
                  Let (id1, Num b2_val,
                  Let (id2, Prim ("divide", [b1]),
                  Let (id3, Prim ("divide", [id1]),
                  Let (id4, Prim ("plus", [id2; id3]),
                  Let (nb1, Prim ("divide", [id4]),
                  Let (id5, Prim ("divide", [m1; b1]),
                  Let (id6, Num value_val,
                  Let (id7, Prim ("divide", [id6; id1]),
                  Let (id8, Prim ("plus", [id5; id7]),
                  Let (nm1, Prim ("times", [nb1; id8]),
                  Let ((m2, NumType, Unknown),
                       Prim ("normal", [nm1; nb1]),
                       next)))))))))))
                in
                let new_observe next =
                  Let (nb2, Prim ("plus", [b1; b2]),
                  Let (nm2, Id m1,
                  Observe ("normal",
                           [nm2; nb2],
                           value,
                           next)))
                in
                let prog = replace_let m2 new_let prog in
                let prog = replace_observe (prim, args, value) new_observe prog in
                (true, rebuild_values prog)

            | _, _, _ -> commute_sample_observe_expr next)

        | _, _, _ -> commute_sample_observe_expr next)

    | Predict (label, id, next) ->
        commute_sample_observe_expr next

    | x -> (false, prog)

  in commute_sample_observe_expr prog



let rec remove_const_observe = function
  | Let (id, value, expr) ->
      let (c, expr) = remove_const_observe expr in
      (c, Let (id, value, expr))

  | Observe (prim, args, value, next) ->
      let (c, next) = remove_const_observe next in
      if List.fold_right (&&) (List.map is_const (value :: args)) true then
        (true, next)
      else
        (c, Observe (prim, args, value, next))

  | Predict (label, id, next) ->
      let (c1, next) = remove_const_observe next in
      (c1, Predict (label, id, next))

  | x -> (false, x)



let rules = [ local; commute_sample_observe; remove_const_observe ]

let rec apply_rules rules_left all_rules prog =
  match rules_left with
  | [] -> prog
  | rule :: rules_left ->
      let (changed, prog) = rule prog in
      if changed then
        apply_rules all_rules all_rules prog
      else
        apply_rules rules_left all_rules prog

let optimise_prime prog = apply_rules rules rules prog

let optimise prog = transform_K_Prime (optimise_prime (transform_K prog))
