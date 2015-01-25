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



let rec extract_nums = function
  | [] -> Some []
  | (Num x) :: args ->
      (match extract_nums args with
      | Some nums -> Some (x :: nums)
      | None -> None)
  | _ -> None



let is_direct_path_from_let_to_expr let_id target_expr source_expr = 
  let rec find_let = function
    | Let (id, value, expr) when id_name id = let_id -> expr
    | Let (id, value, expr) -> find_let expr
    | If (test, then_expr, else_expr) ->
        (try
          find_let then_expr
        with Not_found ->
          find_let else_expr)
    | Observe (prim, args, value, next) -> find_let next
    | Predict (label, id, next) -> find_let next
    | _ -> raise Not_found

  in
  let rec is_direct_path = function
    | x when x = target_expr -> true
    | Let (id, value, expr) -> is_direct_path expr
    | Observe (prim, args, value, next) -> is_direct_path next
    | Predict (label, id, next) -> is_direct_path next
    | _ -> false

  in
  is_direct_path (find_let source_expr)



let rec gen_const_let let_id value next =
  let gen_const_let_args args next =
    let args = List.map (fun id -> (new_id (), id_type id, id_value id)) args in
    List.fold_right (fun id next -> gen_const_let id (id_value id) next) args (next args)
  in
  match value with
    | Bool b -> Let (let_id, Bool b, next)
    | Num x -> Let (let_id, Num x, next)
    | Prim (prim, args) ->
        gen_const_let_args args (fun args -> Let (let_id, Prim (prim, args), next))
    | TypedPrim (prim, type_c, args) ->
        gen_const_let_args args (fun args -> Let (let_id, TypedPrim (prim, type_c, args), next))
    | _ -> raise Not_found



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



let is_linear_of_prim prim id_in =
  let rec find_prim id =
    let check_arg output arg =
      match find_prim arg with
      | Some id -> Some id
      | None -> output
    in
    match id_value id with
    | Prim (prim2, args) ->
        if prim2 = prim then Some (id_name id)
        else List.fold_left check_arg None args
    | TypedPrim (prim2, type_c, args) ->
        if prim2 = prim then Some (id_name id)
        else List.fold_left check_arg None args
    | _ -> None
  in

  let rec gen_args args next =
    match args with
    | [] -> next []
    | arg :: new_args ->
        let let_id = (new_id (), id_type arg, id_value arg) in
        let new_next = fun new_args -> next (let_id :: new_args) in
        gen_const_let let_id (id_value arg) (gen_args new_args new_next)
  in

  let rec calc_coeff_const prim_id = function
    | Prim (prim2, args) when prim2 = prim ->
        let coeff = (new_id (), NumType, Unknown) in
        let const = (new_id (), NumType, Unknown) in
        Some (prim_id, List.nth args 0, List.nth args 1,
              coeff, const,
              fun next ->
                Let (coeff, Num 1.0,
                Let (const, Num 0.0,
                next)))

    | Prim ("plus", args) ->
        let (const_args, non_const_args) = List.partition is_const args in
        if List.length non_const_args = 1 then
          (match calc_coeff_const prim_id (id_value (List.hd non_const_args)) with
          | Some (prim_id, m1, b1, coeff, const, gen_values) ->
              let new_const = (new_id (), NumType, Unknown) in
              Some (prim_id, m1, b1,
                    coeff, new_const,
                    fun next ->
                      gen_values
                        (gen_args const_args (fun const_args ->
                          (Let (new_const, Prim ("plus", const :: const_args),
                          next)))))
          | None -> None)
        else
          None

    | Prim ("minus", args) ->
        let (const_args, non_const_args) = List.partition is_const args in
        if List.length non_const_args = 1 then
          (match calc_coeff_const prim_id (id_value (List.hd non_const_args)) with
          | Some (prim_id, m1, b1, coeff, const, gen_values) ->
              let new_const = (new_id (), NumType, Unknown) in
              let new_coeff = (new_id (), NumType, Unknown) in
              if List.hd non_const_args = List.hd args then
                Some (prim_id, m1, b1,
                      coeff, new_const,
                      fun next ->
                        gen_values
                          (gen_args const_args (fun const_args ->
                            (Let (new_const, Prim ("minus", const :: const_args),
                            next)))))
              else
                Some (prim_id, m1, b1,
                      new_coeff, new_const,
                      fun next ->
                        gen_values
                          (gen_args const_args (fun const_args ->
                            (Let (new_coeff, Prim ("minus", [coeff]),
                            Let (new_const, Prim ("minus", List.append const_args [const]),
                            next))))))
          | None -> None)
        else
          None

    | Prim ("times", args) ->
        let (const_args, non_const_args) = List.partition is_const args in
        if List.length non_const_args = 1 then
          (match calc_coeff_const prim_id (id_value (List.hd non_const_args)) with
          | Some (prim_id, m1, b1, coeff, const, gen_values) ->
              let new_const = (new_id (), NumType, Unknown) in
              let new_coeff = (new_id (), NumType, Unknown) in
              let id1 = (new_id (), NumType, Unknown) in
              Some (prim_id, m1, b1,
                    new_coeff, new_const,
                    fun next ->
                      gen_values
                        (gen_args const_args (fun const_args ->
                          (Let (id1, Prim ("times", const_args),
                          Let (new_coeff, Prim ("times", [coeff; id1]),
                          Let (new_const, Prim ("times", [const; id1]),
                          next)))))))
          | None -> None)
        else
          None

    | Prim ("divide", args) ->
        let (const_args, non_const_args) = List.partition is_const args in
        if List.length non_const_args = 1 then
          (match calc_coeff_const prim_id (id_value (List.hd non_const_args)) with
          | Some (prim_id, m1, b1, coeff, const, gen_values) ->
              if List.hd non_const_args = List.hd args && List.length const_args > 0 then
                let new_const = (new_id (), NumType, Unknown) in
                let new_coeff = (new_id (), NumType, Unknown) in
                let id1 = (new_id (), NumType, Unknown) in
                Some (prim_id, m1, b1,
                      new_coeff, new_const,
                      fun next ->
                        gen_values
                          (gen_args const_args (fun const_args ->
                            (Let (id1, Prim ("times", const_args),
                            Let (new_coeff, Prim ("divide", [coeff; id1]),
                            Let (new_const, Prim ("divide", [const; id1]),
                            next)))))))
              else
                None
          | None -> None)
        else
          None

    | _ -> None
  in

  match find_prim id_in with
  | Some prim_id -> calc_coeff_const prim_id (id_value id_in)
  | None -> None



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

    (* Do some constant calculations *)
    | Prim ("plus", args) ->
        let values_changed = List.map local_value (List.map id_value args) in
        let values = List.map snd values_changed in
        let changed = List.fold_right (||) (List.map fst values_changed) false in
        (match extract_nums values with
        | Some nums -> (true, Num (List.fold_right (+.) nums 0.))
        | None -> (changed, Prim ("plus", args)))

    | Prim ("minus", args) ->
        let values_changed = List.map local_value (List.map id_value args) in
        let values = List.map snd values_changed in
        let changed = List.fold_right (||) (List.map fst values_changed) false in
        (match extract_nums values with
        | Some nums ->
            if List.length nums = 1 then
              (true, Num (0. -. List.hd nums))
            else
              (true, Num (List.hd nums -. List.fold_right (+.) (List.tl nums) 0.))
        | None -> (changed, Prim ("minus", args)))

    | Prim ("times", args) ->
        let values_changed = List.map local_value (List.map id_value args) in
        let values = List.map snd values_changed in
        let changed = List.fold_right (||) (List.map fst values_changed) false in
        (match extract_nums values with
        | Some nums -> (true, Num (List.fold_right ( *. ) nums 1.))
        | None -> (changed, Prim ("times", args)))

    | Prim ("divide", args) ->
        let values_changed = List.map local_value (List.map id_value args) in
        let values = List.map snd values_changed in
        let changed = List.fold_right (||) (List.map fst values_changed) false in
        (match extract_nums values with
        | Some nums ->
            if List.length nums = 1 then
              (true, Num (1. /. List.hd nums))
            else
              (true, Num (List.hd nums /. List.fold_right ( *. ) (List.tl nums) 1.))
        | None -> (changed, Prim ("divide", args)))

    (* End of optimisations, just recurse *)
    | Lambda (args, expr) ->
        let (c, expr) = local_expr expr in
        (c, Lambda (args, expr))
        
    | x -> (false, x)

  in
  let (c, prog) = local_expr prog in
  (c, rebuild_values prog)



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
        (match prim, args with
        | "normal", [m2; b2] ->
            (match is_linear_of_prim "normal" m2 with
            | Some (prim_id, m1, b1, coeff, const, gen_values) ->
                if    is_direct_path_from_let_to_expr prim_id (Observe (prim, args, value, next)) prog
                   && is_const m1
                   && is_const b1
                   && is_const b2
                   && is_const value
                then
                  let nm1 = (new_id (), NumType, Unknown) in
                  let nb1 = (new_id (), NumType, Unknown) in
                  let nm2 = (new_id (), NumType, Unknown) in
                  let nb2 = (new_id (), NumType, Unknown) in
                  let gend_b2 = (new_id (), NumType, Unknown) in
                  let gend_value = (new_id (), NumType, Unknown) in
                  let id1 = (new_id (), NumType, Unknown) in
                  let id2 = (new_id (), NumType, Unknown) in
                  let id3 = (new_id (), NumType, Unknown) in
                  let id4 = (new_id (), NumType, Unknown) in
                  let id5 = (new_id (), NumType, Unknown) in
                  let id6 = (new_id (), NumType, Unknown) in
                  let id7 = (new_id (), NumType, Unknown) in
                  let id8 = (new_id (), NumType, Unknown) in
                  let id9 = (new_id (), NumType, Unknown) in
                  let id10 = (new_id (), NumType, Unknown) in
                  let id11 = (new_id (), NumType, Unknown) in
                  let new_let next =
                    gen_values
                    (gen_const_let gend_b2 (id_value b2)
                    (gen_const_let gend_value (id_value value)
                    (Let (id1, Prim ("divide", [b1]),
                    Let (id2, Prim ("times", [coeff; coeff]),
                    Let (id3, Prim ("divide", [id2; gend_b2]),
                    Let (id4, Prim ("plus", [id1; id3]),
                    Let (nb1, Prim ("divide", [id4]),
                    Let (id5, Prim ("divide", [m1; b1]),
                    Let (id6, Prim ("divide", [coeff; gend_b2]),
                    Let (id7, Prim ("minus", [gend_value; const]),
                    Let (id8, Prim ("times", [id6; id7]),
                    Let (id9, Prim ("plus", [id5; id8]),
                    Let (nm1, Prim ("times", [nb1; id9]),
                    Let ((prim_id, NumType, Unknown),
                         Prim ("normal", [nm1; nb1]),
                         next)))))))))))))))
                  in
                  let new_observe next =
                    Let (id10, Prim ("times", [id2; b1]),
                    Let (nb2, Prim ("plus", [gend_b2; id10]),
                    Let (id11, Prim ("times", [coeff; m1]),
                    Let (nm2, Prim ("plus", [id11; const]),
                    Observe ("normal",
                             [nm2; nb2],
                             gend_value,
                             next)))))
                  in
                  let prog = replace_let prim_id new_let prog in
                  let prog = replace_observe (prim, args, value) new_observe prog in
                  let prog = rebuild_values prog in
                  (true, prog)
            
                else commute_sample_observe_expr next
            | None -> commute_sample_observe_expr next)
        | _, _ -> commute_sample_observe_expr next)

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



let rec apply_rules rules_left all_rules prog =
  match rules_left with
  | [] -> prog
  | rule :: rules_left ->
      let (changed, prog) = rule prog in
      if changed then
        apply_rules all_rules all_rules prog
      else
        apply_rules rules_left all_rules prog

let optimise_prime rules prog = apply_rules rules rules prog

let optimise level prog =
  let rules =
    if level = 0 then [ ]
    else if level = 1 then [ local ]
    else [ local; commute_sample_observe; remove_const_observe ]
  in
  transform_K_Prime (optimise_prime rules (transform_K prog))
