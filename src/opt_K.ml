open List

open AST_K_Prime
module F = AST_F
open Trans_F_to_K
open Common


(* find_uses_expr :: string -> expr -> id list *)
let rec find_uses_expr target = function
  | Let (id, value, expr) ->
      concat [find_uses_value target value;
             find_uses_expr target expr]

  | If (test, then_expr, else_expr) ->
      concat [find_uses_id target test;
              find_uses_expr target then_expr;
              find_uses_expr target else_expr]

  | App (expr, args) ->
      concat (map (find_uses_id target) (expr :: args))

  | Observe (prim, args, value, next) ->
      concat [concat (map (find_uses_id target) (value :: args));
              find_uses_expr target next]

  | UnvaluedObserve (prim, args, next) ->
      concat [concat (map (find_uses_id target) args);
              find_uses_expr target next]

  | Predict (label, id, next) ->
      concat [find_uses_id target id;
              find_uses_expr target next]

  | Halt -> []

(* find_uses_value :: string -> value -> id list *)
and find_uses_value target = function
  | Id id -> find_uses_id target id

  | Lambda (args, expr) ->
      concat [concat (map (find_uses_id target) args);
              find_uses_expr target expr]

  | Prim (prim, args) ->
      concat (map (find_uses_id target) args)

  | TypedPrim (prim, type_c, args) ->
      concat (map (find_uses_id target) args)

  | Mem id -> find_uses_id target id

  | _ -> []

(* find_uses_id :: string -> id -> id list *)
and find_uses_id target id =
  if target = id_name id then [id] else []



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
           map (replace_id_id source target) args)

  | Observe (prim, args, value, next) ->
      Observe (prim,
               map (replace_id_id source target) args,
               replace_id_id source target value,
               replace_id_expr source target next)

  | UnvaluedObserve (prim, args, next) ->
      UnvaluedObserve (prim,
                       map (replace_id_id source target) args,
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
      Prim (prim, map (replace_id_id source target) args)

  | TypedPrim (prim, type_c, args) ->
      TypedPrim (prim, type_c, map (replace_id_id source target) args)

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
      fold_right (||) (map (is_free_in_id target) (expr :: args)) false

  | Observe (prim, args, value, next) ->
         fold_right (||) (map (is_free_in_id target) (value :: args)) false
      || is_free_in_expr target next

  | UnvaluedObserve (prim, args, next) ->
         fold_right (||) (map (is_free_in_id target) args) false
      || is_free_in_expr target next

  | Predict (label, id, next) ->
         is_free_in_id target id
      || is_free_in_expr target next

  | Halt -> false

and is_free_in_value target = function
  | Id id -> is_free_in_id target id

  | Lambda (args, expr) -> is_free_in_expr target expr

  | Prim (prim, args) ->
      fold_right (||) (map (is_free_in_id target) args) false

  | TypedPrim (prim, type_c, args) ->
      fold_right (||) (map (is_free_in_id target) args) false

  | Mem id -> is_free_in_id target id

  | x -> false

and is_free_in_id target id =
  id_name id = id_name target



(* Note this should only be used for values not using Lambda or Mem *)
(* list_ids_used :: id -> string list *)
let rec list_ids_used (id, _, value) =
  id :: list_ids_used_value value

(* list_ids_used_value :: value -> string list *)
and list_ids_used_value = function
  | Id id -> (id_name id) :: (list_ids_used id)
  | Prim (prim, args) -> concat (map list_ids_used args)
  | TypedPrim (prim, type_c, args) -> concat (map list_ids_used args)
  | _ -> []



(* Returns true iff the given id is determined,
   it may use probabilistic primitives but they have all been
   sampled already and now it is just arithmetic *)
let rec is_determined (_, _, value) =
  match value with
  | Unknown -> false
  | Bool b -> true
  | Num x -> true
  | Id id -> is_determined id
  | Lambda (args, expr) -> false
  | Prim (prim, args) -> fold_right (&&) (map is_determined args) true
  | TypedPrim (prim, type_c, args) -> fold_right (&&) (map is_determined args) true
  | Mem id -> false



(* Returns true iff the given id has a constant value,
   ie: it uses no probabilistic primitives or lambdas *)
let rec is_const (_, _, value) =
  match value with
  | Unknown -> false
  | Bool b -> true
  | Num x -> true
  | Id id -> is_const id
  | Lambda (args, expr) -> false
  | Prim (prim, args) ->
      if is_probabilistic_prim prim then false
      else fold_right (&&) (map is_const args) true
  | TypedPrim (prim, type_c, args) ->
      if is_probabilistic_prim prim then false
      else fold_right (&&) (map is_const args) true
  | Mem id -> false



(* extract_nums :: args -> (float list * args) *)
let rec extract_nums = function
  | [] -> ([], [])
  | (_, _, Num x) :: args ->
      let (nums, non_num_args) = extract_nums args in
      (x :: nums, non_num_args)
  | arg :: args ->
      let (nums, non_num_args) = extract_nums args in
      (nums, arg :: non_num_args)



(* extract_bools :: args -> (bool list * args) *)
let rec extract_bools = function
  | [] -> ([], [])
  | (_, _, Bool b) :: args ->
      let (bools, non_bool_args) = extract_bools args in
      (b :: bools, non_bool_args)
  | arg :: args ->
      let (bools, non_bool_args) = extract_bools args in
      (bools, arg :: non_bool_args)



(* evaluate_pairwise :: ('a -> 'a -> bool) -> 'a list -> bool -> bool *)
let rec evaluate_pairwise f xs e =
  match xs with
  | x :: y :: xs ->
      (f x y) && (evaluate_pairwise f (y :: xs) e)
  | _ -> e



(* find_let :: string -> expr -> expr *)
let rec find_let let_id = function
  | Let (id, value, expr) when id_name id = let_id ->
      Let (id, value, expr)
  | Let (id, value, expr) -> find_let let_id expr
  | If (test, then_expr, else_expr) ->
      (try find_let let_id then_expr
      with Not_found -> find_let let_id else_expr)
  | Observe (prim, args, value, next) -> find_let let_id next
  | UnvaluedObserve (prim, args, next) -> find_let let_id next
  | Predict (label, id, next) -> find_let let_id next
  | _ -> raise Not_found



(* defined_in :: string -> expr -> bool *)
let rec defined_in let_id = function
  | Let (id, value, expr) when id_name id = let_id -> true
  | Let (id, value, expr) -> defined_in let_id expr
  | If (test, then_expr, else_expr) ->
      defined_in let_id then_expr || defined_in let_id else_expr
  | Observe (prim, args, value, next) -> defined_in let_id next
  | UnvaluedObserve (prim, args, next) -> defined_in let_id next
  | Predict (label, id, next) -> defined_in let_id next
  | _ -> false



(* is_direct_path_from_let_to_expr :: string -> expr -> expr -> bool *)
let is_direct_path_from_let_to_expr let_id target_expr source_expr = 
  let rec is_direct_path = function
    | x when x = target_expr -> true
    | Let (id, value, expr) -> is_direct_path expr
    | Observe (prim, args, value, next) -> is_direct_path next
    | UnvaluedObserve (prim, args, next) -> is_direct_path next
    | Predict (label, id, next) -> is_direct_path next
    | _ -> false
  in
  try is_direct_path (find_let let_id source_expr)
  with Not_found -> false



(* When given a prim and a list of ids,
   returns a tuple of two lists:
   - pa = subset of ids which of the given prim
   - npa = everything else *)
(* partition_by_prim :: string -> id list -> (id list * id list) *)
let rec partition_by_prim prim ids =
  let is_prim_app = function
    | (_, _, Prim (prim2, _)) -> prim2 = prim
    | _ -> false

  in partition is_prim_app ids



(* When given a list of ids,
   returns a list of the args to prims *)
(* extract_prim_args :: id list -> id list list *)
let rec extract_prim_args ids =
  let extract id prim_argss =
    (match id with
    | (_, _, Prim (_, prim_args)) -> prim_args :: prim_argss
    | _ -> prim_argss)

  in fold_right extract ids []
      


(* gen_const_let :: id -> value -> expr -> expr *)
let rec gen_const_let let_id value next =
  let gen_const_let_args args next =
    let args = map (fun id -> (new_id (), id_type id, id_value id)) args in
    fold_right (fun id next -> gen_const_let id (id_value id) next) args (next args)
  in
  match value with
    | Bool b -> Let (let_id, Bool b, next)
    | Num x -> Let (let_id, Num x, next)
    | Prim (prim, args) ->
        gen_const_let_args args (fun args -> Let (let_id, Prim (prim, args), next))
    | TypedPrim (prim, type_c, args) ->
        gen_const_let_args args (fun args -> Let (let_id, TypedPrim (prim, type_c, args), next))
    | _ -> raise Not_found



(* replace_let :: string -> (expr -> expr) > expr -> expr *)
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

  | UnvaluedObserve (prim, args, next) ->
      UnvaluedObserve (prim, args, replace_let src_id new_let next)

  | Predict (label, id, next) ->
      Predict (label, id, replace_let src_id new_let next)

  | x -> x



(* find_observed_ids :: (string * args) -> expr -> id list *)
let rec find_observed_ids t_prim t_args = function
  | Let (id, value, expr) ->
      find_observed_ids t_prim t_args expr

  | Observe (prim, args, value, next) ->
      if prim = t_prim && args = t_args then
        value :: find_observed_ids t_prim t_args next
      else
        find_observed_ids t_prim t_args next

  | Predict (label, id, next) ->
      find_observed_ids t_prim t_args next

  | _ -> []



(* replace_observe :: (string * args * id) -> (expr -> expr) -> expr -> expr *)
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



(* replace_unvalued_observe :: (string * args * id) -> (expr -> expr) -> expr -> expr *)
let rec replace_unvalued_observe src_observe new_observe = function
  | Let (id, value, expr) ->
      Let (id, value, replace_unvalued_observe src_observe new_observe expr)

  | Observe (prim, args, value, next) ->
      Observe (prim, args, value, replace_unvalued_observe src_observe new_observe next)

  | UnvaluedObserve (prim, args, next) ->
      if src_observe = (prim, args) then
        new_observe next
      else
        UnvaluedObserve (prim, args, replace_unvalued_observe src_observe new_observe next)

  | Predict (label, id, next) ->
      Predict (label, id, replace_unvalued_observe src_observe new_observe next)

  | x -> x



(* remove_observes :: string -> args -> id list -> prog -> prog *)
let rec remove_observes t_prim t_args t_values = function
  | Let (id, value, expr) ->
      Let (id, value, remove_observes t_prim t_args t_values expr)

  | If (test, then_expr, else_expr) ->
      If (test,
          remove_observes t_prim t_args t_values then_expr,
          remove_observes t_prim t_args t_values else_expr)

  | Observe (prim, args, value, next) ->
      if   prim = t_prim
        && args = t_args
        && contains (eq value) t_values
      then
        remove_observes t_prim t_args t_values next
      else
        Observe (prim, args, value, remove_observes t_prim t_args t_values next)

  | UnvaluedObserve (prim, args, next) ->
      if   prim = t_prim
        && args = t_args
      then
        remove_observes t_prim t_args t_values next
      else
        UnvaluedObserve (prim, args, remove_observes t_prim t_args t_values next)

  | Predict (label, id, next) ->
      Predict (label, id, remove_observes t_prim t_args t_values next)

  | x -> x



let is_linear_of_prim prim id_in prog =
  let rec find_prims (id, _, value) =
    match value with
    | Prim (prim2, args) ->
        if prim2 = prim then [id]
        else concat (map find_prims args)
    | TypedPrim (prim2, type_c, args) ->
        if prim2 = prim then [id]
        else concat (map find_prims args)
    | _ -> []
  in

  let rec contains_id target (id, _, value) =
    if id = target then true
    else
      match value with
      | Id id ->
          contains_id target id
      | Prim (prim2, args) ->
          fold_right (||) (map (contains_id target) args) false
      | TypedPrim (prim2, type_c, args) ->
          fold_right (||) (map (contains_id target) args) false
      | _ -> false
  in

  let rec calc_coeff_const prim_id = function
    | Prim (prim2, args) when prim2 = prim ->
        let coeff = (new_id (), NumType, Unknown) in
        let const = (new_id (), NumType, Unknown) in
        Some (prim_id, nth args 0, nth args 1,
              coeff, const,
              [],
              fun next ->
                Let (coeff, Num 1.0,
                Let (const, Num 0.0,
                next)))

    | Prim ("plus", args) ->
        let (containing_args, non_containing_args) = partition (contains_id prim_id) args in
        if length containing_args = 1 then
          (match calc_coeff_const prim_id (id_value (hd containing_args)) with
          | Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values) ->
              let new_const = (new_id (), NumType, Unknown) in
              Some (prim_id, m1, b1,
                    coeff, new_const,
                    concat (dependent_ids :: (map list_ids_used non_containing_args)),
                    fun next ->
                      gen_values
                        (Let (new_const, Prim ("plus", const :: non_containing_args),
                         next)))
          | None -> None)
        else
          None

    | Prim ("minus", args) ->
        let (containing_args, non_containing_args) = partition (contains_id prim_id) args in
        if length containing_args = 1 then
          (match calc_coeff_const prim_id (id_value (hd containing_args)) with
          | Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values) ->
              let new_const = (new_id (), NumType, Unknown) in
              let new_coeff = (new_id (), NumType, Unknown) in
              if hd containing_args = hd args then
                Some (prim_id, m1, b1,
                      coeff, new_const,
                      concat (dependent_ids :: (map list_ids_used non_containing_args)),
                      fun next ->
                        gen_values
                          (Let (new_const, Prim ("minus", const :: non_containing_args),
                           next)))
              else
                Some (prim_id, m1, b1,
                      new_coeff, new_const,
                      concat (dependent_ids :: (map list_ids_used non_containing_args)),
                      fun next ->
                        gen_values
                          (Let (new_coeff, Prim ("minus", [coeff]),
                           Let (new_const, Prim ("minus", append non_containing_args [const]),
                           next))))
          | None -> None)
        else
          None

    | Prim ("times", args) ->
        let (containing_args, non_containing_args) = partition (contains_id prim_id) args in
        if length containing_args = 1 then
          (match calc_coeff_const prim_id (id_value (hd containing_args)) with
          | Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values) ->
              let new_const = (new_id (), NumType, Unknown) in
              let new_coeff = (new_id (), NumType, Unknown) in
              let id1 = (new_id (), NumType, Unknown) in
              Some (prim_id, m1, b1,
                    new_coeff, new_const,
                    concat (dependent_ids :: (map list_ids_used non_containing_args)),
                    fun next ->
                      gen_values
                        (Let (id1, Prim ("times", non_containing_args),
                         Let (new_coeff, Prim ("times", [coeff; id1]),
                         Let (new_const, Prim ("times", [const; id1]),
                         next)))))
          | None -> None)
        else
          None

    | Prim ("divide", args) ->
        let (containing_args, non_containing_args) = partition (contains_id prim_id) args in
        if length containing_args = 1 then
          (match calc_coeff_const prim_id (id_value (hd containing_args)) with
          | Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values) ->
              if hd containing_args = hd args && length non_containing_args > 0 then
                let new_const = (new_id (), NumType, Unknown) in
                let new_coeff = (new_id (), NumType, Unknown) in
                let id1 = (new_id (), NumType, Unknown) in
                Some (prim_id, m1, b1,
                      new_coeff, new_const,
                      concat (dependent_ids :: (map list_ids_used non_containing_args)),
                      fun next ->
                        gen_values
                          (Let (id1, Prim ("times", non_containing_args),
                           Let (new_coeff, Prim ("divide", [coeff; id1]),
                           Let (new_const, Prim ("divide", [const; id1]),
                           next)))))
              else
                None
          | None -> None)
        else
          None

    | _ -> None
  in

  let ids_used = list_ids_used id_in in
  let f prim_id output =
    (* The idea here is that if prim_id appears in id_in more than once
       then it cannot possibly be linear, however if it only appears once
       and we determine it to be linear then we can later rearrange let
       expressions so that everything is defined correctly.
     *)
    if length (filter (eq prim_id) ids_used) = 1 then
      match calc_coeff_const prim_id (id_value id_in) with
      | Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values) ->
          Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values)
      | None -> output
    else
      output
  in
  fold_right f (find_prims id_in) None



(* move_let_after :: string -> string list -> prog -> prog *)
let move_let_after let_id targets prog =
  (* find_first_non_dependent_let :: string -> expr -> expr *)
  let rec find_first_non_dependent_let let_id = function
    | Let (id, value, expr) ->
        let ids_used = list_ids_used_value value in
        if not (contains (eq let_id) ids_used) then
          Let (id, value, expr)
        else
          find_first_non_dependent_let let_id expr
    | Observe (label, args, value, next) ->
        find_first_non_dependent_let let_id next
    | UnvaluedObserve (label, args, next) ->
        find_first_non_dependent_let let_id next
    | Predict (label, value, next) ->
        find_first_non_dependent_let let_id next
    | _ -> raise Not_found

  in
  (* move_let_after_target :: expr -> expr *)
  let move_let_back_one = function
    | Let (id_1, value_1, expr_1) ->
        (match find_first_non_dependent_let (id_name id_1) expr_1 with
        | Let (id_2, value_2, expr_2) ->
            Let (id_2,
                 value_2,
                 replace_let (id_name id_2)
                             (fun next -> next)
                             (Let (id_1, value_1, expr_1)))

        | _ -> raise Not_found)
    | _ -> raise Not_found

  in
  (* must be called with the desired let_id at the head of the expr *)
  (* move_let_after_target :: string -> expr -> prog *)
  (* may throw Not_found *)
  let rec move_let_after_target target expr =
    if defined_in target expr then
      match move_let_back_one expr with
      | Let (id, value, expr) ->
          Let (id, value, move_let_after_target target expr)
      | Observe (label, args, value, next) ->
          Observe (label, args, value, move_let_after_target target next)
      | UnvaluedObserve (label, args, next) ->
          UnvaluedObserve (label, args, move_let_after_target target next)
      | Predict (label, value, next) ->
          Predict (label, value, move_let_after_target target next)
      | _ -> raise Not_found
    else
      expr

  in
  fold_right
    (fun target prog ->
      let old_let = (find_let let_id prog) in
      let new_let = move_let_after_target target old_let in
      replace_let let_id (fun _ -> new_let) prog)
    targets
    prog




let local prog =
  (* local_expr :: expr -> (bool * expr) *)
  let rec local_expr = function
    (* If an id is assigned to another id, remove the second id
       and replace by the first id throughout the code *)
    | Let (id_1, Id id_2, expr) ->
        (true, replace_id_expr id_1 id_2 expr)

    (* Remove lets where the value is never used *)
    | Let (id, value, expr) when not (is_free_in_expr id expr) ->
        (true, expr)

    (* Perform optimisations on values *)
    | Let (id, value, expr) ->
        let (c, expr) = local_expr expr in
        (match local_value id value with
        | Some let_gen -> (true, let_gen expr)
        | None -> (c, Let (id, value, expr)))

    (* End of optimisations, just recurse *)
    | If (test, then_expr, else_expr) ->
        let (c1, then_expr) = local_expr then_expr in
        let (c2, else_expr) = local_expr else_expr in
        (c1 || c2, If (test, then_expr, else_expr))

    | Observe (prim, args, value, next) ->
        let (c1, next) = local_expr next in
        (c1, Observe (prim, args, value, next))

    | UnvaluedObserve (prim, args, next) ->
        let (c1, next) = local_expr next in
        (c1, UnvaluedObserve (prim, args, next))

    | Predict (label, id, next) ->
        let (c1, next) = local_expr next in
        (c1, Predict (label, id, next))

    | x -> (false, x)

  (* local_value :: id -> value -> (value * (expr -> expr)) option *)
  and local_value let_id = function
    (* Remove trivial continuations that are equivalent
       to some other named function *)
    | Lambda ([l_arg], App (f, [f_arg]))
      when l_arg = f_arg ->
        Some (fun next ->
          Let (let_id, Id f, next))

    (* Do some constant calculations *)
    | Prim ("plus", [arg]) ->
        Some (fun next ->
          Let (let_id, Id arg, next))

    | Prim ("minus", [(_, _, Num x)]) ->
        Some (fun next ->
          Let (let_id, Num (0. -. x), next))

    | Prim ("times", [arg]) ->
        Some (fun next ->
          Let (let_id, Id arg, next))

    | Prim ("divide", [(_, _, Num x)]) ->
        Some (fun next ->
          Let (let_id, Num (1. /. x), next))

    | Prim ("plus", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length nums > 1 then
          let id_1 = (new_id (), NumType, Unknown) in
          Some (fun next ->
            Let (id_1, Num (fold_right (+.) nums 0.),
            Let (let_id, Prim ("plus", id_1 :: non_num_args),
            next)))

        else None

    | Prim ("minus", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Num (fold_left (-.) (hd nums) (tl nums)),
            next))
        else if length nums > 1 then
          let id_1 = (new_id (), NumType, Unknown) in
          (match hd args with
          | (_, _, Num x) when x = hd nums ->
              Some (fun next ->
                Let (id_1, Num (fold_left (-.) (hd nums) (tl nums)),
                Let (let_id, Prim ("minus", id_1 :: non_num_args),
                next)))
          | _ ->
              Some (fun next ->
                Let (id_1, Num (fold_right (+.) nums 0.),
                Let (let_id, Prim ("minus", append non_num_args [id_1]),
                next))))
        else None

    | Prim ("times", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length nums > 1 then
          let id_1 = (new_id (), NumType, Unknown) in
          Some (fun next ->
            Let (id_1, Num (fold_right ( *. ) nums 1.),
            Let (let_id, Prim ("times", id_1 :: non_num_args),
            next)))
        else None

    | Prim ("divide", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Num (fold_left (/.) (hd nums) (tl nums)),
            next))
        else if length nums > 1 then
          let id_1 = (new_id (), NumType, Unknown) in
          (match hd args with
          | (_, _, Num x) when x = hd nums ->
              Some (fun next ->
                Let (id_1, Num (fold_left (/.) (hd nums) (tl nums)),
                Let (let_id, Prim ("divide", id_1 :: non_num_args),
                next)))
          | _ ->
              Some (fun next ->
                Let (id_1, Num (fold_right ( *. ) nums 1.),
                Let (let_id, Prim ("divide", append non_num_args [id_1]),
                next))))
        else None

    | Prim ("eq", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          let x = hd nums in
          Some (fun next ->
            Let (let_id, Bool (fold_right (&&) (map (eq x) nums) true),
            next))
        else
          let (bools, non_bool_args) = extract_bools args in
          if length non_bool_args = 0 then
            let x = hd bools in
            Some (fun next ->
              Let (let_id, Bool (fold_right (&&) (map (eq x) bools) true),
              next))
          else None

    | Prim ("neq", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (fold_right (&&) (map (fun x -> length (map (eq x) nums) = 1) nums) true),
            next))
        else
          let (bools, non_bool_args) = extract_bools args in
          if length non_bool_args = 0 then
            Some (fun next ->
              Let (let_id, Bool (fold_right (&&) (map (fun x -> length (map (eq x) bools) = 1) bools) true),
              next))
          else None

    | Prim ("lt", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (evaluate_pairwise (<) nums true), next))
        else None

    | Prim ("gt", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (evaluate_pairwise (>) nums true), next))
        else None

    | Prim ("leq", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (evaluate_pairwise (<=) nums true), next))
        else None

    | Prim ("geq", args) ->
        let (nums, non_num_args) = extract_nums args in
        if length non_num_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (evaluate_pairwise (>=) nums true), next))
        else None

    | Prim ("and", args) ->
        let (bools, non_bool_args) = extract_bools args in
        if length non_bool_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (fold_right (&&) bools true), next))
        else None

    | Prim ("or", args) ->
        let (bools, non_bool_args) = extract_bools args in
        if length non_bool_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (fold_right (||) bools false), next))
        else None

    | Prim ("not", args) ->
        let (bools, non_bool_args) = extract_bools args in
        if length non_bool_args = 0 then
          Some (fun next ->
            Let (let_id, Bool (not (hd bools)), next))
        else None

    (* End of optimisations, just recurse *)
    | Lambda (args, expr) ->
        let (c, expr) = local_expr expr in
        if c then
          Some (fun next ->
            Let (let_id, Lambda (args, expr), next))
        else None
        
    | x -> None

  in
  let (c, prog) = local_expr prog in
  (c, rebuild_values prog)



let merge_arithmetic prog =
  (* id_used_once :: id -> bool *)
  let id_used_once id =
    length (find_uses_expr (id_name id) prog) = 1
  in

  (* merge_arithmetic_expr :: expr -> (bool * expr) *)
  let rec merge_arithmetic_expr = function
    | Let (id, value, expr) ->
        (match merge_arithmetic_value id value with
        | Some let_gen -> (true, replace_let (id_name id) let_gen prog)
        | None -> merge_arithmetic_expr expr)

    | If (test, then_expr, else_expr) ->
        let (c1, prog1) = merge_arithmetic_expr then_expr in
        if c1 then (c1, prog1)
        else merge_arithmetic_expr else_expr

    | Observe (prim, args, value, next) ->
        merge_arithmetic_expr next

    | UnvaluedObserve (prim, args, next) ->
        merge_arithmetic_expr next

    | Predict (label, id, next) ->
        merge_arithmetic_expr next

    | _ -> (false, prog)

  (* merge_arithmetic_value :: id -> value -> (value * (expr -> expr)) option *)
  and merge_arithmetic_value let_id = function
    | Prim ("plus", args) ->
        let (plus_args, non_plus_args) = partition_by_prim "plus" args in
        let (mergeable_args, non_mergeable_args) = partition id_used_once plus_args in
        if length mergeable_args > 0 then
          let plus_args_args = extract_prim_args mergeable_args in
          let new_args = concat [concat plus_args_args; non_mergeable_args; non_plus_args] in
          Some (fun next ->
            Let (let_id, Prim ("plus", new_args),
            next))
        else None

    | Prim ("minus", args) ->
        (match hd args with
        | (id, _, Prim ("minus", minus_args)) when id_used_once (hd args) ->
            let new_args = append minus_args (tl args) in
            Some (fun next ->
              Let (let_id, Prim ("minus", new_args),
              next))
        | _ ->
            let (plus_args, non_plus_args) = partition_by_prim "plus" (tl args) in
            let (mergeable_args, non_mergeable_args) = partition id_used_once plus_args in
            if length mergeable_args > 0 then begin
              let plus_args_args = extract_prim_args mergeable_args in
              let new_args = concat [[hd args]; non_mergeable_args; non_plus_args; concat plus_args_args] in
              Some (fun next ->
                Let (let_id, Prim ("minus", new_args),
                next))
            end
            else None)

    | Prim ("times", args) ->
        let (times_args, non_times_args) = partition_by_prim "times" args in
        let (mergeable_args, non_mergeable_args) = partition id_used_once times_args in
        if length mergeable_args > 0 then
          let times_args_args = extract_prim_args mergeable_args in
          let new_args = concat [concat times_args_args; non_mergeable_args; non_times_args] in
          Some (fun next ->
            Let (let_id, Prim ("times", new_args),
            next))
        else None

    | Prim ("divide", args) ->
        (match hd args with
        | (id, _, Prim ("divide", divide_args)) when id_used_once (hd args) ->
            let new_args = append divide_args (tl args) in
            Some (fun next ->
              Let (let_id, Prim ("divide", new_args),
              next))
        | _ ->
            let (times_args, non_times_args) = partition_by_prim "times" (tl args) in
            let (mergeable_args, non_mergeable_args) = partition id_used_once times_args in
            if length mergeable_args > 0 then begin
              let times_args_args = extract_prim_args mergeable_args in
              let new_args = concat [[hd args]; non_mergeable_args; non_times_args; concat times_args_args] in
              Some (fun next ->
                Let (let_id, Prim ("divide", new_args),
                next))
            end
            else None)

    | _ -> None

  in merge_arithmetic_expr prog



let merge_samples prog =
  let id_used_once id =
    length (find_uses_expr (id_name id) prog) = 1

  in
  let rec merge_samples_expr = function
    | Let (let_id, Prim ("plus", args), expr) ->
        let (c, expr) = merge_samples_expr expr in

        let (normal_args, non_normal_args) = partition_by_prim "normal" args in
        if (  length normal_args >= 2
           && fold_right (&&) (map id_used_once normal_args) true) then
          let normal_args_args = extract_prim_args normal_args in
          let ms = map hd normal_args_args in
          let bs = map hd (map tl normal_args_args) in
          let nm = (new_id (), NumType, Unknown) in
          let nb = (new_id (), NumType, Unknown) in
          let nv = (new_id (), NumType, Unknown) in
          (true,
          Let (nm, Prim ("plus", ms),
          Let (nb, Prim ("plus", bs),
          Let (nv, Prim ("normal", [nm; nb]),
          Let (let_id, Prim ("plus", nv :: non_normal_args),
            expr)))))

        else
        let (poisson_args, non_poisson_args) = partition_by_prim "poisson" args in
        if (  length poisson_args >= 2
           && fold_right (&&) (map id_used_once poisson_args) true) then
          let poisson_args_args = extract_prim_args poisson_args in
          let ls = map hd poisson_args_args in
          let nl = (new_id (), NumType, Unknown) in
          let nv = (new_id (), NumType, Unknown) in
          (true,
          Let (nl, Prim ("plus", ls),
          Let (nv, Prim ("poisson", [nl]),
          Let (let_id, Prim ("plus", nv :: non_poisson_args),
            expr))))

        else
          (c, Let (let_id, Prim ("plus", args), expr))

    | Let (id, value, expr) ->
        let (c, expr) = merge_samples_expr expr in
        (c, Let (id, value, expr))

    | If (test, then_expr, else_expr) ->
        let (c1, then_expr) = merge_samples_expr then_expr in
        let (c2, else_expr) = merge_samples_expr else_expr in
        (c1 || c2, If (test, then_expr, else_expr))

    | Observe (prim, args, value, next) ->
        let (c, next) = merge_samples_expr next in
        (c, Observe (prim, args, value, next))

    | UnvaluedObserve (prim, args, next) ->
        let (c, next) = merge_samples_expr next in
        (c, UnvaluedObserve (prim, args, next))

    | Predict (label, id, next) ->
        let (c, next) = merge_samples_expr next in
        (c, Predict (label, id, next))

    | x -> (false, x)

  in merge_samples_expr prog



let commute_sample_observe prog =
  let rec commute_sample_observe_expr = function
    (* beta sample *)
    | Let (p, Prim ("beta", [a; b]), expr) ->
        (* multiple flip observes *)
        let try_flip_observes () =
          if is_const a && is_const b then
            let observed_ids = find_observed_ids "flip" [p] expr in
            let (mergeable_ids, non_mergeable_ids) = partition is_const observed_ids in
            if length mergeable_ids > 0 then
              let num_true = length (filter (function (_, _, Bool b) -> b | _ -> false) mergeable_ids) in
              let num_false = length mergeable_ids - num_true in
              let na = (new_id (), NumType, Unknown) in
              let nb = (new_id (), NumType, Unknown) in
              let id1 = (new_id (), NumType, Unknown) in
              let id2 = (new_id (), NumType, Unknown) in
              let new_let next =
                Let (id1, Num (float_of_int num_true),
                Let (id2, Num (float_of_int num_false),
                Let (na, Prim ("plus", [a; id1]),
                Let (nb, Prim ("plus", [b; id2]),
                Let (p, Prim ("beta", [na; nb]),
                     next)))))
              in
              let prog = replace_let (id_name p) new_let prog in
              let prog = remove_observes "flip" [p] mergeable_ids prog in
              let prog = rebuild_values prog in
              (true, prog)

            else (false, prog)
          else (false, prog)

        in
        (* multiple geometric observes *)
        let try_geometric_observes () =
          let observed_ids = find_observed_ids "geometric" [p] expr in
          if length observed_ids > 0 then
            let na = (new_id (), NumType, Unknown) in
            let nb = (new_id (), NumType, Unknown) in
            let num_values = (new_id (), NumType, Unknown) in
            let neg_num_values = (new_id (), NumType, Unknown) in
            let summed_values = (new_id (), NumType, Unknown) in
            let ids_used = concat (map list_ids_used observed_ids) in
            let prog = move_let_after (id_name p) ids_used prog in
            let new_let next =
              Let (num_values, Num (float_of_int (length observed_ids)),
              Let (neg_num_values, Num (-. float_of_int (length observed_ids)),
              Let (summed_values, Prim ("plus", neg_num_values :: observed_ids),
              Let (na, Prim ("plus", [a; num_values]),
              Let (nb, Prim ("plus", [b; summed_values]),
              Let (p, Prim ("beta", [na; nb]),
                   next))))))
            in
            let new_observe next =
              UnvaluedObserve ("beta_geometric", [na; nb; num_values; summed_values], next)
            in
            let prog = replace_let (id_name p) new_let prog in
            let prog = replace_observe ("geometric", [p], hd observed_ids) new_observe prog in
            let prog = remove_observes "geometric" [p] observed_ids prog in
            let prog = rebuild_values prog in
            (true, prog)

          else (false, prog)

        in
        let flip_try = try_flip_observes () in
        if fst flip_try then flip_try else

        let geometric_try = try_geometric_observes () in
        if fst geometric_try then geometric_try else

        commute_sample_observe_expr expr

    (* normal sample, normal observe *)
    | Observe ("normal", [m2; b2], value, next) ->
        (try
          (match is_linear_of_prim "normal" m2 prog with
          | Some (prim_id, m1, b1, coeff, const, dependent_ids, gen_values) ->
              if    is_direct_path_from_let_to_expr prim_id (Observe ("normal", [m2; b2], value, next)) prog
                 && is_determined m1
                 && is_determined b1
                 && is_determined b2
                 && is_determined value
              then
                let ids_used =
                  concat [dependent_ids;
                          list_ids_used b2;
                          list_ids_used value] in
                let prog = move_let_after prim_id ids_used prog in
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
                let prog = replace_observe ("normal", [m2; b2], value) new_observe prog in
                let prog = rebuild_values prog in
                (true, prog)

              else commute_sample_observe_expr next
          | None -> commute_sample_observe_expr next)
        with Not_found -> commute_sample_observe_expr next)

    (* now just recurse *)
    | Let (id, value, expr) ->
        commute_sample_observe_expr expr

    | If (test, then_expr, else_expr) ->
        let (c, prog) = commute_sample_observe_expr then_expr in
        if c then (c, prog)
        else commute_sample_observe_expr else_expr

    | Observe (prim, args, value, next) ->
        commute_sample_observe_expr next

    | UnvaluedObserve (prim, args, next) ->
        commute_sample_observe_expr next

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
      if fold_right (&&) (map is_const (value :: args)) true then
        (true, next)
      else
        (c, Observe (prim, args, value, next))

  | UnvaluedObserve (prim, args, next) ->
      let (c, next) = remove_const_observe next in
      if fold_right (&&) (map is_const args) true then
        (true, next)
      else
        (c, UnvaluedObserve (prim, args, next))

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
    else if level = 1 then [ local; merge_arithmetic ]
    else [ local; merge_arithmetic; merge_samples; commute_sample_observe; remove_const_observe ]
  in
  transform_K_Prime (optimise_prime rules (transform_K prog))
