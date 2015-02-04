let id_prefix = "id_"

(* returns an unique id every time it is called *)
(* new_id :: unit -> string *)
let last_id = ref 0
let new_id () =
  last_id := !last_id + 1;
  id_prefix ^ (string_of_int !last_id)

(* removes the id_prefix from an id to return the number *)
(* id_index :: string -> string *)
let rec id_index id =
  let l = String.length id in
  let s = String.length id_prefix in
  String.sub id s (l - s)

(* indent :: int -> string *)
let indent n = "\n" ^ String.make n ' '

(* map_and_concat :: ('a -> string) -> string -> 'a list -> string *)
let map_and_concat f sep xs = String.concat sep (List.map f xs)

(* returns a list containing n copies of the element a *)
(* duplicate :: int -> 'a -> 'a list *)
let rec duplicate n a =
  if n = 0 then [] else a :: duplicate (n - 1) a

(* removes duplicates from a list *)
(* remove_dups :: 'a list -> 'a list *)
let rec remove_dups = function
  | [] -> []
  | x :: xs ->
      x :: (remove_dups (List.filter (fun y -> x <> y) xs))

(* returns true iff xs contains an element that satisfies pred *)
(* contains :: ('a -> bool) -> 'a list -> bool *)
let contains pred xs =
	try let _ = List.find pred xs in true
  with Not_found -> false

(* eq, neq :: 'a -> 'a -> bool *)
let eq a b = (a = b)
let neq a b = not (a = b)

(* returns the last element of a list *)
(* last :: 'a list -> 'a *)
let rec last = function
  | [] -> raise (Failure "Trying to get last item of empty list")
  | [x] -> x
  | x :: xs -> last xs

(* returns all but the last element of a list *)
(* remove_last :: 'a list -> 'a list *)
let rec remove_last = function
  | [] -> raise (Failure "Trying to remove last item of empty list")
  | [x] -> []
  | x :: xs -> x :: remove_last xs



(* returns true iff the given prim is probabilistic *)
(* is_probabilistic_prim :: string -> bool *)
and is_probabilistic_prim = function
  | "beta" -> true
  | "flip" -> true
  | "gamma" -> true
  | "normal" -> true
  | "poisson" -> true
  | "exponential" -> true
  | "uniform-continuous" -> true
  | "uniform-discrete" -> true
  | "discrete" -> true
  | "categorical" -> true
  | _ -> false
