let id_prefix = "id_"

let last_id = ref 0
let new_id () =
  last_id := !last_id + 1;
  id_prefix ^ (string_of_int !last_id)

let rec id_index id =
  let l = String.length id in
  let s = String.length id_prefix in
  String.sub id s (l - s)

let indent_level = 2
let indent n = "\n" ^ String.make n ' '

let map_and_concat f sep xs = String.concat sep (List.map f xs)

let rec remove_dups = function
  | [] -> []
  | x :: xs ->
      x :: (remove_dups (List.filter (fun y -> x <> y) xs))
