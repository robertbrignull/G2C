let last_id = ref 0
let new_id () =
  last_id := !last_id + 1;
  "genid_" ^ (string_of_int !last_id)

let indent_level = 2
let indent n = "\n" ^ String.make n ' '

let map_and_concat f sep xs = String.concat sep (List.map f xs)

let rec remove_dups = function
  | [] -> []
  | x :: xs ->
      x :: (remove_dups (List.filter (fun y -> x <> y) xs))
