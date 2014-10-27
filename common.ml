let last_id = ref 0
let new_id () =
  last_id := !last_id + 1;
  "genid_" ^ (string_of_int !last_id)
