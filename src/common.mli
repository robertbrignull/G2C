val new_id : unit -> string

val id_index : string -> string

val indent : int -> string

val map_and_concat : ('a -> string) -> string -> 'a list -> string

val duplicate : int -> 'a -> 'a list

val remove_dups : 'a list -> 'a list

val contains : ('a -> bool) -> 'a list -> bool

val last : 'a list -> 'a
val remove_last : 'a list -> 'a list



val is_probabilistic_prim : string -> bool
