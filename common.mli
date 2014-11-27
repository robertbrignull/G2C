val new_id : unit -> string

val id_index : string -> string

val indent : int -> string

val map_and_concat : ('a -> string) -> string -> 'a list -> string

val remove_dups : 'a list -> 'a list
