type id = string

type type_c =
  | NumType
  | BoolType
  | CompoundType of type_c * type_c

type expr =
  | Bool of bool
  | Num of float
  | Id of id
  | Lambda of ((id * type_c) list) * expr
  | Let of id * expr * expr
  | If of expr * expr * expr
  | Op of id * expr list
  | App of expr * expr list
