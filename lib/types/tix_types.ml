type base =
  | Int
  | Bool

let show_base = function
  | Int -> "int"
  | Bool -> "bool"

let pp_base fmt b = Format.pp_print_string fmt (show_base b)

type t =
  | BaseType of base
  | Arrow of (t * t)

let rec pp fmt = function
  | BaseType b -> pp_base fmt b
  | Arrow (t1, t2) ->
    Format.fprintf fmt "(%a) -> %a"
      pp t1
      pp t2

let read_base : string -> base = function
  | "int" -> Int
  | "bool" -> Bool
  | s -> Format.ksprintf failwith "Unknown type %s" s
