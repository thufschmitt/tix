type base =
  | Int
  | Bool
  | Nil (* The singleton type for [nil] *)
  | Gradual

let show_base = function
  | Int -> "int"
  | Bool -> "bool"
  | Nil  -> "nil"
  | Gradual -> "¿"

let pp_base fmt b = Format.pp_print_string fmt (show_base b)

type t =
  | BaseType of base
  | Arrow of (t * t)
  | Cons  of (t * t)

let rec pp fmt = function
  | BaseType b -> pp_base fmt b
  | Arrow (t1, t2) ->
    Format.fprintf fmt "(%a) -> %a"
      pp t1
      pp t2
  | Cons (t1, t2) ->
    Format.fprintf fmt "Cons(%a, %a)"
      pp t1
      pp t2

let read_base : string -> base = function
  | "int" -> Int
  | "bool" -> Bool
  | "nil" -> Nil
  | "¿" -> Gradual
  | s -> Format.ksprintf failwith "Unknown type %s" s
