module StrMap = Map.Make(String)

type t = Tix_types.t StrMap.t

let empty = StrMap.empty

let add = StrMap.add

let lookup map elt = StrMap.find elt map
