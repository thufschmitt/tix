module StrMap = Map.Make(String)

type t = Types.t StrMap.t

let empty = StrMap.empty

let singleton = StrMap.singleton

let add = StrMap.add

let merge =
  let merge_fun _ x y = match x,y with
    | _, Some a
    | Some a, None -> Some a
    | None, None -> None
  in
  StrMap.merge merge_fun

let lookup map elt =
  try
    Some (StrMap.find elt map)
  with Not_found -> None
