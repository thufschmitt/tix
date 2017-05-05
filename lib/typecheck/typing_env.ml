module StrMap = Map.Make(String)

type t = Tix_types.t StrMap.t

let empty = StrMap.empty

let singleton = StrMap.singleton

let add = StrMap.add

let merge =
  let merge_fun _ x y = match x,y with
    | Some a, Some b when a = b -> Some a
    | Some _, Some _ -> assert false
    (* TODO: implement a smarter merge as soon as intersection types are available *)
    | None, Some a
    | Some a, None -> Some a
    | None, None -> None
  in
  StrMap.merge merge_fun

let lookup map elt = StrMap.find elt map
