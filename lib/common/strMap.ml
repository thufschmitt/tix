module M = CCMap.Make(CCString)

include M

let add_or key value map =
  match M.get key map with
  | Some value2 -> Error (key, value, value2)
  | None -> Ok (M.add key value map)
