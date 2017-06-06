module StrMap = CCMap.Make(String)

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

let initial_values =
  let open Types.Builtins in
  let int = Types.node int
  and true_type = Types.node true_type
  and any = Types.node any
  and false_type = Types.node false_type in
  [
    "nil", nil;
    "__add", arrow int (Types.node @@ arrow int int);
    "__not", cap (arrow true_type false_type) (arrow false_type true_type);
    "head_int", arrow (Types.node @@ cons int any) int;
  ]

let initial = StrMap.of_list initial_values
