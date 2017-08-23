type 'a t =
  | Finite of 'a list
  | Infinite

let finite l = Finite l
let infinite = Infinite

let half_map f = function
  | Finite l -> finite @@ f l
  | Infinite -> Infinite

let map f = half_map (CCList.map f)

let fold f ~init ~full = function
  | Infinite -> full
  | Finite l -> CCList.fold_left f init l

let merge l1 l2 = match (l1, l2) with
  | Finite l1, Finite l2 -> Finite (l1 @ l2)
  | _ -> Infinite

let flatten l = fold merge ~init:(finite []) ~full:infinite l

let flat_map f l = map f l |> flatten

let concat l1 l2 = match (l1, l2) with
  | Finite l1, Finite l2 -> Finite (l1 @ l2)
  | _ -> Infinite
