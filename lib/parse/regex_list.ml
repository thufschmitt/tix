let (%>) f g x = g (f x)

module L = Location.With_loc
module T = Type_annotations

module Counter = functor () -> struct
  let counter = ref 0

  let incr () = incr counter
  let get () = !counter

  let pop () =
    let res = get () in
    incr (); res
end

module FreshVars = functor () -> struct
  module C = Counter ()

  let get () = "X" ^ (string_of_int @@ C.get ())
  let pop () = "X" ^ (string_of_int @@ C.pop ())
end

type _t =
  | Type of T.t
  | Or of t * t
  | Concat of t * t
  | Star of t
  | Plus of t
  | Maybe of t
  | Empty

and t = _t L.t

let to_type (regex: t) : T.t =
  let module V = FreshVars () in
  let rec aux (regex : t) : (T.t * string) =
    let loc = L.loc regex in
    match L.description regex with
    | Type t ->
      let tail = V.pop () in
      (L.mk loc @@ T.Cons (t, L.mk loc @@ T.Var tail), tail)
    | Or (r1, r2) ->
      let (t1, tl1) = aux r1
      and (t2, tl2) = aux r2
      in
      (L.mk loc @@ T.TyBind (
          [tl1, L.mk loc @@ T.Var tl2],
          L.mk loc @@ T.Infix (T.Infix_constructors.Or, t1, t2)),
       tl2)
    | Concat (r1, r2) ->
      let (t1, tl1) = aux r1
      and (t2, tl2) = aux r2
      in
      (L.mk loc @@ T.TyBind ([tl1, t2], t1), tl2)
    | Star r ->
      let (t, tl) = aux r
      and new_tail = V.pop () in
      (L.mk loc @@ T.TyBind ([(
           tl, L.mk loc @@ T.Infix (T.Infix_constructors.Or,
                                    t, L.mk loc @@ T.Var new_tail))],
                             L.mk loc @@ T.Var tl),
       new_tail)
    | Plus r -> aux (L.mk loc @@ Concat(r, L.mk loc @@ Star r))
    | Maybe r -> aux (L.mk loc @@ Or (r, L.mk loc @@ Empty))
    | Empty ->
      let tail = V.pop () in
      (L.mk loc @@ T.Var tail, tail)
  in
  let (typ, tail) = aux regex in
  let loc = L.loc regex in
  L.mk loc @@ T.TyBind ([tail, L.mk loc @@ T.Var "nil"], typ)
