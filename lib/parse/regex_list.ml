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

type t =
  | Type of T.t
  | Or of t * t
  | Concat of t * t
  | Star of t
  | Plus of t
  | Maybe of t
  | Empty

let to_type (regex: t) : T.t =
  let module V = FreshVars () in
  let rec aux : t -> (T.t * string) = function
    | Type t ->
      let tail = V.pop () in
      (T.Cons (t, T.Var tail), tail)
    | Or (r1, r2) ->
      let (t1, tl1) = aux r1
      and (t2, tl2) = aux r2
      in
      (T.TyBind (
          [tl1, T.Var tl2],
          T.Infix (T.Infix_constructors.Or, t1, t2)),
       tl2)
    | Concat (r1, r2) ->
      let (t1, tl1) = aux r1
      and (t2, tl2) = aux r2
      in
      (T.TyBind ([tl1, t2], t1), tl2)
    | Star r ->
      let (t, tl) = aux r
      and new_tail = V.pop () in
      (T.TyBind ([(tl, T.Infix (T.Infix_constructors.Or, t, T.Var new_tail))],
                 T.Var tl),
       new_tail)
    | Plus r -> aux (Concat(r, Star r))
    | Maybe r -> aux (Or (r, Empty))
    | Empty ->
      let tail = V.pop () in
      (T.Var tail, tail)
  in
  let (typ, tail) = aux regex in
  T.TyBind ([tail, T.Var "nil"], typ)
