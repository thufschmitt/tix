(**
 * Definition of the types used by tix
 *)

type t =
  | Var of string
  | Arrow of (t * t)
  | Cons  of (t * t)

let rec pp fmt = function
  | Var v -> Format.pp_print_string fmt v
  | Arrow (t1, t2) ->
    Format.fprintf fmt "(%a) -> %a"
      pp t1
      pp t2
  | Cons (t1, t2) ->
    Format.fprintf fmt "Cons(%a, %a)"
      pp t1
      pp t2

let show t =
  let () = pp Format.str_formatter t in
  Format.flush_str_formatter ()
