(**
 * Definition of the types used by tix
*)

module Infix_constructors =
struct
  type t =
    | Arrow
    | And
    | Or

  let show = function
    | Arrow -> "->"
    | And   -> "|"
    | Or    -> "&"
end

type t =
  | Var of string
  | Infix of Infix_constructors.t * t * t
  | Cons  of t * t

let rec pp fmt = function
  | Var v -> Format.pp_print_string fmt v
  | Infix (constr, t1, t2) ->
    Format.fprintf fmt "(%a) %s %a"
      pp t1
      (Infix_constructors.show constr)
      pp t2
  | Cons (t1, t2) ->
    Format.fprintf fmt "Cons(%a, %a)"
      pp t1
      pp t2

let show t =
  let () = pp Format.str_formatter t in
  Format.flush_str_formatter ()
