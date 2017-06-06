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

module Singleton =
struct
  type t =
    | Int of int
    | Bool of bool

  let pp fmt = function
    | Int i -> Format.pp_print_int fmt i
    | Bool b -> Format.pp_print_bool fmt b

  let show = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
end

type t =
  | Var of string
  | Singleton of Singleton.t
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
  | Singleton s -> Singleton.pp fmt s

let show t =
  let () = pp Format.str_formatter t in
  Format.flush_str_formatter ()
