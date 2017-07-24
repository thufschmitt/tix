(**
 * Definition of the types used by tix
*)

let (%>) f g x = g (f x)

module Infix_constructors =
struct
  type t =
    | Arrow
    | And
    | Or

  let show = function
    | Arrow -> "->"
    | And   -> "&"
    | Or    -> "|"
end

module Singleton =
struct
  type t =
    | Int of int
    | Bool of bool
    | String of string

  let pp fmt = function
    | Int i -> Format.pp_print_int fmt i
    | Bool b -> Format.pp_print_bool fmt b
    | String s -> Format.fprintf fmt "\"%s\"" s

  let show = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | String s -> "\"" ^ s ^ "\""
end

type _t =
  | Var of string
  | Gradual
  | Singleton of Singleton.t
  | Infix of Infix_constructors.t * t * t
  | Cons  of t * t
  | TyBind of bindings * t

and t = _t Location.With_loc.t

and bindings = (string * t ) list

let rec pp fmt = Location.With_loc.description %> function
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
    | TyBind (binds, t) ->
      Format.fprintf fmt "%a where %a"
        pp t
        pp_bindings binds
    | Singleton s -> Singleton.pp fmt s
    | Gradual -> Format.pp_print_string fmt "?"

and pp_bindings fmt =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " and\n")
    pp_binding
    fmt

and pp_binding fmt (name, typ) =
  Format.fprintf fmt "%s = %a"
    name
    pp typ

let show t =
  let () = pp Format.str_formatter t in
  Format.flush_str_formatter ()
