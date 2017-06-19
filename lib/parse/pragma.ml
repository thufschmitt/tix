module F = Format

type sign = Plus | Minus

let show_sign = function
  | Plus -> "+"
  | Minus -> "-"

type t =
  | Warnings of (sign * int) list
  | Errors of (sign * int) list

let pp_signValue_pair fmt (sign, value) =
  F.fprintf fmt "%s%i"
    (show_sign sign)
    value

let pp_warn_list fmt =
  F.pp_print_list ~pp_sep:(fun _ () -> ()) pp_signValue_pair fmt

let pp fmt = function
  | Warnings warns ->
    F.fprintf fmt "WARN %a" pp_warn_list warns
  | Errors errors ->
    F.fprintf fmt "ERRORS %a" pp_warn_list errors
