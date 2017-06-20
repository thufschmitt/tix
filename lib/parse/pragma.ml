module F = Format

type sign = Plus | Minus

let show_sign = function
  | Plus -> "+"
  | Minus -> "-"

module Warning = struct
  type t =
    | TypeError

  let compare (a: t) b = Pervasives.compare a b

  let show = function
    | TypeError -> "TypeError"

  let read = function
    | "TypeError" -> TypeError
    | _ -> raise (Invalid_argument "Config.Warnings.Warning.read")
end

type t =
  | Warnings of (sign * string) list
  | Errors of (sign * string) list

let pp_signValue_pair fmt (sign, value) =
  F.fprintf fmt "%s%s"
    (show_sign sign)
    value

let pp_warn_list fmt =
  F.pp_print_list ~pp_sep:(fun _ () -> ()) pp_signValue_pair fmt

let pp fmt = function
  | Warnings warns ->
    F.fprintf fmt "WARN %a" pp_warn_list warns
  | Errors errors ->
    F.fprintf fmt "ERRORS %a" pp_warn_list errors
