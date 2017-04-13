module P = Onix_ast
module F = Format

let drop_loc { Onix_location.description = it; _ } = it
let (%>) f g x = g (f x)

let ident = F.pp_print_string
let kwd   = F.pp_print_string

let const fmt = function
  | P.Cbool b -> F.pp_print_bool fmt b
  | P.Cint i-> F.pp_print_int  fmt i

let rec pp_expr fmt = drop_loc %> function
  | P.Evar v ->
    ident fmt v
  | P.Econstant c ->
    const fmt c
  | P.Elambda (p, e) ->
      F.fprintf fmt "@[<2>(%a:@ %a)@]"
        pp_pattern p
        pp_expr e
  | P.EfunApp (e1, e2) ->
      F.fprintf fmt "@[%a@ %a@]"
        pp_expr e1
        pp_expr e2
  | _ -> failwith "TODO"

and pp_pattern fmt = drop_loc %> function
  | P.Pvar v -> ident fmt v
  | _ -> failwith "TODO"
