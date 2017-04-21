module P = Nl_ast
module F = Format

let drop_loc { Onix_location.With_loc.description = it; _ } = it
let (%>) f g x = g (f x)

let ident = F.pp_print_string
let kwd   = F.pp_print_string

let pp_op fmt = function
  | P.Ocons -> F.pp_print_string fmt "Cons"

let const fmt = function
  | P.Cbool b -> F.pp_print_bool fmt b
  | P.Cint i-> F.pp_print_int  fmt i
  | P.Cstring s -> F.pp_print_string fmt s
  | P.Cnil -> F.pp_print_string fmt "nil"

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
  | P.EtyAnnot (e, ty) ->
      F.fprintf fmt "@[(%a /*:@ %a */)@]"
        pp_expr e
        pp_typ ty
  | P.EopApp (op, args) ->
      F.fprintf fmt "@[%a(%a)@]"
        pp_op op
        pp_op_args args
  | _ -> failwith "TODO"

and pp_pattern fmt = drop_loc %> function
  | P.Pvar v -> ident fmt v
  | _ -> failwith "TODO"

and pp_typ fmt = Tix_types.pp fmt

and pp_op_args fmt = function
  | [] -> ()
  | [a] ->
      pp_expr fmt a
  | a::tl ->
      F.fprintf fmt "%a,@ %a"
        pp_expr a
        pp_op_args tl
