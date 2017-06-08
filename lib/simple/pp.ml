(**
   Pretty-printer the [Ast.t]
*)
module P = Ast
module F = Format

let drop_loc { Parse.Location.With_loc.description = it; _ } = it
let (%>) f g x = g (f x)

let pp_ident = F.pp_print_string
let kwd   = F.pp_print_string

let pp_op fmt = function
  | P.Ocons -> F.pp_print_string fmt "Cons"
  | P.Oeq   -> F.pp_print_string fmt "=="
  | P.Oneg  -> F.pp_print_string fmt "-"
  | P.Oplus  -> F.pp_print_string fmt "+"
  | P.Ominus -> F.pp_print_string fmt "-"

let const fmt = function
  | P.Cbool b -> F.pp_print_bool fmt b
  | P.Cint i-> F.pp_print_int  fmt i
  | P.Cstring s -> F.fprintf fmt "\"%s\"" s

let rec pp_expr fmt = drop_loc %> function
    | P.Evar v ->
      pp_ident fmt v
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
    | P.Pvar (v, None) -> pp_ident fmt v
    | P.Pvar (v, Some t) ->
      F.fprintf fmt "(%a: %a)"
        pp_ident v
        pp_typ   t
    | _ -> failwith "TODO"

and pp_typ fmt = Type_annotations.pp fmt

and pp_op_args fmt = function
  | [] -> ()
  | [a] ->
    pp_expr fmt a
  | a::tl ->
    F.fprintf fmt "%a,@ %a"
      pp_expr a
      pp_op_args tl
