module P = Onix_ast
module T = Tix_types
module F = Format

let drop_loc = Onix_location.With_loc.description
let (%>) f g x = g (f x)

let pp_ident = F.pp_print_string
let kwd   = F.pp_print_string

let const fmt = function
  | P.Cbool b -> F.pp_print_bool fmt b
  | P.Cint i-> F.pp_print_int  fmt i
  | P.Cnil -> F.pp_print_string fmt "nil"

let pp_op fmt = function
  | P.Ocons -> F.pp_print_string fmt "Cons"

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
      F.fprintf fmt "(@[%a@ %a@])"
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
  | P.Erecord r -> pp_record fmt r
  | _ -> failwith "TODO"

and pp_pattern fmt = drop_loc %> function
  | P.Pvar (v, None) -> pp_ident fmt v
  | P.Pvar (v, Some t) ->
    F.fprintf fmt "%a /*: %a */"
      pp_ident v
      pp_typ   t
  | P.Pnontrivial (sub_pattern, None) ->
    pp_nontrivial_pattern fmt sub_pattern
  | _ -> failwith "TODO"

and pp_nontrivial_pattern fmt = function
  | P.NPrecord (fields, P.Closed) ->
    F.fprintf fmt "{ %a }"
      pp_pat_record_fields fields
  | _ -> failwith "TODO"

and pp_pat_record_fields fmt = function
  | [] -> ()
  | [f] -> pp_pat_record_field fmt f
  | f::tl ->
    pp_pat_record_field fmt f; F.pp_print_string fmt ", "; pp_pat_record_fields fmt tl

and pp_pat_record_field fmt = function
  | { P.field_name; default_value = None; type_annot = None } ->
    pp_ident fmt field_name
  | { P.field_name; default_value = None; type_annot = Some t } ->
    F.fprintf fmt "%a /*: %a */"
      pp_ident field_name
      pp_typ   t
  | _ -> failwith "TODO"

and pp_typ fmt = T.pp fmt

and pp_op_args fmt = function
  | [] -> ()
  | [a] ->
      pp_expr fmt a
  | a::tl ->
      F.fprintf fmt "%a,@ %a"
        pp_expr a
        pp_op_args tl

and pp_record fmt { P.recursive; fields } =
  if recursive then F.pp_print_string fmt "rec ";
  F.fprintf fmt "@[{@ %a@]}"
    (fun fmt -> List.iter (pp_record_field fmt)) fields

and pp_record_field fmt = drop_loc %> function
  | P.Fdef (name, value) ->
    F.fprintf fmt "%a = %a;@ "
      pp_ident name
      pp_expr value
  | _ -> failwith "TODO"
