(**
   Pretty-printer the [Ast.t]
*)

module P = Ast
module T = Type_annotations
module F = Format

let drop_loc = Location.With_loc.description
let (%>) f g x = g (f x)

let pp_ident = F.pp_print_string
let kwd   = F.pp_print_string

let const fmt = function
  | P.Cbool b -> F.pp_print_bool fmt b
  | P.Cint i-> F.pp_print_int  fmt i
  | P.Cstring s -> F.fprintf fmt "\"%s\"" s

let pp_op fmt = function
  | P.Ocons -> F.pp_print_string fmt "Cons"
  | P.Oeq   -> F.pp_print_string fmt "+"
  | P.Oneg  -> F.pp_print_string fmt "-"
  | P.Oplus  -> F.pp_print_string fmt "+"
  | P.Ominus -> F.pp_print_string fmt "-"

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
    (* XXX: This is printed in an ugly way and not parsable way *)
    | P.EopApp (op, args) ->
      F.fprintf fmt "@[%a(%a)@]"
        pp_op op
        pp_op_args args
    | P.Elet (bindings, e) ->
      F.fprintf fmt "@[%a@ in@;%a@]"
        pp_bindings bindings
        pp_expr e
    | P.Erecord r -> pp_record fmt r
    | P.Epragma (pragma, e) ->
      F.fprintf fmt "#:: %a\n%a"
        Pragma.pp pragma
        pp_expr e
    | P.Eite (eif, ethen, eelse) ->
      F.fprintf fmt "@[if (%a)@;then@ %a@;else@ %a@]"
        pp_expr eif
        pp_expr ethen
        pp_expr eelse
    | P.Eaccess (e, ap, None) ->
      F.fprintf fmt "%a.%a"
        pp_expr e
        pp_ap ap
    | _ -> failwith "TODO"

and pp_ap fmt = F.pp_print_list
    ~pp_sep:(fun fmt () -> F.pp_print_char fmt '.')
    pp_ap_field
    fmt

and pp_ap_field fmt = drop_loc %> function
    | P.AFexpr e ->
      F.fprintf fmt "${%a}" pp_expr e
    | P.AFidentifier s -> F.pp_print_string fmt s

and pp_pattern fmt = drop_loc %> function
    | P.Pvar (v, a) -> pp_pattern_var fmt (v, a)
    | P.Pnontrivial (sub_pattern, None) ->
      pp_nontrivial_pattern fmt sub_pattern
    | _ -> failwith "TODO"

and pp_pattern_var fmt = function
  | (v, None) -> pp_ident fmt v
  | (v, Some t) ->
    F.fprintf fmt "%a /*: %a */"
      pp_ident v
      pp_typ   t

and pp_nontrivial_pattern fmt = function
  | P.NPrecord (fields, P.Closed) ->
    F.fprintf fmt "{ %a }"
      pp_pat_record_fields fields
  | _ -> failwith "TODO"

and pp_pat_record_fields fmt = function
  | [] -> ()
  | [f] -> pp_pat_record_field fmt f
  | f::tl ->
    pp_pat_record_field fmt f;
    F.pp_print_string fmt ", ";
    pp_pat_record_fields fmt tl

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
        pp_ap name
        pp_expr value
    | _ -> failwith "TODO"

and pp_bindings fmt =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;and@ ")
    pp_binding
    fmt

and pp_binding fmt = function
  | P.BstaticDef (pat, e) ->
    Format.fprintf fmt "%a = %a"
      pp_pattern_var pat
      pp_expr e
  | _ -> assert false
