(**
   Pretty-printer the [Ast.t]
*)
module P = Ast
module F = Format

let drop_loc { Parse.Location.With_loc.description = it; _ } = it
let (%>) f g x = g (f x)

let pp_ident = F.pp_print_string
let kwd   = F.pp_print_string

let pp_option printer (fmt : F.formatter) = CCOpt.iter (fun x -> printer fmt x)

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
  | P.Cundef -> F.pp_print_string fmt "%%undef"

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
    | P.Erecord r ->
      F.fprintf fmt "@[{@;%a}@]"
        pp_fields r
    | P.EaccessPath (e, ap, None) -> pp_ap fmt e ap
    | P.Elet (bindings, e) ->
      F.fprintf fmt "@[let %ain@;%a@]"
        pp_bindings bindings
        pp_expr e
    | P.Eite (eif, ethen, eelse) ->
      F.fprintf fmt "@[if (%a)@;then@ %a@;else@ %a@]"
        pp_expr eif
        pp_expr ethen
        pp_expr eelse
    | _ -> failwith "TODO"

and pp_bindings fmt =
  Format.pp_print_list
    ~pp_sep:(fun _ () -> ())
    pp_binding
    fmt

and pp_binding fmt = function
    (pat, e) ->
    Format.fprintf fmt "%a = %a; "
      pp_pattern_var pat
      pp_expr e

and pp_ap fmt e ap =
  F.pp_print_list
    ~pp_sep:(fun fmt () -> F.pp_print_char fmt '.')
    pp_expr
    fmt
    (e::ap)

and pp_fields fmt =
  F.pp_print_list pp_field fmt

and pp_field fmt = function (e1, maybe_annot, e2) ->
  F.fprintf fmt "%a%a = %a; "
    pp_expr e1
    (pp_option pp_type_annot) maybe_annot
    pp_expr e2

and pp_pattern fmt = drop_loc %> function
    | P.Pvar (v, None) -> pp_ident fmt v
    | P.Pvar (v, Some t) ->
      F.fprintf fmt "(%a: %a)"
        pp_ident v
        pp_typ   t
    | P.Pnontrivial (sub_pattern, alias) ->
      pp_nontrivial_pattern fmt sub_pattern;
      pp_option (fun fmt var -> F.fprintf fmt "@%s" var) fmt alias

and pp_pattern_var fmt = function
  | (v, None) -> pp_ident fmt v
  | (v, Some t) ->
    F.fprintf fmt "%a %a"
      pp_ident v
      pp_type_annot   t

and pp_nontrivial_pattern fmt = function
  | P.NPrecord (f, P.Open) when Record.is_empty f ->
    F.pp_print_string fmt "{ ... }"
  | P.NPrecord (fields, open_flag) ->
    F.fprintf fmt "{ %a%s }"
      pp_pat_record_fields fields
      (match open_flag with
       | P.Closed -> ""
       | P.Open -> ", ...")

and pp_pat_record_fields fmt =
  Record.pp
    ~arrow:""
    ~sep:", "
    Format.pp_print_string
    pp_pat_record_field_args
    fmt

and pp_pat_record_field_args fmt (optional, type_annot) =
  F.fprintf fmt "%s%a"
    (if optional then "?" else "")
    (pp_option (fun fmt -> F.fprintf fmt " %a" pp_type_annot)) type_annot

and pp_type_annot fmt = F.fprintf fmt "/*: %a */" pp_typ

and pp_typ fmt = Parse.Type_annotations.pp fmt

and pp_op_args fmt = function
  | [] -> ()
  | [a] ->
    pp_expr fmt a
  | a::tl ->
    F.fprintf fmt "%a,@ %a"
      pp_expr a
      pp_op_args tl
