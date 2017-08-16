(**
   Conversion between [Parse.Ast.t] and [Ast.t]
*)
module A = Common.Type_annotations
module O = Parse.Ast
module N = Ast

module Loc = Common.Location
module WL = Loc.With_loc

let map_loc = WL.map

let rec partition_binop op = function
  | [] -> []
  | hd::_ as l ->
    let (partition_elt, rest) =
      CCList.partition (op hd) l
    in partition_elt :: partition_binop op rest

let filter_inherit fields =
    CCList.partition_map
      (function
        | { WL.description = O.Finherit _; _ } as f -> `Right f
        | { WL.description = O.Fdef (ap, value); location} ->
          `Left { WL.description = (ap, value); location; })
      fields

let rec flatten (fields : ((O.access_path * A.t option) * O.expr) WL.t list) :
  ((O.ap_field * A.t option) * O.expr) WL.t list =
  let flattened_record fields =
    O.(Erecord {
        recursive = false;
        fields =
          List.map
            (WL.map
               (fun ((apf, annot), expr) ->
                  Fdef (([apf], annot), expr))) fields;
      })
  in
  (* Invariant: all the access paths are non-empty *)
  let partitionned_by_first_element = partition_binop
      (CCFun.compose_binop (fun f ->
           (CCList.hd @@ fst @@ fst f.WL.description).WL.description)
          (=))
      fields
  in
  List.map
    (function
      | { WL.description = (([], _), _); _ } :: _
      | [] -> assert false (* A record must have at least one field *)
      | [ { WL.description = (([ident], annot),e); location = _ } as field ] ->
        { field with WL.description = ((ident, annot), e) }
      | { WL.description = ((ident::_, _), _); location = loc } :: _
        as fields ->
        let sub_fields =
          List.map
            (WL.map @@ function
              | ((_::(_::_ as tl), annot), e) -> ((tl, annot), e)
              | _ -> Format.ksprintf failwith
                       "The field %s is defined several times"
                       (Parse.Pp.pp_ap_field Format.str_formatter ident;
                        Format.flush_str_formatter ()))
            fields
        in
        {WL.description = ((ident, None),
                          WL.mk loc @@ flattened_record (flatten sub_fields));
         location = loc;
        })
    partitionned_by_first_element

let operator : O.operator -> N.operator = function
  | O.Ocons -> N.Ocons
  | O.Oeq   -> N.Oeq
  | O.Oneg  -> N.Oneg
  | O.Oplus -> N.Oplus
  | O.Ominus-> N.Ominus

let rec expr_desc : O.expr_desc -> N.expr_desc = function
  | O.Evar s -> N.Evar s
  | O.Econstant c -> N.Econstant (constant c)
  | O.Elambda (pat, e) -> lambda pat e
  | O.EfunApp (e1, e2) -> N.EfunApp (expr e1, expr e2)
  | O.EtyAnnot (e, t)  -> N.EtyAnnot (expr e, t)
  | O.EopApp (o, args) -> N.EopApp (operator o, List.map expr args)
  | O.Elet (binds, e) -> N.Elet (bindings binds, expr e)
  | O.Eite (e0, e1, e2) -> N.Eite (expr e0, expr e1, expr e2)
  (* TODO: smarter compilation of some form of if-then-else *)
  | O.Epragma (p, e) -> N.Epragma (p, expr e)
  | O.Eimport e -> N.Eimport (expr e)
  | O.Erecord r -> record r
  | O.Eaccess (e, ap, default) ->
    N.EaccessPath (expr e, access_path ap, CCOpt.map expr default)
  | _ -> failwith "Not implemented"

and access_path ap = List.map ap_field ap

and apf_to_expr = function
  | O.AFexpr e -> e.WL.description
  | O.AFidentifier s -> O.Econstant (O.Cstring s)

and ap_field f = expr @@ map_loc apf_to_expr f

and bindings b =
  let non_inherit_fields, _ = filter_inherit b in
  let b = flatten non_inherit_fields in
  List.map binding b

and binding b =
  let ((apf, annot), e) = WL.description b in
  match WL.description apf with
  | O.AFidentifier s ->
    ((s, annot), expr e)
  | O.AFexpr _ ->
    failwith "Dynamic let-bindings are not allowed"

and expr e = map_loc expr_desc e


and open_flag = function
  | O.Open -> N.Open
  | O.Closed -> N.Closed

and pattern_record_field { O.field_name; default_value; type_annot } =
  ((field_name, (CCOpt.is_some default_value, type_annot)),
   CCOpt.map (fun e -> (field_name, type_annot), expr e)default_value)


and nontrivial_pattern :
  O.nontrivial_pattern -> N.nontrivial_pattern * N.binding list
  = function
    | O.NPrecord (fields, flag) ->
      let new_fields, default_values =
        List.map pattern_record_field fields
        |> List.split
      in
      let default_values = CCList.flat_map CCOpt.to_list default_values
      and fields =
        try Record.of_list_uniq new_fields
        with Invalid_argument _ -> failwith "Duplicate element in pattern"
      in
      N.NPrecord (fields, open_flag flag), default_values

and pattern_desc : O.pattern_desc -> N.pattern_desc * N.binding list
  = function
    | O.Pvar (s, mt) -> N.Pvar (s, mt), []
    | O.Pnontrivial (sub_pat, alias) ->
      let sub_pat, default_values = nontrivial_pattern sub_pat in
      N.Pnontrivial (sub_pat, alias), default_values

and pattern p =
  let loc = WL.loc p in
  let new_pat, default_values = pattern_desc @@ WL.description p in
  (WL.mk loc new_pat, default_values)

and constant = function
  | O.Cint i -> N.Cint i
  | O.Cbool b -> N.Cbool b
  | O.Cstring s -> N.Cstring s

and record r =
  let { O.fields; recursive } = r in
  if recursive then
    let loc = List.hd fields
            |> WL.loc
    in
    let created_bindings = bindings fields in
    let new_record =
      N.Erecord (List.map (fun ((var, annot), { WL.location = loc; _}) ->
          (WL.mk loc @@ N.Econstant (N.Cstring var),
           annot,
           WL.mk loc @@ N.Evar var))
          created_bindings)
    in
    N.Elet (bindings fields, WL.mk loc new_record)
  else
    let non_inherit_fields, inherit_fields = filter_inherit fields
    in
    assert (inherit_fields = []); (* Not handled now *)
    let new_record = List.map
        (fun { WL.description = ((apf, annot), e); location  } ->
           (expr @@ WL.mk location @@ apf_to_expr (WL.description apf), annot,  expr e))
        (flatten non_inherit_fields)
    in N.Erecord new_record

and lambda pat e =
  let new_pat, default_values = pattern pat
  and loc = WL.loc e in
  let mangle_name = (^) "%%" in
  let mangled_values_def =
    List.map
      (* [let %%x /*: t | %%undef */ = x ] *)
      (fun ((var, annot), e) ->
         let annot = CCOpt.map
             (fun a ->
                let loc = WL.loc a in
                WL.mk loc
                  A.(Infix (
                      Infix_constructors.Or, WL.mk loc (Var "%%undef"), a)))
             annot
         in
         ((mangle_name var, annot),
          WL.mk (WL.loc e) @@ N.Evar var))
      default_values
  in
  let substitute_values =
    List.map
      (fun ((var, annot), e) ->
         let loc = WL.loc e in
         let al = WL.mk loc in
         let new_expr =
           (* [if isUndef [%e %%var] then [%e e] else [%e %%var]] *)
           al @@ N.Eite
             (al @@ N.EfunApp
                (al @@ N.Evar "%%isUndef", al @@ N.Evar (mangle_name var)),
              e,
              al @@ N.Evar (mangle_name var))
         in
         ((var, annot), new_expr))
      default_values
  in

  let body =
    if default_values = [] then expr e else
      WL.mk loc
        (N.Elet (mangled_values_def,
                 (WL.mk loc
                    (N.Elet (substitute_values, expr e)))))
  in
  N.Elambda (new_pat, body)
