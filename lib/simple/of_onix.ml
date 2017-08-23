(**
   Conversion between [Parse.Ast.t] and [Ast.t]
*)
module A = Common.Type_annotations
module O = Parse.Ast
module N = Ast

module Loc = Common.Location
module W = Common.Writer.Make (Common.Warning.List)
module WL = Loc.With_loc

open W.Infix

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
        | { WL.description = O.Finherit (e, fields); _ } -> `Right (e, fields)
        | { WL.description = O.Fdef (ap, value); location} ->
          `Left { WL.description = (ap, value); location; })
      fields

let rec flatten (fields : ((O.access_path * A.t option) * O.expr) WL.t list) :
  ((O.ap_field * A.t option) * O.expr) WL.t list W.t =
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
  W.map_l
    (function
      | { WL.description = (([], _), _); _ } :: _ -> assert false
        (* The access-path can't be empty *)
      | [] -> assert false (* A record must have at least one field *)
      | [ { WL.description = (([ident], annot),e); location = _ } as field ] ->
        W.return { field with WL.description = ((ident, annot), e) }
      | { WL.description = ((ident::_, annot), _); location = loc } :: _
        as fields ->
        let module E = struct
          exception MultipleField of Common.Location.t * O.expr
        end in
        begin try
            let sub_fields =
              W.map_l
                (fun { WL.description; location } ->
                   let description = match description with
                     | ((_::(_::_ as tl), annot), e) ->
                       W.return ((tl, annot), e)
                     | ((_::_, _), e) ->
                       raise (E.MultipleField (location, e))
                     | (([],_),_) -> assert false (* This shouldn't happen *)
                   in
                   W.append
                     (W.log description)
                     (W.return
                        { WL.description = W.value description; location; }))

                fields
            in
            sub_fields >>= fun sub_fields ->
            flatten sub_fields >|= fun flattened ->
            {WL.description = ((ident, None),
                               WL.mk loc @@ flattened_record flattened);
             location = loc;
            }
          with
            E.MultipleField (loc, e) ->
            W.append
              [ Common.Warning.make loc @@
                Format.sprintf
                  "The field %s is defined several times"
                  (Parse.Pp.pp_ap_field Format.str_formatter ident;
                   Format.flush_str_formatter ())]
          (W.return
             { WL.description = ((ident, annot), e); location = loc })
        end
    )
    partitionned_by_first_element

let binop : O.binop -> N.binop = function
  | O.Ocons -> N.Ocons
  | O.Oeq   -> N.Oeq
  | O.Oplus -> N.Oplus
  | O.Ominus-> N.Ominus
  | O.Oand-> N.Oand
  | O.Oor-> N.Oor
  | O.Omerge -> N.Omerge
  | O.OnonEq
  | O.Oimplies -> assert false (* treated separately *)

let monop : O.monop -> N.monop = function
  | O.Onot-> N.Onot
  | O.Oneg  -> N.Oneg

let rec expr_desc : O.expr_desc -> N.expr_desc W.t = function
  | O.Evar s -> W.return @@ N.Evar s
  | O.Econstant c -> W.return @@ N.Econstant (constant c)
  | O.Elambda (pat, e) -> lambda pat e
  | O.EfunApp (e1, e2) ->
    funApp e1 e2
  | O.EtyAnnot (e, t)  -> expr e >|= fun e -> N.EtyAnnot (e, t)
  | O.Ebinop (O.OnonEq, e1, e2) ->
    expr e1 >>= fun e1 ->
    expr e2 >|= fun e2 ->
    N.Emonop (N.Onot, WL.mk
                         (WL.loc e1)
                         (N.Ebinop (N.Oeq, e1, e2)))
  | O.Ebinop (O.Oimplies, e1, e2) ->
    let e1_loc = WL.loc e1 in
    expr e1 >>= fun e1 ->
    expr e2 >|= fun e2 ->
    N.Ebinop (N.Oor, e2, WL.mk e1_loc (N.Emonop (N.Onot, e1)))
  | O.Ebinop (o, e1, e2) ->
    expr e1 >>= fun e1 ->
    expr e2 >|= fun e2 ->
    N.Ebinop (binop o, e1, e2)
  | O.Emonop (o, e) ->
    expr e >|= fun e ->
    N.Emonop (monop o, e)
  | O.Elet (binds, e) ->
    bindings binds >>= fun binds ->
    expr e >|= fun e ->
    N.Elet (binds, e)
  | O.Eite (e0, e1, e2) ->
    expr e0 >>= fun e0 ->
    expr e1 >>= fun e1 ->
    expr e2 >|= fun e2 ->
    N.Eite (e0, e1, e2)
  (* TODO: smarter compilation of some form of if-then-else *)
  | O.Epragma (p, e) -> expr e >|= fun e -> N.Epragma (p, e)
  | O.Erecord r -> record r
  | O.Eaccess (e, ap, default) ->
    expr e >>= fun e ->
    access_path ap >>= fun ap ->
    W.map_opt expr default >|= fun default ->
    N.EaccessPath (e, ap, default)
  | O.EtestMember (e, ap) ->
    expr e >>= fun e ->
    (access_path ap
     |> if List.length ap > 1 then
       W.append
         [Common.Warning.(make ~kind:Warning (WL.loc (CCList.hd ap))
                            "The tail of this access_path has been dropped")]
     else fun x -> x)
    >|= fun ap ->
    N.Ebinop (N.OrecordMember, e, List.hd ap)
    (* FIXME: don't drop the tail of the access_path *)
  | O.Ewith (e1, e2) ->
    expr e1 >>= fun e1 ->
    expr e2 >|= fun e2 ->
    N.Ewith (e1, e2)

and access_path ap = W.map_l ap_field ap

and apf_to_expr = function
  | O.AFexpr e -> e.WL.description
  | O.AFidentifier s -> O.Econstant (O.Cstring s)

and ap_field f = expr @@ map_loc apf_to_expr f

and bindings b =
  let non_inherit_fields, _ = filter_inherit b in
  flatten non_inherit_fields >>= fun b ->
  W.map_l binding b

and binding b =
  let ((apf, annot), e) = WL.description b in
  expr e >>= fun e ->
  match WL.description apf with
  | O.AFidentifier s ->
    W.return ((s, annot), e)
  | O.AFexpr e' ->
    W.append [Common.Warning.make
                ~kind:Common.Warning.Error
                (WL.loc e')
                "Dynamic let-bindings are not allowed"] @@
    W.return (("%%INVALID_LHS%%", annot), e)

and expr e =
  expr_desc (WL.description e) >|= fun description ->
  { e with WL.description }


and open_flag = function
  | O.Open -> N.Open
  | O.Closed -> N.Closed

and pattern_record_field { O.field_name; default_value; type_annot } =
  W.map_opt (fun e ->
      expr e >|= fun e ->
      (field_name, type_annot), e)
    default_value
  >|= fun value ->
  ((field_name, (CCOpt.is_some default_value, type_annot)), value)


and nontrivial_pattern loc :
  O.nontrivial_pattern -> (N.nontrivial_pattern * N.binding list) W.t
  = function
    | O.NPrecord (fields, flag) ->
      W.map_l pattern_record_field fields >|=
      List.split >>= fun (new_fields, default_values) ->
      let default_values = CCList.flat_map CCOpt.to_list default_values
      in
      begin
        try W.return @@ Record.of_list_uniq new_fields
        with Invalid_argument _ ->
          W.append
            [Common.Warning.make loc "Duplicate element in pattern"]
            (W.return Record.empty)
      end
      >|= fun fields ->
      N.NPrecord (fields, open_flag flag), default_values

and pattern_desc loc : O.pattern_desc -> (N.pattern_desc * N.binding list) W.t
  = function
    | O.Pvar (s, mt) -> W.return (N.Pvar (s, mt), [])
    | O.Pnontrivial (sub_pat, alias) ->
      nontrivial_pattern loc sub_pat >|= fun (sub_pat, default_values) ->
      N.Pnontrivial (sub_pat, alias), default_values

and pattern p =
  let loc = WL.loc p in
  pattern_desc loc @@ WL.description p >|= fun (new_pat, default_values) ->
  (WL.mk loc new_pat, default_values)

and constant = function
  | O.Cint i -> N.Cint i
  | O.Cbool b -> N.Cbool b
  | O.Cstring s -> N.Cstring s
  | O.Cpath s -> N.Cpath s
  | O.Cbracketed s -> N.Cbracketed s

and record r =
  let { O.fields; recursive } = r in
  if recursive then
    let loc = List.hd fields
              |> WL.loc
    in
    bindings fields >|= fun created_bindings ->
    let new_record =
      N.Erecord (List.map (fun ((var, annot), { WL.location = loc; _}) ->
          (WL.mk loc @@ N.Econstant (N.Cstring var),
           annot,
           WL.mk loc @@ N.Evar var))
          created_bindings)
    in
    N.Elet (created_bindings, WL.mk loc new_record)
  else
    let non_inherit_fields, inherit_fields = filter_inherit fields
    in
    W.map_l inherit_to_classic inherit_fields >>= fun inherit_fields ->
    flatten non_inherit_fields >>= fun flattened ->
    W.map_l
      (fun { WL.description = ((apf, annot), e); location  } ->
         apf_to_expr (WL.description apf)
         |> WL.mk location
         |> expr
         >>= fun label_expr ->
         expr e >|= fun rhs_expr ->
         (label_expr, annot,  rhs_expr))
      flattened
    >|= fun new_record ->
    N.Erecord (new_record @ CCList.flatten inherit_fields)

and lambda pat e =
  pattern pat >>= fun (new_pat, default_values) ->
  let loc = WL.loc e in
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
  expr e >|= fun body ->
  let body =
    if default_values = [] then body else
      WL.mk loc
        (N.Elet (mangled_values_def,
                 (WL.mk loc
                    (N.Elet (substitute_values, body)))))
  in
  N.Elambda (new_pat, body)

and funApp e1 e2 =
  expr e1 >>= fun e1 ->
  expr e2 >|= fun e2 ->
  match WL.description e1 with
  | N.Evar "import" -> N.Eimport e2
  | _ -> N.EfunApp (e1, e2)

and inherit_to_classic ((base_expr, fields) : O.inherit_)
  : N.field list W.t =
  let mk_classic { WL.description = name; location = loc } =
    let value = match base_expr with
      | None -> W.return @@ WL.mk loc @@ N.Evar name
      | Some e ->
        expr e >|= fun e ->
        WL.mk loc
        @@ N.EaccessPath (e, [WL.mk loc @@ N.Econstant (N.Cstring name)], None)
    in
    value >|= fun value ->
    (WL.mk loc @@ N.Econstant (N.Cstring name), None, value)
  in
  W.map_l mk_classic fields

