(**
   Conversion between [Parse.Ast.t] and [Ast.t]
*)
module O = Parse.Ast
module N = Ast

module Loc = Parse.Location
module W = Loc.With_loc

let map_loc = W.map

let rec partition_binop op = function
  | [] -> []
  | hd::_ as l ->
    let (partition_elt, rest) =
      CCList.partition (op hd) l
    in partition_elt :: partition_binop op rest

let operator : O.operator -> N.operator = function
  | O.Ocons -> N.Ocons
  | O.Oeq   -> N.Oeq
  | O.Oneg  -> N.Oneg
  | O.Oplus -> N.Oplus
  | O.Ominus-> N.Ominus

let rec expr_desc : O.expr_desc -> N.expr_desc = function
  | O.Evar s -> N.Evar s
  | O.Econstant c -> N.Econstant (constant c)
  | O.Elambda (pat, e) -> N.Elambda (pattern pat, expr e)
  | O.EfunApp (e1, e2) -> N.EfunApp (expr e1, expr e2)
  | O.EtyAnnot (e, t)  -> N.EtyAnnot (expr e, t)
  | O.EopApp (o, args) -> N.EopApp (operator o, List.map expr args)
  | O.Elet (binds, e) -> N.Elet (bindings binds, expr e)
  | O.Eite (e0, e1, e2) -> N.Eite (expr e0, expr e1, expr e2)
  (* TODO: smarter compilation of some form of if-then-else *)
  | O.Epragma (p, e) -> N.Epragma (p, expr e)
  | O.Eimport e -> N.Eimport (expr e)
  | O.Erecord r -> N.Erecord (record r)
  | O.Eaccess (e, ap, default) ->
    N.EaccessPath (expr e, access_path ap, CCOpt.map expr default)
  | _ -> failwith "Not implemented"

and access_path ap = List.map ap_field ap

and apf_to_expr = function
  | O.AFexpr e -> e.W.description
  | O.AFidentifier s -> O.Econstant (O.Cstring s)

and ap_field f = expr @@ map_loc apf_to_expr f

and bindings b = List.map binding b

and binding = function
  | O.BstaticDef (var, value) -> (var, expr value)
  | _ -> assert false

and expr e = map_loc expr_desc e

and pattern_desc : O.pattern_desc -> N.pattern_desc = function
  | O.Pvar (s, mt) -> N.Pvar (s, mt)
  | _ -> failwith "Not implemented"

and pattern p = map_loc pattern_desc p

and constant = function
  | O.Cint i -> N.Cint i
  | O.Cbool b -> N.Cbool b
  | O.Cstring s -> N.Cstring s

and record r =
  (* Don't handle recursive records now for the sake of simplicity *)
  assert (not r.O.recursive);
  let fields = r.O.fields in
  let non_inherit_fields, inherit_fields =
    CCList.partition_map
      (function
        | { W.description = O.Finherit _; _ } as f -> `Right f
        | { W.description = O.Fdef (ap, value); location} ->
          `Left { W.description = (ap, value); location; })
      fields
  in
  assert (inherit_fields = []); (* Not handled now *)
  let rec aux (fields : (O.access_path * O.expr) W.t list) : N.field W.t list =
    (* Invariant: all the access paths are non-empty *)
    let partitionned_by_first_element = partition_binop
        (CCFun.compose_binop (fun f ->
             (CCList.hd @@ fst f.W.description).W.description)
            (=))
        fields
    in
    List.map
      (function
        | { W.description = ([], _); _ } :: _
        | [] -> assert false
        | [ { W.description = ([ident],e); location = loc } ] ->
          W.mk loc (expr @@ W.map apf_to_expr ident, expr e)
        | { W.description = (ident::_, _); location = loc } :: _ as fields ->
          let sub_fields =
            List.map
              (W.map @@ function
                | (_::(_::_ as tl), e) -> (tl, e)
                | _ -> Format.ksprintf failwith
                         "The field %s is defined several times"
                         (Parse.Pp.pp_ap_field Format.str_formatter ident;
                          Format.flush_str_formatter ()))
              fields
          in
          W.mk loc
            (expr @@ W.map apf_to_expr ident,
             W.mk loc @@ N.Erecord (aux sub_fields)))
      partitionned_by_first_element
  in aux non_inherit_fields
