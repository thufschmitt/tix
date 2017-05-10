(**
   Conversion between [Onix.Ast.t] and [Nl_ast.t]
 *)
module O = Onix.Ast
module N = Nl_ast

module Loc = Onix.Location

let map_loc : ('a -> 'b) -> 'a Loc.With_loc.t -> 'b Loc.With_loc.t = fun f l ->
  {
    l with
    Loc.With_loc.description = f l.Loc.With_loc.description;
  }

let operator : O.operator -> N.operator = function
  | O.Ocons -> N.Ocons

let rec expr_desc : O.expr_desc -> N.expr_desc = function
  | O.Evar s -> N.Evar s
  | O.Econstant c -> N.Econstant (constant c)
  | O.Elambda (pat, e) -> N.Elambda (pattern pat, expr e)
  | O.EfunApp (e1, e2) -> N.EfunApp (expr e1, expr e2)
  | O.EtyAnnot (e, t)  -> N.EtyAnnot (expr e, t)
  | O.EopApp (o, args) -> N.EopApp (operator o, List.map expr args)
  | _ -> failwith "Not implemented"

and expr e = map_loc expr_desc e

and pattern_desc : O.pattern_desc -> N.pattern_desc = function
  | O.Pvar (s, mt) -> N.Pvar (s, mt)
  | _ -> failwith "Not implemented"

and pattern p = map_loc pattern_desc p

and constant = function
  | O.Cint i -> N.Cint i
  | O.Cbool b -> N.Cbool b
  | O.Cnil -> N.Cnil
