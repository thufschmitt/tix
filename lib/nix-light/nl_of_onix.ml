module O = Onix_ast
module N = Nl_ast

module Loc = Onix_location

let map_loc : ('a -> 'b) -> 'a Loc.with_loc -> 'b Loc.with_loc = fun f l ->
  {
    l with
    Loc.description = f l.Loc.description;
  }

let rec expr_desc : O.expr_desc -> N.expr_desc = function
  | O.Evar s -> N.Evar s
  | O.Econstant c -> N.Econstant (constant c)
  | O.Elambda (pat, e) -> N.Elambda (pattern pat, expr e)
  | O.EfunApp (e1, e2) -> N.EfunApp (expr e1, expr e2)
  | O.EtyAnnot (e, t)  -> N.EtyAnnot (expr e, t)
  | _ -> failwith "Not implemented"

and expr e = map_loc expr_desc e

and pattern_desc : O.pattern_desc -> N.pattern_desc = function
  | O.Pvar s -> N.Pvar s
  | _ -> failwith "Not implemented"

and pattern p = map_loc pattern_desc p

and constant = function
  | O.Cint i -> N.Cint i
  | O.Cbool b -> N.Cbool b
