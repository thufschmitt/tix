module P = Nl_ast
module T = Typed_ast
module E = Typing_env
module L = Onix_location

module Pattern = Typecheck_pat

let typeof_const = function
  | P.Cbool _ -> Tix_types.(BaseType Bool)
  | P.Cint  _ -> Tix_types.(BaseType Int)
  | P.Cnil    -> Tix_types.(BaseType Nil)
  | P.Cstring _  -> assert false

let rec expr (env : E.t) : P.expr -> T.expr = L.With_loc.map @@ function
  | P.Econstant c ->
    let typ = typeof_const c in
    T.With_type.make ~description:(T.Econstant c) ~typ
  | P.Elambda (pat, e) ->
    let (added_env, typed_pat) = Pattern.infer pat in
    let domain = T.get_typ typed_pat in
    let typed_e = expr (E.merge env added_env) e in
    let codomain = T.get_typ typed_e in
    T.With_type.make
      ~description:(T.Elambda (typed_pat, typed_e))
      ~typ:(Tix_types.Arrow (domain, codomain))
  | _ -> (ignore (expr, env); assert false)
