module P = Nl_ast
module T = Typed_ast
module E = Typing_env
module L = Onix.Location

module Pattern = Typecheck_pat

exception TypeError of string

let typeError e = Format.ksprintf (fun s -> raise (TypeError s)) e

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
  | P.Evar v ->
    begin match E.lookup env v with
      | Some t -> T.With_type.make ~description:(T.Evar v) ~typ:t
      | None -> typeError "Unbount variable %s" v
    end
  | P.EfunApp (e1, e2) ->
    let typed_e1 = expr env e1
    and typed_e2 = expr env e2
    in
    let t1 = T.get_typ typed_e1
    and t2 = T.get_typ typed_e2
    in
    begin match t1 with
      | Tix_types.Arrow (domain, codomain) when domain = t2 ->
        T.With_type.make
          ~description:(T.EfunApp (typed_e1, typed_e2))
          ~typ:codomain
      | Tix_types.Arrow (domain, _) ->
        typeError "Expected %s, got %s"
          (Tix_types.show domain)
          (Tix_types.show t2)
      | _ ->
        typeError "%s is not an arrow type"
          (Tix_types.show t1)
    end
  | _ -> (ignore (expr, env); assert false)
