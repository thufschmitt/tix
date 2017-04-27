module P = Nl_ast
module T = Typed_ast
module E = Typing_env
module L = Onix_location

let typeof_const = function
  | P.Cbool _ -> Tix_types.(BaseType Bool)
  | P.Cint  _ -> Tix_types.(BaseType Int)
  | P.Cnil    -> Tix_types.(BaseType Nil)
  | P.Cstring _  -> assert false

let rec expr env : P.expr -> T.expr = L.With_loc.map @@ function
  | P.Econstant c ->
    let typ = typeof_const c in
    T.With_type.make ~description:(T.Econstant c) ~typ
  | P.Elambda (pat, e, annot) ->
    let (dom_annot, codom_annot) = match annot with
      | Some (Tix_types.Arrow (dom, codom)) -> (dom, codom)
      | Some _
      | None   -> assert false
      (* TODO: When gradual types added, make a default annot of [? -> ?] *)
    in
    (* TODO: The annotation may be a conjunction of arrows, in which case we
     * must check all the members of the intersection *)
    let pattern_env = pattern env dom_annot pat in
    T.With_type.make
      ~description:(T.Elambda (expr (E.merge env pattern_env) e))
      ~typ:(Tix_types.Arrow (dom_annot, codom_annot))

  | _ -> (ignore (expr, env); assert false)
