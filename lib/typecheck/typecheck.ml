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
  | _ -> (ignore (expr, env); assert false)
