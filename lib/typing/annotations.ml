(**
   Conversion of type annotations into actual types
*)

module A = Type_annotations
module T = Types

let rec to_type env = function
  | A.Var v ->
    Types.Environment.lookup env v
  | A.Arrow (t1, t2) ->
    CCOpt.map2 T.Builtins.arrow (to_type env t1) (to_type env t2)
  | A.Cons _ -> assert false
