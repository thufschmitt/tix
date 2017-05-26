(**
   Conversion of type annotations into actual types
*)

module A = Type_annotations
module T = Types

let to_type env = function
  | A.Var v ->
    Types.Environment.lookup env v
  | A.Cons _
  | A.Arrow _ -> assert false
