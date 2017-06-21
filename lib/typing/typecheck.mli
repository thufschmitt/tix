(**
   Typecheck nix-light expressions
 *)

module W : module type of Annotations.W

(**
   The inference type-system
*)
module Infer : sig
  (**
     [expr tenv env e] infers a type of the expression [e] under the
     type environment [tenv] and the environment [env].

     @return the type of the expression
     @raise TypeError if the expression is not typeable
  *)
  val expr : Environment.t
    -> Simple.Ast.expr
    -> Types.t W.t
end

module Check : sig
  (** [expr env e t] checks that under the type environment [tenv] and the
      environment [env], the expression [e] admits the type [t].

      @raise TypeError if it is not the case.
  *)
  val expr : Environment.t
    -> Simple.Ast.expr
    -> Types.t
    -> unit W.t
end
