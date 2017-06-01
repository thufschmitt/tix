(**
   Typecheck nix-light expressions
 *)

(**
   Exception raised whenever a typing error occurs
 *)
exception TypeError of Parse.Location.t * string

(**
   The inference type-system
*)
module Infer : sig
  (**
     [expr tenv env e] infers a type of the expression [e] under the
     type environment [tenv] and the environment [env].

     @return the typed AST
     @raise TypeError if the expression is not typeable
  *)
  val expr : Types.Environment.t
    -> Typing_env.t
    -> Simple.Ast.expr
    -> Typed_ast.expr
end

module Check : sig
  (** [expr env e t] checks that under the type environment [tenv] and the
      environment [env], the expression [e] admits the type [t].

      @return the typed AST
      @raise TypeError if it is not the case.
  *)
  val expr : Types.Environment.t
    -> Typing_env.t
    -> Simple.Ast.expr
    -> Types.t
    -> Typed_ast.expr
end
