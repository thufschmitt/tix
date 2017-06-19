(**
   Typecheck nix-light expressions
 *)

(**
   Exception raised whenever a typing error occurs
 *)
type 'a withError = ('a, Parse.Location.t * string) result

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
  val expr : Types.Environment.t
    -> Typing_env.t
    -> Simple.Ast.expr
    -> Types.t withError
end

module Check : sig
  (** [expr env e t] checks that under the type environment [tenv] and the
      environment [env], the expression [e] admits the type [t].

      @raise TypeError if it is not the case.
  *)
  val expr : Types.Environment.t
    -> Typing_env.t
    -> Simple.Ast.expr
    -> Types.t
    -> unit withError
end
