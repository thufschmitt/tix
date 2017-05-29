(**
   Typecheck nix-light expressions
 *)

(**
   Exception raised whenever a typing error occurs
 *)
exception TypeError of string

(**
   [expr env e] Typechecks the expression [e] under the environment [env].
   Raises [TypeError] if the expression is not typeable
 *)
val expr : Types.Environment.t -> Typing_env.t -> Simple.Ast.expr -> Typed_ast.expr
