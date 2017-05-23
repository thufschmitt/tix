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
val expr : Typing_env.t -> Nix_light.Ast.expr -> Typed_ast.expr
