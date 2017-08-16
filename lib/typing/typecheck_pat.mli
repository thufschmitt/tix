(**
   Typechecking of patterns
   @see <https://github.com/regnat/tix-papers> for the theory
*)

(** [infer p] returns the couple [(Γ, p')] where [Γ] is the new environment
   generated by this pattern and [p'] the pattern annotated with type
   informations *)
val infer : ?t_constr:Types.t
  -> Types.Environment.t
  -> Simple.Ast.pattern
  -> ((Typing_env.t * Types.t) * Common.Warning.t list)
