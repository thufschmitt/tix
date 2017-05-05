val expr : Typing_env.t -> Nl_ast.expr -> Typed_ast.expr

exception TypeError of string
