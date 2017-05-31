(**
   Parser for the nix language
*)

(** Parse a nix expression. *)
val expr : (Ast.expr, unit) MParser.t
