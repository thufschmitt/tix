(**
   Parser for the nix language
*)

(** The type of a parser *)
type 'a t = ('a, unit) MParser.t

type 'a return = ('a, string * MParser.error) result

(** Parse a nix expression. *)
val expr : Ast.expr t

(** Parser for a type *)
val typ  : Type_annotations.t t


val parse_string : 'a t -> string -> 'a return
