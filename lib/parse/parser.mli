(**
   Parser for the nix language
*)

(** The type of a parser
 * The [string] argument represents the name of the file being parsed
 * *)
type 'a t = ('a, string) MParser.t

type 'a return = ('a, string * MParser.error) result

(** Parse a nix expression. *)
val expr : Ast.expr t

(** Parser for a type *)
val typ  : Type_annotations.t t


val parse_string : 'a t -> string -> 'a return
