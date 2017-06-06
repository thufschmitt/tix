(**
   Parser for the nix language
*)
type token =
  | EOF
  | COLON
  | SEMICOLON
  | COMMA
  | DOT
  | AROBASE
  | EQUAL
  | OR_KW
  | REC_KW
  | INHERIT_KW
  | LET_KW
  | IN_KW
  | WHERE_KW
  | DOLLAR_BRACE
  | BRACE_LR
  | BRACE_L
  | BRACE_R
  | PAREN_L
  | PAREN_R
  | BRACKET_L
  | BRACKET_R
  | AMPERSAND
  | PIPE
  | CONS_KW
  | TY_START
  | TY_END
  | ARROW_R
  | QUESTION_MARK
  | ID of string
  | BOOL of bool
  | INTEGER of int

(** Parse a nix expression. The type of the function is compatible with Menhir
   generated parsers *)
val onix : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast.expr option

val typ : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Type_annotations.t option
