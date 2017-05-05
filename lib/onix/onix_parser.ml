open Opal

module P = Onix_ast

let mk_with_loc = Onix_location.With_loc.mk

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
  | DOLLAR_BRACE
  | BRACE_LR
  | BRACE_L
  | BRACE_R
  | PAREN_L
  | PAREN_R
  | BRACKET_L
  | BRACKET_R
  | CONS_KW
  | TY_START
  | TY_END
  | ARROW_R
  | QUESTION_MARK
  | ID of string
  | BOOL of bool
  | INTEGER of int


(* A lexbuf used internally. Note that this makes the parser non reentrant, but
 * for now I don't care *)
let local_lexbuf = ref (Lexing.from_string "")

(** Locations **)

(** [add_loc x] wraps [x] into a [Onix_location.With_loc.t] filled with current
 * location of the parser *)
let add_loc x =
  let loc_start = !local_lexbuf.Lexing.lex_start_p
  and loc_end = Lexing.dummy_pos (* TODO *)
  in
  mk_with_loc loc_start loc_end x

(** Some utility functions **)

let in_parens p = between (exactly PAREN_L) (exactly PAREN_R) p

(** Checks wether current token is an ID *)
let ident = any >>= function
  | ID s -> return s
  | _ -> mzero


(** {1 The parser} **)

let expr_ident = (ident => fun s -> add_loc @@ P.Evar s)

and expr_const = any >>= function
  | INTEGER i -> return (add_loc @@ P.Econstant (P.Cint i))
  | BOOL b -> return (add_loc @@ P.Econstant (P.Cbool b))
  | _ -> mzero

(** {2 Types} *)
let rec typ input =
  (typ_arrow <|> typ_atom) input

and typ_atom input =
  ((ident => fun t -> Tix_types.(BaseType (read_base t)))
  <|>
  in_parens typ)
    input

and typ_arrow input =
  (typ_atom >>= fun domain ->
   exactly ARROW_R >>
   typ => fun codomain ->
     Tix_types.Arrow (domain, codomain))
    input

let type_annot =
  exactly TY_START >> typ << exactly TY_END

(** {2 Patterns} *)
let pat_ident =
  (ident >>= fun var ->
   option None (type_annot => fun x -> Some x) =>
   fun annot ->
     add_loc @@ P.Pvar (var, annot))

let pat_record_field input =
  (ident >>= fun var ->
   option None (type_annot => fun x -> Some x) =>
   fun annot ->
     P.{
       field_name = var;
       default_value = None; (* TODO: add a default value *)
       type_annot = annot;
     })
    input

let pat_record input =
  (between (exactly BRACE_L) (exactly BRACE_R) (
      sep_by pat_record_field (exactly COMMA))
   => fun fields ->
     P.NPrecord (fields, P.Closed))
    input

let pat_nontrivial input =
  (pat_record => fun p -> add_loc @@ P.Pnontrivial (p, None))
  (* TODO: add capture variable *)
    input

let pattern = pat_ident <|> pat_nontrivial

(** {3 Expressions} *)
let rec expr input =
  choice
    [expr_fun; expr_apply; ] input

and expr_fun input =
  (pattern >>= fun pat ->
   exactly COLON >>
   expr => fun body ->
     (add_loc @@ P.Elambda (pat, body)))
    input

and expr_atom input =
  (expr_ident  <|>
   expr_parens <|>
   expr_annot <|>
   expr_record <|>
   expr_const
  ) input

and expr_record input =
  (between (exactly BRACE_L) (exactly BRACE_R)
    (end_by expr_record_field (exactly SEMICOLON))
    => fun fields ->
      add_loc @@ P.(Erecord {
          recursive = false; (* TODO *)
          fields;
      })
  )
    input

and expr_record_field input =
  (ident >>= fun name -> (* TODO: this has to be an access path *)
   exactly EQUAL >>
   expr => fun e ->
     add_loc @@ P.Fdef (name, e))
    input

and expr_parens input =
  (in_parens expr) input

and expr_annot input =
  (in_parens (
   expr >>= fun e ->
   type_annot => fun t ->
     (add_loc @@ P.EtyAnnot (e, t))))
    input

and expr_apply input =
  (* Directly parse a list of expressions to avoid left-recursion *)
  (expr_atom >>= fun e0 ->
   many expr_atom =>
   List.fold_left (fun accu e ->
       add_loc @@ P.EfunApp (accu, e))
     e0
  )
    input

(** {1 toplevel functions} **)

let parse_lexbuf parser read_fun lexbuf =
  local_lexbuf := lexbuf;
  let stream = LazyStream.of_function
      (fun () -> match read_fun lexbuf with
         | EOF -> None
         | x -> Some x
      )
  in
  parse parser stream

let onix = parse_lexbuf expr