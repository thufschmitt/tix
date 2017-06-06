open Opal

module P = Ast

let mk_with_loc = Location.With_loc.mk

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
  | IF_KW
  | THEN_KW
  | ELSE_KW
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

(* A lexbuf used internally. Note that this makes the parser non reentrant, but
 * for now I don't care *)
let local_lexbuf = ref (Lexing.from_string "")

(** Locations **)

(** [add_loc x] wraps [x] into a [Location.With_loc.t] filled with current
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

let operator = exactly CONS_KW
  >>= function
  | CONS_KW -> return P.Ocons
  | _ -> mzero

(** {1 The parser} **)

let expr_ident = (ident => fun s -> add_loc @@ P.Evar s)

and expr_const = any >>= function
  | INTEGER i -> return (add_loc @@ P.Econstant (P.Cint i))
  | BOOL b -> return (add_loc @@ P.Econstant (P.Cbool b))
  | _ -> mzero

(** {2 Type_annotations} *)
let rec typ input =
  typ_where input

and typ_where i =
  i |>
  ((typ_arrow >>= fun t ->
    exactly WHERE_KW >>
    typ_bindings => fun binds ->
      Type_annotations.(TyBind (binds, t)))
   <|> typ_arrow)

and typ_binding i =
  i |>
  (ident >>= fun id ->
   exactly EQUAL >>
   typ => fun t ->
     (id, t))

and typ_bindings i =
  i |> (sep_by typ_binding (exactly SEMICOLON))

and typ_arrow i =
  i |>
  ((typ_atom >>= fun domain ->
    exactly ARROW_R >>
    typ => fun codomain ->
      Type_annotations.(Infix (Arrow, domain, codomain)))
   <|> typ_disj)

and typ_disj i =
  i |>
  ((typ_conj >>= fun t1 ->
    exactly PIPE >>
    typ_disj => fun t2 ->
      Type_annotations.(Infix (Or, t1, t2)))
   <|> typ_conj)

and typ_conj i =
  i |>
  ((typ_atom >>= fun t1 ->
    exactly AMPERSAND >>
    typ_conj => fun t2 ->
      Type_annotations.(Infix (And, t1, t2)))
   <|> typ_atom)

and typ_atom input =
  ((ident => fun t -> Type_annotations.Var t)
   <|>
   (exactly QUESTION_MARK >> return @@ Type_annotations.Var "?")
   <|>
   typ_const
   <|>
   typ_cons
   <|>
   in_parens typ)
    input

and typ_const i =
  i |>
  (any >>= function
    | BOOL b -> return @@ Type_annotations.(Singleton (Singleton.Bool b))
    | INTEGER i -> return @@ Type_annotations.(Singleton (Singleton.Int i))
    | _ -> mzero
  )

and typ_cons input =
  (exactly CONS_KW >>
   in_parens (
     typ >>= fun t1 -> (exactly COMMA) >> typ => fun t2 ->
         Type_annotations.Cons(t1, t2)
   )) input

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
    [expr_fun; expr_let; expr_apply; expr_op; expr_ite ] input

and expr_fun input =
  (pattern >>= fun pat ->
   exactly COLON >>
   expr => fun body ->
     add_loc @@ P.Elambda (pat, body))
    input

and expr_let input =
  (exactly LET_KW >>
   bindings >>= fun bindings ->
   exactly IN_KW >>
   expr => fun e ->
     add_loc @@ P.Elet (bindings, e))
    input

and bindings input =
  (end_by binding (exactly SEMICOLON)) input

and binding input =
  (ident >>= fun name ->
   option None (type_annot => fun a -> Some a) >>= fun annot ->
   exactly EQUAL >>
   expr => fun value ->
     P.BstaticDef ((name, annot), value))
    input

and expr_atom input =
  (choice
     [ expr_ident;
       expr_parens;
       expr_annot;
       expr_record;
       expr_list_sugar;
       expr_const
     ]) input

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

and expr_list_sugar input =
  (between (exactly BRACKET_L) (exactly BRACKET_R) (
      many expr_atom =>
      List.rev =>
      List.fold_left (fun acc elt ->
          add_loc @@ P.EopApp (P.Ocons, [ elt; acc ])
        )
        (add_loc @@ P.Evar "nil")
    ))
    input

and expr_op input =
  (operator >>= fun op ->
   in_parens (
     sep_by1 expr (exactly COMMA)
     => fun args ->
       add_loc @@ P.EopApp (op, args)
   )) input

and expr_parens input =
  (in_parens expr) input

and expr_annot input =
  (in_parens (
      expr >>= fun e ->
      type_annot => fun t ->
        add_loc @@ P.EtyAnnot (e, t)))
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

and expr_ite i =
  i |>
  (exactly IF_KW >>
   expr >>= fun e0 ->
   exactly THEN_KW >>
   expr >>= fun e1 ->
   exactly ELSE_KW >>
   expr =>  fun e2 ->
     add_loc @@ P.Eite (e0, e1, e2))

let toplevel = expr << (exactly EOF)

(** {1 toplevel functions} **)

let parse_lexbuf parser read_fun lexbuf =
  local_lexbuf := lexbuf;
  let stream = LazyStream.of_function
      (fun () -> match read_fun lexbuf with
         | x -> Some x
      )
  in
  parse parser stream

let onix = parse_lexbuf toplevel
let typ  = parse_lexbuf typ
