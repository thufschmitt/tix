module A = Ast
module P = MParser
module T = Type_annotations
module W = Location.With_loc

module StrHash = CCHashSet.Make(CCString)

let (>>=) = P.(>>=)
let (|>>) = P.(|>>)
let (<|>) = P.(<|>)
let (>>)  = P.(>>)
let (<<)  = P.(<<)

type 'a t = ('a, unit) MParser.t

type 'a return = ('a, string * MParser.error) result

let mk_with_loc = Location.With_loc.mk'

let keywords = StrHash.of_list [
    "if"; "then"; "else";
    "let"; "in";
    "true"; "false";
  ]

let get_loc =
  P.get_pos |>> fun (_, lnum, cnum) ->
  Location.{ file_name = ""; lnum; cnum; }

let add_loc x =
  let file_name = "" in
  P.get_pos >>= fun (_, lnum, cnum) ->
  x |>> fun x ->
  mk_with_loc ~file_name ~lnum ~cnum x

(** {2 Some utility functions } *)
let comment =
  (P.attempt (P.string "/*" << P.satisfy @@ (<>) ':'))
  >> P.skip_many_chars_until
    P.any_char
    (P.char '*' << P.char '/')

let space = P.skip_many (P.skip P.space <|> comment)

let any x = P.choice @@ List.map P.attempt x

let keyword k = P.string k << P.not_followed_by P.alphanum ""

let alphanum_ = P.alphanum <|> P.char '_'
let letter_ = P.letter <|> P.char '_'

let ident = space >>
  letter_ >>= fun c0 ->
  P.many_chars alphanum_ >>= fun end_name ->
  let name = (CCString.of_char c0) ^ end_name in
  if StrHash.mem keywords name then
    P.zero
  else
    P.return name

let int = space >> P.many1_chars P.digit |>> int_of_string

let parens x = space >> P.char '(' >> x << space << P.char ')'

let bool = space >> any
    [keyword "true" >> P.return true;
     keyword "false" >> P.return false]

let string = space >> P.char '"' >>
  P.many_chars_until
    P.any_char
    (P.satisfy (fun c -> c = '"') << P.prev_char_satisfies (fun c -> c <> '\\'))

let infix_ops =
  let infix sym f assoc = P.Infix (
      (P.get_pos >>= fun (_, lnum, cnum) -> P.skip_string sym >>
       P.return (fun e1 e2 ->
           mk_with_loc ~file_name:"" ~lnum ~cnum (f e1 e2))),
      assoc)
  and prefix sym f = P.Prefix (
      P.get_pos >>= fun (_, lnum, cnum) -> P.skip_string sym >>
      P.return (fun e -> mk_with_loc ~file_name:"" ~lnum ~cnum (f e)))
  in
  [
    [ infix "+" (fun e1 e2 -> A.EopApp (A.Oplus, [e1; e2])) P.Assoc_left;
      infix "-" (fun e1 e2 -> A.EopApp (A.Ominus, [e1; e2])) P.Assoc_left; ];
    [ prefix "-" (fun e -> A.EopApp (A.Oneg, [e])); ];
  ]

(** {2 Begining of the parser } *)

(** {3 Type annotations} *)

let typ_op =
  let module I = T.Infix_constructors in
  let infix sym op assoc = P.Infix (
      P.attempt (space >> P.skip_string sym)
      >> P.return (fun t1 t2 -> T.Infix (op, t1, t2))
    , assoc)
  in
  [
    [ infix "&" I.And P.Assoc_left ];
    [ infix "|" I.Or P.Assoc_left ];
    [ infix "->" I.Arrow P.Assoc_right ];
  ]

let typ_int =
  int |>> fun nb ->
  Type_annotations.(Singleton (Singleton.Int nb))

let typ_bool =
  bool |>> fun b ->
  Type_annotations.(Singleton (Singleton.Bool b))

let typ_string =
  string |>> fun s ->
  Type_annotations.(Singleton (Singleton.String s))

let typ_ident = ident |>> fun t -> Type_annotations.Var t
and typ_singleton = any [typ_int; typ_bool; typ_string ]

let typ_atom = any [ typ_singleton; typ_ident; ]

let rec typ i = i |> P.expression typ_op
                  (any [typ_atom; parens typ])

let type_annot = space >> P.string "/*:" >> typ << space << P.string "*/"

(** {3 Expressions} *)

let expr_int = add_loc (
    int |>> fun nb ->
    A.Econstant (A.Cint nb)
  )

let expr_bool = add_loc (
    bool |>> fun b ->
    A.Econstant (A.Cbool b)
  )

let expr_string = add_loc (
    string |>> fun s ->
    A.Econstant (A.Cstring s)
  )

let expr_ident = add_loc (
    ident |>> fun id ->
    A.Evar id
  )

let pattern_var =
  ident >>= fun id ->
  P.option (P.attempt type_annot) |>> fun annot ->
  (id, annot)

let pattern_ident = add_loc (
    pattern_var |>> fun (id, annot) ->
    A.Pvar (id, annot)
  )

and expr_const =
  any [expr_int; expr_bool; expr_string]

let rec expr i =
  i |>
  any [expr_lambda; expr_let; expr_if; expr_infix; expr_apply]

and expr_infix i =
  i |> (P.expression infix_ops (expr_apply << space))

and expr_if i =
  i |> add_loc
    (space >> keyword "if" >>
     expr >>= fun e_if ->
     space >> keyword "then" >>
     expr >>= fun e_then ->
     space >> keyword "else" >>
     expr |>> fun e_else ->
     A.Eite (e_if, e_then, e_else)
    )

and expr_atom i =
  i |>
  any [expr_const; expr_ident; expr_paren; expr_annot ]

and expr_paren i = i |> parens expr

and expr_annot i =
  i |> add_loc (
    parens (
      expr >>= fun e ->
      type_annot |>> fun t ->
      A.EtyAnnot (e, t)
    ))

and expr_lambda i =
  i |> add_loc (
    pattern >>= fun pat ->
    space >> P.char ':' >>
    expr |>> fun body ->
    A.Elambda (pat, body)
  )

and expr_let i =
  i |> add_loc (
    space >> keyword "let" >>
    P.many1 (P.attempt binding) >>= fun b ->
    space >> keyword "in" >>
    expr |>> fun e ->
    A.Elet (b, e)
  )

and binding i =
  i |>
  (pattern_var >>= fun (id, annot) ->
   space >> P.char '=' >>
   expr << space << P.char ';' |>> fun e ->
   A.BstaticDef ((id, annot), e))

and pattern i = i |> any [pattern_ident]

and expr_apply i =
  i |>
  (get_loc >>= fun loc ->
   expr_atom >>= fun e0 ->
   P.many expr_atom |>>
   List.fold_left (fun accu e -> W.mk loc (A.EfunApp (accu, e))) e0)

let expr =
  expr << space << P.eof

let mpresult_to_result = function
  | MParser.Success x -> Ok x
  | MParser.Failed (msg, e) -> Error (msg, e)

let parse_string parser str =
  MParser.parse_string parser str ()
  |> mpresult_to_result

(* let operator = choice @@ List.map exactly [ CONS_KW ] *)
(*   >>= function *)
(*       | CONS_KW -> return P.Ocons *)
(*       | _ -> mzero *)
(*  *)
(* (** {1 The parser} **) *)
(*  *)
(* and expr_const = any >>= function *)
(*   | INTEGER i -> return (add_loc @@ P.Econstant (P.Cint i)) *)
(*   | BOOL b -> return (add_loc @@ P.Econstant (P.Cbool b)) *)
(*   | _ -> mzero *)
(*  *)
(* (** {2 Type_annotations} *) *)
(* let rec typ input = *)
(*   (typ_arrow <|> typ_atom <|> typ_cons) input *)
(*  *)
(* and typ_atom input = *)
(*   ((ident => fun t -> Type_annotations.Var t) *)
(*   <|> *)
(*   in_parens typ) *)
(*     input *)
(*  *)
(* and typ_arrow input = *)
(*   (typ_atom >>= fun domain -> *)
(*    exactly ARROW_R >> *)
(*    typ => fun codomain -> *)
(*      Type_annotations.Arrow (domain, codomain)) *)
(*     input *)
(*  *)
(* and typ_cons input = *)
(*   (exactly CONS_KW >> *)
(*    in_parens ( *)
(*      typ >>= fun t1 -> (exactly COMMA) >> typ => fun t2 -> *)
(*        Type_annotations.Cons(t1, t2) *)
(*    )) input *)
(*  *)
(* let type_annot = *)
(*   exactly TY_START >> typ << exactly TY_END *)
(*  *)
(* (** {2 Patterns} *) *)
(* let pat_ident = *)
(*   (ident >>= fun var -> *)
(*    option None (type_annot => fun x -> Some x) => *)
(*    fun annot -> *)
(*      add_loc @@ P.Pvar (var, annot)) *)
(*  *)
(* let pat_record_field input = *)
(*   (ident >>= fun var -> *)
(*    option None (type_annot => fun x -> Some x) => *)
(*    fun annot -> *)
(*      P.{ *)
(*        field_name = var; *)
(*        default_value = None; (* TODO: add a default value *) *)
(*        type_annot = annot; *)
(*      }) *)
(*     input *)
(*  *)
(* let pat_record input = *)
(*   (between (exactly BRACE_L) (exactly BRACE_R) ( *)
(*       sep_by pat_record_field (exactly COMMA)) *)
(*    => fun fields -> *)
(*      P.NPrecord (fields, P.Closed)) *)
(*     input *)
(*  *)
(* let pat_nontrivial input = *)
(*   (pat_record => fun p -> add_loc @@ P.Pnontrivial (p, None)) *)
(*   (* TODO: add capture variable *) *)
(*     input *)
(*  *)
(* let pattern = pat_ident <|> pat_nontrivial *)
(*  *)
(* (** {3 Expressions} *) *)
(* let rec expr input = *)
(*   choice *)
(*     [expr_fun; expr_let; expr_apply; expr_op; ] input *)
(*  *)
(* and expr_fun input = *)
(*   (pattern >>= fun pat -> *)
(*    exactly COLON >> *)
(*    expr => fun body -> *)
(*      (add_loc @@ P.Elambda (pat, body))) *)
(*     input *)
(*  *)
(* and expr_let input = *)
(*   (exactly LET_KW >> *)
(*    bindings >>= fun bindings -> *)
(*    exactly IN_KW >> *)
(*    expr => fun e -> *)
(*      (add_loc @@ P.Elet (bindings, e))) *)
(*     input *)
(*  *)
(* and bindings input = *)
(*   (end_by binding (exactly SEMICOLON)) input *)
(*  *)
(* and binding input = *)
(*   (ident >>= fun name -> *)
(*    option None (type_annot => fun a -> Some a) >>= fun annot -> *)
(*    exactly EQUAL >> *)
(*    expr => fun value -> *)
(*      P.BstaticDef ((name, annot), value)) *)
(* input *)
(*  *)
(* and expr_atom input = *)
(*   (choice *)
(*      [ expr_ident; *)
(*        expr_parens; *)
(*        expr_annot; *)
(*        expr_record; *)
(*        expr_list_sugar; *)
(*        expr_const *)
(*      ]) input *)
(*  *)
(* and expr_record input = *)
(*   (between (exactly BRACE_L) (exactly BRACE_R) *)
(*     (end_by expr_record_field (exactly SEMICOLON)) *)
(*     => fun fields -> *)
(*       add_loc @@ P.(Erecord { *)
(*           recursive = false; (* TODO *) *)
(*           fields; *)
(*       }) *)
(*   ) *)
(*     input *)
(*  *)
(* and expr_record_field input = *)
(*   (ident >>= fun name -> (* TODO: this has to be an access path *) *)
(*    exactly EQUAL >> *)
(*    expr => fun e -> *)
(*      add_loc @@ P.Fdef (name, e)) *)
(*     input *)
(*  *)
(* and expr_list_sugar input = *)
(*   (between (exactly BRACKET_L) (exactly BRACKET_R) ( *)
(*       many expr_atom => *)
(*       List.rev => *)
(*       List.fold_left (fun acc elt -> *)
(*           add_loc @@ P.EopApp (P.Ocons, [ elt; acc ]) *)
(*         ) *)
(*         (add_loc @@ P.Econstant P.Cnil) *)
(*     )) *)
(*       input *)
(*  *)
(* and expr_op input = *)
(*   (operator >>= fun op -> *)
(*    in_parens ( *)
(*      sep_by1 expr (exactly COMMA) *)
(*      => fun args -> *)
(*        add_loc @@ P.EopApp (op, args) *)
(*    )) input *)
(*  *)
(* and expr_annot input = *)
(*   (in_parens ( *)
(*    expr >>= fun e -> *)
(*    type_annot => fun t -> *)
(*      (add_loc @@ P.EtyAnnot (e, t)))) *)
(*     input *)
(*  *)
(* and expr_apply input = *)
(*   (* Directly parse a list of expressions to avoid left-recursion *) *)
(*   (expr_atom >>= fun e0 -> *)
(*    many expr_atom => *)
(*    List.fold_left (fun accu e -> *)
(*        add_loc @@ P.EfunApp (accu, e)) *)
(*      e0 *)
(*   ) *)
(*     input *)
(*  *)
(* (** {1 toplevel functions} **) *)
(*  *)
(* let parse_lexbuf parser read_fun lexbuf = *)
(*   local_lexbuf := lexbuf; *)
(*   let stream = LazyStream.of_function *)
(*       (fun () -> match read_fun lexbuf with *)
(*          | EOF -> None *)
(*          | x -> Some x *)
(*       ) *)
(*   in *)
(*   parse parser stream *)
(*  *)
(* let onix = parse_lexbuf expr *)
