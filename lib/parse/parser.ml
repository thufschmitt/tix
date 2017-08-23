module Location = Common.Location

module A = Ast
module P = MParser
module T = Common.Type_annotations
module W = Location.With_loc

module StrHash = CCHashSet.Make(CCString)

let (>>=) = P.(>>=)
let (|>>) = P.(|>>)
let (<|>) = P.(<|>)
let (<?>) = P.(<??>)
let (>>)  = P.(>>)
let (<<)  = P.(<<)

type 'a t = ('a, string) MParser.t

type 'a return = ('a, string * MParser.error) result

let keywords = StrHash.of_list [
    "if"; "then"; "else";
    "let"; "in";
    "true"; "false";
    "import"; (* This isn't a keyword in Nix, but a regular function. *)
    "assert";
    "rec";
    "with";
    "or";
  ]

let get_loc =
  P.get_user_state >>= fun file_name ->
  P.get_pos |>> fun (_, lnum, cnum) ->
  Location.{ file_name; lnum; cnum; }

let add_loc x =
  get_loc >>= fun loc ->
  x |>> fun x ->
  W.mk loc x

(** {2 Some utility functions } *)
let any x = P.choice @@ List.map P.attempt x

let block_comment =
  (P.attempt (P.string "/*" << P.satisfy @@ (<>) ':'))
  >> P.skip_many_chars_until
    P.any_char
    (P.char '*' << P.char '/')

let line_comment = P.char '#' << P.not_followed_by (P.string "::") ""
  >> P.skip_many_until P.any_char P.newline

let comment = any [ block_comment; line_comment; ] <?> "comment"

let space = P.skip_many (P.skip P.space <|> comment <?> "whitespace")

let keyword k = P.string k << P.not_followed_by P.alphanum "" >> space

let alphanum_ = P.alphanum <|> P.char '_'
let letter_ = P.letter <|> P.char '_'

let ident =
  (letter_ >>= fun c0 ->
   P.many_chars alphanum_ << space >>= fun end_name ->
   let name = (CCString.of_char c0) ^ end_name in
   if StrHash.mem keywords name then
     P.zero
   else
     P.return name)
  <?> "ident"

let int = P.many1_chars P.digit << space |>> int_of_string

let parens x = P.char '(' >> x << space << P.char ')' << space

let bool = any
    [keyword "true" >> P.return true;
     keyword "false" >> P.return false]
           << space
           <?> "boolean"

let simple_string = P.char '"' >>
  P.many_chars_until
    P.any_char
    (P.satisfy (fun c -> c = '"')
     << P.prev_char_satisfies (fun c -> c <> '\\'))
  << space
  <?> "simple string"

let multiline_string = P.string "''" >>
  P.many_chars_until
    P.any_char
    (P.char '\'' <<
     P.followed_by (P.char '\'') "EOS") << P.skip_any_char
  << space
  <?> "multiline string"

let string = any [simple_string; multiline_string]

let litteral_path =
  (P.string "./" >>
   P.many_chars (any [ P.alphanum; P.any_of "-/_." ]) << space |>> fun path ->
   "./" ^ path)
  <?> "Path"

let bracketed_path =
  (P.char '<' >>
   P.many_chars (any [ P.alphanum; P.any_of "-/_." ]) <<
   P.char '>' << space |>> fun content ->
   content)
  <?> "Bracketed path"

let infix_ops =
  let infix sym f assoc = P.Infix (
      (get_loc >>= fun loc ->
       P.skip_string sym >> space >>
       P.return (fun e1 e2 ->
           W.mk loc (f e1 e2))),
      assoc)
  and prefix sym f = P.Prefix (
      get_loc >>= fun loc ->
      P.skip_string sym >> space >>
      P.return (fun e -> W.mk loc (f e)))
  in
  [
    [
      infix "==" (fun e1 e2 -> A.EopApp (A.Oeq, [e1; e2])) P.Assoc_left;
      infix "!=" (fun e1 e2 -> A.EopApp (A.OnonEq, [e1; e2])) P.Assoc_left;
      infix "+" (fun e1 e2 -> A.EopApp (A.Oplus, [e1; e2])) P.Assoc_left;
      infix "-" (fun e1 e2 -> A.EopApp (A.Ominus, [e1; e2])) P.Assoc_left;
      infix "&&" (fun e1 e2 -> A.EopApp (A.Oand, [e1; e2])) P.Assoc_left;
      infix "||" (fun e1 e2 -> A.EopApp (A.Oor, [e1; e2])) P.Assoc_left;
      infix "->" (fun e1 e2 -> A.EopApp (A.Oimplies, [e1; e2])) P.Assoc_left;
    ];
    [ prefix "-" (fun e -> A.EopApp (A.Oneg, [e]));
      prefix "!" (fun e -> A.EopApp (A.Onot, [e]));
    ];
  ]

(** {2 Begining of the parser } *)

(** {3 Type annotations} *)

let typ_op =
  let module I = T.Infix_constructors in
  let infix sym op assoc = P.Infix (
      (get_loc >>= fun loc -> P.skip_string sym >> space >>
       P.return (fun t1 t2 ->
           W.mk loc (T.Infix (op, t1, t2)))),
      assoc)
  in
  [
    [ infix "&" I.And P.Assoc_left ];
    [ infix "|" I.Or P.Assoc_left ];
    [ infix "\\" I.Diff P.Assoc_left ];
    [ infix "->" I.Arrow P.Assoc_right ];
  ]


let typ_regex_postfix_op =
  get_loc >>= fun loc ->
  let mkloc = W.mk loc in
  any [
    P.char '*' >> space >> P.return (fun r -> mkloc @@ Regex_list.Star r);
    P.char '+' >> space >> P.return (fun r -> mkloc @@ Regex_list.Plus r);
    P.char '?' >> space >> P.return (fun r -> mkloc @@ Regex_list.Maybe r);
  ]

let typ_int =
  int |>> fun nb ->
  T.(Singleton (Singleton.Int nb))

let typ_bool =
  bool |>> fun b ->
  T.(Singleton (Singleton.Bool b))

let typ_string =
  string |>> fun s ->
  T.(Singleton (Singleton.String s))

let typ_path =
  litteral_path |>> fun s ->
  T.(Singleton (Singleton.Path s))

let typ_ident i = i |> add_loc (
    (ident |>> fun t -> T.Var t)
    <|>
    (P.char '?' >> space >> P.return T.Gradual))
and typ_singleton i = i |> add_loc
  @@ any [typ_int; typ_bool; typ_string; typ_path ]

let rec typ i = i |> (P.expression typ_op
                        (any [typ_atom; typ_list; typ_record])
                      <?> "type")

and typ_atom i = i |> any [ typ_singleton; typ_ident; parens typ]

and typ_regex i =
  i |> (
    any [typ_regex_alt; typ_regex_concat; ]
    <?> "type regex")

and typ_regex_alt i =
  i |> add_loc (
    typ_regex_concat >>= fun t1 ->
    P.char '|' >> space >>
    typ_regex |>> fun t2 ->
    Regex_list.Or (t1, t2))

and typ_regex_postfix i =
  i |> (
    typ_regex_atom >>= fun r0 ->
    P.many typ_regex_postfix_op |>> fun ops ->
    List.fold_left (fun r op -> op r) r0 ops
  )

and typ_regex_atom i =
  i |> (
    parens typ_regex
    <|>
    (add_loc (typ_atom |>> fun t -> Regex_list.Type t)))

and typ_regex_concat i =
  i |> (
    get_loc >>= fun loc ->
    typ_regex_postfix >>= fun r0 ->
    P.many typ_regex_postfix |>> fun tl ->
    List.fold_left (fun accu r ->
        W.mk loc (Regex_list.Concat (accu, r)))
      r0
      tl
  )

and typ_list i =
  i |> (
    P.char '[' >> space >> typ_regex << P.char ']' << space |>>
    Regex_list.to_type)

and typ_record i =
  i |> add_loc ((
      P.char '{' >> space >> typ_record_fields << P.char '}' << space
      |>> fun (fields, is_open) ->
      T.Record (fields, is_open))
      <?> "type record")

and typ_record_fields i =
  i |> any [
    (typ_record_field << P.char ';' << space >>= fun field ->
     typ_record_fields |>> fun (fields, is_open) ->
     (field :: fields, is_open));
    (P.string "..." >> space >> P.return ([], true));
    (typ_record_field |>> fun field -> ([field], false));
    (P.return ([], false));
  ]

and typ_record_field i =
  i |> (
    ident >>= fun name ->
    (P.char '=' >> P.option (P.char '?') << space |>> CCOpt.is_some)
    >>= fun is_optional ->
    typ |>> fun t -> (name, (is_optional, t))
  )

let type_annot = (P.string "/*:" >> space >> typ << P.string "*/" << space)
                 <?> "type annotation"

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

let expr_path = add_loc (
    litteral_path |>> fun s ->
    A.Econstant (A.Cpath s)
  )

let expr_bracket = add_loc (
    bracketed_path |>> fun brack ->
    A.Econstant (A.Cbracketed brack)
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
  (any [expr_int; expr_bool; expr_string; expr_path; expr_bracket])
  <?> "constant"

let rec expr i =
  i |> (
    any [
      expr_pragma; expr_lambda; expr_let; expr_if; expr_infix;
      expr_assert; expr_import; expr_with; expr_apply; expr_apply_or_member
    ]
  )

and expr_pragma i =
  i |> (add_loc (
      P.string "#::" >>
      space >>
      keyword "WARN" >>
      P.many1 warning_annot >>= fun warnings ->
      P.skip_many P.blank >> P.newline >> space >>
      expr |>> fun e ->
      A.Epragma (Pragma.Warnings warnings, e)))

and warning_annot i =
  i |> (
    P.any_of "+-" >>= fun sign_char ->
    let sign = if sign_char = '+' then Pragma.Plus else Pragma.Minus in
    ident >>= fun name ->
    match Pragma.Warning.read name with
    | Some w -> P.return (sign, w)
    | None -> P.fail "Invalid warning name")

and expr_import i =
  i |> add_loc (
    keyword "import" >>
    expr_atom |>> fun e ->
    A.Eimport e)

and expr_with i =
  i |> add_loc (
    keyword "with" >>
    expr << P.char ';' << space >>= fun e1 ->
    expr |>> fun e2 ->
    A.Ewith (e1, e2))

and expr_assert i =
  i |> add_loc (
    get_loc >>= fun loc ->
    keyword "assert" >>
    expr >>= fun assertion ->
    P.char ';' >> space >>
    expr |>> fun k ->
    A.Eite ( assertion, k, W.mk loc @@ A.EfunApp (
        W.mk loc (A.Evar "raise"),
        W.mk loc (A.Econstant (A.Cstring "assertion failed")))))

and expr_infix i =
  i |> (
    P.expression infix_ops expr_apply_or_member)

and expr_infix_member i =
  i |> add_loc (
    expr_apply >>= fun e ->
    P.char '?' >> space >>
    ap |>> fun ap ->
    A.EtestMember (e, ap)
  )

and expr_apply_or_member i =
  i |> (
    P.attempt expr_infix_member
    <|> expr_apply)

and expr_if i =
  i |> (add_loc
          (keyword "if" >>
           expr >>= fun e_if ->
           keyword "then" >>
           expr >>= fun e_then ->
           keyword "else" >>
           expr |>> fun e_else ->
           A.Eite (e_if, e_then, e_else)
          )
        <?> "if-then-else")

and expr_atom i =
  i |> (
    any [expr_record; expr_list;
         expr_const; expr_ident;
         expr_paren; expr_annot
        ]
  )

and expr_record i = 
  i|> add_loc (
    P.option @@ keyword "rec" >>= fun maybe_isrec ->
    let recursive = CCOpt.is_some maybe_isrec in
    expr_record_nonrec |>> fun fields ->
    A.(Erecord { recursive; fields }))

and expr_record_nonrec i =
  i |> (
    P.char '{' >> space >>
    P.many expr_record_field
    << P.char '}' << space
  )

and expr_record_field i =
  i |> add_loc ((
      ap_pattern >>= fun ap ->
      P.char '=' >> space >>
      expr << P.char ';' << space |>> fun value ->
      A.Fdef (ap, value))
      <?> "record field"
    )

and expr_list i =
  i |> (
    get_loc >>= fun loc ->
    P.char '[' >> space >>
    P.many_rev_fold_left
      (fun accu elt -> W.mk loc (A.EopApp (A.Ocons, [elt; accu])))
      (W.mk loc @@ A.Evar "nil")
      expr_atom
    << P.char ']' << space
  )

and expr_paren i = i |> parens expr

and expr_annot i =
  i |> add_loc (
    parens (
      expr >>= fun e ->
      type_annot |>> fun t ->
      A.EtyAnnot (e, t)
    ))

and expr_lambda i =
  i |> (add_loc (
      pattern >>= fun pat ->
      P.char ':' >> space >>
      expr |>> fun body ->
      A.Elambda (pat, body)
    )
        <?> "lambda")

and expr_let i =
  i |> (add_loc (
      keyword "let" >>
      P.many1 (P.attempt binding) >>= fun b ->
      keyword "in" >>
      expr |>> fun e ->
      A.Elet (b, e)
    )
        <?> "let binding")

and binding i =
  i |> add_loc
  (ap_pattern >>= fun access_path ->
   P.char '=' >> space >>
   expr << P.char ';' << space |>> fun e ->
   A.Fdef (access_path, e))

and pattern i = i |> (any [pattern_ident; pattern_complex] <?> "pattern")

and pattern_complex i =
  i |> add_loc @@ any [
    (pattern_record >>= fun record ->
     P.option (P.char '@' >> space >> ident) |>> fun alias_opt ->
     A.Pnontrivial (record, alias_opt));
  ]


and pattern_record_field i =
  i |> ((
      ident >>= fun field_name ->
      P.option (P.char '?' >> space >> expr) >>= fun default_value ->
      P.option type_annot |>> fun type_annot ->
      A.{ field_name; default_value; type_annot })
      <?> "pattern record field")

and pattern_inside i =
  i |> any [
    (pattern_record_field << P.char ',' << space >>= fun field ->
     pattern_inside |>> fun (A.NPrecord (fields, open_flag)) ->
     A.NPrecord (field::fields, open_flag));

    (pattern_record_field |>> fun field ->
     A.NPrecord ([field], A.Closed));

    (P.string "..." >> space >> P.return @@ A.NPrecord ([], A.Open));

    P.return @@ A.NPrecord ([], A.Closed);
  ]

and pattern_record i =
  i |> ((
      P.char '{' >> space >>
      pattern_inside << P.char '}' << space)
      <?> "record pattern")

and expr_apply i =
  i |>
  (get_loc >>= fun loc ->
   expr_select >>= fun e0 ->
   P.many expr_select |>>
   List.fold_left (fun accu e -> W.mk loc (A.EfunApp (accu, e))) e0)

and expr_select i =
  i |> (P.attempt @@
        add_loc (
          expr_atom >>= fun e ->
          P.char '.' >> space >>
          ap >>= fun a ->
          P.option (P.attempt expr_select_guard) |>> fun guard ->
          A.Eaccess (e, a, guard)
        )
        <|>
        expr_atom)

and ap i = i |> P.sep_by1 ap_field (P.char '.' >> space)

and expr_select_guard i = i |> ( keyword "or" >> expr)

and ap_pattern i =
  i |> (
    ap >>= fun access_path ->
    P.option (P.attempt type_annot) |>> fun annot ->
    (access_path, annot))

and ap_field i =
  i |> add_loc (
    (
      P.attempt (P.string "${") >> space >>
      expr >>= fun e ->
      P.char '}' >> space >>
      P.return (A.AFexpr e)
    )
    <|>
    (
      ident |>> fun f_name ->
      A.AFidentifier f_name
    )
  )

let expr =
  space >> expr << P.eof

let typ =
  space >> typ << P.eof

let mpresult_to_result = function
  | MParser.Success x -> Ok x
  | MParser.Failed (msg, e) -> Error (msg, e)

let parse_string parser str =
  MParser.parse_string parser str "-"
  |> mpresult_to_result
