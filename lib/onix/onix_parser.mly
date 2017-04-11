%{
  module A = Onix_ast

  let mk_with_loc = Onix_location.mk_with_loc
%}
%token EOF
%token COLON
%token SEMICOLON
%token COMMA
%token DOT
%token AROBASE
%token EQUAL
%token OR_KW
%token REC_KW
%token INHERIT_KW
%token LET_KW
%token IN_KW
%token DOLLAR_BRACE
%token BRACE_L
%token BRACE_R
%token PAREN_L
%token PAREN_R
%token QUESTION_MARK
%token<string> ID
%token<int> INTEGER

(* %nonassoc OR_KW *)
(* %left DOT (* Below OR_KW (a.b or c.d) *) *)
(* %nonassoc COLON *)

%start<Onix_ast.expression> onix
%%

%inline mkrhs(symb): symb
  {
    mk_with_loc $startpos $endpos $1
  }

onix:
  | e = expression EOF { e }

expression:
  | ed = mkrhs(expression_desc) { ed }
  | e = simple_expression { e }

simple_expression:
  | PAREN_L e = expression PAREN_R { e }
  | ed = mkrhs(simple_expression_desc) { ed }

simple_expression_desc:
  | x = ID { Onix_ast.Var x }
  | c = constant { Onix_ast.Constant c }

expression_desc:
  | e1 = simple_expression; e2 = simple_expression { Onix_ast.Fun_app (e1, e2) }
  | ap = access_path { Onix_ast.Access_path ap }
  | p = pattern COLON e = expression { Onix_ast.Lambda ( p, e) }
  | record = record_expr { Onix_ast.Record record }
  | LET_KW bindings = list(letb_def) IN_KW e = expression
  { Onix_ast.Let (bindings, e) }


access_path:
  | e = simple_expression DOT f = ap_field { Onix_ast.Ap_field (e, f, None) }
  | e = simple_expression DOT f = ap_field OR_KW e2 = expression
    { Onix_ast.Ap_field (e, f, Some e2) }

ap_field:
  | fd = mkrhs(ap_field_desc) { fd }

ap_field_desc:
  | x = ID { Onix_ast.Fdesc_identifier x }
  | DOLLAR_BRACE e = expression BRACE_R { Onix_ast.Fdesc_interpol e }
  (* TODO : add strings *)

constant:
  | i = INTEGER { Onix_ast.Cst_int i }

pattern:
  | desc = mkrhs(pattern_desc) { desc }

pattern_desc:
  | x = ID { Onix_ast.Pvar x }
  | p = nontrivial_pattern { Onix_ast.Pnontrivial p }
  | p = nontrivial_pattern AROBASE x = ID
  | x = ID AROBASE p = nontrivial_pattern
  { Onix_ast.Paliased (p, x) }

nontrivial_pattern:
  | BRACE_L fields = separated_list(COMMA, field_pattern) BRACE_R
  { Onix_ast.Precord (fields, Onix_ast.Closed, None) }

field_pattern:
  | x = ID default = option(default_value) { (x, default) }

default_value:
  | QUESTION_MARK e = expression { e }

record_expr:
  | REC_KW re = record_def { Onix_ast.{ recursive = true; fields = re } }
  | re = record_def { Onix_ast.{ recursive = false; fields = re } }

record_def:
  | BRACE_L fields = list(field_def) BRACE_R
  { fields }

field_def:
  | f = mkrhs (field_def_descr) { f }

field_def_descr:
  | ap = access_path EQUAL e = expression SEMICOLON
  { Onix_ast.Field_definition (ap, e) }
  | INHERIT_KW base_e = option(inherit_base_expr) es = list(mkrhs (ID)) SEMICOLON
  { Onix_ast.Inherit (base_e, es) }

inherit_base_expr:
  | PAREN_L e = expression PAREN_R { e }

letb_def:
  | ap = access_path EQUAL e = expression SEMICOLON
  { Onix_ast.Bdef (ap, e) }
  | INHERIT_KW base_e = option(inherit_base_expr) es = list(mkrhs (ID)) SEMICOLON
  { Onix_ast.Binherit (base_e, es) }
