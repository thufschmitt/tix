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

%start<Onix_ast.expression> onix
%%

onix:
  | e = expression EOF { e }

expression:
  | x = ID { Onix_ast.Var x }
  | PAREN_L e = expression PAREN_R { e }
  | ap = access_path { Onix_ast.Access_path ap }
  | c = constant { Onix_ast.Constant c }
  | p = pattern COLON e = expression { Onix_ast.Lambda ( p, e) }
  | e1 = expression; e2 = expression { Onix_ast.Fun_app (e1, e2) }
  | record = record_expr { Onix_ast.Record record }
  | LET_KW bindings = list(letb_def) IN_KW e = expression
  { Onix_ast.Let (bindings, e) }


access_path:
  | e = expression DOT f = field_desc { Onix_ast.Ap_field (e, f, None) }
  | e = expression DOT f = field_desc OR_KW e2 = expression
    { Onix_ast.Ap_field (e, f, Some e2) }

field_desc:
  | x = ID { Onix_ast.Fdesc_identifier x }
  | DOLLAR_BRACE e = expression BRACE_R { Onix_ast.Fdesc_interpol e }
  (* TODO : add strings *)

constant:
  | i = INTEGER { Onix_ast.Cst_int i }

pattern:
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
  | ap = access_path EQUAL e = expression SEMICOLON
  { Onix_ast.Field_definition (ap, e) }
  | INHERIT_KW base_e = option(inherit_base_expr) es = list(ID) SEMICOLON
  { Onix_ast.Inherit (base_e, es) }

inherit_base_expr:
  | PAREN_L e = expression PAREN_R { e }

letb_def:
  | ap = access_path EQUAL e = expression SEMICOLON
  { Onix_ast.Bdef (ap, e) }
  | INHERIT_KW base_e = option(inherit_base_expr) es = list(ID) SEMICOLON
  { Onix_ast.Binherit (base_e, es) }
