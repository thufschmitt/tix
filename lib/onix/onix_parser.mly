%token EOF
%token COLON
%token SEMICOLON
%token COMMA
%token DOT
%token OR_KW
%token DOLLAR_BRACE
%token BRACE_L
%token BRACE_R
%token QUESTION_MARK
%token<string> ID
%token<int> INTEGER

%start<Onix_ast.expression> onix
%%

onix:
  | e = expression EOF { e }

expression:
  | ap = access_path { Onix_ast.Access_path ap }
  | c = constant { Onix_ast.Constant c }
  | p = pattern COLON e = expression { Onix_ast.Lambda ( p, e) }
  | e1 = expression e2 = expression { Onix_ast.Fun_app (e1, e2) }

access_path:
  | x = ID { Onix_ast.Ap_var x }
  | x = ID DOT f = field_desc { Onix_ast.Ap_field (x, f, None) }
  | x = ID DOT f = field_desc OR_KW e = expression
    { Onix_ast.Ap_field (x, f, Some e) }

field_desc:
  | x = ID { Onix_ast.Fdesc_identifier x }
  | DOLLAR_BRACE e = expression BRACE_R { Onix_ast.Fdesc_interpol e }
  (* TODO : add strings *)

constant:
  | i = INTEGER { Onix_ast.Cst_int i }

pattern:
  | x = ID { Onix_ast.Pvar x }
  | BRACE_L fields = separated_list(COMMA, field_pattern) BRACE_R
  { Onix_ast.Precord (fields, Onix_ast.Closed, None) }

field_pattern:
  | x = ID default = option(default_value) { (x, default) }

default_value:
  | QUESTION_MARK e = expression { e }
