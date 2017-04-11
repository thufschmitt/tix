open OUnit2

let test_parsing src ast _ctx =
  assert_equal
    (Onix_parser.onix (Onix_lexer.read) (Lexing.from_string src))
    ast

let test_let =
  test_parsing
    "let x.y = 1; in y"
    (Onix_ast.Let
       ([Onix_ast.Bdef
           (Onix_ast.Ap_field (Onix_ast.Var "x",
                               Onix_ast.Fdesc_identifier "y", None),
            Onix_ast.Constant (Onix_ast.Cst_int 1))],
        Onix_ast.Var "y"))

let test_lambda = test_parsing
    "x: x"
    (Onix_ast.Lambda (Onix_ast.Pvar "x", Onix_ast.Var "x"))

let test_pattern_lambda_1 = test_parsing
    "{x ? 1}@y: x"
    (Onix_ast.Lambda
       (Onix_ast.Paliased
          (Onix_ast.Precord
             ([("x",
                Some (Onix_ast.Constant (Onix_ast.Cst_int 1)))],
              Onix_ast.Closed, None),
           "y"),
        Onix_ast.Var "x"))

let parsing =
  "parsing">:::
  ["let">:: test_let;
   "lambda">:: test_lambda;
   "pattern_1">:: test_pattern_lambda_1
  ]

let () =
  run_test_tt_main parsing
