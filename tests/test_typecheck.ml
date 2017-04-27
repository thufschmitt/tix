open OUnit2
module T = Tix_types

let get_type node =
  node.Onix_location.With_loc.description.Typed_ast.With_type.typ

let test_typecheck_expr input expected_type _ =
  let tast =
    Onix_parser.onix Onix_lexer.read (Lexing.from_string input)
    |> Nl_of_onix.expr
    |> Typecheck.expr Typing_env.empty
  in
  assert_equal
    expected_type
    (get_type tast)

let testsuite =
  "tix_typecheck">:::
  [
    "test_const_int">:: test_typecheck_expr "1" (T.BaseType T.Int);
    "test_const_bool">:: test_typecheck_expr "true" (T.BaseType T.Bool);
    "test_lambda">:: test_typecheck_expr "x: /*: int -> int */ x"
      T.(Arrow (BaseType Int, BaseType Int))
  ]
