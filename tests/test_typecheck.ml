open OUnit2
module T = Tix_types

exception ParseError

let get_type node =
  node.Onix_location.With_loc.description.Typed_ast.With_type.typ

let test_typecheck_expr input expected_type _ =
  let tast =
    begin
    match Onix_parser.onix Onix_lexer.read (Lexing.from_string input) with
    | Some s ->
      Nl_of_onix.expr s
      |> Typecheck.expr Typing_env.empty
    | None -> raise ParseError
    end;
  in
  assert_equal
    expected_type
    (get_type tast)

let testsuite =
  "tix_typecheck">:::
  [
    "test_const_int">:: test_typecheck_expr "1" (T.BaseType T.Int);
    "test_const_bool">:: test_typecheck_expr "true" (T.BaseType T.Bool);
  ]
