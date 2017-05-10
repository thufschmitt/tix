open OUnit2
module T = Tix_types

exception ParseError

let get_type node =
  node.Onix.Location.With_loc.description.Typed_ast.With_type.typ

let test_typecheck_expr input expected_type _ =
  let tast =
    begin
    match Onix.Parser.onix Onix.Lexer.read (Lexing.from_string input) with
    | Some s ->
      Nl.Of_onix.expr s
      |> Typecheck.expr Typing_env.empty
    | None -> raise ParseError
    end;
  in
  assert_equal
    expected_type
    (get_type tast)

let test_typecheck_expr_fail input _ =
  begin
    match Onix.Parser.onix Onix.Lexer.read (Lexing.from_string input) with
    | Some s ->
      begin try
          Nl.Of_onix.expr s
          |> Typecheck.expr Typing_env.empty
          |> ignore;
          assert_failure "Type error not detected"
        with
          Typecheck.TypeError _ -> ()
      end
    | None -> raise ParseError
  end

let testsuite =
  "tix_typecheck">:::
  [
    "test_const_int">:: test_typecheck_expr "1" (T.BaseType T.Int);
    "test_const_bool">:: test_typecheck_expr "true" (T.BaseType T.Bool);
    "test_lambda">:: test_typecheck_expr "x /*: int */: 1"
      T.(Arrow (BaseType Int, BaseType Int));
    "test_lambda_var">:: test_typecheck_expr "x /*: int */: x"
      T.(Arrow (BaseType Int, BaseType Int));
    "test_fail_unbound_var">:: test_typecheck_expr_fail "x";
    "test_apply">:: test_typecheck_expr "(x /*: int */: x) 1" T.(BaseType Int);
    "test_fail_apply2">:: test_typecheck_expr_fail "(x /*: bool */: x) 1";
    "test_fail_apply3">:: test_typecheck_expr_fail "(x /*: int */: x) true";
  ]
