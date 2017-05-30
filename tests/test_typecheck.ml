open OUnit2
module TA = Type_annotations
module T  = Typing.Types

exception ParseError

let get_type node =
  node.Parse.Location.With_loc.description.Typing.Typed_ast.With_type.typ

let test_typecheck_expr input expected_type _ =
  let tast =
    begin
    match Parse.Parser.onix Parse.Lexer.read (Lexing.from_string input) with
    | Some s ->
      Simple.Of_onix.expr s
      |> Typing.Typecheck.expr Typing.Types.Environment.default Typing.Typing_env.empty
    | None -> raise ParseError
    end;
  in
  assert_equal
    ~cmp:T.T.equiv
    ~printer:T.T.Print.string_of_type
    expected_type
    (get_type tast)

let test_var _ =
  let tast =
    begin
    match Parse.Parser.onix Parse.Lexer.read (Lexing.from_string "x") with
    | Some s ->
      Simple.Of_onix.expr s
      |> Typing.Typecheck.expr Typing.Types.Environment.default Typing.Typing_env.(add "x" Typing.Types.Builtins.int empty)
    | None -> raise ParseError
    end;
  in
  assert_equal
    Typing.Types.Builtins.int
    (get_type tast)

let test_typecheck_expr_fail input _ =
  begin
    match Parse.Parser.onix Parse.Lexer.read (Lexing.from_string input) with
    | Some s ->
      begin try
          Simple.Of_onix.expr s
          |> Typing.Typecheck.expr Typing.Types.Environment.default Typing.Typing_env.empty
          |> ignore;
          assert_failure "Type error not detected"
        with
          Typing.Typecheck.TypeError _ -> ()
      end
    | None -> raise ParseError
  end

let testsuite =
  "tix_typecheck">:::
  [
    "test_const_int">:: test_typecheck_expr "1"
      (T.(Builtins.interval (Intervals.singleton_of_int 1)));
    "test_const_bool">:: test_typecheck_expr "true" T.Builtins.true_type;
    "test_lambda">:: test_typecheck_expr "x /*: Int */: 1"
      (T.Builtins.(arrow
         int
         (interval @@ T.Intervals.singleton_of_int 1)));
    "test_lambda_var">:: test_typecheck_expr "x /*: Int */: x"
      (T.Builtins.(arrow int int));
    "test_fail_unbound_var">:: test_typecheck_expr_fail "x";
    "test_apply">:: test_typecheck_expr "(x /*: Int */: x) 1" T.Builtins.int;
    "test_fail_apply">:: test_typecheck_expr_fail "1 1";
    "test_fail_apply2">:: test_typecheck_expr_fail "(x /*: Bool */: x) 1";
    "test_fail_apply3">:: test_typecheck_expr_fail "(x /*: Int */: x) true";
  ]
  @
  [
    "test_var">::test_var;
  ]
