open OUnit2
module TA = Type_annotations
module T  = Typing.Types

exception ParseError

let get_type node =
  node.Parse.Location.With_loc.description.Typing.Typed_ast.With_type.typ

let test_infer_expr input expected_type _ =
  let tast =
    begin
      match Parse.Parser.onix Parse.Lexer.read (Lexing.from_string input) with
      | Some s ->
        Simple.Of_onix.expr s
        |> Typing.(Typecheck.Infer.expr
                     Types.Environment.default
                     Typing_env.empty)
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
        |> Typing.(Typecheck.Infer.expr
                     Types.Environment.default
                     Typing_env.(add "x" Types.Builtins.int empty))
      | None -> raise ParseError
    end;
  in
  assert_equal
    Typing.Types.Builtins.int
    (get_type tast)

let test_infer_expr_fail input _ =
  begin
    match Parse.Parser.onix Parse.Lexer.read (Lexing.from_string input) with
    | Some s ->
      begin try
          Simple.Of_onix.expr s
          |> Typing.(Typecheck.Infer.expr
                       Types.Environment.default
                       Typing_env.empty)
          |> ignore;
          assert_failure "Type error not detected"
        with
          Typing.Typecheck.TypeError _ -> ()
      end
    | None -> raise ParseError
  end

let one_singleton = T.Builtins.interval @@ T.Intervals.singleton_of_int 1

let testsuite =
  "tix_typecheck">:::
  [
    (* ----- Positive tests ----- *)
    "test_const_int">:: test_infer_expr "1" one_singleton;
    "test_const_bool">:: test_infer_expr "true" T.Builtins.true_type;
    "test_lambda">:: test_infer_expr "x /*: Int */: 1"
      (T.Builtins.(arrow int one_singleton));
    "test_lambda_var">:: test_infer_expr "x /*: Int */: x"
      (T.Builtins.(arrow int int));
    "test_apply">:: test_infer_expr "(x /*: Int */: x) 1" T.Builtins.int;
    "test_arrow_annot">:: test_infer_expr
      "x /*: Int -> Int */: x"
      T.Builtins.(arrow (arrow int int) (arrow int int));
    "test_let_1">:: test_infer_expr "let x = 1; in x" one_singleton;
    "test_let_2">:: test_infer_expr "let x /*:Int*/ = 1; in x"
      T.Builtins.int;
    "test_let_3">:: test_infer_expr "let x /*:Int*/ = 1; y = x; in y"
      T.Builtins.int;
    "test_let_4">:: test_infer_expr "let x = 1; y = x; in y"
      T.Builtins.grad;
    "test_let_5">:: test_infer_expr "let x = x; in x"
      T.Builtins.grad;
    "test_shadowing">:: test_infer_expr
      "let x = true; in let x = 1; in x"
      one_singleton;
    "test_union">:: test_infer_expr "x /*: Int | Bool */: x"
      T.Builtins.(arrow (cup int bool) (cup int bool));
    "test_intersection">:: test_infer_expr "x /*: Int & Int */: x"
      T.Builtins.(arrow int int);

    (* ----- Negative tests ----- *)
    "test_fail_unbound_var">:: test_infer_expr_fail "x";
    "test_fail_apply">:: test_infer_expr_fail "1 1";
    "test_fail_apply2">:: test_infer_expr_fail "(x /*: Bool */: x) 1";
    "test_fail_apply3">:: test_infer_expr_fail "(x /*: Int */: x) true";
  ]
  @
  [
    "test_var">::test_var;
  ]
