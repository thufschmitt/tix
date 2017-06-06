open OUnit2
module TA = Type_annotations
module T  = Typing.Types

exception ParseError

let parse tokens =
  match Parse.Parser.onix Parse.Lexer.read tokens with
  | Some s -> Simple.Of_onix.expr s
  | None -> raise ParseError

let typ s =
  let maybe_t =
    CCOpt.flat_map
      (fun t -> Typing.(Annotations.to_type Types.Environment.default t))
      (Parse.Parser.typ Parse.Lexer.read (Lexing.from_string s))
  in
  CCOpt.get_exn maybe_t

let infer tenv env tokens =
  parse tokens
  |> Typing.(Typecheck.Infer.expr tenv env)

let check tenv env tokens expected_type =
  Typing.(Typecheck.Check.expr tenv env (parse tokens) expected_type)

let test_infer_expr input expected_type _ =
  let expected_type = typ expected_type in
  let typ =
    let open Typing in
    infer Types.Environment.default
      Typing_env.initial
      (Lexing.from_string input)
  in
  assert_equal
    ~cmp:T.T.equiv
    ~printer:T.T.Print.string_of_type
    expected_type
    typ

let test_check input expected_type _=
  let expected_type = typ expected_type in
  let tast =
    let open Typing in
    check
      Types.Environment.default
      Typing_env.initial
      (Lexing.from_string input)
      expected_type
  in ignore tast

let test_var _ =
  let tenv = Typing.(Typing_env.(add "x" Types.Builtins.int initial)) in
  let typ =
    infer Typing.Types.Environment.default tenv (Lexing.from_string "x")
  in
  assert_equal Typing.Types.Builtins.int typ

let test_fail typefun _ =
  try
    ignore @@ typefun ();
    assert_failure "type error not detected"
  with Typing.Typecheck.TypeError _ -> ()

let test_infer_expr_fail input =
  test_fail @@ fun () ->
  let open Typing in
  infer
    Types.Environment.default
    Typing_env.initial
    (Lexing.from_string input)

let test_check_fail input expected_type =
  let expected_type = typ expected_type in
  test_fail @@ fun () ->
  let open Typing in
  check
    Types.Environment.default
    Typing_env.initial
    (Lexing.from_string input)
    expected_type

let one_singleton = T.Builtins.interval @@ T.Intervals.singleton_of_int 1

let testsuite =
  "typecheck">:::
  [
    (* ----- Positive tests ----- *)
    "infer_var">::test_var;
    "infer_const_int">:: test_infer_expr "1" "1";
    "infer_const_bool">:: test_infer_expr "true" "true";
    "infer_builtins_not">:: test_infer_expr "__not"
      "((true -> false) & (false -> true))";
    "infer_lambda">:: test_infer_expr "x /*: Int */: 1" "Int -> 1";
    "infer_lambda_var">:: test_infer_expr "x /*: Int */: x" "Int -> Int";
    "infer_apply">:: test_infer_expr "(x /*: Int */: x) 1" "Int";
    "infer_arrow_annot">:: test_infer_expr
      "x /*: Int -> Int */: x"
      "(Int -> Int) -> Int -> Int";
    "infer_let_1">:: test_infer_expr "let x = 1; in x" "1";
    "infer_let_2">:: test_infer_expr "let x /*:Int*/ = 1; in x" "Int";
    "infer_let_3">:: test_infer_expr "let x /*:Int*/ = 1; y = x; in y" "Int";
    "infer_let_4">:: test_infer_expr "let x = 1; y = x; in y" "?";
    "infer_let_5">:: test_infer_expr "let x = x; in x" "?";
    "infer_let_6">:: test_infer_expr "let x /*: Int -> Int */ = y: y; in x"
      "Int -> Int";
    "infer_let_7">:: test_infer_expr "let x /*: Int -> Int -> Int */ = \
                                      y: y: y; in x"
      "Int -> Int -> Int";
    "infer_shadowing">:: test_infer_expr "let x = true; in let x = 1; in x" "1";
    "infer_union">:: test_infer_expr "x /*: Int | Bool */: x"
      "(Int | Bool) -> (Int | Bool)";
    "infer_intersection">:: test_infer_expr "x /*: Int & Int */: x"
      "Int -> Int";
    "test_not_true">:: test_infer_expr "__not true" "false";
    (* "test_list">:: test_infer_expr "[1 true false]" *)
    (*   "Cons (1, Cons(true, Cons(false, nil)))"); *)

    (* ----- Negative tests ----- *)
    "infer_fail_unbound_var">:: test_infer_expr_fail "x";
    "infer_fail_apply">:: test_infer_expr_fail "1 1";
    "infer_fail_apply2">:: test_infer_expr_fail "(x /*: Bool */: x) 1";
    "infer_fail_apply3">:: test_infer_expr_fail "(x /*: Int */: x) true";
    "infer_fail_notalist">:: test_infer_expr_fail "Cons (1, 2)";

    (* ------ positive check ----- *)
    "check_const_one">:: test_check "1" "1";
    "check_const_int">:: test_check "1" "Int";
    "check_const_union">:: test_check "1" "1 | Bool";
    "check_arrow_1">:: test_check "x: x" "Int -> Int";
    "check_arrow_2">:: test_check "x: x" "1 -> Int";
    "check_intersect_arrow">:: test_check "x: x"
      "(Int -> Int) & (Bool -> Bool)";
    "check_let">:: test_check "let x = 1; in y: y" "Int -> Int";

    (* ------ negative check ----- *)
    "check_fail_const_int">:: test_check_fail "1" "Bool";
    "check_fail_unbound_var">:: test_check_fail "x" "1";
    "check_fail_bad_intersect_arrow">:: test_check_fail "x: x"
      "(Int -> Bool) & (Bool -> Int)";
    "check_fail_inside_let">:: test_check_fail "let x = y: y; in x"
      "Int -> Int";
  ]
