open OUnit2
module TA = Common.Type_annotations
module T  = Typing.Types

module W = Typing.Typecheck.W

open W.Infix

exception ParseError of string

let parse str =
  match Parse.Parser.(parse_string expr) str with
  | Ok s -> Simple.Of_onix.expr s
  | Error (msg, _) -> raise (ParseError msg)

let typ str =
  match Parse.Parser.(parse_string typ) str with
  | Ok t -> Typing.Typecheck.W.value
              Typing.(Annotations.to_type Types.Environment.default t)
  | Error (msg, _) -> raise (ParseError msg)

let infer env tokens =
  parse tokens >>=
  Typing.(Typecheck.Infer.expr env)

let check env tokens expected_type =
  parse tokens >>= fun ast ->
  Typing.(Typecheck.Check.expr env ast expected_type)

let test_infer_expr input expected_type _ =
  let expected_type = typ expected_type in
  let typ =
    let open Typing in
    infer Environment.default input
  in
  Typing.Typecheck.W.iter
    (assert_equal
       ~cmp:T.T.equiv
       ~printer:T.T.Print.string_of_type
       expected_type)
    typ

let test_check input expected_type _=
  let expected_type = typ expected_type in
  let tast =
    let open Typing in
    check
      Environment.default
      input
      expected_type
  in ignore tast

let test_var _ =
  let typ =
    infer
      Typing.Environment.(add_value default "x" Typing.Types.Builtins.int)
      "x"
  in
  Typing.Typecheck.W.iter (assert_equal Typing.Types.Builtins.int) typ

let test_fail typefun _ =
  let result = typefun () in
  if Typing.Typecheck.W.log result = Common.Warning.List.empty then
    assert_failure "type error not detected"

let test_infer_expr_fail input =
  test_fail @@ fun () ->
  let open Typing in
  infer Environment.default input

let test_check_fail input expected_type =
  let expected_type = typ expected_type in
  test_fail @@ fun () ->
  let open Typing in
  check Environment.default  input
    expected_type

let one_singleton = T.Builtins.interval @@ T.Intervals.singleton_of_int 1

let testsuite =
  "typecheck">:::

  ("infer_var">::test_var) ::
  (* ----- Positive tests ----- *)
  List.map (fun (name, expr, result) -> name >:: test_infer_expr expr result)
    [
      "infer_const_int", "1", "1";
      "infer_const_bool", "true", "true";
      "infer_builtins_not", "__not", "((true -> false) & (false -> true))";
      "infer_lambda", "x /*: Int */: 1", "Int -> 1";
      "infer_lambda_var", "x /*: Int */: x", "Int -> Int";
      "infer_apply", "(x /*: Int */: x) 1", "Int";
      ("infer_arrow_annot", "x /*: Int -> Int */: x",
       "(Int -> Int) -> Int -> Int");
      "infer_let_1", "let x = 1; in x", "1";
      "infer_let_2", "let x /*:Int*/ = 1; in x", "Int";
      "infer_let_3", "let x /*:Int*/ = 1; y = x; in y", "Int";
      "infer_let_4", "let x = 1; y = x; in y", "?";
      "infer_let_5", "let x = x; in x", "?";
      "infer_let_6", "let x /*: Int -> Int */ = y: y; in x", "Int -> Int";
      ("infer_let_7", "let x /*: Int -> Int -> Int */ = y: y: y; in x",
       "Int -> Int -> Int");
      "infer_shadowing", "let x = true; in let x = 1; in x", "1";
      "infer_union", "x /*: Int | Bool */: x", "(Int | Bool) -> (Int | Bool)";
      "infer_intersection", "x /*: Int & Int */: x", "Int -> Int";
      "test_not_true", "__not true", "false";
      "test_list", "[1 true false]", "[1 true false]";
      ("infer_ite_classic", "let x /*: Bool */ = true; in if x then 1 else 2",
       "1 | 2");
      "infer_ite_dead_branch", "if true then 1 else __add 1 true", "1";
      ("infer_ite_typecase_1",
       "let x /*: Int | Bool */ = 1; in if isInt x then x else __not x",
       "Int | Bool");
      "infer_plus", "1 + 1", "Int";
      "infer_string", "\"aze\"", "\"aze\"";
      "infer_string_annot", "x /*: \"foo\" */: x", "\"foo\" -> \"foo\"";
      ("infer_record_pattern",
       "{ x /*: Bool */, y ? 1 /*: Int */ }: x",
       "{ x= Bool; y =? Int } -> Bool");
      "infer_arrow_no_annot_1", "x: x", "? -> ?";
      "infer_arrow_no_annot_2", "x: y: y", "? -> ? -> ?";
      "gradual_apply", "(x: x) 1", "?";
      "gradual_apply_2", "let z = z; in z 1", "?";
      "gradual_apply_3", "let z = z; in z z", "?";
      "infer_record_1", "{ x = 1; y = 2; }", "{ x = 1; y = 2; }";
      ("infer_recursive_record",
       "rec { x = 1; y = x; z /*: Int */ = x; }",
       "{ x = 1; y = ?; z = Int; }");
      "infer_path", "./foo", "./foo";
      "infer_record_access1", "{ x = 1; }.x", "1";
      "infer_record_access2", "{ x.y = 1; }.x.y", "1";
      "infer_record_access_dynamic", "{ x = 1; }.${\"x\"}", "1";
    ] @
  (* ----- Negative tests ----- *)
  List.map (fun (name, expr) -> name >:: test_infer_expr_fail expr)
    [
      "infer_fail_unbound_var", "x";
      "infer_fail_apply", "1 1";
      "infer_fail_apply2", "(x /*: Bool */: x) 1";
      "infer_fail_apply3", "(x /*: Int */: x) true";
      ("infer_fail_ite_not_bool_cond",
       "let x /*: Int | Bool */ = 1; in if x then 1 else 1");
      ("infer_fail_ite_no_refine_1",
       "let x /*: Bool */ = true; in if x then __add x 1 else x");
      ("infer_fail_ite_no_refine_2",
       "let f /*: Int -> Bool */ = x: true; x = 1; \
        in if f x then __add x 1 else __not x");
      "infer_fail_plus_not_int", "1 + true";
      "infer_fail_false_string", "(\"false\" /*: false*/)";
      "infer_fail_record_access", "{ x = 1; }.y";
      "infer_fail_record_access_dynamic", "{ x = 1; }.${(\"x\" /*: String */)}";
    ] @
  (* ------ positive check ----- *)
  List.map (fun (name, expr, result) -> name >:: test_check expr result)
    [
      "check_const_one", "1", "1";
      "check_const_int", "1", "Int";
      "check_const_union", "1", "1 | Bool";
      "check_arrow_1", "x: x", "Int -> Int";
      "check_arrow_2", "x: x", "1 -> Int";
      "check_intersect_arrow", "x: x", "(Int -> Int) & (Bool -> Bool)";
      "check_let", "let x = 1; in y: y", "Int -> Int";
      "check_ite", "let x /*: Bool */ = true; in if x then 1 else 2", "Int";
      ("check_ite_refine",
       "let x /*: Int | Bool */ = 1; in if isInt x then __add x 1 else true",
       "Int | true");
      ("check_ite_dead_branch",
       "let x = true; in if x then true else false",
       "true");
      "check_cons", "[1]", "[1]";
      "check_cons_union", "[1]", "[1] | [ Bool ]";
      "check_add", "1 + 1", "Int";
      "check_minus", "1 - 1", "Int";
      "check_unary_minus", "- (-1)", "1";
      "check_gradual", "1", "?";
      "check_gradual_lambda", "x: x", "? -> ?";
    ] @
  List.map (fun (name, expr, result) -> name >:: test_check_fail expr result)
    [
      (* ------ negative check ----- *)
      "check_fail_const_int", "1", "Bool";
      "check_fail_unbound_var", "x", "1";
      "check_fail_bad_intersect_arrow", "x: x", "(Int -> Bool) & (Bool -> Int)";
      "check_fail_inside_let", "let x = y /*: Bool */: y; in x", "Int -> Int";
      "check_fail_ite_not_bool", "if 1 then 1 else 1", "Int";
      "check_fail_cons", "[1]", "[ Bool ]";
      "check_fail_cons_length", "[1]", "[ 1 1 ]";
      "check_fail_unary_minus", "-1", "1";
    ]
