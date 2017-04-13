open OUnit2

exception ParseError


let test_parse_pp_str input expected_output _ =
  let output =
    Onix_parser.onix Onix_lexer.read (Lexing.from_string input)
    |> fun s -> Onix_pp.pp_expr Format.str_formatter s;
    Format.flush_str_formatter ()
  in
  assert_equal
    ~printer:(fun x -> x)
    expected_output
    output

let testsuite =
  "onix_parser">:::
    List.map (fun (name, input, output) ->
      name >:: test_parse_pp_str input output)
    [
      "test_var", "x", "x";
      "test_const_int", "1234", "1234";
      "test_const_true", "true", "true";
      "test_const_false", "false", "false";
      "test_lambda", "x: x", "(x: x)";
      "test_app", "x y", "x y";
      "test_lambda_app", "(x: y) z", "(x: y) z";
      "test_app_lambda", "x: y z", "(x: y z)";
      "test_Y_comb", "(x: x x) (x: x x)", "(x: x x) (x: x x)";
    ]
