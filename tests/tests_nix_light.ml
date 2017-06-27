open OUnit2

exception ParseError


let test_parse_pp_str ?(isTodo=false) input expected_output _ =
  if isTodo then todo "Not implemented yet";
  let output =
    begin
      match Parse.Parser.parse_string Parse.Parser.expr input with
      | Ok x ->
        Simple.Of_onix.expr x
        |> fun s -> Simple.Pp.pp_expr Format.str_formatter s
      | Error _ -> raise ParseError
    end;
    Format.flush_str_formatter ()
  in
  assert_equal
    ~printer:(fun x -> x)
    expected_output
    output

let test_parse_pp_str_fail ?(isTodo=false) input _ =
  if isTodo then todo "Not implemented yet";
  begin
    match Parse.Parser.parse_string Parse.Parser.expr input with
    | Ok x ->
      begin
        try
          ignore @@ Simple.Of_onix.expr x;
          assert_failure "Translation error not raised"
        with
          Failure _ -> ()
      end

    | Error _ -> raise ParseError
  end

let isTodo = true (* To use [~isTodo] as a shortcut for [~isTodo=true] *)

let testsuite =
  "nix_light">:::
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
      "test_annot", "(x /*: int */)", "(x /*: int */)";
      "test_annot_arrow", "(x /*: int -> int */)", "(x /*: (int) -> int */)";
      "test_string", "\"x\"", "\"x\"";
      "test_list", "[1 2 3]", "Cons(1, Cons(2, Cons(3, nil)))";
      "test_record_1", "{ x = 1; y = 2; }", "{ \"x\" = 1; \"y\" = 2; }";
      "test_record_2", "{ x.y = 1; }", "{ \"x\" = { \"y\" = 1; }; }";
      ("test_record_3",
       "{ x.y = 1; x.z = 2; }",
       "{ \"x\" = { \"y\" = 1; \"z\" = 2; }; }");
      "test_apath", "x.y.${\"z\"}", "x.\"y\".\"z\"";
    ] @
  List.map (fun (name, input) ->
      name >:: test_parse_pp_str_fail input)
    [
      "test_record_fail_1", "{ x = 1; x = 2; }";
      "test_record_fail_2", "{ x.y = 1; x.y = 2; }";
    ]
